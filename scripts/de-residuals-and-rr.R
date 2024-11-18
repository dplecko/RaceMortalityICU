
ricu:::init_proj()

de_rr <- function(country, outcome) {
  
  c(pop_dat, dat) %<-% pop_and_dat(country, full = TRUE)
  
  dat[, diag_grp := apache_iii_diag]
  
  ts_risk <- as.data.table(
    expand.grid(age = unique(dat$age), diag_grp = unique(dat$diag_grp),
                majority = c(0, 1), year = unique(dat$year))
  )
  
  ts_risk <- merge(
    ts_risk, dat[, .N, by = c("age", "diag_grp", "majority", "year")],
    all.x = TRUE
  )
  ts_risk[is.na(N), N := 0]
  
  ts_risk <- merge(
    ts_risk, pop_dat[, sum(value), by = c("age", "majority", "year")], 
    by = c("age", "majority", "year"), all.x = TRUE
  )
  
  wgh_dat <- dat[, .N, by = c("age")][, list(age = age, wgh = N/sum(N))]
  
  ts_risk <- merge(
    ts_risk[, list(risk = sum(N) / sum(V1)), 
            by = c("diag_grp", "majority", "age")],
    wgh_dat, by = c("age"), all.x = TRUE
  )
  
  ts_risk <- ts_risk[, list(risk = sum(risk * wgh) / sum(wgh)), 
                     by = c("majority", "diag_grp")]
  
  res <- ts_risk[, list(rr = risk[majority == 0] / risk[majority == 1]), 
                 by = c("diag_grp")]
  res <- setnames(res, "diag_grp", "apache_iii_diag")
  
  # bring in the causal forest info
  src <- ifelse(country == "AU", "aics", "nzics")
  
  de_residuals <- function(src, outcome, method = c("osd", "crf")) {
    
    method <- match.arg(method, c("osd", "crf"))
    
    if (method == "osd") {
      
      dgs <- c(0, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 201, 
               202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 301, 
               303, 305, 306, 307, 308, 309, 310, 311, 312, 313, 401, 402, 403, 
               404, 405, 406, 407, 408, 409, 410, 501, 502, 503, 504, 601, 602, 
               603, 604, 605, 701, 702, 703, 704, 801, 802, 901, 902, 903, 1001, 
               1002, 1101, 1102, 1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208, 
               1209, 1210, 1211, 1212, 1213, 1301, 1302, 1303, 1304, 1401, 1403, 
               1404, 1405, 1406, 1407, 1408, 1409, 1410, 1411, 1412, 1413, 1501, 
               1502, 1503, 1504, 1505, 1506, 1601, 1602, 1603, 1604, 1605, 1701, 
               1703, 1704, 1705, 1801, 1802, 1803, 1901, 1902, 1903, 1904, 2101, 
               2201, 3202, 3203, 3204, 3205, 3206, 3207, 3208, 3209, 3210, 3211, 
               3212, 3213, 3301, 3302, 3303, 3304, 3401, 3403, 3404, 3405, 3406, 
               3407, 3408, 3409, 3410, 3411, 3412, 3413, 3501, 3502, 3503, 3504, 
               3505, 3506, 3601, 3602, 3603, 3604, 3605, 3701, 3703, 3704, 3705, 
               3801, 3802, 3803, 3902, 3903, 3904, 4101, 4201)
      
      E_lst <- lapply(dgs, function(i) list(apache_iii_diag = i, majority = 0))
      attr(E_lst, "E_names") <- data.frame(apache_iii_diag = dgs, 
                                           majority = "minority")
      names(E_lst) <- dgs
      dat <- load_data(src, split_elective = TRUE)
      dat <- dat[, diag_grp := floor(apache_iii_diag / 100)]
      c(X, Z, W, Y) %<-% attr(dat, "sfm")
      
      cde <- cnd_effect(as.data.frame(dat), X = X, Z = Z, W = W, Y = Y, 
                        E_lst = E_lst)
      cde <- as.data.table(cde)
      cde <- setnames(cde, "effect", "de_resid")
    } else if (method == "crf") {
      
      dat <- load_data(src, outcome = outcome)
      root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
      load(file.path(root, "data", paste0("de-crf-", src, "-", outcome, ".RData")))
      dat <- dat[, de_mean := rowMeans(de_crf, na.rm = TRUE)]
      cde <- dat[, list(de_resid = mean(de_mean)), by = "apache_iii_diag"]
    }
    
    cde
  }
  
  res <- merge(res, de_residuals(src, outcome), by = "apache_iii_diag")
  
  res[, Admission := ifelse(apache_iii_diag >= 3000, "Surgical (Elective)", 
                            ifelse(apache_iii_diag >= 1200, "Surgical (Emergency)",
                                   "Medical"))]
  
  res[, outcome := ifelse(outcome == "death", "In-hospital Mortality", "Readmission")]
  res[, country := ifelse(src == "aics", "Australia", "New Zealand")]
  
  lmod <- lm(de_resid ~ rr, data = res[!is.infinite(rr)], 
             weights = res[!is.infinite(rr)]$N)
  
  attr(res, "lmod") <- lmod
  res
}

rr <- c()
for (country in c("AU", "NZ")) {

  for (outcome in c("death", "readm")) {
    
    rr <- rbind(rr, de_rr(country, outcome))
  }
}

rr[, size := as.numeric(N / max(N)), by = c("outcome", "country")]
ggplot(
  rr[!is.infinite(rr)], aes(x = rr, y = de_resid, size = size, color = Admission)) +
  geom_point() + theme_bw() +
  xlab("Risk ratio for ICU admission") +
  ylab("Diagnosis-specific Direct Effect") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "bottom",
        legend.box.background = element_rect()) +
  guides(size = "none") +
  facet_grid(rows = vars(outcome), cols = vars(country)) +
  geom_vline(xintercept = 1, color = "gray", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")

ggsave(file.path("results", "de-rr.png"), width = 12, height = 8)
