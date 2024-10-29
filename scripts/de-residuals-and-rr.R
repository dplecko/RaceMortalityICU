
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
  src <- "anzics"
  dat <- load_data(src, outcome = outcome)
  root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
  load(file.path(root, "data", paste0("de-crf-", outcome, ".RData")))
  
  dat <- dat[, de_mean := rowMeans(de_crf, na.rm = TRUE)]
  dat <- if (country == "AU") dat[country == "AU"] else dat[country == "NZ"]
  
  res <- merge(
    res, dat[, list(de_avg = mean(de_mean), .N), by = "apache_iii_diag"],
    by = "apache_iii_diag"
  )
  
  res[, Admission := ifelse(apache_iii_diag >= 3000, "Surgical (Elective)", 
                            ifelse(apache_iii_diag >= 1200, "Surgical (Emergency)",
                                   "Medical"))]
  
  res[, outcome := ifelse(outcome == "death", "In-hospital Mortality", "Readmission")]
  res[, country := ifelse(country == "AU", "Australia", "New Zealand")]
  
  lmod <- lm(de_avg ~ rr, data = res[!is.infinite(rr)], 
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
  rr[!is.infinite(rr)], aes(x = rr, y = de_avg, size = size, color = Admission)) +
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
