
ricu:::init_proj()

de_rr <- function(country, outcome, de_method = c("osd", "crf")) {
  
  src <- ifelse(country == "AU", "aics", "nzics")
  c(pop_dat, dat) %<-% pop_and_dat(country, full = TRUE)
  
  dat[, diag_grp := apache_iii_diag]
  
  # expand the grid of all cubes
  ts_risk <- as.data.table(
    expand.grid(age = unique(dat$age), diag_grp = unique(dat$diag_grp),
                majority = c(0, 1), year = unique(dat$year))
  )
  
  # merge the cubes with the admissions data
  ts_risk <- merge(
    ts_risk, dat[, list(admit = .N), by = c("age", "diag_grp", "majority", "year")],
    all.x = TRUE
  )
  ts_risk[is.na(admit), admit := 0]
  
  # merge in the population counts by cube
  ts_risk <- merge(
    ts_risk, pop_dat[, list(pop = sum(value)), by = c("age", "majority", "year")], 
    by = c("age", "majority", "year"), all.x = TRUE
  )
  
  # get the age distribution in the data
  wgh_dat <- dat[, .N, by = c("age")][, list(age = age, wgh = N/sum(N))]
  
  # compute risk = admitted / population; aggregate over different years!
  ts_risk <- ts_risk[, list(risk = sum(admit) / sum(pop)), 
                     by = c("diag_grp", "majority", "age")]
  
  # merge in the age distribution
  ts_risk <- merge(ts_risk, wgh_dat, by = c("age"), all.x = TRUE)
  
  # compute the age-weighted average risk by diagnostic group
  ts_risk <- ts_risk[, list(risk = sum(risk * wgh) / sum(wgh)), 
                     by = c("majority", "diag_grp")]
  
  # compute the risk ratio
  res <- ts_risk[, list(rr = risk[majority == 0] / risk[majority == 1]), 
                 by = c("diag_grp")]
  res <- setnames(res, "diag_grp", "apache_iii_diag")
  
  # compute the DE residuals
  res <- merge(res, de_residuals(src, outcome, method = de_method), 
               by = "apache_iii_diag")
  
  res[, Admission := ifelse(apache_iii_diag >= 3000, "Surgical (Elective)", 
                            ifelse(apache_iii_diag >= 1200, "Surgical (Emergency)",
                                   "Medical"))]
  
  res[, outcome := ifelse(outcome == "death", "In-hospital Mortality", "Readmission")]
  res[, country := ifelse(src == "aics", "Australia", "New Zealand")]
  
  res <- merge(res, dat[, list(diag_size = .N), by = c("apache_iii_diag")],
               by = "apache_iii_diag")

  lmod <- lm(de_resid ~ rr, data = res[!is.infinite(rr)], 
             weights = res[!is.infinite(rr)]$diag_size)
  
  attr(res, "lmod") <- lmod
  res
}

rr <- c()
for (country in c("AU", "NZ")) {

  for (outcome in c("death", "readm")) {
    
    rr <- rbind(rr, de_rr(country, outcome))
  }
}

ggplot(
  rr[!is.infinite(rr)], aes(x = rr, y = de_resid, size = diag_size, 
                            color = Admission)) +
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
