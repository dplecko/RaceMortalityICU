
ricu:::init_proj()

de_rr <- function(country, outcome, de_method = c("osd", "crf")) {
  
  src <- ifelse(country == "AU", "aics", "nzics")
  
  res <- rr_compute(NULL, "apache_iii")
  res <- setnames(res, "diag_grp", "apache_iii_diag")
  
  # compute the DE residuals
  res <- merge(
    res, 
    de_residuals(src, outcome, method = de_method), 
    by = "apache_iii_diag"
  )
  
  res[, Admission := ifelse(apache_iii_diag >= 3000, "Surgical (Elective)", 
                            ifelse(apache_iii_diag >= 1200, "Surgical (Emergency)",
                                   "Medical"))]
  
  res[, outcome := ifelse(outcome == "death", "In-hospital Mortality", "Readmission")]
  res[, country := ifelse(src == "aics", "Australia", "New Zealand")]
  
  dat <- load_data(src, outcome, split_elective = TRUE)
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
    
    rr <- rbind(rr, de_rr(country, outcome, de_method = "crf"))
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
  facet_grid(cols = vars(outcome), rows = vars(country), scales = "free_x") +
  geom_vline(xintercept = 1, color = "gray", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")

ggsave(file.path("results", "de-rr.png"), width = 12, height = 5)
