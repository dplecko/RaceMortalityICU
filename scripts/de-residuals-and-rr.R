
ricu:::init_proj()

derr_plts <- list()

for (country in c("AU", "NZ")) {

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
  dat <- load_data(src)
  root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
  load(file.path(root, "data", "de_crf.RData"))
  
  dat <- dat[, de_mean := rowMeans(de_crf, na.rm = TRUE)]
  dat <- if (country == "AU") dat[country == "AU"] else dat[country == "NZ"]
  
  res <- merge(
    res, dat[, list(de_avg = mean(de_mean), .N), by = "apache_iii_diag"],
    by = "apache_iii_diag"
  )
  lmod <- lm(de_avg ~ rr, data = res[!is.infinite(rr)], 
             weights = res[!is.infinite(rr)]$N)
  cat("--- Country =", country, "---\n")
  print(summary(lmod))
  derr_plts[[country]] <- ggplot(
    res[!is.infinite(rr)], aes(x = rr, y = de_avg, size = N)) +
    geom_point(show.legend = FALSE) + theme_bw() +
    geom_abline(intercept = coef(lmod)[1], slope = coef(lmod)[2],
                color = "red", linewidth = 1.2) +
    xlab("Risk ratio for ICU admission") +
    ylab("Diagnosis-specific Direct Effect") +
    scale_y_continuous(labels = scales::percent) +
    ggtitle(ifelse(country == "AU", "Australia", "New Zealand")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
}

cowplot::plot_grid(plotlist = derr_plts, labels = c("(A)", "(B)"), ncol = 2L)
ggsave(file.path(root, "results", "de-rr.png"), width = 12, height = 4)
