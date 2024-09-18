
ricu:::init_proj()
dat <- load_data("anzics")

# ROD Analysis
rod_calibration <- function(rod, rod_col) {
  
  breaks <- c(-Inf, quantile(rod[[rod_col]], probs = seq(0.1, 0.9, 0.1)), Inf)
  rod[, decile := .bincode(get(rod_col), breaks = breaks)]
  
  plt_dat <- NULL
  for (boot in 1:50) {
    
    idx <- sample(nrow(rod), replace = TRUE)
    plt_dat <- rbind(
      plt_dat, rod[idx, mean(death), by = c("decile", "indig")]
    )
  }
  
  ggplot(
    plt_dat[, list(mean(V1), sd(V1)), by = c("decile", "indig")],
    aes(x = decile, y = V1, color = factor(indig), fill = factor(indig))
  ) + geom_line() + geom_point() + theme_bw() +
    geom_ribbon(aes(ymin = V1 - 1.96 * V2, ymax = V1 + 1.96 * V2), alpha = 0.3) +
    theme(
      legend.position = "bottom"
    ) + ylab("Average Mortality") + xlab("APACHE-III ROD Decile") +
    scale_fill_discrete(name = "Indigenous", labels = c("No", "Yes")) +
    scale_color_discrete(name = "Indigenous", labels = c("No", "Yes"))
}

# ROD Calibration
rod_calibration(dat, "apache_iii_rod")

# Re-calibrate Using Isotonic Regression
plt_dat <- NULL
for (grp in c(0, 1)) {
  
  idx <- which(dat$indig == grp)
  iso_mod <- isoreg(dat$apache_iii_rod[idx], y = as.numeric(dat$death[idx]))
  ord_inv <- order(order(iso_mod$x))
  x_ord <- iso_mod$x[iso_mod$ord]
  plt_dat <- rbind(
    plt_dat, 
    unique(data.frame(apache = x_ord, rcb_apache = iso_mod$yf, grp = grp))
  )
  dat[idx, rcb_ap3 := iso_mod$yf[ord_inv]]
}

rod_calibration(dat, "rcb_ap3")

# Testing if re-calibration works
ggplot(plt_dat, aes(x = apache, y = rcb_apache, color = factor(grp))) +
  geom_line() + theme_bw() + theme(legend.position = "bottom") +
  scale_color_discrete(name = "Group", labels = c("Majority", "First Nations"))
