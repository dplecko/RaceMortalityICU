
ricu:::init_proj()

#' * effects of minority status on illness severity *
set.seed(2024)
ils_res <- c()
ils_decomp_plts <- list()
for (src in c("aics", "miiv")) {

  dat <- load_data(src, split_elective = TRUE)
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  Y <- if (src == "aics") "apache_iii_rod" else "acu_24"
  
  # Decomposing the disparity
  fcb_ils <- fairness_cookbook(
    data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1,
    method = "debiasing"
  )
  
  meas <- summary(fcb_ils)$measures
  ils_res <- cbind(meas, src = src)
  
  ils_decomp_plts[[src]] <-
    autoplot(fcb_ils) + 
    geom_col(fill = "white", color = "black") +
    geom_errorbar(aes(x = Measure, ymin = Value - 1.96 * StdDev,
                      ymax = Value + 1.96 * StdDev), width = 0.5) +
    ggtitle(ifelse(src == "miiv", "United States", "Australia")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
    )
}

cowplot::plot_grid(plotlist = ils_decomp_plts)
ggsave(file.path("results", "ils-de.png"), width = 10, height = 4)

# Linear model verification


