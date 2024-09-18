
ricu:::init_proj()

#' * effects of minority status on illness severity *

set.seed(2024)
ils_decomp_plts <- list()
for (src in c("anzics", "miiv")) {

  dat <- load_data(src)
  X <- "majority"
  
  if (src == "anzics") {
    
    
    Z <- c("age", "sex", "country")
    Y <- "apache_iii_rod"
    W <- c("apache_iii_diag")
  } else if (src == "miiv") {
    
    Z <- c("age", "sex")
    W <- c("charlson", "diag_index")
    Y <- "acu_24"
  }
  
  # Decomposing the disparity
  fcb_ils <- fairness_cookbook(
    data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1,
    model = "ranger", nboot1 = 5L
  )
  ils_decomp_plts[[src]] <-
    autoplot(fcb_ils) + 
    geom_col(fill = "white", color = "black") +
    geom_errorbar(aes(x = Measure, ymin = Value - 1.96 * StdDev, 
                      ymax = Value + 1.96 * StdDev), width = 0.5) +
    ggtitle(ifelse(src == "miiv", "United States", "Australia and New Zealand")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
    )
  
  lmod <- lm(as.formula(paste(Y, "~", paste0(c(X, Z, W), collapse = "+"))), 
             data = dat)
  print(summary(lmod))
}

cowplot::plot_grid(plotlist = ils_decomp_plts)
ggsave(file.path("results", "race-to-ils.png"), width = 10, height = 4)

# Linear model verification


