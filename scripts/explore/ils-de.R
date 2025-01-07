
ricu:::init_proj()

#' * effects of minority status on illness severity *
set.seed(2024)
ils_res <- c()
ils_decomp_plts <- list()
for (src in c("aics", "miiv")) {

  dat <- load_data(src, split_elective = TRUE)
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  Y <- "acu_24"
  
  if (src == "aics") {
    
    dat <- merge(dat, load_concepts("sofa_anz", "anzics"), all.x = TRUE)
    dat <- rename_cols(dat, "acu_24", "sofa_anz")
    W <- setdiff(W, "apache_iii_rod")
  } else W <- setdiff(W, "acu_24")
  
  # Decomposing the disparity
  fcb_ils <- fairness_cookbook(
    data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1,
    method = "debiasing"
  )
  
  meas <- summary(fcb_ils)$measures
  ils_res <- rbind(ils_res, cbind(meas, src = src))
  
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
ggsave(file.path("results", "ils-de.png"), width = 10, height = 4, bg = "white")

# sf_dat <- c()
# for (src in c("aics", "miiv")) {
#   
#   dat <- load_data(src, split_elective = TRUE)
#   dat$src <- src
#   if (src == "aics") {
#     
#     dat <- merge(dat, load_concepts("sofa_anz", "anzics"), all.x = TRUE)
#     dat <- rename_cols(dat, "acu_24", "sofa_anz")
#   }
#   
#   wgh <- merge(dat[majority == 0, .N, by = "age"], dat[majority == 1, .N, by = "age"], by = "age")
#   wgh[, wgh := N.x / sum(N.x) / (N.y / sum(N.y))]
#   dat <- merge(dat, wgh, by = "age")
#   
#   ids <- sample.int(n = nrow(dat[majority == 1]), replace = TRUE, prob = dat[majority == 1]$wgh)
#   
#   dat_adj <- rbind(dat[majority == 0], dat[majority == 1][ids])
#   
#   sf_dat <- rbind(sf_dat, dat_adj[, c("death", "acu_24", "src", "majority", "age"), with=FALSE])
# }



