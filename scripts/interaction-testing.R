

#' Performing interaction testing, to investigate interactions of different
#' causal pathways (direct, indirect, spurious). In total, 5 interactions are 
#' tested: (1) TE x SE, for the interaction of total (causal) and spurious paths,
#' (2) DE x IE, interaction of direct and indirect,
#' (3) DE x SE, direct and spurious, (4) IE x SE, indirect and spurious,
#' (5) DE x IE x SE, direct, indirect and spurious.
ricu:::init_proj()
target_ia <- c("TE x SE", "DE x IE", "DE x SE", "IE x SE", "DE x IE x SE")

for (src in c("anzics", "miiv", "nzics", "aics")) {
  
  dat <- load_data(src)
  dat[, death := as.integer(death)]
  if (is.element("country", names(dat))) 
    dat[, country := as.integer(country == "Australia")]
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  
  for (scale in c(FALSE, TRUE)) {
    
    # Performing interaction testing
    res <- ia_tests(as.data.frame(dat[, c(X, Z, W, Y), with = FALSE]), 
                    X, Z, W, Y, log_scale = scale)
    
    cat("Interaction testing for", src, "with log_scale =", scale, "\n")
    # Computing the p-values
    for (ia_type in target_ia) {
      
      pval <- 2 * pnorm(-abs(res[ia == ia_type]$psi_osd / res[ia == ia_type]$dev))
      cat("p-value for interaction", ia_type, "=", pval, "\n")
    }
  }
}
