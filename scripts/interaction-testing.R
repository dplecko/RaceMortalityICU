
ricu:::init_proj()

target_ia <- c("TE x SE", "DE x IE", "DE x SE", "IE x SE", "DE x IE x SE")

# Constructing the SFM
X <- "majority"
Z <- c("age", "sex")
W <- c("apache_iii_rod", "apache_iii_diag")
Y <- "death"

for (src in c("anzics", "miiv")) {
  
  dat <- load_data(src)
  dat[, death := as.integer(death)]
  
  if (src == "miiv") {
    
    # Updating the SFM for new mediators
    Z <- c("age", "sex")
    W <- c("charlson", "acu_24", "diag_index")
  }
  
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
