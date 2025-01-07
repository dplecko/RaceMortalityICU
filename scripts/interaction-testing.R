
#' Performing interaction testing, to investigate interactions of different
#' causal pathways (direct, indirect, spurious). In total, 5 interactions are 
#' tested: (1) TE x SE, for the interaction of total (causal) and spurious paths,
#' (2) DE x IE, interaction of direct and indirect,
#' (3) DE x SE, direct and spurious, (4) IE x SE, indirect and spurious,
#' (5) DE x IE x SE, direct, indirect and spurious.
# nohup taskset -c 0-63 Rscript scripts/interaction-testing.R > ia-pvals.log 2>&1 &
ricu:::init_proj()
set.seed(2024)

# specify all the target interactions
target_ia <- c("TE x SE", "DE x IE", "DE x SE", "IE x SE", "DE x IE x SE")

# choose MIMIC-IV and ANZICS APD data sources
for (src in c("miiv", "aics")) {
  
  # load the data from the target source
  dat <- load_data(src, split_elective = TRUE)
  dat[, death := as.integer(death)]
  
  # print the Standard Fairness Model information
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  print_sfm(X, Z, W, Y)
  
  for (risk in c(FALSE, TRUE)) {
    
    cat("Interaction testing for", src, "with log_risk =", risk, "\n")
    
    # perform interaction testing
    res <- one_step_debias(as.data.frame(dat[, c(X, Z, W, Y), with = FALSE]), 
                           X, Z, W, Y, log_risk = risk)
    res <- as.data.table(res)
    
    # compute the p-values
    res[, pval := 2 * pnorm(-abs(value) / sd)]
    
    # print the p-values
    for (ia_type in target_ia)
      cat("p-value for interaction", ia_type, "=", res[measure == ia_type]$pval,
          "\n")
  }
}
