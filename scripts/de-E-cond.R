
#' Inspection of conditional direct effects.
# nohup taskset -c 0-63 Rscript scripts/de-E-cond.R > de-E-cond.log 2>&1 &
ricu:::init_proj()
set.seed(2024)

# specify the data sources
srcs <- c("aics", "miiv")

# choose the method for conditional effect estimation (causal forests)
methods <- "crf"

# specify the outcomes: in-hospital mortality and readmission
outcomes <- c("death", "readm")
no_ttl <- TRUE

# specify events for computing conditional direct effects
E_sets <- list(C = c("diag_grp", "age"))

# create folds if they are not already there
for (src in srcs) {
  
  success <- tryCatch({
    config(src)
    TRUE
  }, error = function(e) FALSE)
  
  if (!success) {

    create_folds(src)
  }
}

for (src in srcs) {
  
  for (outcome in outcomes) {
    
    # load data for the target outcome
    dat <- load_data(src, outcome = outcome, split_elective = TRUE)
    if (src != "miiv") dat <- dat[, diag_grp := floor(apache_iii_diag / 100)]
    
    # print the Standard Fairness Model information
    c(X, Z, W, Y) %<-% attr(dat, "sfm")
    print_sfm(X, Z, W, Y)
    
    # go over the choice of methods
    for (method in methods) {
      
      # train the inference object
      cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                             method = method)
      
      # iterate over all conditioning set values
      for (i in seq_along(E_sets)) {
        
        sel_covs <- E_sets[[i]] # 
        sel_covs <- c(sel_covs, "majority")
        
        E_lst <- cmbn_E(sel_covs, src)
        cde <- infer(cndE_obj, E_lst)
        
        # create plot title
        fl_name <- paste(c(src, outcome, method, sel_covs, no_ttl), 
                         collapse = "-")
        ttl <- if (src == "miiv") "United States" else "Australia"
        if (no_ttl) ttl <- NULL
        
        # create plot depending on dimensionality
        E_plt <- plt_E_cnd(cde, sel_covs, ttl, 
                           flip_color = (outcome == "readm"))
        
        # save plot
        save_plt(E_plt, fl_name, width = 7, height = 5, bg = "white")
      }
    }
  }
}
