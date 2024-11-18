
cnd_effect <- function(data, X, Z, W, Y, E_lst) {
  
  E_to_ind <- function(E, data) {
    
    ind <- rep(TRUE, nrow(data))
    for (i in seq_along(E)) {
      
      var <- names(E)[i]
      ind <- ind & (data[[var]] %in%  E[[var]])
    }
    
    ind
  }
  
  #' Note: xz and xw are always equal for this implementation.
  n <- nrow(data)
  
  # split into K folds
  K <- 10
  folds <- sample(x = rep(1:K, each = ceiling(n / K))[1:n])
  
  # eta1 <- eta2 <- rep(0, NA)
  y <- data[[Y]]
  x <- data[[X]]
  pso <- list(list(), list())
  pso <- lapply(seq_along(E_lst), function(i) pso)
  
  for (ei in seq_along(E_lst)) 
    for (xy in c(0, 1))
      pso[[ei]][[xy+1]] <- rep(NA, n)
  
  for (i in seq_len(K)) {
    
    # split into dev, val, tst
    tst <- folds == i
    dev <- folds %in% setdiff(seq_len(K), i)[1:6]
    val <- folds %in% setdiff(seq_len(K), i)[7:9]
    
    # develop models on dev
    mod_x_z <- cv_xgb(data[dev, Z], data[dev, X])
    mod_x_zw <- cv_xgb(data[dev, c(Z, W)], data[dev, X])
    mod_y_xz <- cv_xgb(data[dev, c(X, Z)], data[dev, Y])
    mod_y_xzw <- cv_xgb(data[dev, c(X, Z, W)], data[dev, Y])
    
    # get the val set predictions
    px_zw_val <- pred_xgb(mod_x_zw, data[val, c(Z, W)])
    px_zw_val <- list(1 - px_zw_val, px_zw_val)
    px_z_val <- pred_xgb(mod_x_z, data[val, Z])
    px_z_val <- list(1 - px_z_val, px_z_val)
    y_xzw_val <- list(
      pred_xgb(mod_y_xzw, data[val, c(X, Z, W)], intervention = 0, X = X),
      pred_xgb(mod_y_xzw, data[val, c(X, Z, W)], intervention = 1, X = X)
    )
    
    # get the test set values
    px_zw_tst <- pred_xgb(mod_x_zw, data[tst, c(Z, W)])
    px_zw_tst <- list(1 - px_zw_tst, px_zw_tst)
    px_z_tst <- pred_xgb(mod_x_z, data[tst, Z])
    px_z_tst <- list(1 - px_z_tst, px_z_tst)
    y_xzw_tst <- list(
      pred_xgb(mod_y_xzw, data[tst, c(X, Z, W)], intervention = 0, X = X),
      pred_xgb(mod_y_xzw, data[tst, c(X, Z, W)], intervention = 1, X = X)
    )
    y_xz_tst <- list(
      pred_xgb(mod_y_xz, data[tst, c(X, Z)], intervention = 0, X = X),
      pred_xgb(mod_y_xz, data[tst, c(X, Z)], intervention = 1, X = X)
    )
    
    for (xy in c(0, 1)) {
      
      eta1 <- (y[tst] - y_xzw_tst[[xy+1]]) / px_zw_tst[[xy+1]]
      eta2 <- y_xzw_tst[[xy+1]]
      
      for (ei in seq_along(E_lst)) {
        
        E <- E_lst[[ei]]
        E_x <- E
        E_x[[X]] <- NULL # remove the X part if needed
        E_ind <- E_to_ind(E, data)
        E_x_ind <- E_to_ind(E_x, data)
        
        xi1 <- (x[tst] == xy & E_x_ind[tst]) / mean(E_ind)
        if (!is.null(E[[X]])) xi1 <- xi1 * px_zw_tst[[E[[X]]+1]] 
        xi2 <- E_ind[tst] / mean(E_ind)
        
        pso[[ei]][[xy+1]] <- xi1 * eta1 + xi2 * eta2
      }
    }
  }
  
  browser()
  
  res <- NULL
  for (ei in seq_along(E_lst)) {
    
    # Y_{xy, W_{xw}} | xw - Y_{xw, W_{xw}} | xw
    psi <- pso[[ei]][[1 + 1]] - pso[[ei]][[0 + 1]]
    
    rw <- data.frame(xy = 1, xw = 0, effect = mean(psi), 
                     sd = sqrt(var(psi) / length(psi)))
    rw$E <- list(E_lst[[ei]])
    res <- rbind(res, rw)
  }
  
  res <- cbind(res, attr(E_lst, "E_names"))
  as.data.table(res)
}

cmbn_E <- function(sel_covs) {
  
  covs <- list(
    apache_iii_diag = list(
      med = list(
        apache_iii_diag = 0:1199
      ),
      surg_em = list(
        apache_iii_diag = 1200:2299
      ),
      surg_el = list(
        apache_iii_diag = 2300:4299
      )
    ),
    age = list(
      `18-50` = list(age = 18:50), `51-66` = list(age = 51:66), 
      `67-76` = list(age = 67:76), `77-100` = list(age = 77:100)
    ),
    majority = list(minority = list(majority = 0))
  )
  
  covs <- covs[sel_covs]
  
  grid <- expand.grid(lapply(covs, function(x) seq_along(x)))
  
  E_nms <- list()
  E_lst <- list()
  for (i in seq_len(nrow(grid))) {
    
    block <- c()
    nm <- c()
    for (j in seq_len(ncol(grid))) {
      
      block <- c(block, covs[[j]][grid[i, j]][[1]])
      nm <- c(nm, names(covs[[j]][grid[i, j]]))
    }

    E_nms[[i]] <- nm
    E_lst[[i]] <- block
  }
  
  E_nms <- data.frame(do.call(rbind, E_nms))
  names(E_nms) <- sel_covs
  attr(E_lst, "E_names") <- E_nms
  E_lst
}


