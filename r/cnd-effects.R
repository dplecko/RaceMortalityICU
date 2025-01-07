
cnd_effect <- function(data, X, Z, W, Y, method = c("osd", "crf"), ...) {
  
  method <- match.arg(method, c("osd", "crf"))
  switch (method,
    osd = cnd_effect_osd(data, X, Z, W, Y),
    crf = cnd_effect_crf(data, X, Z, W, Y, ...)
  )
}

cnd_effect_crf <- function(data, X, Z, W, Y, subfolder = NULL) {
  
  src <- attr(data, "src")
  outcome <- Y
  
  crf <- list(de = NULL, te = NULL)
  for (effect in c("de", "te")) {
    
    fl_nm <- paste0(effect, "-crf-", src, "-", outcome, ".RData")
    if (!is.null(subfolder)) fl_nm <- file.path(subfolder, fl_nm)
    fl_path <- file.path("data", fl_nm)
    
    covs <- attr(data, "sfm")$Z 
    if (effect == "de") covs <- c(covs, attr(data, "sfm")$W)
    
    if (file.exists(fl_path)) {
      
      crf_obj <- load(fl_path)
      crf[[effect]] <- get(crf_obj)
    } else {
      
      ce_crf <- boot_crf(src = src, data = data, X = covs, W = attr(data, "sfm")$X, 
                         Y = attr(data, "sfm")$Y)
      
      crf[[effect]] <- ce_crf
      save(ce_crf, file = fl_path)
    }
  }
  
  structure(
    list(
      te_crf = crf[["te"]],
      de_crf = crf[["de"]],
      data = data,
      X = X, Z = Z, W = W, Y = Y
    ), class = "crf"
  )
}

cnd_effect_osd <- function(data, X, Z, W, Y, log_risk = FALSE) {
  
  data <- as.data.frame(data)
  
  #' Note: xz and xw are always equal for this implementation.
  n <- nrow(data)
  
  # split into K folds
  K <- 10
  folds <- sample(x = rep(1:K, each = ceiling(n / K))[1:n])
  
  eta1 <- eta2 <- ey_nest <- y_xzw <- list(rep(NA, nrow(data)), rep(NA, nrow(data)))
  px_zw <- px_z <- list(rep(NA, nrow(data)), rep(NA, nrow(data)))
  y <- data[[Y]]
  
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
    
    for (xw in c(0, 1)) {
      
      xy <- 1 - xw
      y_tilde <- pred_xgb(mod_y_xzw, data[val, c(X, Z, W)],
                          intervention = xy, X = X)
      if (log_risk) y_tilde <- log(y_tilde)
      mod_nested <- cv_xgb(data[val, c(X, Z)], y_tilde)
      ey_nest[[xy+1]][tst] <- pred_xgb(mod_nested, data[tst, c(X, Z)],
                                      intervention = xw, X = X)
    }
    
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
      
      eta1[[xy+1]][tst] <- (y[tst] - y_xzw_tst[[xy+1]]) / px_zw_tst[[xy+1]]
      eta2[[xy+1]][tst] <- y_xzw_tst[[xy + 1]]
      px_zw[[xy+1]][tst] <- px_zw_tst[[xy + 1]]
      px_z[[xy+1]][tst] <- px_z_tst[[xy + 1]] 
      y_xzw[[xy+1]][tst] <- y_xzw_tst[[xy + 1]]
    }
  }
  
  structure(
    list(
      eta1 = eta1,
      eta2 = eta2,
      y_xzw = y_xzw,
      ey_nest = ey_nest,
      px_z = px_z,
      px_zw = px_zw,
      data = data,
      X = X, Z = Z, W = W, Y = Y
    ), class = "osd"
  )
}

E_to_ind <- function(E, data) {
  
  ind <- rep(TRUE, nrow(data))
  for (i in seq_along(E)) {
    
    var <- names(E)[i]
    ind <- ind & (data[[var]] %in%  E[[var]])
  }
  
  ind
}

infer <- function(object, E_lst, effect = c("DE", "IE"), ...) {
  UseMethod("infer")
}

infer.osd <- function(object, E_lst, effect = c("DE", "IE"), ...) {
  
  effect <- match.arg(effect, c("DE", "IE"))
  data <- object$data
  px_z <- object$px_z
  px_zw <- object$px_zw
  y_xzw <- object$y_xzw
  ey_nest <- object$ey_nest
  y <- data[[object$Y]]
  x <- data[[object$X]]
  
  if (effect == "DE") {
    
    pso <- list(list(), list())
    pso <- lapply(seq_along(E_lst), function(i) pso)
    
    for (xy in c(0, 1)) {
      
      eta1 <- object$eta1[[xy+1]]
      eta2 <- object$eta2[[xy+1]]
      
      for (ei in seq_along(E_lst)) {
        
        E <- E_lst[[ei]]
        E_x <- E
        E_x[[object$X]] <- NULL # remove the X part if needed
        E_ind <- E_to_ind(E, data)
        E_x_ind <- E_to_ind(E_x, data)
        
        xi1 <- (x == xy & E_x_ind) / mean(E_ind)
        if (!is.null(E[[object$X]])) xi1 <- xi1 * object$px_zw[[E[[object$X]]+1]] 
        xi2 <- E_ind / mean(E_ind)
        
        pso[[ei]][[xy+1]] <- xi1 * eta1 + xi2 * eta2
      }
    }
    
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
    return(as.data.table(res))
  } else if (effect == "IE") {
    
    pso <- list(list(list(), list()), list(list(), list()))
    pso <- lapply(seq_along(E_lst), function(i) pso)
    
    for (xy in c(0, 1)) for (xw in c(0, 1)) for (ei in seq_along(E_lst)) {
      
      E <- E_lst[[ei]]
      xE <- E[[object$X]]
      E_x <- E
      E_x[[object$X]] <- NULL # remove the X part if needed
      E_ind <- E_to_ind(E, data)
      E_x_ind <- E_to_ind(E_x, data)
      
      # get P(E | z)
      pE_z <- px_z[[xE+1]] * E_x_ind
      pE <- mean(E_ind)
      
      # get Term T1
      t1 <- (x == xy) / pE * pE_z / px_z[[xw+1]] * 
        px_zw[[xw+1]] / px_zw[[xy+1]] * (y - y_xzw[[xy+1]])
      
      # get Term T2
      t2 <- (x == xw) / pE * pE_z / px_z[[xw+1]] * (y_xzw[[xy+1]] - ey_nest[[xy+1]])
      
      # get Term T3
      t3 <- E_ind / pE * ey_nest[[xy+1]]
      
      pso[[ei]][[xy+1]][[xw+1]] <- t1 + t2 + t3
    }
    
    res <- NULL
    for (ei in seq_along(E_lst)) {
      
      # Y_{xy, W_{xw}} | xw - Y_{xy, W_{xw}} | xw
      psi <- pso[[ei]][[1 + 1]][[1 + 1]] - pso[[ei]][[1 + 1]][[0 + 1]]
      
      rw <- data.frame(effect = mean(psi), sd = sqrt(var(psi) / length(psi)))
      rw$E <- list(E_lst[[ei]])
      res <- rbind(res, rw)
    }
    
    res <- cbind(res, attr(E_lst, "E_names"))
    return(as.data.table(res))
  }
}

infer.crf <- function(object, E_lst, effect = c("DE", "IE"), rep = NULL, ...) {
  
  effect <- match.arg(effect, c("DE", "IE"))
  data <- object$data
  de_crf <- object$de_crf
  te_crf <- object$te_crf
  res <- NULL
  E_names <- attr(E_lst, "E_names")
  
  for (ei in seq_along(E_lst)) {
    
    E <- E_lst[[ei]]
    E_ind <- E_to_ind(E, data)
    if (sum(E_ind) > 0) {
      
      de_crf_loc <- de_crf[E_ind, , drop = FALSE]
      te_crf_loc <- te_crf[E_ind, , drop = FALSE]
      
      if (effect == "DE") {
        
        if (is.null(rep)) {
          
          effect_est <- mean(de_crf_loc[, 1])
          sd <- sd(colMeans(de_crf_loc, na.rm = TRUE))
        } else {
          
          effect_est <- mean(de_crf_loc[, rep])
          sd <- NA
        }
      } else if (effect == "IE") {
        
        # inferring indirect effect via difference method (IE = TE - DE)
        if (is.null(rep)) {
          
          effect_est <- (mean(te_crf_loc[, 1]) - mean(de_crf_loc[, 1]))
          sd <- sd(colMeans(te_crf_loc, na.rm = TRUE) - 
                     colMeans(de_crf_loc, na.rm = TRUE))
        } else {
          
          effect_est <- (mean(te_crf_loc[, rep]) - mean(de_crf_loc[, rep]))
          sd <- NA
        }
      }
    } else effect_est <- sd <- NA
    
    rw <- data.frame(effect = effect_est, sd = sd)
    rw$E <- list(E_lst[[ei]])
    res <- rbind(res, rw)
  }
  
  res <- cbind(res, E_names)
  as.data.table(res)
}

cmbn_E <- function(sel_covs, src = "aics") {
  
  if (is.element(src, c("aics", "nzics", "anzics"))) {
    
    dgs <- c(0, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 201, 
             202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 301, 
             303, 305, 306, 307, 308, 309, 310, 311, 312, 313, 401, 402, 403, 
             404, 405, 406, 407, 408, 409, 410, 501, 502, 503, 504, 601, 602, 
             603, 604, 605, 701, 702, 703, 704, 801, 802, 901, 902, 903, 1001, 
             1002, 1101, 1102, 1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208, 
             1209, 1210, 1211, 1212, 1213, 1301, 1302, 1303, 1304, 1401, 1403, 
             1404, 1405, 1406, 1407, 1408, 1409, 1410, 1411, 1412, 1413, 1501, 
             1502, 1503, 1504, 1505, 1506, 1601, 1602, 1603, 1604, 1605, 1701, 
             1703, 1704, 1705, 1801, 1802, 1803, 1901, 1902, 1903, 1904, 2101, 
             2201, 3202, 3203, 3204, 3205, 3206, 3207, 3208, 3209, 3210, 3211, 
             3212, 3213, 3301, 3302, 3303, 3304, 3401, 3403, 3404, 3405, 3406, 
             3407, 3408, 3409, 3410, 3411, 3412, 3413, 3501, 3502, 3503, 3504, 
             3505, 3506, 3601, 3602, 3603, 3604, 3605, 3701, 3703, 3704, 3705, 
             3801, 3802, 3803, 3902, 3903, 3904, 4101, 4201)
    dgs_lst <- lapply(dgs, function(i) list(apache_iii_diag = i))
    diag_grp <- list(
      `Medical` = list(
        apache_iii_diag = 0:1199
      ),
      `Surgical (Emergency)` = list(
        apache_iii_diag = 1200:2299
      ),
      `Surgical (Elective)` = list(
        apache_iii_diag = 2300:4299
      )
    )
  } else {
    
    dgs <- c(0:3, 5:15, 25:35)
    dgs_lst <- lapply(dgs, function(i) list(diag_index = i))
    diag_grp <- list(
      `Medical` = list(
        diag_index = 0:3
      ),
      `Surgical (Emergency)` = list(
        diag_index = 5:15
      ),
      `Surgical (Elective)` = list(
        diag_index = 25:35
      )
    )
  }
  names(dgs_lst) <- dgs
  
  covs <- list(
    diag_grp = diag_grp,
    by_diag = dgs_lst,
    age = list(
      `18-49` = list(age = 18:49), `50-64` = list(age = 50:64), 
      `65-74` = list(age = 65:74), `74-100` = list(age = 74:100)
    ),
    majority = list(minority = list(majority = 0))
  )
  
  names(covs)[2] <- if (src == "miiv") "diag_index" else "apache_iii_diag"
  
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

plt_E_cnd <- function(cde, sel_covs, ttl, flip_color = FALSE) {
  
  cov_lbl <- function(x) {
    
    if (x == "age") return("Age group (years)")
    if (x == "diag_grp") return("Diagnosis Group")
  }
  
  plt_covs <- setdiff(sel_covs, "majority")
  if (is.element("diag_grp", plt_covs)) {
    
    lvl_ord <- c("Medical", "Surgical (Emergency)", "Surgical (Elective)")
    cde[, diag_grp := factor(diag_grp, levels = lvl_ord)]
  }
  
  if (length(plt_covs) == 1) {
    
    cde[[plt_covs]] <- factor(cde[[plt_covs]])
    p <- ggplot(cde, aes(x = .data[[plt_covs]], y = effect)) +
      geom_col() + theme_bw() +
      geom_errorbar(aes(ymin = effect - 1.96 * sd, ymax = effect + 1.96 * sd)) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.text = element_text(size = rel(1.1)),  # Scale axis tick labels
        axis.title = element_text(size = rel(1.1)) # Scale axis titles
      ) +
      ylab("DE(x0 -> x1 | E)") +
      ggtitle(ttl)
  } else {
    
    lwr_clr <- "blue"
    upr_clr <- "red"
    if (flip_color) {
      
      lwr_clr <- "red"
      upr_clr <- "blue"
    }
    
    # get 1\sigma, 2\Sigma opacity labels
    cde[, opa := ifelse(abs(effect) / sd > 2, 1, 
                        ifelse(abs(effect) / sd > 1, 0.6, 0.3))]
    
    cde[[plt_covs[1]]] <- factor(cde[[plt_covs[1]]])
    cde[[plt_covs[2]]] <- factor(cde[[plt_covs[2]]])
    p <- ggplot(cde, aes(x = .data[[plt_covs[1]]], y = .data[[plt_covs[2]]], 
                         fill = effect)) +
      geom_tile(aes(alpha = opa)) + 
      theme_minimal() +
      scale_fill_gradient2(low = lwr_clr, high = upr_clr, mid = "white", 
                           midpoint = 0, name = "Direct\nEffect", labels = scales::percent) +
      geom_text(
        aes(label = paste0(round(effect * 100, 1), "%\n", "[", 
                           round((effect-1.96*sd)*100, 1), "%, ",
                           round((effect+1.96*sd)*100, 1), "%]")), 
        color = "black", size = 4
      ) +
      ggtitle(ttl) +
      guides(
        alpha = "none"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.text = element_text(size = rel(1.1)),  # Scale axis tick labels
        axis.title = element_text(size = rel(1.1)) # Scale axis titles
      ) +
      xlab(cov_lbl(plt_covs[1])) + ylab(cov_lbl(plt_covs[2]))
  }
  
  p
}

de_residuals <- function(src, outcome, method = c("osd", "crf")) {
  
  dat <- load_data(src = src, outcome = outcome, split_elective = TRUE)
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  method <- match.arg(method, c("osd", "crf"))
  E_lst <- cmbn_E(c("apache_iii_diag", "majority"))
  cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                         method = method)
  de_resid <- infer(cndE_obj, E_lst)
  de_resid[, apache_iii_diag := as.integer(apache_iii_diag)]
  de_resid <- setnames(de_resid, "effect", "de_resid")
  de_resid[, c("apache_iii_diag", "de_resid"), with = FALSE]
}
