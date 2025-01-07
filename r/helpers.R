
trim_ends <- function(x, eps) {
  
  x[x > eps] <- eps
  x[x < -eps] <- -eps
  x
}

boot_crf <- function(src, data, X, W, Y, nboot = 10) {
  
  data_path <- tempfile(fileext = ".csv")
  fwrite(data, data_path)
  
  n <- nrow(data)
  preds <- matrix(NA, nrow = n, ncol = nboot)
  
  for (i in seq_len(nboot)) {
    cat("Entering round", i)
    
    # Define the output path for the current iteration
    output_path <- tempfile(fileext = ".csv")
    
    # Build the system call to run the worker script
    cmd <- sprintf(
      "Rscript scripts/boot-crf.R %d %s %s '%s' %s %s %s",
      i, src, data_path, paste(X, collapse = ","), W, Y, output_path
    )
    
    # Run the worker script
    system(cmd, wait = TRUE)
    
    # Read the output predictions
    preds_dt <- fread(output_path)
    
    # Fill the preds matrix
    preds[preds_dt$id, i] <- preds_dt$pred
    
    cat(", and completing it.\n")
  }
  
  # Clean up temporary files
  unlink(data_path)
  
  preds
}

std_diag_idx <- function(idx, src) {
  
  std_idx <- rep(NA, length(idx))
  if (src == "miiv") {
    
    std_idx[idx %in% 0:4] <- "Medical"
    std_idx[idx %in% 5:15] <- "Surgical (Emergency)"
    std_idx[idx %in% 25:35] <- "Surgical (Elective)"
  } else if (src == "aics") {
    
    std_idx[idx %in% 0:11] <- "Medical"
    std_idx[idx %in% 12:22] <- "Surgical (Emergency)"
    std_idx[idx > 22] <- "Surgical (Elective)"
  } else std_idx <- NULL
  
  std_idx
}

std_diag <- function(admg, elective = NULL) {
  
  pairs <- matrix(
    c(
      "MED", "Internal Medicine",
      "CMED", "Cardiology",
      "NMED", "Neurology",
      "OMED", "Oncology",
      "CSURG", "Cardiac Surgery",
      "NSURG", "Neurosurgery",
      "SURG", "General Surgery",
      "TRAUM", "Trauma",
      "VSURG", "Vascular Surgery",
      "TSURG", "Thoracic Surgery",
      "ORTHO", "Orthopedic Surgery",
      "GU", "Other",
      "GYN", "Other",
      "ENT", "Other",
      "OBS", "Other",
      "PSURG", "Other",
      "DENT", "Other",
      "OTH", "Other"
    ), ncol = 2L, byrow = TRUE
  )
  
  adm_grp <- pairs[, 2][match(admg, pairs[, 1])]
  levels <- pairs[, 2][1:12]
  if (!is.null(elective)) {
    
    adm_grp[elective == 1] <- paste(adm_grp[elective == 1], "(Elect.)")
    lvls <- c(pairs[, 2][1:12], paste(pairs[, 2][5:12], "(Elect.)"))
  }
  
  factor(adm_grp, levels = lvls)
}

anz_std_diag <- function(x, split_names = FALSE) {
  
  if (any(x > 30)) {
    
    elect_idx <- x > 30
    x[x > 30] <- x[x > 30] - 20
  }
  
  map <- unique(data.frame(
    diag_grp = floor(anzics$d_diagnoses$diagnosis_code / 100), 
    grp_name = anzics$d_diagnoses$diagnosis_group
  ))
  map <- as.data.table(map)
  
  map[diag_grp <= 11, grp_name := paste(grp_name, "(Med.)")]
  map[diag_grp > 11, grp_name := paste(grp_name, "(Surg.)")] 
  
  nms <- map$grp_name[match(x, map$diag_grp)]
  
  if (!split_names) {
    
    return(factor(nms, levels = unique(map$grp_name)))
  } else {
    
    nms[elect_idx] <- gsub("\\(Surg.\\)", "(Elect. Surg.)", nms[elect_idx])
    nms[!elect_idx] <- gsub("\\(Surg.\\)", "(Emerg. Surg.)", nms[!elect_idx])
    lvls <- unique(map$grp_name)
    lvls_med <- lvls[!grepl("\\(Surg.\\)", lvls)]
    lvls_surg <- lvls[grepl("\\(Surg.\\)", lvls)]
    lvls <- c(lvls_med, gsub("\\(Surg.\\)", "(Emerg. Surg.)", lvls_surg),
              gsub("\\(Surg.\\)", "(Elect. Surg.)", lvls_surg))
    return(factor(nms, levels = lvls))
  }
}

age_grp <- function(ages, quart = FALSE) {
  
  if (quart) {
    
    labels <- c("18-49", "50-64", "65-74", "75-100")
    cuts <- c(49, 64, 74)
  } else {
    
    labels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", 
                "20-24 years", "25-29 years", "30-34 years", "35-39 years", 
                "40-44 years", "45-49 years", "50-54 years", "55-59 years", 
                "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
                "80-84 years", "85+ years")
    cuts <- c(4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84)
  }
  
  age_groups <- cut(ages, breaks = c(-Inf, cuts, Inf), labels = labels,
                    right = TRUE)
  
  return(as.character(age_groups))
}

pmf_compute <- function(dat_adj, var) {
  
  ils_dat <- dat_adj[, .N, by = c("majority", var)]
  ils_dat <- ils_dat[, list(pmf = N / sum(N), ils = get(var)), 
                     by = c("majority")]
  ils_dat <- merge(
    as.data.table(expand.grid(majority = c(0, 1), 
                              ils = seq(min(ils_dat$ils), max(ils_dat$ils)))),
    ils_dat, all.x = TRUE
  )
  ils_dat[is.na(pmf), pmf := 0]
  ils_dat
}

qnormmix <- function(p, value, sd) {
  
  k <- length(value)
  samples <- replicate(10000, {
    component <- sample(1:k, 1)
    rnorm(1, mean = value[component], sd = sd[component])
  })
  
  c(mean(value), lapply(p, function(pi) quantile(samples, probs = pi)))
}

local_de <- function(dat, X, Z, W, Y, nboot = 50, 
                     engine = c("xgboost", "ranger")) {
  
  engine <- match.arg(engine, c("xgboost", "ranger"))
  res <- NULL
  
  for (i in seq_len(nboot)) {
    
    if (i == 1) boot_idx <- seq_len(nrow(dat)) else 
      boot_idx <- sample.int(nrow(dat), replace = TRUE)
    
    # get the part of space with age >= 65 and admission == surgery
    if (is.element("apache_iii_diag", names(dat))) {
      
      Iset <- dat[["age"]][boot_idx] >= 65 & 
              dat[["apache_iii_diag"]][boot_idx] >= 1100
    } else {
      
      Iset <- dat[["age"]][boot_idx] >= 65 & 
        dat[["adm_diag"]][boot_idx] %in% c("CSURG", "NSURG", "ORTHO", "SURG", 
                                           "TRAUM", "TSURG", "VSURG")
    }
    
    if (engine == "ranger") {
      
      rf <- ranger::ranger(
        formula = as.formula(paste(X, "~", paste(c(Z, W), collapse = "+"))),
        data = dat[boot_idx], probability = TRUE
      )
      px_zw <- rf$predictions[, 2][Iset]
    } else if (engine == "xgboost") {
      
      params <- list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = 0.01
      )
      
      # Run cross-validation
      cv <- xgb.cv(
        params = params,
        data = as.matrix(dat[, c(W, Z), with = FALSE]),
        label = dat[[X]],
        nrounds = 2000,
        nfold = 5,
        prediction = TRUE,
        early_stopping_rounds = 10,
        verbose = FALSE
      )
      cat("nrounds =", cv$best_iteration, ";")
      
      px_zw <- cv$pred[Iset]
    }
    
    idx1 <- (dat[[X]][boot_idx] == 1)[Iset]
    y_bt <- dat[[Y]][boot_idx][Iset]
    
    est <- (sum(idx1 / px_zw))^(-1) * sum(y_bt * idx1 / px_zw) -
      (sum(!idx1 / (1-px_zw)))^(-1) * sum(y_bt * !idx1 / (1 - px_zw))
    
    res <- rbind(
      res, data.frame(rep = i, estimate = est)
    )
    
    cat("Bootstrap repetition", i, "finished\n")
  }
  
  res
}

oval <- function(scores, x, alpha = c(0.05, 0.1, 0.5)) {
  
  res <- vapply(
    alpha,
    function(alph) ovalue:::DiT_ovalue_exact(scores[x==1], scores[x==0], alph)(
      mean(x), "ATT"
    ), numeric(1L)
  )
  names(res) <- alpha
  res
}

mc_dataset <- function(dat, n_mc = list(10^6, 10^6)) {
  
  dat[, age := round(age)]
  
  # pre-compute age look-ups
  lookup <- replicate(17, NULL)
  for (i in seq(range(dat$age)[1], range(dat$age)[2])) {
    
    lookup[[i]] <- list(NULL)
    lookup[[i]][[1]] <- which(dat$age == i & dat$majority == 0)
    lookup[[i]][[2]] <- which(dat$age == i & dat$majority == 1)
  }
  
  if (max(dat$age) == 100) {
    
    if (length(lookup[[100]][[1]]) == 0) 
      lookup[[100]][[1]] <- lookup[[99]][[1]]
    
    if (length(lookup[[98]][[1]]) == 0) 
      lookup[[98]][[1]] <- lookup[[99]][[1]]
  }
  
  ret <- c()
  for (maj in c(0, 1)) {
    
    # sample age distribution
    if (maj == 0 || n_mc[[1]] != n_mc[[2]])
      ag <- sample(dat$age, n_mc[[maj + 1]], replace = TRUE)
    
    rows <- lapply(
      unique(ag),
      function(agi) {
        
        n_samp <- sum(ag == agi)
        sample(x = lookup[[agi]][[maj + 1]], n_samp, replace = TRUE)
      }
    )
    
    ret <- if (maj == 0) dat[do.call(c, rows)] else rbind(ret, dat[do.call(c, rows)])
  }
  
  ret
}

create_folds <- function(src, nfolds = 10) {
  
  pids <- id_col(load_data(src))
  folds <- lapply(
    seq_len(nfolds),
    function(i) {
      
      if (i == 1) return(pids)
      sample(pids, replace = TRUE)
    }
  )
  
  config(src, value = folds)
}

Sys.setenv(N_CORES = parallel::detectCores()) # set to 64 for the cluster