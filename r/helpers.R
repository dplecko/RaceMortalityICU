
trim_ends <- function(x, eps) {
  
  x[x > eps] <- eps
  x[x < -eps] <- -eps
  x
}

cv_xgboost <- function(data, X, W, Y, nboot = 50) {
  
  n <- nrow(data)
  preds0 <- preds1 <- matrix(NA, nrow = n, ncol = nboot)
  
  for (i in seq_len(nboot)) {
    set.seed(i)
    in_bag <- sample(1:n, size = round(0.8 * n))
    out_bag <- setdiff(1:n, in_bag)
    
    dtrain <- xgb.DMatrix(data = as.matrix(data[in_bag, c(X, W), with = FALSE]), 
                          label = data[in_bag][[Y]])
    dtest <- xgb.DMatrix(data = as.matrix(data[out_bag, c(X, W), with = FALSE]))
    
    # create intervened data
    data0 <- data1 <- copy(data)
    data0[[W]] <- 0
    data1[[W]] <- 1
    
    dtest0 <- xgb.DMatrix(data = as.matrix(data0[out_bag, c(X, W), with = FALSE]))
    dtest1 <- xgb.DMatrix(data = as.matrix(data1[out_bag, c(X, W), with = FALSE]))
    
    cv <- xgb.cv(params = list(eta = 0.1, objective = "binary:logistic"),
                 data = dtrain, nrounds = 200, nfold = 5, 
                 early_stopping_rounds = 10, verbose = 0)
    
    bst <- xgb.train(params = list(eta = 0.1, objective = "binary:logistic"),
                     data = dtrain, nround = cv$best_iteration)
    
    preds0[out_bag, i] <- predict(bst, dtest0)
    preds1[out_bag, i] <- predict(bst, dtest1)
  }
  
  # return(list(preds0, preds1))
  rowMeans(preds1, na.rm = TRUE) - rowMeans(preds0, na.rm = TRUE)
}

boot_crf <- function(data, X, W, Y, nboot = 10) {
  
  data_path <- tempfile(fileext = ".csv")
  fwrite(data, data_path)
  
  n <- nrow(data)
  preds <- matrix(NA, nrow = n, ncol = nboot)
  
  for (i in seq_len(nboot)) {
    cat("Entering round", i, "\n")
    
    # Define the output path for the current iteration
    output_path <- tempfile(fileext = ".csv")
    
    # Build the system call to run the worker script
    cmd <- sprintf(
      "Rscript scripts/boot-crf.R %d %s '%s' %s %s %s",
      i, data_path, paste(X, collapse = ","), W, Y, output_path
    )
    
    # Run the worker script
    system(cmd, wait = TRUE)
    
    # Read the output predictions
    preds_dt <- fread(output_path)
    
    # Fill the preds matrix
    preds[preds_dt$id, i] <- preds_dt$pred
    
    cat("Finishing round", i, "\n")
  }
  
  # Clean up temporary files
  unlink(data_path)
  
  preds
}

std_diag <- function(admg) {
  
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
  
  factor(
    pairs[, 2][match(admg, pairs[, 1])],
    levels = pairs[, 2][1:12]
  )
}

anz_std_diag <- function(x) {
  
  map <- unique(data.frame(diag_grp = floor(anzics$d_diagnoses$diagnosis_code / 100), 
                           grp_name = anzics$d_diagnoses$diagnosis_group))
  map <- as.data.table(map)
  map[diag_grp <= 11, grp_name := paste(grp_name, "(Med.)")]
  map[diag_grp > 11, grp_name := paste(grp_name, "(Surg.)")]
  
  factor(map$grp_name[match(x, map$diag_grp)], levels = unique(map$grp_name))
}

age_grp <- function(ages) {
  labels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", 
              "20-24 years", 
              "25-29 years", "30-34 years", "35-39 years", "40-44 years", 
              "45-49 years", 
              "50-54 years", "55-59 years", "60-64 years", "65-69 years", 
              "70-74 years", 
              "75-79 years", "80-84 years", "85+ years")
  
  age_groups <- cut(ages, 
                    breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 
                               59, 64, 69, 74, 79, 84, Inf),
                    labels = labels,
                    right = TRUE)
  
  return(as.character(age_groups))
}

study_flowchart <- function(src) {
  
  if (src == "anzics") {
    
    steps <- list(
      list(cnc = "age", fn = function(x) x < 18),
      list(cnc = "sex", fn = function(x) is.na(x)),
      list(cnc = "elective", fn = function(x) is.na(x)),
      list(cnc = "adm_episode", fn = function(x) x > 1),
      list(cnc = "indig", fn = function(x) is.na(x))
    )
    
    dat <- merge(
      id_tbl(stay_id = anzics$main$ICUStayID),
      load_concepts(c(vapply(steps, function(x) x[["cnc"]], character(1L)), 
                      "site"), 
                    "anzics", verbose = FALSE), all.x = TRUE
    )
  } else if (src == "miiv") {
    
    steps <- list(
      list(cnc = "age", fn = function(x) x < 18),
      list(cnc = "sex", fn = function(x) is.na(x)),
      list(cnc = "adm_episode", fn = function(x) x > 1),
      list(cnc = "race", 
           fn = function(x) is.na(x) | !(x %in% c("Caucasian", 
                                                  "African American")))
    )
    
    dat <- merge(
      id_tbl(stay_id = miiv$icustays$stay_id),
      load_concepts(vapply(steps, function(x) x[["cnc"]], character(1L)), 
                    "miiv", verbose = FALSE), all.x = TRUE
    )
  }
  
  cat("Starting with", nrow(dat), "admissions\n")
  for (i in seq_along(steps)) {
    
    ndat <- dat[!steps[[i]][["fn"]](get(steps[[i]][["cnc"]]))]
    cat("Removing", nrow(dat) - nrow(ndat), "admissions based on",
        steps[[i]][["cnc"]], "\n")
    dat <- ndat
  }
  
  browser()
  
  cat("Ending with", nrow(dat), "admissions")
  if (src == "anzics") cat(" from", length(unique(dat$site)), "sites\n")
  cat("\n")
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
