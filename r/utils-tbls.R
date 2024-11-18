
pts_info <- function(source, patient_ids, vars) {

  tbl_list <- lapply(
    names(vars),
    function(x) {
      load_concepts(x, source, patient_ids = patient_ids, 
                    keep_components = T)
    }
  )
  
  # get patient table rows
  tbl_cbs <- Map(
    function(x, y) {
      
      # if (is.character(x[["concept_name"]]) && x[["concept_name"]] == "SOFA") browser()
      
      ret <- x[["callback"]](y, unlist(patient_ids))
      ret <- data.frame(Reduce(cbind, ret))
      
      if (is.character(x[["concept_name"]])) {
        
        ret[1, 1] <- x[["concept_name"]]
      } else if (is.list(x[["concept_name"]])) {
        
        nm_map <- unlist(x[["concept_name"]][names(x[["concept_name"]]) != "header"])
        ret[, 1] <- nm_map[match(ret[, 1], names(nm_map))]
        if (!is.null(x[["concept_name"]][["header"]])) {

          ret <- rbind(
            c(x[["concept_name"]][["header"]], "", ""), ret
          )
        }
      }
      
      names(ret) <- c("Variable", "Reported", srcwrap(source))
      ret
    }, vars, tbl_list
  )
  
  # summary statistics for p-values
  smr_stats <- Map(function(x, y) y[[x]], names(vars), tbl_list)
  
  pts_tbl <- Reduce(rbind, tbl_cbs)
  
  cohort_info <- as.data.frame(cbind("Cohort size", "n", length(patient_ids)))
  names(cohort_info) <- names(pts_tbl)
  
  pts_tbl <- rbind(cohort_info, pts_tbl)
  
  attr(pts_tbl, "smr_stats") <- smr_stats
  pts_tbl
}

patient_table <- function(source, patient_ids, nms, vars, report_pvals = FALSE) {
  
  
  tbls <- Map(pts_info, source, patient_ids, list(vars))
  res <- Reduce(
    function(x, y) merge(x, y, by = c("Variable", "Reported"), sort = F), tbls
  )
  
  names(res) <- c("Variable", "Reported", nms)
  
  if (length(source) == 2 & report_pvals) {

    pvals <- Map(compute_pval, attr(tbls[[1]], "smr_stats"), 
                 attr(tbls[[2]], "smr_stats"), names(vars))
    pvals <- unlist(pvals)
    
    df_pval <- c()
    for (i in seq_along(vars)) {
      
      var_name <- vars[[i]][["concept_name"]]
      if (is.list(var_name)) {
        
        if (!is.null(var_name[["header"]])) {
          
          var_name <- var_name[["header"]]
        } else {
          
          var_name <- var_name[[2]]
        }
      }
      pval_i <- if (pvals[i] < 0.001) "$ < 0.001$" else 
        as.character(spec_dec(pvals[i], 3))
      df_pval <- rbind(df_pval, data.frame(Variable = var_name, 
                                           `p.value` = pval_i))
    }

    res$p.value <- ""
    res$p.value[match(df_pval$Variable, res$Variable)] <- df_pval$p.value
  }

  res
}

table_to_tex <- function(res) {
  
  kableExtra::kable(res, format = "latex", booktabs = TRUE, 
        align = c('l', rep('c', ncol(res) - 1))) %>%
    kableExtra::kable_styling(latex_options = c("hold_position"))
}

med_iqr <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  if(is_ts_tbl(x)) x <- x[get(index_var(x)) == 24L]
  quants <- quantile(x[[val_col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  res <- paste0(
    round(quants[2], 2), " (",
    round(quants[1], 2), "-",
    round(quants[3], 2), ")"
  )
  
  list(val_col, "Median (IQR)", res)
}

mean_med_iqr <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  if(is_ts_tbl(x)) x <- x[get(index_var(x)) == 24L]
  quants <- quantile(x[[val_col]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  res <- paste0(
    spec_dec(mean(x[[val_col]], na.rm = TRUE), 3), ", ",
    spec_dec(quants[2], 3), " (",
    spec_dec(quants[1], 3), "-",
    spec_dec(quants[3], 3), ")"
  )
  
  list(val_col, "Mean, Median (IQR)", res)
}

multi_med_iqr <- function(x, patient_ids) {
  
  val_cols <- setdiff(names(x), meta_vars(x))
  res <- lapply(
    val_cols, function(vcol) med_iqr(x[, c(meta_vars(x), vcol), with = FALSE], 
                                     patient_ids)
  )
  
  lapply(1:3, function(i) {
    Reduce(c, lapply(res, `[[`, i))
  })
  
}

tab_design <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  res <- table(x[[val_col]])
  res <- round(100 * res / sum(res))
  
  if(val_col == "adm" & nrow(x) == 0L) {
    
    return(
      list(c("med", "surg", "other"), "%", rep(NA, 3))
    )
    
  }
  
  list(names(res), "%", as.integer(res))
  
}

tab_design_np <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  res <- x[[val_col]]
  
  if (length(res) < length(patient_ids)) {
    
    tbres <- table(res)
    imp_val <- names(tbres)[which.max(tbres)]
    if (is.logical(res)) imp_val <- as.logical(imp_val)
    res <- c(res, rep(imp_val, length(patient_ids) - length(res)))
  }
  
  res <- table(res)
  labs <- names(res)
  res <- paste0(
    res, " (", round(100 * res / sum(res)), ")"
  )
  
  if(val_col == "adm" & nrow(x) == 0L) {
    
    return(
      list(c("med", "surg", "other"), "%", rep(NA, 3))
    )
    
  }
  
  list(labs, "n (%)", res)
}

percent_fun1 <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  
  list(
    val_col, 
    "n (%)", 
    paste0(
      sum(x[[val_col]]), " (",
      spec_dec(100 * sum(x[[val_col]]) / length(patient_ids), 1), "%)"
    )
  )
}

percent_fun0 <- function(x, patient_ids) {
  
  val_col <- setdiff(names(x), meta_vars(x))
  
  list(
    val_col, 
    "n (%)", 
    paste0(
      sum(x[[val_col]]), " (",
      spec_dec(100 * sum(x[[val_col]]) / length(patient_ids), 0), "%)"
    )
  )
}

na_fun <- function(x, patient_ids) return(c(NA, NA, NA))

spec_dec <- function(x, k) trimws(format(round(x, k), nsmall=k))

compute_pval <- function(x, y, cnc) {

  if (length(unique(x)) < 10) {
    
    p.val <- chisq.test(
      table(
        c(rep("A", length(x)), rep("B", length(y))),
        c(x, y)
      )
    )$p.val
    
    cat(cnc, "(ChiSq) p-value =", p.val, "\n")
  } else {
    
    p.val <- wilcox.test(x = x, y = y)$p.val
    cat(cnc, "(Rank-Sum) p-value =", p.val,"\n")
  }
  
  p.val
}

concept_translator <- list(
  age = "Age (years)",
  med = "- Medical",
  surg = "- Surgical",
  oth = "- Other",
  emergency_surgery = "- Surgical (Emergency)",
  elective_surgery = "- Surgical (Elective)",
  death = "Mortality",
  diab = "Diabetic",
  is_vent = "Ventilated",
  elix = "Elixhauser Index",
  is_chr = "Chronic Comorbidities",
  `Cohort size` = "Cohort size",
  los_icu = "ICU LOS (days)",
  los_hosp = "Hospital LOS (days)",
  Male = "Sex (Male)",
  Female = "Sex (Female)",
  apache_iii = "APACHE-III Score",
  apache_iii_rod = "APACHE-III Risk of Death",
  anzrod_risk = "ANZROD Risk of Death",
  pci = "Persistent Critical Illness",
  pci_or_death = "Persistent Critical Illness OR Mortality"
)
