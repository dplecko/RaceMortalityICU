
ricu:::init_proj()

vars <- list(
  age = list(
    concept_name = "Age (years)",
    callback = med_iqr
  ),
  adm = list(
    concept_name = list(
      header = "Admission type", med = "    - Medical", surg = "    - Surgical",
      other = "Other"
    ),
    callback = tab_design
  ),
  death = list(
    concept_name = "Mortality",
    callback = percent_fun1
  ),
  los_icu = list(
    concept_name = "ICU LOS (days)",
    callback = med_iqr
  ),
  los_hosp = list(
    concept_name = "Hospital LOS (days)",
    callback = med_iqr
  ),
  sex = list(
    concept_name = list(header = NULL, Male = "Sex (Male)", Female = "Sex (Female)"),
    callback = tab_design
  )
)

miss_tbl <- FALSE
for (src in c("miiv", "anzics", "aics", "nzics")) {
  
  if (is.element(src, c("anzics", "nzics", "aics"))) {
    
    
    tot_vars <- c(
      vars,
      list(
        is_vent2 = list(
          concept_name = "Ventilated",
          callback = percent_fun1
        ),
        apache_iii = list(
          concept_name = "APACHE-III Score",
          callback = mean_med_iqr
        ),
        apache_iii_rod = list(
          concept_name = "APACHE-III Risk of Death",
          callback = mean_med_iqr
        )
      )
    )
    
    if (miss_tbl) {
      
      nms <- c("ANZICS APD (Indig. Status Recorded)", 
               "ANZICS APD (Indig. Status Missing)")
    } else nms <- c("ANZICS APD (Majority)", "ANZICS APD (First Nations)")
    
  } else {
    
    nms <- c("MIMIC-IV (White)", "MIMIC-IV (African-American)")
    tot_vars <- c(
      vars,
      list(
        is_vent = list(
          concept_name = "Ventilated",
          callback = percent_fun1
        ),
        sofa = list(
          concept_name = list(
            header = "SOFA", sofa_resp_comp = "    - Respiratory",
            sofa_coag_comp = "    - Coagulation",
            sofa_liver_comp = "    - Hepatic",
            sofa_cardio_comp = "    - Cardio",
            sofa_cns_comp = "    - CNS",
            sofa_renal_comp = "    - Renal",
            sofa = "    - Total"
          ),
          callback = multi_med_iqr
        )
      )
    )
  }
  
  if (!miss_tbl) {
    
    cohorts <- list(
      A = id_col(load_data(src)[majority == 1]),
      B = id_col(load_data(src)[majority == 0])
    )
  } else {
    
    tot <- id_col(load_data(src, no_miss = FALSE))
    nomis <- id_col(load_data(src, no_miss = TRUE))
    cohorts <- list(A = nomis, B = setdiff(tot, nomis))
  }
  
  if (is.element(src, c("anzics", "nzics", "aics"))) src <- "anzics"

  res <- patient_table(rep(src, 2), cohorts, nms, vars, report_pvals = TRUE)
  table_to_tex(res)
}




