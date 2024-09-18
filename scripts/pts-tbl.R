
ricu:::init_proj()

src <- "miiv"
if (src == "anzics") {
  
  nms <- c("ANZICS APD (Majority)", "ANZICS APD (First Nations)")
} else {
  
  nms <- c("MIMIC-IV (White)", "MIMIC-IV (African-American)")
}
cohorts <- list(
  A = id_col(load_data(src)[majority == 1]),
  B = id_col(load_data(src)[majority == 0])
)

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

if (src == "anzics") {
  
  vars <- c(
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
} else if (src == "miiv") {
  
  vars <- c(
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

res <- patient_table(rep(src, 2), cohorts, nms, vars, report_pvals = TRUE)
table_to_tex(res)




