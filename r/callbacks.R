
anzics_hosp_epi_cb <- function(x, val_var, ...) {
  
  lvl <- "patient"
  
  by_vars <- c("PatientID", "DSITEID")
  if (lvl == "patient-year") by_vars <- c(by_vars, "IcuAdmitYYYY")
  
  pts <- x[, list(icustay_cnt = .N), by = by_vars]
  pts[, patient_id := seq_along(PatientID)]
  x <- merge(x, pts, by = by_vars)
  x <- setorderv(x, cols = c("ICU_AD_DTM"))
  x <- setorderv(x, cols = c("patient_id"))
  
  # calculate the ICU re-admission episode for the unique patient
  x[, readm_epi := seq_along(PatientID), by = "patient_id"]
  
  # calculate the hospital re-admission episode for the unique patient
  x[, hosp_epi := cumsum(AdmEpisode %in% c(0, 1)), by = "patient_id"]
  
  # count total number of hospital admissions
  x[, max_epi := max(hosp_epi), by = "patient_id"]
  
  # set single hospital admissions (no-readmission) to 0
  x[max_epi == 1, hosp_epi := 0]
  
  x[, c(val_var) := hosp_epi]
}

anzics_irsad_cb <- function(x, val_var, env, ...) {
  
  x <- merge(x, env$poa_seifa[, c("postcode", "irsad_decile")], 
             by = "postcode", all.x = TRUE)
  x[, postcode := irsad_decile]
  x
}

miiv_hosp_epi_cb <- function(x, val_var, ...) {
  
  epi_01 <- function(x) if (length(x) == 1) return(0) else seq_along(x)
  
  x <- merge(x, list(...)$env$icustays[, c("stay_id", "intime")],
             by = "stay_id")
  x <- as.data.table(x)
  x <- setorderv(x, cols = c("subject_id", "intime"))
  x[, hadm_lag := shift(hadm_id), by = c("subject_id")]
  x[, new_hadm := (hadm_lag != hadm_id)]
  
  x[is.na(new_hadm), new_hadm := TRUE]
  x[, hosp_episode := cumsum(new_hadm), by = c("subject_id")]
  x[, max_hosp_epi := max(hosp_episode), by = c("subject_id")]
  x[max_hosp_epi == 1, hosp_episode := 0]
  x <- as_id_tbl(x[, c("stay_id", "hosp_episode"), with=FALSE],
                 id_vars = "stay_id")
  x[, c(val_var) := hosp_episode]
}

anzics_diab_cb <- function(x, ...) {
  
  x[, DIABETES := DIABETES != 5]
  x
}

anzics_cmb_cb <- function(interval, ...) {
  
  cmb <- Reduce(merge, list(...))
  cmb[, anz_cmb := rowSums(cmb[, -1])]
  cmb[, c(id_vars(cmb), "anz_cmb"), with=FALSE]
}
