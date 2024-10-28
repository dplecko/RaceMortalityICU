
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
