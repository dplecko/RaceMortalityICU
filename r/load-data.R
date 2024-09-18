
load_data <- function(src, quick = FALSE) {
  
  root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
  fl_path <- file.path(root, "data", paste0("dat-", src, ".RData"))
  if (file.exists(fl_path)) {
  
    load(fl_path)     
    return(dat)
  }
  
  if (src == "anzics") {
    
    sel_coh <- load_concepts(c("adm_episode", "age", "adm_year"), "anzics", 
                             verbose = FALSE)
    patient_ids <- id_col(sel_coh[adm_episode %in% c(0, 1) & age >= 18 &
                                  adm_year >= 2018])
    
    dat <- load_concepts(c("death", "age", "country", "apache_iii_rod", 
                           "apache_iii_diag", "sex", "indig", "elective", 
                           "adm_diag"), "anzics", 
                         patient_ids = patient_ids, verbose = FALSE)
    
    dat[, sex := as.integer(sex == "Male")]
    dat[, majority := 1 - indig]
    dat[, indig := NULL]
    dat[, c(index_var(dat)) := NULL]
    imp_lst <- list(
      age = 65,
      apache_iii_rod = median(dat$apache_iii_rod, na.rm = TRUE),
      death = FALSE,
      apache_iii_diag = 0,
      indig = NA,
      elective = NA,
      sex = NA,
      adm_diag = "OTH"
    )
    
    for (i in seq_len(ncol(dat))) {
      
      var <- names(dat)[i]
      if (any(is.na(dat[[var]])) & !is.null(imp_lst[[var]]))
        dat[is.na(get(var)), c(var) := imp_lst[[var]]]
    }
    
    dat <- dat[complete.cases(dat)]
    
    # create diag_dt
    diag_dt <- as.data.table(anzics$d_diagnoses)
    diag_dt <- setnames(diag_dt, names(diag_dt), 
                        c("apache_iii_diag", "diag_name", "group_name"))
    diag_dt[, diag_group := floor(apache_iii_diag / 100)]
    
    attr(dat, "X") <- c("apache_iii_rod", "apache_iii_diag", "age", "sex", 
                        "elective")
    attr(dat, "W") <- "majority"
    attr(dat, "Y") <- "death"
    attr(dat, "diag_col") <- "apache_iii_diag"
    attr(dat, "diag_dt") <- diag_dt
  } else if (src == "miiv") {
    
    sel_coh <- load_concepts(c("adm_episode", "age"), src, verbose = FALSE)
    patient_ids <- id_col(sel_coh[adm_episode == 1 & age >= 18])
    
    if (quick) {
      
      dat <- load_concepts(c("death", "age", "charlson", "adm_diag", 
                             "sex", "race"), 
                           src, patient_ids = patient_ids, verbose = FALSE)
    } else {
      
      dat <- load_concepts(c("death", "age", "acu_24", "charlson", "adm_diag", 
                             "sex", "race"), 
                           src, patient_ids = patient_ids, verbose = FALSE)
    }
    
    dat[, sex := as.integer(sex == "Male")]
    dat[, c(index_var(dat)) := NULL]
    dat <- dat[race %in% c("Caucasian", "African American")]
    dat[, majority := as.integer(race == "Caucasian")]
    dat[, race := NULL]
    imp_lst <- list(
      age = 65,
      acu_24 = median(dat$acu_24, na.rm = TRUE),
      death = FALSE,
      adm_diag = 0,
      race = NA,
      charlson = 0
    )
    
    # diagnoses grouping
    diag_dt <- structure(
      list(diag_index = 0:17, 
           adm_diag = c("MED", "CMED",  "NMED", "OMED", "PSYCH", "GU", "TRAUM", 
                        "ENT", "CSURG", "NSURG", "ORTHO", "PSURG", "SURG", 
                        "TSURG", "VSURG", "GYN", "OBS", "DENT")
          ), 
      row.names = c(NA, -18L), class = c("data.table", "data.frame")
    )
    
    dat <- merge(dat, diag_dt, by = "adm_diag", all.x = TRUE)
    diag_dt <- setnames(diag_dt, c("diag_group", "group_name"))
    
    for (i in seq_len(ncol(dat))) {
      
      var <- names(dat)[i]
      if (any(is.na(dat[[var]])) & !is.null(imp_lst[[var]]))
        dat[is.na(get(var)), c(var) := imp_lst[[var]]]
    }
    
    dat <- dat[complete.cases(dat)]
    
    attr(dat, "X") <- c("acu_24", "charlson", "age", "diag_index", "sex")
    attr(dat, "W") <- "majority"
    attr(dat, "Y") <- "death"
    attr(dat, "diag_col") <- "adm_diag"
    attr(dat, "diag_dt") <- diag_dt
  }
  
  save(dat, file = fl_path)
  dat
}
