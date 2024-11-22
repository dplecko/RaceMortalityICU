
load_data <- function(src, outcome = "death", 
                      split_elective = FALSE, one_hot = FALSE, 
                      no_miss = TRUE, quick = FALSE) {
  
  src <- match.arg(src, c("miiv", "anzics", "aics", "nzics"))
  
  root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
  fl_path <- file.path(root, "data", paste0("dat-", src, "-", outcome, ".RData"))
  if (file.exists(fl_path)) {
  
    load(fl_path)
    sfm <- attr(dat, "sfm")
  } else {
    
    if (is.element(src, c("anzics", "aics", "nzics"))) {
      
      sel_coh <- load_concepts(c("adm_episode", "hosp_episode", "age", "adm_year", 
                                 "death"), "anzics", verbose = FALSE)
      
      if (outcome == "death") {
        
        patient_ids <- id_col(sel_coh[adm_episode %in% c(0, 1) & age >= 18 &
                                        adm_year >= 2018])
        
      } else if (outcome == "readm") {
        
        patient_ids <- id_col(
          sel_coh[(adm_episode %in% c(0, 1)) & (hosp_episode %in% c(0, 1)) & 
                    age >= 18 & adm_year >= 2018 & (death != TRUE)]
        )
        
        readm <- sel_coh[, c(meta_vars(sel_coh), "hosp_episode"), with = FALSE]
      }
      
      dat <- load_concepts(c("death", "age", "country", "apache_iii_rod", 
                             "apache_iii_diag", "sex", "indig", "elective", 
                             "adm_diag", "irsad"), "anzics", 
                           patient_ids = patient_ids, verbose = FALSE)
      
      if (outcome == "readm") {
        
        dat <- merge(dat, readm, all.x = TRUE)
        dat[, readm := hosp_episode]
        dat[, c("hosp_episode") := NULL]
      }
      
      dat[, sex := as.integer(sex == "Male")]
      dat[, majority := 1 - indig]
      
      dat[, c(index_var(dat), "indig") := NULL]
      imp_lst <- list(
        age = 65,
        apache_iii_rod = median(dat$apache_iii_rod, na.rm = TRUE),
        irsad = median(dat$irsad, na.rm = TRUE),
        death = FALSE,
        apache_iii_diag = 0,
        majority = NA,
        elective = NA,
        sex = NA,
        adm_diag = "OTH"
      )
      
      for (i in seq_len(ncol(dat))) {
        
        var <- names(dat)[i]
        if (any(is.na(dat[[var]])) & !is.null(imp_lst[[var]]))
          dat[is.na(get(var)), c(var) := imp_lst[[var]]]
      }
      
      # subset to Australia or New Zealand accordingly
      if (src == "aics") {
        
        dat <- dat[country == "Australia"]
        dat[, c("country") := NULL]
      } else if (src == "nzics") {
        
        dat <- dat[country == "New Zealand"]
        dat[, c("country", "irsad") := NULL]
      } else dat[, c("irsad") := NULL]
      
      # create diag_dt
      diag_dt <- as.data.table(anzics$d_diagnoses)
      diag_dt <- setnames(diag_dt, names(diag_dt), 
                          c("apache_iii_diag", "diag_name", "group_name"))
      diag_dt[, diag_group := floor(apache_iii_diag / 100)]
      
      sfm <- list(X = "majority", Z = c("age", "sex", "country", "irsad"),
                  W = c("apache_iii_diag", "elective", "apache_iii_rod"),
                  Y = outcome)
      
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
                               "elective", "sex", "race"), 
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
        elective = NA,
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
      
      sfm <- list(X = "majority", Z = c("age", "sex"),
                  W = c("charlson", "acu_24", "diag_index", "elective"),
                  Y = outcome)
      
      attr(dat, "diag_col") <- "adm_diag"
      attr(dat, "diag_dt") <- diag_dt
    }
    
    attr(dat, "sfm") <- sfm
    save(dat, file = fl_path)
  }
  
  if (no_miss) {
    
    dat <- dat[complete.cases(dat)]
  } else {

    dat <- dat[complete.cases(dat[, -c("majority")])]
  }
  
  if (is.element(src, c("aics", "nzics"))) sfm$Z <- setdiff(sfm$Z, "country")
  if (is.element(src, c("anzics", "nzics"))) sfm$Z <- setdiff(sfm$Z, "irsad")
  
  if (src != "miiv" & split_elective) {
    
    dat[apache_iii_diag >= 1200, 
        apache_iii_diag := apache_iii_diag + 2000 * elective]
    sfm$W <- setdiff(sfm$W, "elective")
  } else if (src == "miiv" & split_elective) {
    
    dat[diag_index >= 5, diag_index := diag_index + 20 * elective]
    sfm$W <- setdiff(sfm$W, "elective")
    attr(dat, "diag_col") <- "diag_index"
  }
  
  if (one_hot) {
    
    diag_col <- attr(dat, "diag_col")
    dat[, c(diag_col) := as.factor(get(diag_col))]
    diag_mat <- model.matrix(~ . - 1, dat[, diag_col, with = FALSE])[, -1]
    dat <- cbind(dat, diag_mat)
    sfm$W <- c(setdiff(sfm$W, diag_col), colnames(diag_mat))
  }
  
  dat[, age := round(age)]
  attr(dat, "sfm") <- sfm
  attr(dat, "src") <- src
  dat
}
