
#' Computing the study flowchart numbers.

ricu:::init_proj()
study_flowchart <- function(src) {
  
  if (src == "anzics") {
    
    steps <- list(
      list(cnc = "age", fn = function(x) x < 18),
      list(cnc = "sex", fn = function(x) is.na(x)),
      list(cnc = "elective", fn = function(x) is.na(x)),
      list(cnc = "adm_episode", fn = function(x) x > 1),
      list(cnc = "indig", fn = function(x) is.na(x))
    )
    
    after_2018 <- id_col(
      load_concepts("adm_year", "anzics", verbose = FALSE)[adm_year >= 2018]
    )
    
    dat <- merge(
      id_tbl(stay_id = intersect(anzics$main$ICUStayID, after_2018)),
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
  
  cat("Starting with", nrow(dat), "admissions from", 
      length(unique(dat$site)), "\n")
  for (i in seq_along(steps)) {
    
    ndat <- dat[!steps[[i]][["fn"]](get(steps[[i]][["cnc"]]))]
    cat("Removing", nrow(dat) - nrow(ndat), "admissions based on",
        steps[[i]][["cnc"]], "\n")
    dat <- ndat
  }
  
  cat("Ending with", nrow(dat), "admissions")
  if (src == "anzics") {
    
    dat <- merge(dat, load_concepts("country", "anzics"), all.x = TRUE)
    print(table(dat$country))
    for (cntry in c("Australia", "New Zealand")) {
      
      cat(cntry, " from", length(unique(dat[country == cntry]$site)), "sites\n")
    }
  }
  cat("\n")
}

study_flowchart("anzics")
