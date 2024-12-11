
rr_compute <- function(E_cfd, dg_mod, country = c("AU", "NZ"), 
                       nboot = 1, rep = 1) {

  if (nboot > 1) {
    
    ret <- do.call(
      rbind, lapply(
        seq_len(nboot), 
        function(i) {
          rri <- rr_compute(E_cfd, dg_mod, country, nboot = 1, rep = i)
          rri[, rep := i]
        }
      )
    )
    by_vars <- setdiff(names(ret), c("country", "rep", "rr"))
    ret <- merge(ret[rep == 1], ret[, list(sd = sd(rr)), by = by_vars], 
                 by = by_vars)
    return(ret[, rep := NULL])
  }
  
  country <- match.arg(country, c("AU", "NZ"))
  
  cfd_E <- function(sel_covs = NULL) {
    
    age_bins <- c("15-19 years", "20-24 years", 
                  "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                  "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", 
                  "75-79 years", "80-84 years", "85+ years")
    age_lst <- lapply(age_bins, function(i) list(age = i))
    names(age_lst) <- age_bins
    
    years <- seq(2018, 2024)
    year_lst <- lapply(years, function(i) list(year = i))
    names(year_lst) <- years
    
    covs <- list(
      age = age_lst,
      age_quart = list(
        `18-49` = list(age = c("15-19 years", "20-24 years", "25-29 years", 
                               "30-34 years", "35-39 years", "40-44 years", 
                               "45-49 years")),
        `50-64` = list(age = c("50-54 years", "55-59 years", "60-64 years")), 
        `65-74` = list(age = c("65-69 years", "70-74 years")), 
        `74-100` = list(age = c("75-79 years", "80-84 years", "85+ years"))
      ),
      year = year_lst
    )
    
    ret <- as.data.table(expand.grid(age = age_bins, year = years,
                                     stringsAsFactors = FALSE))
    
    if (is.null(sel_covs)) {
      
      sel_covs <- "grp"
      ret <- cbind(ret, grp = 1)
    } else {
      
      for (cov in setdiff(sel_covs, c("age", "year"))) {
        
        targ_cov <- names(covs[[cov]][[1]])
        ret[, c(cov) := NA_character_]
        for (lvl in names(covs[[cov]])) {
          
          ret[get(targ_cov) %in% covs[[cov]][[lvl]][[1]], c(cov) := lvl]
        }
      }
    }
    
    ret
  }
  
  c(pop, adm) %<-% pop_and_dat(country, dg_mod = dg_mod)
  
  if (rep > 1) adm <- adm[sample(nrow(adm), replace = TRUE)]
  
  cfds <- c("age", "year")
  all <- c("diag_grp", "majority", cfds)
  
  basis <- lapply(all, function(x) unique(adm[[x]]))
  names(basis) <- all
  adm_cnt <- as.data.table(expand.grid(basis))
  adm_cnt <- merge(adm_cnt, adm[, list(n = .N), by = all], by = all, all.x = TRUE)
  adm_cnt[is.na(n), n := 0]
  
  risk <- merge(
    adm_cnt, pop[, list(N = sum(value)), by = c(cfds, "majority")], 
    by = c(cfds, "majority"), all.x = TRUE
  )
  
  risk[, rsk := n / N]
  
  # get P(z,t | E)
  # first, get P(z, t) joint
  wgh <- merge(cfd_E(E_cfd), pop[, list(N = sum(value)), by = c(cfds)], by = cfds)
  if (is.null(E_cfd)) E_cfd <- "grp"
  
  wgh[, weight := N / sum(N), by = E_cfd]
  
  risk <- merge(risk, wgh[, unique(c(cfds, E_cfd, "weight")), with=FALSE], by = cfds)
  
  agg_risk <- risk[, list(rsk = sum(rsk * weight) / sum(weight)), 
                   by = c("diag_grp", "majority", E_cfd)]
  
  agg_risk <- merge(agg_risk[majority == 0], agg_risk[majority == 1],
                    by = c("diag_grp", E_cfd))
  agg_risk[, rr := rsk.x / rsk.y]
  agg_risk[, country := ifelse(country == "AU", "Australia", "New Zealand")]
  ret_covs <- c("diag_grp", E_cfd, "rr", "country")
  lvl_ord <- c("Medical", "Surgical (Emergency)", "Surgical (Elective)")
  if (dg_mod == "apache_group") {
    
    ret_covs <- c(ret_covs, "diag_name", "diag_cluster")
    agg_risk[, diag_name := anz_std_diag(diag_grp, split_names = FALSE)]
    agg_risk[, diag_cluster := ifelse(diag_grp <= 11, "Medical", 
                                      ifelse(diag_grp <= 22, "Surgical (Emergency)", 
                                             "Surgical (Elective)"))]
    agg_risk[, diag_cluster := factor(diag_cluster, levels = lvl_ord)]
  } else if (dg_mod == "adm_type") {
    
    agg_risk[, diag_grp := factor(diag_grp, levels = lvl_ord)]
  }
  
  agg_risk[, ret_covs, with=FALSE]
}
