
ricu:::init_proj()

rr_boot <- function(country, nboot = 50) {
  
  c(pop_dat_org, dat_org) %<-% pop_and_dat(country)
  ret <- c()
  for (boot in seq_len(nboot)) {
    
    dat <- copy(dat_org)
    pop_dat <- copy(pop_dat_org)
    set.seed(boot)
    if (boot > 1) {
      
      boot_idx <- sample(nrow(dat), replace = TRUE)
      dat <- dat[boot_idx]
    }
    
    dat[, diag_grp := as.integer(diag_grp <= 11)]
    
    ts_risk <- as.data.table(
      expand.grid(age = unique(dat$age), diag_grp = unique(dat$diag_grp),
                  majority = c(0, 1), year = unique(dat$year))
    )
    
    ts_risk <- merge(
      ts_risk, dat[, .N, by = c("age", "diag_grp", "majority", "year")],
      all.x = TRUE
    )
    ts_risk[is.na(N), N := 0]
    
    ts_risk <- merge(
      ts_risk, pop_dat[, sum(value), by = c("age", "majority", "year")], 
      by = c("age", "majority", "year"), all.x = TRUE
    )
    
    wgh_dat <- dat[, .N, by = c("age")][, list(age = age, wgh = N/sum(N))]
    
    ts_risk <- merge(
      ts_risk[, list(risk = sum(N) / sum(V1)), 
              by = c("diag_grp", "majority", "age")],
      wgh_dat, by = c("age"), all.x = TRUE
    )
    
    ts_risk <- ts_risk[, list(risk = sum(risk * wgh) / sum(wgh)), 
                       by = c("majority", "diag_grp")]
    
    res <- ts_risk[, list(rr = risk[majority == 0] / risk[majority == 1]), 
                   by = c("diag_grp")]
    res[, seed := boot]
    ret <- rbind(ret, res)
  }
  
  ret
} 

for (country in c("AU", "NZ")) {
  
  cat(country, "\n")
  rrb <- rr_boot(country, nboot = 100)
  print(rrb[, mean(rr), by = "diag_grp"])
}
