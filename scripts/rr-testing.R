
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
    
    dat[, diag_grp := (diag_grp <= 11) + (diag_grp <= 22)]
    
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
  
  ret[, diag_grp_name := ifelse(diag_grp == 0, "Elective Surgery",
                                ifelse(diag_grp == 1, "Emergency Surgery",
                                       "Medical"))]
  ret[, c("country") := country]
  ret
} 

rrb <- NULL
for (country in c("AU", "NZ")) rrb <- rbind(rrb, rr_boot(country, nboot = 100))

rrb <- rrb[, list(rr = rr[seed==1], sd = sd(rr)), 
           by = c("diag_grp_name", "country")] 
print(rrb)

ggplot(rrb, aes(x = diag_grp_name, y = rr, fill = country)) +
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = rr - 1.96 * sd, ymax = rr + 1.96 * sd),
                position = position_dodge(0.9),
                color = "black", width = 0.4) +
  theme_bw() + xlab("Admission Type") + ylab("Risk Ratio") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "orange",
             linewidth = 1.25) +
  scale_fill_discrete(name = "Country", labels = c("Australia", "New Zealand")) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(),
    title = element_text(size = 16)
  )

ggsave("results/rr-testing.png", width = 8, height = 5)
