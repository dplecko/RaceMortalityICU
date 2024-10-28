
ricu:::init_proj()

# interpolate and process the census data
intrvl <- c(2018, 2022)

ts_risk <- function(country, boot = 1, split_elective = TRUE) {
  
  age_adjust <- TRUE
  c(pop_dat, dat) %<-% pop_and_dat(country)
  
  if (boot > 1) {
    
    boot_idx <- sample(nrow(dat), replace = TRUE)
    dat <- dat[boot_idx]
  }
  
  # massage the diag_grp
  dat[, diag_grp_el := diag_grp]
  
  if (split_elective) {
    
    dat[diag_grp >= 12, diag_grp_el := diag_grp_el + 100 * elective]
  }
  
  
  # get a table
  ts_risk <- as.data.table(
    expand.grid(age = unique(dat$age), diag_grp_el = unique(dat$diag_grp_el),
                majority = c(0, 1), year = unique(dat$year))
  )
  
  ts_risk <- merge(
    ts_risk, dat[, .N, by = c("age", "diag_grp_el", "majority", "year")],
    all.x = TRUE
  )
  ts_risk[is.na(N), N := 0]
  
  # risk by year
  ts_risk <- merge(
    ts_risk, pop_dat[, sum(value), by = c("age", "majority", "year")], 
    by = c("age", "majority", "year"), all.x = TRUE
  )
  
  if (age_adjust) {
    
    wgh_dat <- dat[, .N, by = c("age", "year")][, list(age = age, wgh = N/sum(N)),
                                                by = "year"]
    
    # # compute N(people) in each age group (both indigenous + majority)
    # wgh_dat <- pop_dat[, sum(value), by = c("age", "year")]
    # 
    # # compute P(age group) in each year => includes < 18 years old
    # wgh_dat <- wgh_dat[, list(age = age, wgh = V1 / sum(V1)), by = c("year")]
    
    ts_risk <- merge(ts_risk, wgh_dat, by = c("age", "year"), all.x = TRUE)
    ts_risk[, risk := N / V1]
    
    ts_risk <- ts_risk[, list(risk = sum(risk * wgh) / sum(wgh)), 
                       by = c("year", "majority", "diag_grp_el")] 
  } else {
    # pooled estimate
    ts_risk <- ts_risk[, list(risk = sum(N) / sum(V1)), 
                       by = c("year", "diag_grp_el", "majority")]
  }

  ts_rr <- merge(
    ts_risk[majority == 0, c("year", "diag_grp_el", "risk"), with=FALSE],
    ts_risk[majority == 1, c("year", "diag_grp_el", "risk"), with=FALSE],
    by = c("year", "diag_grp_el")
  )
  
  ts_rr[, rr := risk.x / risk.y]
  ts_rr[, diag_grp := diag_grp_el]
  if (split_elective) {
  
    ts_rr[diag_grp_el > 100, diag_grp := diag_grp_el - 100]
  }
  
  ts_rr[, diag_name := anz_std_diag(diag_grp)]
  ts_rr[, diag_grp_name := ifelse(diag_grp_el <= 11, "Medical", "Surgical")]
  
  if (split_elective) {
    
    ts_rr[diag_grp_el >= 12, 
          diag_grp_name := ifelse(diag_grp_el > 100, 
                                  paste(diag_grp_name, "(Elective)"),
                                  paste(diag_grp_name, "(Non-elective)"))]
  }
  
  if (country == "AU") ts_rr[, country := "Australia"] else 
    ts_rr[, country := "New Zealand"]
  
  ts_rr
}

ts_rr <- rbind(ts_risk("AU"), ts_risk("NZ"))
ts_rr[risk.x == 0 & risk.y == 0, rr := 1]

ggplot(ts_rr[(diag_grp %in% 1:9 | diag_grp %in% c(11:19)) & 
              year >= intrvl[1] & year <= intrvl[2]], 
       aes(x = year, y = log(rr), color = diag_name)) +
  geom_line(linewidth = 0.5) + theme_bw() + geom_point() +
  xlab("Year") + ylab("log(Risk Ratio of Admission)") +
  scale_x_continuous(breaks = seq(intrvl[1], intrvl[2])) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_discrete(name = "Diagnosis group") +
  scale_linetype_discrete(name = "Admission type", 
                          labels = c("Medical", "Surgical")) +
  geom_text_repel(
    aes(label = ifelse(year == intrvl[1] + 
                         as.integer(diag_name) %% (intrvl[2] - intrvl[1] + 1), 
                       as.character(diag_name), "")),
    show.legend = FALSE, max.overlaps = 26
  ) + guides(text = NULL) +
  facet_grid(cols = vars(diag_grp_name), rows = vars(country),
             scales = "free_y")

ggsave("results/pop-risk-ts.png", width = 16, height = 8)
