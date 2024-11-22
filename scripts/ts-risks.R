
#' Investigation the risk ratio of ICU admission between 2018 and 2024.
ricu:::init_proj()

# interpolate and process the census data
intrvl <- c(2018, 2024)

ts_risk <- function(country, boot = 1, split_elective = TRUE) {
  
  age_adjust <- TRUE
  c(pop_dat, dat) %<-% pop_and_dat(country)
  
  if (boot > 1) {
    
    boot_idx <- sample(nrow(dat), replace = TRUE)
    dat <- dat[boot_idx]
  }
  
  # get a table
  ts_risk <- 
  
  ts_risk <- merge(
    ts_risk, dat[, .N, by = c("age", "diag_grp", "majority", "year")],
    all.x = TRUE
  )
  ts_risk[is.na(N), N := 0]
  
  # risk by year
  ts_risk <- merge(
    ts_risk, pop_dat[, sum(value), by = c("age", "majority", "year")], 
    by = c("age", "majority", "year"), all.x = TRUE
  )
  
  browser()
  
  age_strat <- copy(ts_risk)
  age_strat <- age_strat[diag_grp < 12, list(risk = sum(N) / sum(V1)), 
                      by = c("year", "age", "majority")]
  age_strat <- merge(age_strat[majority == 0], age_strat[majority == 1], 
                     by = c("year", "age")) 
  age_strat[, rr := risk.x / risk.y]
  
  ggplot(age_strat, aes(x = age, y = rr)) +
    facet_wrap(~ year) + theme_bw() +
    geom_line() + geom_point()
  
  if (age_adjust) {
    
    wgh_dat <- dat[, .N, by = c("age", "year")][, 
                   list(age = age, wgh = N/sum(N)),
                   by = "year"]
    
    ts_risk <- merge(ts_risk, wgh_dat, by = c("age", "year"), all.x = TRUE)
    ts_risk[, risk := N / V1]
    
    ts_risk <- ts_risk[, list(risk = sum(risk * wgh) / sum(wgh)), 
                       by = c("year", "majority", "diag_grp")] 
  } else {
    # pooled estimate
    ts_risk <- ts_risk[, list(risk = sum(N) / sum(V1)), 
                       by = c("year", "diag_grp", "majority")]
  }

  ts_rr <- merge(
    ts_risk[majority == 0, c("year", "diag_grp", "risk"), with=FALSE],
    ts_risk[majority == 1, c("year", "diag_grp", "risk"), with=FALSE],
    by = c("year", "diag_grp")
  )
  
  ts_rr[, rr := risk.x / risk.y]
  ts_rr[, diag_grp := diag_grp]
  
  ts_rr[, diag_name := anz_std_diag(diag_grp)]
  ts_rr[, diag_grp_name := ifelse(diag_grp <= 11, "Medical", 
                                  ifelse(diag_grp <= 22, "Surgical (Emergency)", 
                                         "Surgical (Elective)"))]
  
  if (country == "AU") ts_rr[, country := "Australia"] else 
    ts_rr[, country := "New Zealand"]
  
  ts_rr
}

ts_rr <- rbind(ts_risk("AU"), ts_risk("NZ"))
ts_rr[risk.x == 0 & risk.y == 0, rr := 1]
lvl_ord <- c("Medical", "Surgical (Emergency)", "Surgical (Elective)")
ts_rr[, diag_grp_name := factor(diag_grp_name, levels = lvl_ord)]

p1 <- ggplot(ts_rr[(diag_grp %in% 1:9 | diag_grp %in% c(11:19) | diag_grp %in% c(31:39)) & 
              year >= intrvl[1] & year <= intrvl[2]], 
       aes(x = year, y = log(rr), color = diag_name)) +
  geom_line(linewidth = 0.5) + theme_bw() + geom_point() +
  xlab("Year") + ylab("log(Risk Ratio of Admission)") +
  scale_x_continuous(breaks = seq(intrvl[1], intrvl[2])) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_discrete(name = "Diagnosis group") +
  geom_text_repel(
    aes(label = ifelse(year == intrvl[1] + 
                         as.integer(diag_name) %% (intrvl[2] - intrvl[1] + 1), 
                       as.character(diag_name), "")),
    show.legend = FALSE, max.overlaps = 26
  ) + guides(text = NULL) +
  facet_grid(cols = vars(diag_grp_name), rows = vars(country),
             scales = "free_y") +
  theme(
    legend.box.background = element_rect(),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    title = element_text(size = 16),
    strip.text = element_text(size = 14)
  )

ggsave("results/pop-risk-ts.png", plot = p1, width = 18, height = 9)
