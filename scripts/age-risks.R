
#' Computing the risk ratio of ICU admission, stratified by age group and type
#' of admission diagnosis.
ricu:::init_proj()

# interpolate and process the census data
cutoff_year <- 2018

for (country in c("AU", "NZ")) {
  
  c(pop_dat, dat) %<-% pop_and_dat(country)
  
  # aggregate risk
  agg_risk <- merge(
    dat[year >= cutoff_year, .N, by = c("age", "diag_grp", "majority")],
    pop_dat[year >= cutoff_year, sum(value), by = c("age", "majority")], 
    by = c("age", "majority"), all.x = TRUE
  )
  
  agg_risk[, risk := N / V1]
  
  pop_rr <- merge(
    agg_risk[majority == 0, c("age", "diag_grp", "risk"), with=FALSE],
    agg_risk[majority == 1, c("age", "diag_grp", "risk"), with=FALSE],
    by = c("age", "diag_grp")
  )
  pop_rr[, rr := risk.x / risk.y ]
  pop_rr[, diag_name := anz_std_diag(diag_grp, split_names = TRUE)]
  
  pop_rr[, age := gsub("years", "", age)]
  p <- ggplot(pop_rr[(diag_grp %in% 1:9 | diag_grp %in% c(11:19) | 
                       diag_grp %in% c(31:39))], 
         aes(x = diag_name, y = age, fill = trim_ends(log(rr), 2))) +
    geom_tile() +
    scale_fill_gradient2(name = "Risk Ratio", low = "blue", high = "red", 
                         mid = "white", midpoint = 0) + 
    theme_minimal() +
    geom_text(aes(label = round(log(rr), 1)),
              color = "red", size = 3) +
    geom_vline(xintercept = 9.5, color = "red") +
    geom_vline(xintercept = 19.5, color = "red") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
          legend.title = element_text(hjust = 0.5)) +
    xlab("Diagnosis group") + ylab("Age group (years)")
  
  ggsave(
    file.path("results", paste0("age-risk-", tolower(country), ".png")),
    plot = p, width = 12, height = 8, bg = "white"
  )
}



