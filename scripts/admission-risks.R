
#' Inspection of baseline risks of ICU admission. Using the workhorse function
#' rr_compute(). 
ricu:::init_proj()
set.seed(2024)

# risk stratified by year, APACHE-III diagnosis group:
ts_rr <- rr_compute(c("year"), "apache_group")

# specify the range for the data analysis, from 2018 to 2024
intrvl <- c(2018, 2024)

# plot the admission risk ratio for different admission categories
ggplot(ts_rr[(diag_grp %in% 1:9 | diag_grp %in% c(11:19) | diag_grp %in% c(31:39))], 
             aes(x = year, y = log(rr), color = diag_name)) +
  geom_line(linewidth = 0.5) + theme_bw() + geom_point() +
  xlab("Year") + ylab("log(Risk Ratio of Admission)") +
  scale_x_continuous(breaks = seq(intrvl[1], intrvl[2])) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_discrete(name = "Diagnosis group") +
  facet_grid(cols = vars(diag_cluster), # rows = vars(country),
             scales = "free_y") + guides(text = NULL) +
  theme(
    legend.position = "right",
    legend.box.background = element_rect(),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    title = element_text(size = 16),
    strip.text = element_text(size = 14)
  ) +
  geom_text_repel(
    aes(label = ifelse(year == intrvl[1] + diag_grp %% (intrvl[2] - intrvl[1] + 1),
                       as.character(diag_name), "")),
    show.legend = FALSE, max.overlaps = 26
  )

ggsave("results/ts-diag-risks.png", width = 15, height = 5)
  
# compute risks stratified by age quartiles, admission type
age_adm_rr <- rr_compute(c("age_quart"), "adm_type", nboot = 10)
  
ggplot(age_adm_rr, aes(x = diag_grp, y = age_quart, fill = rr)) +
  geom_tile() + theme_minimal() +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", 
    midpoint = 1, name = "Risk\nRatio"
  ) +
  geom_text(
    aes(label = paste0(round(rr, 1), "\n", "[", 
                       round((rr-1.96*sd), 1), ", ",
                       round((rr+1.96*sd), 1), "]")), 
    color = "black", size = 4
  ) +
  xlab("Diagnosis group") + ylab("Age group (years)") +
  # ggtitle("Baseline Risk of ICU Admission") +
  theme(
    axis.text = element_text(size = rel(1.1)),  # Scale axis tick labels
    axis.title = element_text(size = rel(1.1)) # Scale axis titles
  )

ggsave("results/age-diag-risks.png", width = 7, height = 5, bg = "white")