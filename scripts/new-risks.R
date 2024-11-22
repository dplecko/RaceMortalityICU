
ricu:::init_proj()

# risk stratified by year, AP3 group: E_cfd = "year", dg_mod = "apache_group"
ts_rr <- rr_compute(c("year"), "apache_group")

intrvl <- c(2018, 2024)

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
    legend.position = "bottom",
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
  
# risk stratified by age quartiles, admission type
age_adm_rr <- rr_compute(c("age_quart"), "adm_type")
  
ggplot(age_adm_rr, aes(x = diag_grp, y = age_quart, fill = log(rr))) +
  geom_tile() + theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, name = "RR", labels = scales::percent) +
  geom_text(
    aes(label = round(rr, 1)), 
    color = "black", size = 3
  ) +
  xlab("Diagnosis group") + ylab("Age group (years)")

ggsave("results/cache/age-diag-rr.png", width = 7, height = 5, bg = "white")
