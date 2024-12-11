
#' Inspection of baseline risks of ICU admission.
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
  
# risk stratified by age quartiles, admission type
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

# age x admission risks
# pop_rr <- rr_compute(c("age"), "apache_group")
# p <- ggplot(pop_rr[(diag_grp %in% 1:9 | diag_grp %in% c(11:19) | 
#                       diag_grp %in% c(31:39))], 
#             aes(x = diag_name, y = age, fill = trim_ends(log(rr), 2))) +
#   geom_tile() +
#   scale_fill_gradient2(name = "Risk Ratio", low = "blue", high = "red", 
#                        mid = "white", midpoint = 0) + 
#   theme_minimal() +
#   geom_text(aes(label = round(log(rr), 1)),
#             color = "red", size = 3) +
#   geom_vline(xintercept = 9.5, color = "red") +
#   geom_vline(xintercept = 19.5, color = "red") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
#         legend.title = element_text(hjust = 0.5)) +
#   xlab("Diagnosis group") + ylab("Age group (years)")
# 
# ggsave(
#   file.path("results", paste0("age-risk-", tolower(country), ".png")),
#   plot = p, width = 12, height = 8, bg = "white"
# )

# risk ratio testing
# rr_compute(NULL, "adm_type")
# ggplot(rrb, aes(x = diag_grp_name, y = rr, fill = country)) +
#   geom_col(position = "dodge", color = "black") +
#   geom_errorbar(aes(ymin = rr - 1.96 * sd, ymax = rr + 1.96 * sd),
#                 position = position_dodge(0.9),
#                 color = "black", width = 0.4) +
#   theme_bw() + xlab("Admission Type") + ylab("Risk Ratio") +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "orange",
#              linewidth = 1.25) +
#   scale_fill_discrete(name = "Country", labels = c("Australia", "New Zealand")) +
#   theme(
#     legend.position = "bottom",
#     legend.text = element_text(size = 12),
#     axis.text = element_text(size = 16),
#     axis.title.x = element_text(size = 18),
#     axis.text.x = element_text(),
#     title = element_text(size = 16)
#   )
# ggsave("results/rr-testing.png", width = 8, height = 5)
