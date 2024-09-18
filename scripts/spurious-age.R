
ricu:::init_proj()

nz_age <- load_data("anzics")[country == "NZ"]
aus_age <- load_data("anzics")[country == "AU"]
us_age <- load_data("miiv", quick = TRUE)[, c("age", "majority"), with = FALSE]

us_age[, mean(age), by = "majority"][, diff(V1)]
aus_age[, mean(age), by = "majority"][, diff(V1)]
nz_age[, mean(age), by = "majority"][, diff(V1)]

p1 <- ggplot(
  us_age,
  aes(x = age, fill = factor(majority))
) + theme_minimal() + ylab("Probability Density") + xlab("Age") +
  scale_fill_discrete(name = "Group", labels = c("African-American", "White")) +
  theme(
    legend.position = "inside", legend.position.inside = c(0.25, 0.8),
    legend.box.background = element_rect(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  ) +
  geom_density(alpha = 0.4) + ggtitle("United States")

p2 <- ggplot(
  aus_age,
  aes(x = age, fill = factor(majority))
) + theme_minimal() + ylab("Probability Density") + xlab("Age") +
  theme(
    legend.position = "inside", legend.position.inside = c(0.25, 0.8),
    legend.box.background = element_rect(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  ) +
  geom_density(alpha = 0.4) + 
  scale_fill_discrete(name = "Group", labels = c("First Nations", "Majority")) +
  ggtitle("Australia")

p3 <- ggplot(
  nz_age,
  aes(x = age, fill = factor(majority))
) + theme_minimal() + ylab("Probability Density") + xlab("Age") +
  scale_fill_discrete(name = "Group", labels = c("Māori", "Majority")) +
  theme(
    legend.position = "inside", legend.position.inside = c(0.25, 0.8),
    legend.box.background = element_rect(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  ) +
  geom_density(alpha = 0.4) +
  ggtitle("New Zealand")

cowplot::plot_grid(p2, p3, p1, ncol = 3L, labels = c("(A)", "(B)", "(C)"))
ggsave("results/age-distributions.png", bg = "white", width = 18, height = 5)
