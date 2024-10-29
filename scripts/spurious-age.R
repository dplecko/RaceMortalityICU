
#' Investigating spurious effects and age distributions.
ricu:::init_proj()

# Load datasets
datasets <- list(
  US = load_data("miiv", quick = TRUE)[, c("age", "majority"), with = FALSE],
  AU = load_data("aics"),
  NZ = load_data("nzics")
)

# Plot settings for each dataset
plot_params <- list(
  US = list(title = "United States", fill_labels = c("African-American", "White")),
  AU = list(title = "Australia", fill_labels = c("First Nations", "Majority")),
  NZ = list(title = "New Zealand", fill_labels = c("Māori", "Majority"))
)

# Create plots using a loop
plots <- list()
i <- 1
for (name in names(datasets)) {
  data <- datasets[[name]]
  params <- plot_params[[name]]
  
  plots[[i]] <- ggplot(data, aes(x = age, fill = factor(majority))) +
    theme_minimal() + ylab("Probability Density") + xlab("Age") +
    scale_fill_discrete(name = "Group", labels = params$fill_labels) +
    theme(
      legend.position = "inside", legend.position.inside = c(0.25, 0.8),
      legend.box.background = element_rect(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    geom_density(alpha = 0.4) + ggtitle(params$title)
  
  i <- i + 1
}

# Combine plots into a single plot grid
cowplot::plot_grid(plotlist = plots, ncol = 3L, labels = c("(A)", "(B)", "(C)"))
ggsave("results/age-distributions.png", bg = "white", width = 18, height = 5)
