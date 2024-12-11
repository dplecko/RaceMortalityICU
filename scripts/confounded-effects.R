
ricu:::init_proj()

age_dat <- function(src) load_data(src)[, c("age", "majority"), with=FALSE]

# Data and settings list
dat_lst <- list(
  list(src = "miiv", title = "United States", labels = c("African-American", "White")),
  list(src = "aics", title = "Australia", labels = c("First Nations", "Majority"))
)

# Initialize an empty list to store the plots
plots <- list()

# Loop to create plots
for (i in seq_along(dat_lst)) {
  
  dat <- dat_lst[[i]] 
  dat$data <- age_dat(dat[["src"]])
  cat("Mean age difference", diff(dat$data[, mean(age), by = "majority"]$V1), "\n")
  plots[[i]] <- ggplot(dat$data, aes(x = age, fill = factor(majority))) +
    theme_minimal() +
    ylab("Probability Density") +
    xlab("Age") +
    scale_fill_discrete(name = "Group", labels = dat$labels) +
    scale_y_continuous(breaks = c(0, 0.01, 0.02)) +
     theme(
      legend.position = "inside", legend.position.inside = c(0.2, 0.8),
      legend.box.background = element_rect(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.text = element_text(size = rel(1.5)),  # Scale axis tick labels
      axis.title = element_text(size = rel(1.5)) # Scale axis titles
    ) +
    geom_density(alpha = 0.4) +
    ggtitle(dat$title)
  
  ggsave(filename = paste0("results/age-", dat[["src"]], ".png"), 
         plot = plots[[i]], bg = "white", width = 6, height = 4)
}

ses <- pmf_compute(load_data("aics"), "irsad")
ggplot(ses, aes(x = ils, y = pmf, fill = factor(majority))) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.5, 
           color = "black", width = 1) + 
  theme_bw() +
  scale_fill_discrete(name = "Group", 
                      labels = c("First Nations", "Majority")) +
  theme(
    legend.position = "inside", legend.position.inside = c(0.7, 0.8),
    legend.box.background = element_rect(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text = element_text(size = rel(1.5)),  # Scale axis tick labels
    axis.title = element_text(size = rel(1.5)) # Scale axis titles
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  xlab("Decile of Socioeconomic Status") + ylab("Probability Mass") +
  ggtitle("Australia") +
  scale_y_continuous(labels = scales::percent)

ggsave("results/ses-distr-aics.png", bg = "white", width = 6, height = 4)
