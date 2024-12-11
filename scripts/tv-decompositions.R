
#' Performing a causal explanation of the disparity between groups.
#' The total variation (TV) measure is decomposed into direct, indirect, and
#' spurious effects.
ricu:::init_proj()
set.seed(2024)

res <- c()
for (src in c("miiv", "aics")) {
  
  dat <- load_data(src, split_elective = TRUE)
  cat("Decomposing TV on", srcwrap(src), "with SFM\n")
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  print_sfm(X, Z, W, Y)
  
  fcb <- fairness_cookbook(
    data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
    method = "debiasing"
  )
  
  res <- rbind(
    res, cbind(summary(fcb)$measures, Dataset = srcwrap(src))
  )
}

xlabz <- c(
  tv = "Total Variation", ctfde = "Direct",
  ctfie = "Indirect", ctfse = "Confounded"
)

res <- res[res$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]

# change IE, SE signs of easier interpretability
res[res$measure %in% c("ctfse", "ctfie"), ]$value <- 
  - res[res$measure %in% c("ctfse", "ctfie"), ]$value
res$measure <- factor(res$measure, levels = c("ctfse", "ctfie", "ctfde", "tv"))
ggplot(res, aes(x = measure, y = value, fill = Dataset,
                ymin = value - 1.96 * sd, ymax = value + 1.96 * sd)) +
  geom_bar(position="dodge", stat = "identity", linewidth = 1.2,
           color = "black") +
  theme_minimal() +
  geom_errorbar(
    aes(group = Dataset),
    position = position_dodge(0.9),
    color = "black", width = 0.25
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.75, 0.25),
    legend.box.background = element_rect(),
    legend.text = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    # axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
    title = element_text(size = 16)
  ) + scale_x_discrete(labels = xlabz) +
  xlab("Causal Fairness Measure") + ylab("Value") +
  scale_y_continuous(labels = scales::percent)

ggsave("results/tv-decomp.png", width = 8, height = 4.5, bg = "white")
