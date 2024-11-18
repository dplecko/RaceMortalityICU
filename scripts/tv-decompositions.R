
#' Performing a causal explanation of the disparity between groups.
#' The total variation (TV) measure is decomposed into direct, indirect, and
#' spurious effects.
ricu:::init_proj()
set.seed(2024)

res <- NULL
for (src in c("anzics", "miiv", "nzics", "aics")) {
  
  dat <- load_data(src)
  cat("Decomposing TV on", srcwrap(src), method, "with SFM\n")
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  print_sfm(X, Z, W, Y)
  
  fcb <- fairness_cookbook(
    data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
    method = "debias"
  )
  
  res <- rbind(
    res, cbind(summary(fcb)$measures, Dataset = srcwrap(src), 
               method = method)
  )
}

xlabz <- c(
  tv = "Total Variation", ctfde = "Direct Effect",
  ctfie = "Indirect Effect", ctfse = "Spurious Effect"
)

res <- res[res$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]

# change IE, SE signs of easier interpretability
res[res$measure %in% c("ctfse", "ctfie"), ]$value <- 
  - res[res$measure %in% c("ctfse", "ctfie"), ]$value
res$measure <- as.factor(res$measure)

ggplot(res, aes(x = measure, y = value, fill = interaction(Dataset, method),
                ymin = value - 1.96 * sd, ymax = value + 1.96 * sd)) +
  geom_bar(position="dodge", stat = "identity", linewidth = 1.2,
           color = "black") +
  theme_minimal() +
  geom_errorbar(
    aes(group = interaction(Dataset, method)),
    position = position_dodge(0.9),
    color = "black", width = 0.25
  ) +
  theme(
    legend.position = "right",
    legend.position.inside = c(0.4, 0.8),
    legend.box.background = element_rect(),
    legend.text = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    title = element_text(size = 16)
  ) + scale_x_discrete(labels = xlabz) +
  xlab("Causal Fairness Measure") + ylab("Value") +
  scale_y_continuous(labels = scales::percent)

ggsave("results/tv-decomp-no-elective.png", width = 10, height = 6, bg = "white")
