
ricu:::init_proj()

#' * Causal Explanation of Disparity (ANZ) *
set.seed(2024)
dat <- load_data("anzics")

# Constructing the SFM
X <- "majority"
Z <- c("age", "sex", "country")
Y <- "death"
W <- c("apache_iii_rod", "apache_iii_diag")

# Decomposing the disparity
fcb_anz <- fairness_cookbook(
  data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1
)

#' * Causal Explanation of Disparity (US) *
dat <- load_data("miiv")

# Updating the SFM for new mediators
Z <- c("age", "sex")
W <- c("charlson", "acu_24", "diag_index")

# Decomposing the Disparity
fcb_us <- fairness_cookbook(
  data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1
)

res <- rbind(
  cbind(summary(fcb_anz)$measures, Dataset = "ANZICS APD"),
  cbind(summary(fcb_us)$measures, Dataset = "MIMIC-IV")
)

xlabz <- c(
  tv = "Total Variation", # latex2exp::TeX("$TV_{x_0, x_1}(y)$"),
  ctfde = "Direct Effect", # latex2exp::TeX("$Ctf$-$DE_{x_0, x_1}(y | x_0)$"),
  ctfie = "Indirect Effect", # latex2exp::TeX("$Ctf$-$IE_{x_1, x_0}(y | x_0)$"),
  ctfse = "Spurious Effect" # latex2exp::TeX("$Ctf$-$SE_{x_1, x_0}(y)$")
)

res <- res[res$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]

# change IE, SE signs of easier interpretability
res[res$measure %in% c("ctfse", "ctfie"), ]$value <- 
  - res[res$measure %in% c("ctfse", "ctfie"), ]$value
res$measure <- as.factor(res$measure)

ggplot(res, aes(x = measure, y = value, fill = Dataset,
                ymin = value - 2.58*sd, ymax = value + 2.58*sd)) +
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

ggsave("results/tv-decomp.png", width = 10, height = 6, bg = "white")
