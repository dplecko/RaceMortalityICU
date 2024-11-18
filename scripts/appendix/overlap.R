
ricu:::init_proj()
set.seed(2024)

#' * Overlap analysis * 
src <- "anzics"
dat <- load_data(src)
c(X, Z, W, Y) %<-% attr(dat, "sfm")

# Decomposing the Disparity
fcb <- fairness_cookbook(
  data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, method = "debiasing"
)

eps <- 0.001 # 0.1%
nolap <- data.table(
  X = fcb$pw[["px_zw"]] < eps | fcb$pw[["px_zw"]] > 1-eps,
  W = dat$adm_diag,
  Z = dat$age
)

nolap[, Z := age_grp(Z)]
nolap[, W := std_diag(W)]

mean_dt <- nolap[, list(mean_X = mean(X)), by = c("Z", "W")]
mean_dt <- merge(
  as.data.table(expand.grid(Z = unique(mean_dt$Z), W = unique(mean_dt$W), 
                            stringsAsFactors = FALSE)),
  mean_dt, by = c("Z", "W"), all.x = TRUE
)
mean_dt[is.na(mean_X), mean_X := 0]
ggplot(mean_dt[W != "Other"], aes(x = W, y = Z, fill = trim_ends(mean_X, 0.2))) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, labels = scales::percent,
                       name = "P(extreme weight)") +
  theme_minimal() + 
  geom_text(aes(label = paste0(round(mean_X * 100, 1), "%")), 
            color = "black", size = 3) +
  xlab("Diagnosis group") + ylab("Age group") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
    legend.title = element_text(hjust = 0.5)
  ) +
  geom_vline(xintercept = 4.5, color = "red")

ggsave(paste0("results/overlap-", src, ".png"), width = 10, height = 7, 
       bg = "white")

# Computing the DE for age >= 65 and admission == surgery
lde <- local_de(dat, X, Z, W, Y)

# Using a Gaussian assumption
round(100 * c(mean(lde$estimate) - 1.96 * sd(lde$estimate), 
              mean(lde$estimate) + 1.96 * sd(lde$estimate)), 2)

# sensitivity analysis for threshold impact
eps_seq <- c(0.0001, 0.001, 0.01)
eps_sens <- cbind(summary(fcb)$measures, eps = 0)
for (eps in eps_seq) {
  
  fcb_eps <- fairness_cookbook(
    data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, method = "debiasing",
    eps = eps
  )
  
  eps_sens <- rbind(
    eps_sens, cbind(summary(fcb_eps)$measures, eps = eps)
  )
}

eps_sens[eps_sens$measure %in% c("ctfse", "ctfie"), ]$value <- 
  -eps_sens[eps_sens$measure %in% c("ctfse", "ctfie"), ]$value

meas <- c("tv", "ctfde", "ctfse", "ctfie")
new_meas <- c("Total Variation", "Direct", "Indirect", "Spurious")
eps_sens <- eps_sens[eps_sens$measure %in% meas, ]
eps_sens$measure <- new_meas[match(eps_sens$measure, meas)]

ggplot(eps_sens, aes(x = factor(log(eps, 10)), y = value, group = measure)) +
  geom_line() + geom_point() +
  geom_ribbon(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd),
              linewidth = 0, alpha = 0.4) +
  theme_bw() +
  facet_wrap(~ measure) +
  ylab("Effect Estimate") + xlab(latex2exp::TeX("$\\log_{10}(\\epsilon)$")) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) +
  scale_y_continuous(labels = scales::percent)

ggsave(paste0("results/overlap-sens-", src, ".png"), width = 11, height = 7, 
       bg = "white")
  