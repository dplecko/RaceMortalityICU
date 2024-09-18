
ricu:::init_proj()

#' * Overlap analysis * 
src <- "anzics"
dat <- load_data(src)

# Constructing the SFM
X <- "majority"
Z <- c("age", "sex")
Y <- "death"
if (src == "anzics") W <- c("apache_iii_rod", "apache_iii_diag") else
  W <- c("acu_24", "charlson", "diag_index")

# Decomposing the Disparity
fcb <- fairness_cookbook(
  data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1
)

nolap <- data.table(
  X = fcb$pw[["px_zw"]] < 0.01 | fcb$pw[["px_zw"]] > 0.99,
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

