
ricu:::init_proj()
dat <- load_data("anzics")

#' * Causal Explanation of Disparity *

# Constructing the SFM
X <- "majority"
Z <- c("age", "sex")
W <- c("apache_iii_rod", "apache_iii_diag")
Y <- "death"

# Decomposing the Disparity
fcb <- fairness_cookbook(
  data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1
)

#' * TODO: this needs to be updated (!) *
# Swap Labels
dat[, majority := 1 - majority]
fcb_swap <- fairness_cookbook(
  data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1
)

#' * TODO: implement interaction testing *
p1 <- autoplot(fcb)
p2 <- autoplot(fcb_swap)
cowplot::plot_grid(p1, p2, labels = c("(a)", "(b)"), ncol = 2L)
