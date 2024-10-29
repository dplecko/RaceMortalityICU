
#' Sensitivity analysis for the effects of missing data. Multiple imputation is
#' performed, with the indigenous status imputed. The results are compared with
#' the decomposition of the TV measure obtained on the complete dataset, with no
#' indigenous status missingness.

# obtaining the TV decomposition on full data
src <- "anzics"
dat <- load_data(src)
c(X, Z, W, Y) %<-% attr(dat, "sfm")
print_sfm(X, Z, W, Y)

fcb_complete <- fairness_cookbook(
  data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
  method = "debiasing"
)

res <- summary(fcb_complete)$measures
# change IE, SE signs of easier interpretability
res[res$measure %in% c("ctfse", "ctfie"), ]$value <- 
  - res[res$measure %in% c("ctfse", "ctfie"), ]$value

mi_data <- function(src, n_imp) {
  
  dat <- load_data(src, no_miss = FALSE)
  cmp_dat <- dat[complete.cases(dat)]
  mis_dat <- dat[!complete.cases(dat)]
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  
  # train on the complete data
  x_learn <- cv_xgb(cmp_dat[, c(Z, W, Y), with=FALSE], y = cmp_dat[[X]])
  mis_probs <- pred_xgb(x_learn, mis_dat[, c(Z, W, Y), with=FALSE])
  mi_lst <- list()
  for (i in seq_len(n_imp)) {
    
    imp <- rbinom(length(mis_probs), size = 1, prob = mis_probs)
    mis_dat[, c(X) := imp]
    mi_lst[[i]] <- rbind(cmp_dat, mis_dat)
  }
  
  mi_lst
}

qnormmix <- function(p, value, sd) {
  
  k <- length(value)
  samples <- replicate(10000, {
    component <- sample(1:k, 1, prob = lambda)
    rnorm(1, mean = value[component], sd = sd[component])
  })
  
  c(mean(value), lapply(p, function(pi) quantile(samples, probs = pi)))
}

n_imp <- 10
mi_lst <- mi_data(src, n_imp)
mi_res <- NULL
for (i in seq_len(n_imp)) {
  
  fcb <- fairness_cookbook(
    data = mi_lst[[i]], X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
    method = "debiasing"
  )
  
  add <- summary(fcb)$measures
  # change IE, SE signs of easier interpretability
  add[add$measure %in% c("ctfse", "ctfie"), ]$value <- 
    - add[add$measure %in% c("ctfse", "ctfie"), ]$value
  add$mi_rep <- i
  mi_res <- rbind(mi_res, add)
}


mi_res <- as.data.table(mi_res)
mi_res[, qnormmix(c(0.025, 0.975), value, sd), by = "measure"]
mi_res <- setnames(mi_res, c("V1", "V2", "V3"), c("value", "lwr", "upr"))

res <- res[, list(value = value, lwr = value - 1.96 * sd, upr = value + 1.96 * sd), 
           by = "measure"]

res <- res[res$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]
res$measure <- as.factor(res$measure)

plt <- rbind(cbind(res, type = "Complete"), 
             cbind(mi_res, type = "Multiple Imputation"))
xlabz <- c(
  tv = "Total Variation", ctfde = "Direct Effect",
  ctfie = "Indirect Effect", ctfse = "Spurious Effect"
)

ggplot(res, aes(x = measure, y = value, fill = factor(type),
                ymin = lwr, ymax = upr)) +
  geom_bar(position="dodge", stat = "identity", linewidth = 1.2,
           color = "black") +
  theme_minimal() +
  geom_errorbar(
    aes(group = factor(type)),
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

