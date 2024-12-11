
#' Sensitivity analysis for the effects of missing data. Multiple imputation is
#' performed, with the indigenous status imputed. The results are compared with
#' the decomposition of the TV measure obtained on the complete dataset, with no
#' indigenous status missingness.
# grep 'cpu' /proc/stat | awk 'NR>1 {usage=($2+$3+$4)*100/($2+$3+$4+$5); if (usage < 10) print NR-2}'
# nohup taskset -c 0-63 Rscript scripts/appendix/miss-sensitivity.R > miss-sens.log 2>&1 &
ricu:::init_proj()

# obtaining the TV decomposition on full data
src <- "aics"
dat <- load_data(src, split_elective = TRUE, no_miss = FALSE)
c(X, Z, W, Y) %<-% attr(dat, "sfm")
print_sfm(X, Z, W, Y)

fcb_complete <- fairness_cookbook(
  data = dat[complete.cases(dat)], X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
  method = "debiasing"
)

res <- summary(fcb_complete)$measures
# change IE, SE signs of easier interpretability
res[res$measure %in% c("ctfse", "ctfie"), ]$value <- 
  - res[res$measure %in% c("ctfse", "ctfie"), ]$value
res <- as.data.table(res)

mi_data <- function(dat, n_imp) {
  
  # fix the country value if needed
  if (is.element("country", names(dat))) 
    dat[, c("country") := as.integer(country == "Australia")]
  
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

n_imp <- 10
mi_lst <- mi_data(dat, n_imp)
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

# save(res, mi_res, file = "data/miss-sens-data.RData")
mi_res <- as.data.table(mi_res)
mi_res <- mi_res[, qnormmix(c(0.025, 0.975), value, sd), by = "measure"]
mi_res <- setnames(mi_res, c("V1", "V2", "V3"), c("value", "lwr", "upr"))

res <- res[, list(value = value, lwr = value - 1.96 * sd, upr = value + 1.96 * sd), 
           by = "measure"]

plt <- rbind(cbind(res, type = "Complete Data"), 
             cbind(mi_res, type = "Multiple Imputation"))
plt <- plt[plt$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]
xlabz <- c(
  tv = "Total Variation", ctfse = "Confounded", ctfde = "Direct",
  ctfie = "Indirect"
)

plt$measure <- factor(plt$measure, levels = c("ctfse", "ctfie", "ctfde", "tv"))
ggplot(plt, aes(x = measure, y = value, fill = factor(type),
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
    legend.position = "bottom",
    legend.position.inside = c(0.4, 0.8),
    legend.box.background = element_rect(),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 21),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    title = element_text(size = 16)
  ) + 
  scale_fill_discrete(name = "Method ") +
  scale_x_discrete(labels = xlabz) +
  xlab("Causal Fairness Measure") + ylab("Value") +
  scale_y_continuous(labels = scales::percent)

ggsave("results/miss-sens.png", width = 8, height = 4.5, bg = "white")
