
#' Sensitivity analysis for the effects of missing data. Multiple imputation is
#' performed, with the indigenous status imputed. The results are compared with
#' the decomposition of the TV measure obtained on the complete dataset, with no
#' indigenous status missingness.
# grep 'cpu' /proc/stat | awk 'NR>1 {usage=($2+$3+$4)*100/($2+$3+$4+$5); if (usage < 10) print NR-2}'
# nohup taskset -c 0-63 Rscript scripts/appendix/miss-sensitivity.R > miss-sens.log 2>&1 &
ricu:::init_proj()

# specify the target source (ANZICS APD)
src <- "aics"

# load the data and print the Standard Fairness Model
dat <- load_data(src, split_elective = TRUE, no_miss = FALSE)
c(X, Z, W, Y) %<-% attr(dat, "sfm")
print_sfm(X, Z, W, Y)

# run the decomposition of the complete data (no missingness in X)
fcb_complete <- fairness_cookbook(
  data = dat[complete.cases(dat)], X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
  method = "debiasing"
)

# change IE, SE signs of easier interpretability
res <- summary(fcb_complete)$measures
res[res$measure %in% c("ctfse", "ctfie"), ]$value <- 
  - res[res$measure %in% c("ctfse", "ctfie"), ]$value
res <- as.data.table(res)

# missing data imputation helper
mi_data <- function(dat, n_imp) {
  
  # fix the country value if needed
  if (is.element("country", names(dat))) 
    dat[, c("country") := as.integer(country == "Australia")]
  
  # get the complete data
  cmp_dat <- dat[complete.cases(dat)]
  
  # get the data with missing values
  mis_dat <- dat[!complete.cases(dat)]
  
  # get the Standard Fairness model
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  
  # learn the distribution P(X | Z, W, Y) on the complete data
  x_learn <- cv_xgb(cmp_dat[, c(Z, W, Y), with=FALSE], y = cmp_dat[[X]])
  
  # obtain the predicted P(X | Z, W, Y) probabilities on the missing data
  mis_probs <- pred_xgb(x_learn, mis_dat[, c(Z, W, Y), with=FALSE])
  
  # perform multiple imputation over n_imp repetitions
  mi_lst <- list()
  for (i in seq_len(n_imp)) {
    
    imp <- rbinom(length(mis_probs), size = 1, prob = mis_probs)
    mis_dat[, c(X) := imp]
    mi_lst[[i]] <- rbind(cmp_dat, mis_dat)
  }
  
  # return the different imputed datasets
  mi_lst
}

# set number of repetitions to 10
n_imp <- 10

# obtain the impute dataset
mi_lst <- mi_data(dat, n_imp)
mi_res <- NULL

# range over all repetitions
for (i in seq_len(n_imp)) {
  
  # perform the TV decomposition on the imputed data
  fcb <- fairness_cookbook(
    data = mi_lst[[i]], X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
    method = "debiasing"
  )
  
  # save the fairness measures on the imputed data
  add <- summary(fcb)$measures
  
  # change IE, SE signs of easier interpretability
  add[add$measure %in% c("ctfse", "ctfie"), ]$value <- 
    - add[add$measure %in% c("ctfse", "ctfie"), ]$value
  add$mi_rep <- i
  mi_res <- rbind(mi_res, add)
}

# get confidence intervals for the effect estimates after taking into account
# the multiple imputation step
mi_res <- as.data.table(mi_res)
mi_res <- mi_res[, qnormmix(c(0.025, 0.975), value, sd), by = "measure"]
mi_res <- setnames(mi_res, c("V1", "V2", "V3"), c("value", "lwr", "upr"))

res <- res[, list(value = value, lwr = value - 1.96 * sd, upr = value + 1.96 * sd), 
           by = "measure"]

# prepare plot labels
plt <- rbind(cbind(res, type = "Complete Data"), 
             cbind(mi_res, type = "Multiple Imputation"))
plt <- plt[plt$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]
xlabz <- c(
  tv = "Total Variation", ctfse = "Confounded", ctfde = "Direct",
  ctfie = "Indirect"
)

plt$measure <- factor(plt$measure, levels = c("ctfse", "ctfie", "ctfde", "tv"))

# plot the sensitivity analysis for missing data
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
