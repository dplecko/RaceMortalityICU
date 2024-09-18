
ricu:::init_proj()

dat <- load_concepts(c("indig", "frailty", "age", "adm_year"), "anzics")
dat <- dat[adm_year >= 2018 & age >= 18]
dat <- dat[complete.cases(dat)]

dat[, adm_year := NULL]

# linear model fit
lmod <- lm(frailty ~ age + indig, data = dat)

# grouping / heatmap

dat[, age_bin := age_grp(age)]

ggplot(
  dat[, mean(frailty[indig == 1]) / mean(frailty[indig == 0]) - 1, 
      by = c("age_bin")],
  aes(x = age_bin, y = V1)
) +
  geom_col() + theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  xlab("Age group") + ylab("Excess Frailty of First Nations group") +
  scale_y_continuous(labels = scales::percent)
