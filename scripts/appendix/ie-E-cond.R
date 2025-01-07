
#' Inspection of conditional indirect effects.
# nohup taskset -c 0-63 Rscript scripts/ie-E-cond.R > ie-E-cond.log 2>&1 &
ricu:::init_proj()
set.seed(2024)

# select conditioning covariates
sel_covs <- c("age", "majority")

# choose the computation method for the indirect effect (causal forests)
methods <- "crf"

# choose the death outcome
outcome <- "death"

cie <- xtow_dat <- wtoy_dat <- c()

# specify target sources
for (src in c("aics", "miiv")) {
  
  # loading the data
  dat <- load_data(src, outcome = outcome, split_elective = TRUE)
  if (src == "miiv") {
    
    dat <- dat[, diag_grp := diag_index]
  } else  dat <- dat[, diag_grp := floor(apache_iii_diag / 100)]
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  
  # range across inference methods
  for (method in methods) {
    
    cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                           method = method)
    E_lst <- cmbn_E(sel_covs, src)
    cie <- rbind(cie, cbind(infer(cndE_obj, E_lst, effect = "IE"), 
                            method = method, src = src))
  }
  
  # group medical and emergency surgery admissions into 'urgent'
  dat[, urgent := diag_grp <= 25]
  
  # create age groups (approximate quartiles)
  dat[, age_bin := cut(age, c(-Inf, 49, 64, 74, Inf),
                       labels = c("18-49", "50-64", "65-74", "75-100"))]
  
  # mean probability of urgent admission by age group and minority status
  xtow_dat <- rbind(
    xtow_dat, 
    cbind(dat[, mean(urgent), by = c("age_bin", "majority")], src = src)
  )
  
  # mean probability of death for the majority grouup for age group and urgency status
  wtoy_dat <- rbind(
    wtoy_dat,
    cbind(dat[majority == 1, mean(death), by = c("age_bin", "urgent")], src = src)
  )
}

# IE modification plot
p1 <- ggplot(cie, aes(x = age, y = effect, color = srcwrap(src), 
                      fill = srcwrap(src), group = srcwrap(src))) +
  geom_point() + geom_line() + 
  geom_ribbon(aes(ymin = effect - 1.96 * sd, ymax = effect + 1.96 * sd),
              alpha = 0.4) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  ylab("Indirect effect") +
  xlab("Age group (years)") +
  scale_color_discrete(name = "Dataset") +
  scale_fill_discrete(name = "Dataset") +
  theme(legend.position = "bottom",
        # legend.position.inside = c(0.2, 0.2),
        legend.box.background = element_rect(),
        # legend.direction = "horizontal",
        axis.text = element_text(size = rel(1.2)),  # Scale axis tick labels
        axis.title = element_text(size = rel(1.2)))

ggsave(paste0("results/ie-E-cond.png"), plot = p1, width = 6, height = 4)

# X to W effect inspection
p_xtow <- ggplot(xtow_dat, aes(x = age_bin, y = V1, fill = factor(majority))) +
  geom_col(position = "dodge") + theme_bw() +
  scale_fill_discrete(
    name = "Group", labels = c("Minority", "Majority")
  ) +
  xlab("Age group (years)") + 
  facet_grid(~ srcwrap(src)) +
  ylab("Probability of Medical or Emergency Surgery") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = rel(1.2)),  # Scale axis tick labels
        axis.title = element_text(size = rel(1.2)))

ggsave(paste0("results/x-to-w.png"), plot = p_xtow, 
       width = 7, height = 4.5)

# W to Y effect inspection
p_wtoy <- ggplot(wtoy_dat, aes(x = age_bin, y = V1, fill = urgent)) +
  geom_col(position = "dodge") + theme_bw() +
  scale_fill_discrete(
    name = "Admission Type", labels = c("Elective", "Emergency")
  ) + 
  facet_grid(~ srcwrap(src)) +
  xlab("Age group (years)") + ylab("Mortality") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom",
        axis.text = element_text(size = rel(1.2)),  # Scale axis tick labels
        axis.title = element_text(size = rel(1.2)))

ggsave(paste0("results/w-to-y.png"), plot = p_wtoy, 
       width = 7, height = 4.5)
