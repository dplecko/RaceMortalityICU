
#' Analysis of overlap (+ sensitivity).
# nohup taskset -c 64-95 Rscript scripts/appendix/overlap.R > overlap2.log 2>&1 &
ricu:::init_proj()
set.seed(2024)

# sensitivity analysis for threshold impact
eps_sens <- ovalues <- c()
for (src in c("aics", "miiv")) {
  
  dat <- load_data(src, split_elective = TRUE)
  c(X, Z, W, Y) %<-% attr(dat, "sfm")
  print_sfm(X, Z, W, Y)
  x <- dat$majority
  
  # Decomposing the Disparity
  fcb <- fairness_cookbook(
    data = dat, X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, method = "debiasing"
  )
  
  # sensitivity over quantile-based trimming thresholds
  scores_notrim <- fcb$pw[["px_zw"]]
  eps_seq <- quantile(pmin(scores_notrim, 1 - scores_notrim), probs = 1:5 / 100)
  eps_sens <- rbind(eps_sens, cbind(summary(fcb)$measures, eps = 0, quant = 0, 
                                    src = src))
  oval_init <- as.data.frame(t(oval(fcb$pw[["px_zw"]], x)))
  ovalues <- rbind(
    ovalues, 
    cbind(oval_init, eps = 0, quant = 0, src = src)
  )
  fcb_eps <- list()
  for (i in seq_along(eps_seq)) {
    
    eps <- eps_seq[i]
    o_idx <- which(pmin(scores_notrim, 1 - scores_notrim) > eps)
    fcb_eps[[i]] <- fairness_cookbook(
      data = dat[o_idx], X = X, Z = Z, W = W, Y = Y, x0 = 0, x1 = 1, 
      method = "debiasing" # , eps_trim = eps
    )
    
    oval_iter <- as.data.frame(t(oval(fcb_eps[[i]]$pw[["px_zw"]], x[o_idx])))
    ovalues <- rbind(
      ovalues, cbind(oval_iter, eps = eps, quant = i / 100, src = src)
    )
    
    eps_sens <- rbind(
      eps_sens, 
      cbind(summary(fcb_eps[[i]])$measures, eps = eps, quant = i / 100, src = src)
    )
    
    cat("Iteration", i, "of", length(eps_seq), "finished.\n")
  }
}

print(ovalues[, "0.05"] > ovalues[, "eps"])

xlabz <- c(
  ctfse = "Confounded", ctfie = "Indirect", ctfde = "Direct", 
  tv = "Total Variation"
)

eps_sens <- eps_sens[eps_sens$measure %in% c("tv", "ctfde", "ctfse", "ctfie"), ]

# change IE, SE signs of easier interpretability
eps_sens[eps_sens$measure %in% c("ctfse", "ctfie"), ]$value <- 
  - eps_sens[eps_sens$measure %in% c("ctfse", "ctfie"), ]$value
eps_sens$measure <- factor(eps_sens$measure, 
                           levels = c("ctfse", "ctfie", "ctfde", "tv"))
levels(eps_sens$measure) <- xlabz

ggplot(eps_sens, aes(x = quant, y = value, group = src, 
                     color = srcwrap(src), fill = srcwrap(src))) +
  geom_line() + geom_point() +
  geom_ribbon(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd),
              linewidth = 0, alpha = 0.4) +
  theme_bw() +
  facet_wrap(~ measure, scales = "free_y") +
  ylab("Effect Estimate") + xlab("Percentile Trimming Threshold") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Dataset") +
  scale_color_discrete(name = "Dataset") +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed")

ggsave(paste0("results/overlap-sens.png"), width = 10, height = 6,
       bg = "white")
