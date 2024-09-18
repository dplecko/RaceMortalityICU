
ricu:::init_proj()
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
set.seed(2024)

#' * Heterogeneity of (z, w)-DE_{x_0, x_1} * 
src <- "anzics"
dat <- load_data(src)
nz_only <- FALSE

if (nz_only) {
  
  dat <- dat[country == "NZ"]
  src <- "nzics"
}

# dat <- dat[sample(nrow(dat), 1/10 * nrow(dat))]
X <- attr(dat, "X")
W <- attr(dat, "W")
Y <- attr(dat, "Y")
diag_dt <- attr(dat, "diag_dt")
diag_col <- attr(dat, "diag_col")
attr(dat, "X") <- attr(dat, "W") <- attr(dat, "Y") <- 
  attr(dat, "diag_dt") <- attr(dat, "diag_col") <- NULL

if (src == "anzics" & file.exists(file.path(root, "data", "de_crf.RData"))) {
  
  load(file.path(root, "data", "de_crf.RData"))
} else {
  
  de_crf <- boot_crf(data = dat, X = X, W = W, Y = Y)
  if (src == "anzics") {
    
    save(de_crf, file = file.path(root, "data", "de_crf.RData"))
  }
}

dat <- dat[, de_mean := rowMeans(de_crf, na.rm = TRUE)]
dat <- dat[, de_var := rowVars(de_crf, na.rm = TRUE)]

dat[, diag_group := std_diag(adm_diag)]
dat[, age_group := age_grp(age)]

mean_dt <- dat[, list(mean_X = mean(de_mean), var_X = mean(de_var)), 
               by = c("age_group", "diag_group")]
mean_dt[, mean_X := trim_ends(mean_X, 0.02)]
mean_dt[, p_val := pnorm(abs(mean_X/sqrt(var_X)), lower.tail = FALSE)]
ggplot(mean_dt[diag_group != "Other"], 
       aes(x = diag_group, y = age_group, fill = mean_X, alpha = 1 - p_val)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, name = "DE", labels = scales::percent) +
  theme_minimal() + 
  geom_text(aes(label = paste0(round(mean_X * 100, 1), "%")), 
            color = "black", size = 3) +
  guides(alpha = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1)) +
  geom_vline(xintercept = 4.5, color = "red") +
  ylab("Age group") + xlab("Diagnosis group")

ggsave(paste0("results/de-heterogeneity-", src, ".png"), width = 10, height = 7, 
       bg = "white")

# additional extended plot for the supplements
# if (src == "anzics") {
#   # make additional plot
# }
# investigate the behavior of extreme weights
# load("results/pw-dt.RData")
# dat <- merge(dat, pw_dt, all.x = TRUE)
# ggplot(
#   aes(x = de_var, y = -log(pw)), data = dat[complete.cases(dat)]
# ) +
#   geom_point(size = 0.05) + theme_bw()
