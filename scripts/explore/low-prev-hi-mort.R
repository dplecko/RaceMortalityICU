
grp_var <- "apache_iii_diag" # "apache_iii_subcode" # 

dat <- load_concepts(c(grp_var, "weight", "sex", "death", "adm_year"), "anzics")
dat[is.na(death), death := FALSE]
dat[, majority := weight > median(weight, na.rm = TRUE), by = "sex"]
# dat <- dat[adm_year >= 2018]
# dat[, majority := 1 - indig]
dat <- dat[complete.cases(dat)]

res <- dat[sex == "Male", list(count = .N, how_min = mean(majority == 0), 
                  rr_mtof = mean(death[majority == 0]) / 
                    mean(death[majority == 1])), 
           by = c(grp_var)]

res <- res[count > 50]
ggplot(
  res, aes(x = log(how_min), y = rr_mtof)
) +
  geom_point() + theme_bw() +
  xlab("log(P(fat))") + ylab("P(death | fat) / P(death | slim)")


srcs <- c("anzics", "miiv")
dat_lst <- lapply(srcs, load_data)
names(dat_lst) <- srcs

for (src in srcs) {
  
  dat <- dat_lst[[src]]
  diag_var <- if (src == "anzics") "apache_iii_diag" else "diag_index"
  X <- "majority"
  Y <- "death"
  if (src == "anzics") {
    
    Z <- c("age", "sex", "country")
    W <- c("apache_iii_rod")
  } else if (src == "miiv") {
    
    Z <- c("age", "sex")
    W <- c("charlson", "acu_24")
  }
  
  frm <- as.formula(paste(Y, "~", paste0(c(X, Z, W), collapse = "+")))
  
  res <- dat[, list(count = .N, how_min = mean(majority == 0)), by = c(diag_var)]
  res <- res[how_min > 0 & how_min < 1]
  for (diag_lvl in unique(res[[diag_var]])) {
    
    dat_lvl <- dat[get(diag_var) == diag_lvl]
    logreg <- glm(frm, data = dat_lvl)
    res[get(diag_var) == diag_lvl, OR := exp(coef(logreg)["majority"])]
  }
  
  lmod <- lm(OR ~ how_min, data = res, weights = res$count)
  ggplot(res, aes(x = how_min, y = OR, size = count)) +
    geom_point(show.legend = FALSE) + theme_bw() + 
    geom_abline(intercept = coef(lmod)[1], slope = coef(lmod)[2], color = "red",
                linewidth = 1.2) +
    xlab("Proportion minority group") + ylab("Odds Ratio for death")
}

# Causal Forest DEs vs. RR by diagnosis

