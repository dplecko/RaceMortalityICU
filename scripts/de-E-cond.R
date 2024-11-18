
#' Inspection of conditional direct effects.
ricu:::init_proj()
src <- "nzics"
dat <- load_data(src, split_elective = TRUE)
dat <- dat[, diag_grp := floor(apache_iii_diag / 100)]
c(X, Z, W, Y) %<-% attr(dat, "sfm")

print_sfm(X, Z, W, Y)

# 3 admission groups
sel_covs <- c("apache_iii_diag") # "majority"
E_lst <- cmbn_E(sel_covs)
cde <- cnd_effect(as.data.frame(dat), X = X, Z = Z, W = W, Y = Y, E_lst = E_lst)

dg_grp_cnd <- ggplot(cde, aes(x = factor(apache_iii_diag), y = effect)) +
  geom_col() + theme_bw() +
  geom_errorbar(aes(ymin = effect - 1.96 * sd, ymax = effect + 1.96 * sd)) +
  xlab("Diagnosis Group") + ylab("DE(x0 -> x1 | E)")

save_plt(dg_grp_cnd, paste0("diag-grp-de-all", src), width = 5, height = 4)

# 3 groups x 4 age quartiles
sel_covs <- c("age", "apache_iii_diag", "majority")
E_lst <- cmbn_E(sel_covs)
cde <- cnd_effect(as.data.frame(dat), X = X, Z = Z, W = W, Y = Y, E_lst = E_lst)

dg_age_cnd <- ggplot(cde, aes(x = factor(apache_iii_diag), y = factor(age), 
                              fill = effect)) +
  geom_tile() + theme_minimal() +
  xlab("Diagnosis Group") + ylab("Age Group") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, name = "DE", labels = scales::percent) +
  geom_text(aes(label = paste0(round(effect * 100, 1), "%")), 
            color = "black", size = 3)

save_plt(dg_age_cnd, paste0("diag-age-de-minority", src), 
         width = 5, height = 5, bg = "white")

cde <- de_residuals(src, "death")
cde$apache_iii_diag <- as.numeric(cde$apache_iii_diag)
cde$diag_group <- cut(cde$apache_iii_diag, 
                      breaks = c(-Inf, 1199.5, 2299.5, Inf),
                      labels = c("Med", "Surg (Emergency)", "Surg (Elective)"))

ggplot(cde, aes(x = factor(apache_iii_diag), y = de_resid, color = diag_group)) +
  geom_line() + geom_point() + theme_bw() +
  geom_errorbar(aes(ymin = effect - 1.96 * sd, ymax = effect + 1.96 * sd)) +
  xlab("APACHE-III Diagnosis") + ylab("DE(x0 -> x1 | E)") +
  coord_cartesian(ylim = c(-0.1, 0.1))


