
#' Inspection of conditional direct effects.
# nohup Rscript scripts/de-E-cond.R > de-E-cond.log 2>&1 &
ricu:::init_proj()
set.seed(2024)
src <- "aics"
outcome <- "readm"
dat <- load_data(src, outcome = outcome, split_elective = TRUE)
dat <- dat[, diag_grp := floor(apache_iii_diag / 100)]
c(X, Z, W, Y) %<-% attr(dat, "sfm")

print_sfm(X, Z, W, Y)

# new S3 implementation of conditional effects
E_sets <- list(
  A = c("diag_grp"),
  B = c("age"),
  C = c("diag_grp", "age"),
  D = c("apache_iii_diag")
)

# train the inference object
for (method in c("osd", "crf")) {
  
  cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                         method = method)
  
  for (i in seq_along(E_sets)) {
    
    for (minority in c(FALSE, TRUE)) {
      
      sel_covs <- E_sets[[i]] # 
      if (minority) sel_covs <- c(sel_covs, "majority")
      
      E_lst <- cmbn_E(sel_covs)
      cde <- infer(cndE_obj, E_lst)
      
      # create plot title
      ttl <- paste(c(src, outcome, method, sel_covs), collapse = "-")
      
      # create plot depending on dimensionality
      E_plt <- plt_E_cnd(cde, sel_covs, ttl)
      # save plot
      save_plt(E_plt, paste0(ttl, ".png"), width = 5, height = 5, bg = "white")
    }
  }
}

# concordance of residuals
# E_lst <- cmbn_E("apache_iii_diag")
# concord <- NULL
# for (method in c("osd", "crf")) {
#   
#   cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
#                          method = method)
#   cde <- infer(cndE_obj, E_lst)
#   concord <- rbind(concord, cbind(cde, method = method))
# }
# 
# plt_concord <- merge(
#   concord[method == "osd", c("effect", "apache_iii_diag")],
#   concord[method == "crf", c("effect", "apache_iii_diag")], 
#   by = "apache_iii_diag"
# )
# 
# ggplot(plt_concord, aes(x = effect.x, y = effect.y)) +
#   theme_bw() + xlab("OSD") + ylab("CRF") +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, color = "red") +
#   coord_cartesian(xlim = c(-0.1, 0.1))
