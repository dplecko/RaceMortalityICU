
#' Permutation tests for significance of the observed age-diagnosis patterns.
ricu:::init_proj()
set.seed(2024)

country <- "AU"
src <- if (country == "AU") "aics" else "nzics"

# baseline risk ratio
rr_age_diag <- rr_compute("age_quart", "adm_type")
brr_mat <- dcast(rr_age_diag, age_quart ~ diag_grp, value.var = "rr")
brr_mat <- as.matrix(brr_mat[, -1])

# death
method <- "crf"
sel_covs <- c("diag_grp", "age", "majority")

dat <- load_data(src, outcome = "death", split_elective = TRUE)
c(X, Z, W, Y) %<-% attr(dat, "sfm")
print_sfm(X, Z, W, Y)
cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                       method = method)
E_lst <- cmbn_E(sel_covs, src)
cde <- infer(cndE_obj, E_lst)
cde$diag_grp <- factor(cde$diag_grp, levels = levels(rr_age_diag$diag_grp))

death_mat <- dcast(cde, age ~ diag_grp, value.var = "effect")
death_mat <- as.matrix(death_mat[, -1])

# readmission
dat <- load_data(src, outcome = "readm", split_elective = TRUE)
Y <- "readm"
cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                       method = method)
E_lst <- cmbn_E(sel_covs, src)
cde <- infer(cndE_obj, E_lst)
cde$diag_grp <- factor(cde$diag_grp, levels = levels(rr_age_diag$diag_grp))

readm_mat <- dcast(cde, age ~ diag_grp, value.var = "effect")
readm_mat <- -as.matrix(readm_mat[, -1])

# combine all matrices together
mat <- list(brr_mat, death_mat, readm_mat)

# permutation testing over pairs
prs <- combn(3, 2, simplify = FALSE)
pat <- c()
nms <- c("brr", "death", "readm")
for (cmb in seq_along(prs)) {
  
  i <- prs[[cmb]][1]
  j <- prs[[cmb]][2]
  
  cat("Pair", nms[i], nms, "\n")
  
  for (norm_method in c("zscore", "minmax", "frobenius", "rank")[1]) {
    
    for (corr_method in c("pearson", "spearman", "distance")[3]) {
      
      pval <- perm_mat_test(mat[[i]], mat[[j]], norm_method, corr_method)$p_value
      pat <- rbind(
        pat,
        data.frame(pval = pval, norm_method = norm_method, 
                   corr_method = corr_method, A = nms[i], B = nms[j])
      )
    }
  }
}
