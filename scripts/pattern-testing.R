
#' Permutation tests for significance of the observed age-diagnosis patterns.
ricu:::init_proj()
set.seed(2024)

# specify country and data source
country <- "AU"
src <- "aics"

# compute baseline risk ratio across age quartiles and admission type
rr_age_diag <- rr_compute("age_quart", "adm_type")

# represent the baseline risk ratio as a matrix
brr_mat <- dcast(rr_age_diag, age_quart ~ diag_grp, value.var = "rr")
brr_mat <- as.matrix(brr_mat[, -1])

# compute the protective direct effect of minority status on mortality
method <- "crf"
sel_covs <- c("diag_grp", "age", "majority")

# load the data
dat <- load_data(src, outcome = "death", split_elective = TRUE)
c(X, Z, W, Y) %<-% attr(dat, "sfm")

# compute the inference object
cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                       method = method)

# specify the conditioning events
E_lst <- cmbn_E(sel_covs, src)

# infer conditional direct effects on mortality
cde <- infer(cndE_obj, E_lst)
cde$diag_grp <- factor(cde$diag_grp, levels = levels(rr_age_diag$diag_grp))

# represent the protective direct effect of minority status as a matrix
death_mat <- dcast(cde, age ~ diag_grp, value.var = "effect")
death_mat <- as.matrix(death_mat[, -1])

# compute the minority status effects on readmission

# load readmission data
dat <- load_data(src, outcome = "readm", split_elective = TRUE)
Y <- "readm"

# compute the inference object
cndE_obj <- cnd_effect(dat, X = X, Z = Z, W = W, Y = Y,
                       method = method)

# infer conditional direct effects on readmission
cde <- infer(cndE_obj, E_lst)
cde$diag_grp <- factor(cde$diag_grp, levels = levels(rr_age_diag$diag_grp))

# represent the direct effect of minority status on readmission as a matrix
readm_mat <- dcast(cde, age ~ diag_grp, value.var = "effect")
readm_mat <- -as.matrix(readm_mat[, -1])

# combine all matrices in a list
mat <- list(brr_mat, death_mat, readm_mat)

# permutation testing over pairs
prs <- combn(3, 2, simplify = FALSE)
pat <- c()
nms <- c("brr", "death", "readm")
for (cmb in seq_along(prs)) {
  
  i <- prs[[cmb]][1]
  j <- prs[[cmb]][2]
  
  cat("Pair", nms[i], nms, "\n")
  
  # use z-scoring for normalizing the matrices (which are on different scales)
  for (norm_method in c("zscore")) {
    
    # use L2 distance as the matrix similarity metric
    for (corr_method in c("distance")) {
      
      pval <- perm_mat_test(mat[[i]], mat[[j]], norm_method, corr_method)$p_value
      pat <- rbind(
        pat,
        data.frame(pval = pval, norm_method = norm_method, 
                   corr_method = corr_method, A = nms[i], B = nms[j])
      )
    }
  }
}
