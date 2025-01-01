
#' Permutation tests for significance of the observed age-diagnosis patterns.
ricu:::init_proj()
set.seed(2024)

recover_mat <- function(src, x, i = NULL) {
  
  # specify factor levels
  lvls <- c("Medical", "Surgical (Emergency)", "Surgical (Elective)")
  country <- if (src == "aics") "AU" else "US"
  
  if (x == "brr") {
    
    rr_age_diag <- rr_compute("age_quart", "adm_type", nboot = 1, rep = i)
    
    # represent the baseline risk ratio as a matrix
    mat <- dcast(rr_age_diag, age_quart ~ diag_grp, value.var = "rr")
    mat <- as.matrix(mat[, -1])
  } else if (is.element(x, c("death", "readm"))) {
    
    dat <- load_data(src, outcome = x, split_elective = TRUE)
    c(X, Z, W, Y) %<-% attr(dat, "sfm")
    
    # compute the inference object
    cndE_obj <- cnd_effect(data = copy(dat), X = X, Z = Z, W = W, Y = Y,
                           method = "crf")
    
    # specify the conditioning events
    E_lst <- cmbn_E(c("diag_grp", "age", "majority"), src)
    
    # infer conditional direct effects on mortality
    cde <- infer(cndE_obj, E_lst, rep = i)
    cde$diag_grp <- factor(cde$diag_grp, levels = lvls)
    
    # represent the protective direct effect of minority status as a matrix
    mat <- dcast(cde, age ~ diag_grp, value.var = "effect")
    mat <- as.matrix(mat[, -1])
    if (x == "readm") mat <- -mat
  }
  
  mat
}

rho_ci <- function(src, A, B, nrep = 10) {
  
  ret <- c()
  for (i in seq_len(nrep)) {
    
    # get matrices for ith fold.
    Amat <- recover_mat(src, A, i)
    Bmat <- recover_mat(src, B, i)
    
    # compute the correlation
    ret <- rbind(ret, data.frame(A = A, B = B, rep = i, 
                                 cor = cor(as.vector(Amat), as.vector(Bmat))))
  }
  
  ret <- as.data.table(ret)
  ret[, sd := sd(cor)]
  ret <- ret[1]
  ret[, lwr := cor - 1.96 * sd]
  ret[, upr := cor + 1.96 * sd]
  ret[, src := src]
  ret
}

rci <- c()
prs <- combn(3, 2, simplify = FALSE)
nms <- c("brr", "death", "readm")
for (src in c("aics", "miiv")) {
  
  for (cmb in seq_along(prs)) {
     
    i <- prs[[cmb]][1]
    j <- prs[[cmb]][2]
    if (i == 1 & src == "miiv") next
    cat("Pair", nms[i], nms[j], "with src =", src, "\n")
    rci <- rbind(rci, rho_ci(src, nms[i], nms[j]))
  }
}

rci[, pval := 2 * pnorm(-abs(cor) / sd) ]

# if (src == "aics") brr_mat <- recover_mat(src, "brr", i = 1)
# 
# death_mat <- recover_mat(src, "death")
# readm_mat <- recover_mat(src, "readm")
# 
# if (src == "miiv") {
#   
#   cat(perm_mat_test(death_mat, readm_mat, norm_method = "zscore", 
#                     corr_method = "distance")$p_value)
# } else {
#   
#   # combine all matrices in a list
#   mat <- list(brr_mat, death_mat, readm_mat)
#   
#   # permutation testing over pairs
#   prs <- combn(3, 2, simplify = FALSE)
#   pat <- c()
#   nms <- c("brr", "death", "readm")
#   for (cmb in seq_along(prs)) {
#     
#     i <- prs[[cmb]][1]
#     j <- prs[[cmb]][2]
#     
#     cat("Pair", nms[i], nms[j], "\n")
#     
#     # use z-scoring for normalizing the matrices (which are on different scales)
#     for (norm_method in c("zscore", "frobenius", "minmax", "rank")) {
#       
#       # use L2 distance as the matrix similarity metric
#       for (corr_method in c("distance", "pearson", "spearman")) {
#         
#         pval <- perm_mat_test(mat[[i]], mat[[j]], norm_method, corr_method)$p_value
#         pat <- rbind(
#           pat,
#           data.frame(pval = pval, norm_method = norm_method, 
#                      corr_method = corr_method, A = nms[i], B = nms[j])
#         )
#       }
#     }
#   }
# }
# 
# print(pat)
# 
# # pattern testing deep-dive
# mat <- list(brr_mat, death_mat, readm_mat, death_mat2, readm_mat2)
# 
# prs <- combn(2, 2, simplify = FALSE)
# pat <- c()
# plts <- list()
# nms <- c("brr", "death", "readm", "death2", "readm2")
# 
# for (norm_method in c("none", "zscore")) {
# 
#   for (cmb in seq_along(prs)) {
#   
#     i <- prs[[cmb]][1]
#     j <- prs[[cmb]][2]
#   
#     cat("Pair", nms[i], nms[j], "\n")
#   
#     # use z-scoring for normalizing the matrices (which are on different scales)
#     
#     # use L2 distance as the matrix similarity metric
#     for (corr_method in c("pearson")) {
#       
#       perm <- perm_mat_test(mat[[i]], mat[[j]], norm_method, corr_method)
#       plti <- ggplot(
#         data = data.frame(x = perm$permuted_corrs), aes(x = x)
#       ) +
#         geom_density() +
#         geom_vline(xintercept = perm$original_corr, color = "red") +
#         geom_vline(xintercept = median(perm$permuted_corrs), color = "blue") +
#         theme_bw() + xlab("corr") + 
#         ggtitle(paste(c(nms[i], nms[j], norm_method), collapse = "-")) +
#         coord_cartesian(xlim = c(-1, 1))
#       plts[[length(plts) + 1]] <- plti
#     }
#   }
# }
# 
# cowplot::plot_grid(plotlist = plts, ncol = 2)
# 
# rci <- rho_ci("miiv", "death", "readm")
