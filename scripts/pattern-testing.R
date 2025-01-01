
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