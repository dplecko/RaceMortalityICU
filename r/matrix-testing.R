
normalize_matrix <- function(mat, method = "zscore") {
  
  if (method == "zscore") {
    
    return((mat - mean(mat)) / sd(mat))
  } else if (method == "minmax") {
    
    return((mat - min(mat)) / (max(mat) - min(mat)))
  } else if (method == "frobenius") {
    
    return(mat / sqrt(sum(mat^2)))
  } else if (method == "rank") {
    
    return(matrix(rank(mat), nrow = nrow(mat), ncol = ncol(mat)))
  } else {
    
    stop("Unknown normalization method")
  }
}

correlation_method <- function(mat1, mat2, method = "pearson") {
  mat1_vec <- as.vector(mat1)
  mat2_vec <- as.vector(mat2)
  
  if (method == "pearson") {
    
    return(cor(mat1_vec, mat2_vec, method = "pearson"))
  } else if (method == "spearman") {
    
    return(cor(mat1_vec, mat2_vec, method = "spearman"))
  } else if (method == "distance") {

    return(energy::dcor(mat1_vec, mat2_vec))
  } else {
    
    stop("Unknown correlation method")
  }
}

perm_mat_test <- function(A, B, norm_method = "zscore", corr_method = "pearson", 
                          n_permutations = 1000) {
  # Normalize matrices
  A_norm <- normalize_matrix(A, norm_method)
  B_norm <- normalize_matrix(B, norm_method)
  
  # Compute original correlation
  original_corr <- correlation_method(A_norm, B_norm, corr_method)
  
  # Permutation testing
  permuted_corrs <- numeric(n_permutations)
  for (i in seq_len(n_permutations)) {
    permuted_B <- B_norm[sample(nrow(B_norm)), sample(ncol(B_norm))]
    permuted_corrs[i] <- correlation_method(A_norm, permuted_B, corr_method)
  }
  
  # Compute p-value
  p_value <- mean(abs(permuted_corrs) >= abs(original_corr))
  
  return(list(original_corr = original_corr, p_value = p_value, permuted_corrs = permuted_corrs))
}
