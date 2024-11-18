
srcwrap <- function(src) {
  
  if (length(src) > 1L) {
    return(vapply(src, srcwrap, character(1L)))
  }
  
  switch(src,
         mimic = "MIMIC-III",
         miiv = "MIMIC-IV",
         eicu = "eICU",
         hirid = "HiRID",
         aumc = "AUMC",
         mimic_demo = "MIMIC Demo",
         eicu_demo = "eICU Demo",
         anzics = "ANZICS APD",
         nzics = "ANZICS APD (New Zealand)",
         aics = "ANZICS APD (Australia)",
         sic = "SICdb",
         stop("unknown data source")
  )
}

print_sfm <- function(X, Z, W, Y) {
  
  cat("X:", paste(X, collapse = ", "), "\n")
  cat("Z:", paste(Z, collapse = ", "), "\n")
  cat("W:", paste(W, collapse = ", "), "\n")
  cat("Y:", paste(Y, collapse = ", "), "\n")
}

save_plt <- function(p, name, cache = TRUE, ...) {
  
  folder <- "results"
  if (cache) {
    folder <- file.path(folder, "cache")
    name <- paste0(name, "-", Sys.Date())
  }
  name <- paste0(name, ".png")
  ggsave(
    filename = file.path(folder, name),
    plot = p, ...
  )
}
