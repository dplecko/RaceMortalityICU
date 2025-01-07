
# Load necessary libraries
library(grf)
library(data.table)
library(assertthat)
invisible(source("r/utils-config.R"))

#' * Heterogeneity of (z, w)-DE_{x_0, x_1} * 

# Retrieve command-line arguments
args <- commandArgs(trailingOnly = TRUE)
i <- as.integer(args[1])
src <- args[2]
data_path <- args[3]
X <- unlist(strsplit(args[4], ","))
W <- args[5]
Y <- args[6]
output_path <- args[7]

# Load data
dat <- fread(data_path)

# Set seed
set.seed(i)

# Perform bootstrap iteration
n <- nrow(dat)
if (i <= 10) {
  
  in_bag <- match(config(src)[[i]], dat[[1]])
  in_bag <- in_bag[!is.na(in_bag)]
} else in_bag <- sample(seq_len(n), size = n, replace = TRUE)
out_bag <- setdiff(seq_len(n), in_bag)

crf <- causal_forest(
  X = as.matrix(dat[in_bag, X, with = FALSE]),
  W = dat[in_bag][[W]], Y = dat[in_bag][[Y]],
  num.threads = 64
)

# out-of-bag predictions (if any sample out-of-bag exists)
if (length(out_bag) > 0) {
  
  preds <- predict(
    crf, as.matrix(dat[out_bag, X, with = FALSE])
  )$predictions
} else preds <- NULL

# in-bag oob predictions
preds <- c(preds, crf$predictions[, 1])

# Save predictions to file
write.csv(unique(data.table(id = c(out_bag, in_bag), pred = preds)), 
          file = output_path, row.names = FALSE)
