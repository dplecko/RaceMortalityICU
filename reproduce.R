
ricu:::init_proj()

scripts <- c(
  "tv-decompositions.R", # Figure 2(b)
  "confounded-effects.R", # Figure 3(a, b)
  "indirect-effects.R", # Figure 3(c, d, e)
  "interaction-testing.R",
  "de-E-cond.R", # Figure 5
  "new-risks.R", # Figures 5, 6(a)
  "pattern-testing.R", 
  "iice-radar.R", # Figure
  # Appendix figures
  "rr-testing.R", # RR values in text with confidence intervals
  "overlap.R" # Figures C4 & C5
)

for (file in scripts) {
  
  file <- file.path("scripts", file)
  source(file)
  rm(list = ls())
  gc()
}