
ricu::init_proj()

scripts <- c(
  "tv-decompositions.R", # Figure 2(b)
  "spurious-age.R", # Figure 3
  "indirect-rod.R", # Figure 4
  "interaction-testing.R", # S4
  "de-heterogeneity.R", # Figure 5 & D6
  "ts-risks.R", # Figures 6
  "age-risks.R", # Figures A2 & A3
  "rr-testing.R", # RR values in text with confidence intervals
  "de-residuals-and-rr.R", # Figure 7
  "overlap.R" # Figures C4 & C5
)

for (file in scripts) {
  
  file <- file.path("scripts", file)
  source(file)
  rm(list = ls())
  gc()
}