
ricu:::init_proj()

scripts <- c(
  "tv-decompositions.R", # Figure 2(b)
  "confounded-effects.R", # Figure 3(a, b)
  "indirect-effects.R", # Figure 3(c, d, e)
  "interaction-testing.R",
  "de-E-cond.R", # Figure 5
  "admission-risks.R", # Figures 5, 6(a)
  "pattern-testing.R", 
  "iice-radar.R", # Figure 7
  # Appendix figures
  "appendix/study-flowchart.R", # Figure A1
  "appendix/overlap.R", # Figure C2
  "appendix/ie-E-cond.R", # Figure D3
  "appendix/miss-sensitivity.R" # Figure E4
)

for (file in scripts) {
  
  file <- file.path("scripts", file)
  source(file)
  rm(list = ls())
  gc()
}