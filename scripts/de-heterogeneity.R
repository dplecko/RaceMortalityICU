
#' Investigating the heterogeneity of direct effects along different age groups
#' and admission diagnoses. Two outcomes are considered, first the death outcome,
#' and second the re-admission outcome.
# nohup Rscript scripts/de-heterogeneity.R > de-heterogeneity.log 2>&1 &

ricu:::init_proj()
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))
set.seed(2024)

#' * Heterogeneity of (z, w)-DE_{x_0, x_1} * 
outcomes <- "death" # c("death", "readm")
srcs <- "aics" # c("anzics", "aics", "nzics", "miiv")

for (outcome in outcomes) {
  
  for (src in srcs) {
    
    fl_path <- file.path(root, "data", paste0("de-crf-", src, "-", outcome, ".RData"))
    dat <- load_data(src, outcome = outcome, split_elective = TRUE, one_hot = FALSE)
    if (is.element("country", names(dat))) 
      dat[, country := as.integer(country == "Australia")]
    
    if (file.exists(fl_path)) load(fl_path) else {
      
      de_crf <- boot_crf(data = dat, X = c(attr(dat, "sfm")$Z, attr(dat, "sfm")$W), 
                         W = attr(dat, "sfm")$X, Y = attr(dat, "sfm")$Y)
      save(de_crf, file = fl_path)
    }
    
    dat <- dat[, de_mean := rowMeans(de_crf, na.rm = TRUE)]
    dat <- dat[, de_var := rowVars(de_crf, na.rm = TRUE)]
    
    dat[, diag_group := std_diag(adm_diag, elective)]
    dat[, age_group := age_grp(age)]
    
    mean_dt <- dat[, list(mean_X = mean(de_mean), var_X = mean(de_var)), 
                   by = c("age_group", "diag_group")]
    mean_dt[, mean_X := trim_ends(mean_X, 0.02)]
    mean_dt[, p_val := pnorm(abs(mean_X/sqrt(var_X)), lower.tail = FALSE)]
    mean_dt[, age_group := gsub(" years", "", age_group)]
    
    ggplot(mean_dt[diag_group != "Other"], 
           aes(x = diag_group, y = age_group, fill = mean_X, alpha = 1 - p_val)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, name = "DE", labels = scales::percent) +
      theme_minimal() + 
      geom_text(aes(label = paste0(round(mean_X * 100, 1), "%")), 
                color = "black", size = 3) +
      guides(alpha = "none") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 13)) +
      geom_vline(xintercept = 4.5, color = "red") +
      ylab("Age group (years)") + xlab("Diagnosis group")
    
     ggsave(paste0("results/de-hetero-", src, "-", outcome, ".png"), 
           width = 10, height = 7, bg = "white")
  }
}