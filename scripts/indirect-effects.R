
#' Investigating indirect effects: illness severity and chronic health 
#' distributions.
ricu:::init_proj()
set.seed(2024)

# specify the target sources
srcs <- c("aics", "miiv")
dat_lst <- lapply(srcs, function(src) mc_dataset(load_data(src)))
names(dat_lst) <- srcs

# specify plotting theme settings
theme_block <- theme(
  legend.position = "inside", legend.position.inside = c(0.7, 0.7),
  legend.box.background = element_rect(),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
  legend.title = element_text(size = 14),  # Adjust size of legend title
  legend.text = element_text(size = 12),
  axis.text = element_text(size = rel(1.5)),  # Scale axis tick labels
  axis.title = element_text(size = rel(1.5))
)

# comparison of illness severity
ils_plts <- list()
for (src in srcs) {
  
  dat <- load_data(src)
  dat_adj <- dat_lst[[src]]
  
  anz_sofa <- TRUE
  if (is.element(src, c("aics", "anzics")) & !anz_sofa) {
    
    # use APACHE-III risk of death for illness severity on ANZICS APD
    ils_var <- "apache_iii_rod"
    
    # plot APACHE-III distribution across groups
    ils_plt <- ggplot(
      dat_adj, aes(x = .data[[ils_var]], fill = factor(majority))
    ) +
      geom_density(alpha = 0.5) + theme_bw()  +
      scale_fill_discrete(name = "Group", 
                          labels = c("Indigenous", "Non-Indigenous")) +
      theme_block +
      xlab("APACHE-III Risk of Death") + ylab("Probability Density") +
      ggtitle("Australia")
    
  } else {
    
    # use SOFA score for illness severity
    if (is.element(src, c("aics", "anzics"))) {
      
      sofa <- load_concepts("sofa_anz", "anzics")
      dat <- merge(dat, sofa, all.x = TRUE)
      dat_adj <- merge(dat_adj, sofa, all.x = TRUE)
      ils_var <- "sofa_anz"
      country <- "Australia"
      labs <- c("Indigenous", "Non-Indigenous")
    } else {
      
      ils_var <- "acu_24"
      country <- "United States"
      labs <- c("African-American", "White")
    }

    # compute the probability mass function of the SOFA score distribution at 24h
    pmf_dat <- pmf_compute(dat_adj, ils_var)
    
    # plot SOFA at 24 hours distribution across groups
    ils_plt <- ggplot(pmf_dat, 
                      aes(x = ils, y = pmf, fill = factor(majority))) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5, 
               color = "black", width = 1) + 
      theme_bw() +
      scale_fill_discrete(name = "Group", labels = labs) +
      theme_block +
      xlab("SOFA score on ICU day one") + ylab("Probability Mass") +
      ggtitle(country)
  }
  
  ils_plts[[src]] <- ils_plt
  ggsave(filename = paste0("results/ils-", src, ".png"), 
         plot = ils_plt, bg = "white", width = 5, height = 4)
}

# chronic health comparison (MIMIC-IV)
p_charlson <- ggplot(pmf_compute(dat_adj, "charlson"), aes(x = ils, y = pmf, 
                                             fill = factor(majority))) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.5, 
           color = "black", width = 1) + 
  theme_bw() +
  scale_fill_discrete(name = "Group", labels = c("African-American", "White")) +
  theme_block +
  xlab("Charlson Comorbidity Index") + ylab("Probability Mass") +
  ggtitle("United States")

ggsave(file.path("results", "charlson-miiv.png"), width = 5, height = 4,
       plot = p_charlson)

# compute the p-values for mean differences
srcs <- c("miiv", "aics")
mc_reps <- 100
for (src in srcs) {
  
  ils_var <- if (src == "aics") "sofa_anz" else "acu_24"
  dat <- load_data(src)
  dat[, age := round(age)]
  
  if (src == "aics")
    dat <- merge(dat, load_concepts("sofa_anz", "anzics"), all.x = TRUE)
  
  pval <- mean_tst <- c()
  for (rep in seq_len(mc_reps)) {
    
    cat("MC iteration \r", rep)
    
    mc_dat <- mc_dataset(dat, n_mc = list(sum(dat$majority == 0), 
                                          sum(dat$majority == 1)))
    mean_tst <- rbind(mean_tst, mc_dat[, mean(get(ils_var)), by = "majority"])
  }
  cat("\n")
  
  mean_vals <- data.table(A = mean_tst[majority==1]$V1, 
                          B = mean_tst[majority==0]$V1)
  cat(src, "\n")
  print(mean_vals[, list(pvalue = 2 * min(mean(A < B), mean(A >= B)))])
}
