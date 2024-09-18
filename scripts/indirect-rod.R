
ricu:::init_proj()

srcs <- c("anzics", "miiv")
dat_lst <- lapply(
  srcs, 
  function(src) {
    dat <- load_data(src)
    dat[, age := round(age)]
    n_mc <- 10^6
    
    # pre-compute age look-ups
    lookup <- replicate(17, NULL)
    for (i in seq(range(dat$age)[1], range(dat$age)[2])) {
      
      lookup[[i]] <- list(NULL)
      lookup[[i]][[1]] <- which(dat$age == i & dat$majority == 0)
      lookup[[i]][[2]] <- which(dat$age == i & dat$majority == 1)
    }
    
    rows <- lapply(
      seq_len(n_mc),
      function(i) {
        
        ag <- sample(dat$age, 1)
        s_maj <- sample(lookup[[ag]][[2]], 1)
        
        if (ag == 98) {
          
          s_min <- sample(lookup[[ag+1]][[1]], 1)
        } else if (ag == 100) {
          
          s_min <- lookup[[100]][[1]][1]
        } else s_min <- sample(lookup[[ag]][[1]], 1)
        
        c(s_min, s_maj)
      }
    )
    rows <- do.call(rbind, rows)
    rbind(dat[rows[, 1]], dat[rows[, 2]])  
  }
)
names(dat_lst) <- srcs

#' * comparison of illness severity *
ils_plts <- list()
for (src in srcs) {
  
  dat_adj <- dat_lst[[src]]
  if (src == "anzics") {
    
    ils_var <- "apache_iii_rod"
    ils_plt <- ggplot(
      dat_adj, aes(x = .data[[ils_var]], fill = factor(majority))
    ) +
      geom_density(alpha = 0.5) + theme_bw()  +
      scale_fill_discrete(name = "Group", 
                          labels = c("First Nations", "Majority")) +
      theme(legend.position = "inside", legend.position.inside = c(0.7, 0.7),
            legend.box.background = element_rect(),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +
      xlab("APACHE-III Risk of Death") + ylab("Probability Density") +
      ggtitle("Australia and New Zealand")
    
  } else if (src == "miiv") {
    
    pmf_compute <- function(dat_adj, ils_var) {

      ils_dat <- dat_adj[, .N, by = c("majority", ils_var)]
      ils_dat <- ils_dat[, list(pmf = N / sum(N), ils = get(ils_var)), 
                         by = c("majority")]
      ils_dat <- merge(
        as.data.table(expand.grid(majority = c(0, 1), 
                                  ils = seq(0, max(ils_dat$ils)))),
        ils_dat, all.x = TRUE
      )
      ils_dat[is.na(pmf), pmf := 0]
      ils_dat
    }
    
    # get p-value for sofa distributions
    pmf_dat <- pmf_compute(dat_adj, "acu_24")
    pmf_dat[, maj := majority]
    pmf_dat[, size := nrow(dat[majority == maj]), by = c("ils", "maj")]
    pmf_dat[, obs_counts := round(pmf * size)]
    print(chisq.test(x = cbind(pmf[majority == 1]$obs_counts, pmf[majority == 0]$obs_counts)))
    
    ils_plt <- ggplot(pmf_dat, 
                      aes(x = ils, y = pmf, fill = factor(majority))) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5, 
               color = "black", width = 1) + 
      theme_bw() +
      scale_fill_discrete(name = "Group", 
                          labels = c("African-American", "White")) +
      theme(legend.position = "inside", legend.position.inside = c(0.7, 0.7),
            legend.box.background = element_rect(),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +
      xlab("SOFA score at 24 hours") + ylab("Probability Mass") +
      ggtitle("United States")
  }
  
  ils_plts[[src]] <- ils_plt
}

cowplot::plot_grid(plotlist = ils_plts, labels = c("(A)", "(B)"))
ggsave(file.path("results", "ils-effect.png"), width = 12, height = 4)

#' * diagnosis comparison * 
diag_plts <- list()
for (src in srcs) {
  
  dat_adj <- dat_lst[[src]]
  diag_var <- if (src == "anzics") "apache_iii_diag" else "diag_index"
  mort <- dat_adj[, list(mort = mean(death), size = .N), by = diag_var]
  mort <- setnames(mort, diag_var, "diag")
  mort_prop <- merge(
    dat_adj[majority == 0, .N, by = diag_var][, list(prop0 = N / sum(N), 
                                                     diag = get(diag_var))],
    dat_adj[majority == 1, .N, by = diag_var][, list(prop1 = N / sum(N), 
                                                     diag = get(diag_var))]
  )
  mort_prop <- merge(mort_prop, mort)
  mort_prop[, rep_ratio := prop0 / prop1]
  lm_mp <- lm(mort ~ rep_ratio, data = mort_prop, weights = mort_prop$size)
  diag_plt <- ggplot(mort_prop, aes(x = rep_ratio, y = mort, size = size)) +
    geom_point(show.legend = FALSE) + theme_bw() +
    geom_abline(intercept = lm_mp$coefficients[1], slope = lm_mp$coefficients[2],
                color = "red", linewidth = 1.2) +
    xlab("Representation ratio") + ylab("Mortality")
  diag_plts[[src]] <- diag_plt
}

cowplot::plot_grid(plotlist = diag_plts, labels = c("(A)", "(B)"))
ggsave(file.path("results", "diag-effect.png"), width = 12, height = 4)  

#' * chronic health comparison (MIMIC-IV) * 
p_charlson <- ggplot(pmf_compute(dat_adj, "charlson"), aes(x = ils, y = pmf, 
                                             fill = factor(majority))) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.5, 
           color = "black", width = 1) + 
  theme_bw() +
  scale_fill_discrete(name = "Group", labels = c("African-American", "White")) +
  theme(legend.position = "inside", legend.position.inside = c(0.7, 0.7),
        legend.box.background = element_rect(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +
  xlab("Charlson Comorbidity Index") + ylab("Probability Mass") +
  ggtitle("United States")

ils_plts <- c(ils_plts, list(p_charlson))
cowplot::plot_grid(plotlist = ils_plts, labels = c("(A)", "(B)", "(C)"),
                   ncol = 3L)
ggsave(file.path("results", "ils-effect.png"), width = 15, height = 4)
# ggsave(file.path("results", "charlson-miiv.png"), width = 6, height = 4)

#' * computing the p-values *
srcs <- c("anzics", "miiv")
mc_reps <- 100
for (src in srcs) {
  
  dat <- load_data(src)
  dat[, age := round(age)]
  
  # pre-compute age look-ups
  lookup <- replicate(17, NULL)
  for (i in seq(range(dat$age)[1], range(dat$age)[2])) {
    
    lookup[[i]] <- list(NULL)
    lookup[[i]][[1]] <- which(dat$age == i & dat$majority == 0)
    lookup[[i]][[2]] <- which(dat$age == i & dat$majority == 1)
  }
  
  pval <- mean_tst <- c()
  for (rep in seq_len(mc_reps)) {
    
    mc_dat <- c() 
    for (lvl in c(0, 1)) {
      
      n_mc <- nrow(dat[majority == lvl])
      rows <- lapply(
        seq_len(n_mc),
        function(i) {
          
          ag <- sample(dat$age, 1)
          if (lvl == 1) return(sample(lookup[[ag]][[2]], 1))
          
          if (ag == 98) {
            
            s_min <- sample(lookup[[ag+1]][[1]], 1)
          } else if (ag == 100) {
            
            s_min <- lookup[[100]][[1]][1]
          } else s_min <- sample(lookup[[ag]][[1]], 1)
          
          return(s_min)
        }
      )
      mc_dat <- if (lvl == 0) dat[unlist(rows)] else rbind(mc_dat, dat[unlist(rows)])
    }
    
    if (src == "miiv")
      mean_tst <- rbind(mean_tst, mc_dat[, mean(acu_24), by = "majority"])
    # pval <- c(pval, wilcox.test(x = mc_dat[majority == 1]$acu_24, 
    #                             mc_dat[majority == 0]$acu_24)$p.value)
  }
  
  if (src == "miiv") {
    
    mean_vals <- data.table(A = mean_tst[majority==1]$V1, 
                            B = mean_tst[majority==0]$V1)
    print(mean_vals[, list(pvalue = 2 * min(mean(A < B), mean(A >= B)))])
  }
}
