
#' Inspection of parametric models and interactions DE x IE, IE x SE.
ricu:::init_proj()
src <- "aics"
dat <- load_data(src, split_elective = TRUE)
# dat <- dat[apache_iii_diag != 0]
dat <- dat[, diag_grp := factor(floor(apache_iii_diag / 100))]

mod0 <- glm(death ~ age + apache_iii_rod + sex + irsad + majority +
             diag_grp, data = dat)

mod <- glm(death ~ age + apache_iii_rod + sex + irsad + majority +
             diag_grp + majority * diag_grp, data = dat)

anova(mod, mod0, test = "Chisq")

cf <- summary(mod)$coef

# get a cleaner extraction
# cf[rownames(cf) %in% paste0("diag_grp", 1:42), "Estimate"] +
#   cf[rownames(cf) %in% paste0("majority:diag_grp", 1:42), "Estimate"]

cf <- cf[grepl("majority", rownames(cf)), ]
res <- data.frame(
  effect = cf[1, "Estimate"] + cf[-1, "Estimate"],
  sd = sqrt(cf[1, "Std. Error"]^2 + cf[-1, "Std. Error"]^2),
  diag = rownames(cf[-1, ])
)
res$diag_grp <- as.integer(gsub("majority:diag_grp", "", rownames(res)))
res$type <- cut(res$diag_grp, c(-Inf, 11.5, 22.5, Inf), 
                labels = c("med", "surg (em)", "surg (el)"))