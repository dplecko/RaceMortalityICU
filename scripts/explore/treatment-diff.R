
ricu:::init_proj()

# Treatment Differences
for (cnc in c("is_vent", "is_rrt", "is_trache", "is_inotrop")) {
  
  cat("----", cnc, "----\n")
  dat <- load_concepts(c(cnc, "age", "sex", "apache_iii_rod", "apache_iii_diag",
                         "indig"), "anzics",
                       verbose = FALSE)
  dat[, apache_iii_diag := as.factor(apache_iii_diag)]
  dat[is.na(indig), indig := 0]
  dat <- dat[complete.cases(dat)]
  mod <- glm(
    as.formula(paste(cnc, "~ . - ICUStayID")),
    data = dat, family = "binomial"
  )
  print(summary(mod)$coefficients["indig", ])
  cat("----  ----\n")
}

# Treatment Effects
trt_var <- "is_inotrop"

dat <- merge(
  load_data("anzics"),
  load_concepts(trt_var, "anzics"), all.x = TRUE
)

physio <- load_concepts(c("map", "po2", "pco2", "resp"), "anzics")
physio[, c(index_var(physio)) := NULL]

dat <- merge(dat, physio, all.x = TRUE)
dat[, sex := ifelse(sex == "Male", 1, 0)]
dat[is.na(get(trt_var)), c(trt_var) := FALSE]

for (var in c("map", "po2", "pco2", "resp")) {
  
  var_med <- median(dat[[var]], na.rm = TRUE)
  dat[is.na(get(var)), c(var) := var_med]
}


for (indig_grp in c(0, 1)) {
  
  mod <- glm(
    death ~ age + apache_iii_rod + factor(apache_iii_diag) + sex + elective +
            po2 + pco2 + map + resp + is_inotrop,
    data = dat[indig == indig_grp], family = "binomial"
  )
  print(summary(mod))
}

# causal forest approach
W <- c("is_vent")
X <- c("age", "sex", "apache_iii_rod", "apache_iii_diag", "elective",
       "map", "po2", "pco2", "resp")
Y <- "death"
vent_crf <- causal_forest(
  X = dat[, X, with = FALSE],
  W = dat[[W]], Y = dat[[Y]]
)

# MIMIC-IV / AUMC / Salzburg analyses
src <- "sic"
patient_ids <- id_col(load_concepts("adm_episode", src, 
                                    verbose = FALSE)[adm_episode == 1])
dat <- load_concepts(c("age", "sex", "saps3", "death"), 
                     src, patient_ids = patient_ids)
dat[, c(index_var(dat)) := NULL]
imp_lst <- list(
  age = 65,
  acu_24 = 0,
  saps3 = 42,
  charlson = 0,
  lact_24 = 1,
  ast_24 = 20,
  pafi_24 = 500,
  death = FALSE
)

for (i in seq_len(ncol(dat))) {
  
  var <- names(dat)[i]
  if (any(is.na(dat[[var]])) & !is.null(imp_lst[[var]]))
    dat[is.na(get(var)), c(var) := imp_lst[[var]]]
}

trt <- "mech_vent"
trt_dat <- load_concepts(trt, src)
trt_dat <- trt_dat[, list(is_trt = any(get(trt) == "invasive" | get(trt) > 0)), 
                   by = c(id_vars(trt_dat))]


dat <- merge(dat, trt_dat, all.x = TRUE)
dat[is.na(is_trt), is_trt := FALSE]
mod <- glm(
  death ~ age + sex + saps3 + is_trt,
  data = dat, family = "binomial"
)
summary(mod)
