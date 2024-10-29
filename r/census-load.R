
census_AU <- function() {
  
  census_all <- read.csv("data/aus_age_overall.csv")
  names(census_all) <- gsub("X", "", names(census_all))
  census_all <- census_all[, c(1, 4, 3, 2)]
  
  census_fn <- read.csv("data/aus_age_firstnations.csv")
  
  # split bins to have better granularity
  aux_data <- structure(
    list(age = c("0-4 years", "5-9 years", "10-14 years", 
                 "15-19 years", "20-24 years", "25-29 years", "30-34 years", 
                 "35-39 years", "40-44 years", "45-49 years", "50-54 years", 
                 "55-59 years", "60-64 years", "65-69 years", "70-74 years", 
                 "75-79 years", "80-84 years", "85+ years"), 
         people = c(105798, 109003, 111041, 95914, 86639, 78564, 66914, 
                    57121, 49015, 50122, 47660, 40124, 32446, 22889, 15180, 8076, 
                    4346, 2857)
    ), 
    class = "data.frame", row.names = c(NA, -18L)
  )
  
  # Calculate proportions for splitting merged age groups
  split_data16 <- function(original, aux) {
    
    proportions <- aux$people / sum(aux$people)
    proportions * original$X2016
  }
  
  split_data21 <- function(original, aux) {
    
    proportions <- aux$people / sum(aux$people)
    proportions * original$X2021
  }
  
  # Apply proportions to split merged age groups
  census_fnc <- data.frame(
    age = c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", 
            "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
            "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", 
            "75-79 years", "80-84 years", "85+ years"),
    `2016` = c(census_fn$X2016[1],
               split_data16(census_fn[2, ], aux_data[2:3, ]),
               split_data16(census_fn[3, ], aux_data[4:5, ]),
               split_data16(census_fn[4, ], aux_data[6:7, ]),
               split_data16(census_fn[5, ], aux_data[8:9, ]),
               split_data16(census_fn[6, ], aux_data[10:11, ]),
               split_data16(census_fn[7, ], aux_data[12:13, ]),
               split_data16(census_fn[8, ], aux_data[14:18, ])),
    `2021` = c(census_fn$X2021[1],
               split_data21(census_fn[2, ], aux_data[2:3, ]),
               split_data21(census_fn[3, ], aux_data[4:5, ]),
               split_data21(census_fn[4, ], aux_data[6:7, ]),
               split_data21(census_fn[5, ], aux_data[8:9, ]),
               split_data21(census_fn[6, ], aux_data[10:11, ]),
               split_data21(census_fn[7, ], aux_data[12:13, ]),
               split_data21(census_fn[8, ], aux_data[14:18, ]))
  )
  names(census_fnc) <- c("age", "2016", "2021")
  
  list(total = census_all, minority = census_fnc)
}

census_NZ <- function() {
  
  nz_data <- fread("data/nz_population.csv", skip = 2, 
                   colClasses = list(character = 3:41))
  
  # Remove the "Percent(1)" column and all columns after that
  perc_idx <- which(as.vector(nz_data[1]) == "Percent(1)")
  nz_data <- nz_data[, seq_len(perc_idx - 1), with=FALSE]
  
  # Remove commas from numeric columns
  suppressWarnings(
    nz_data[, (3:ncol(nz_data)) := lapply(.SD, function(x) as.numeric(gsub(",", "", x))), 
            .SDcols = 3:ncol(nz_data)]
  )
  
  # Rename the first two columns
  setnames(nz_data, old = names(nz_data)[1:2], new = c("EthnicGroup", "Year"))
  
  # remove rows where Year is NA
  nz_data <- nz_data[!is.na(Year)]
  
  # Define correct age groups, including the combination of "85-89" and "90+" into "85+ years"
  age_groups <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years",
                  "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years",
                  "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years",
                  "75-79 years", "80-84 years", "85+ years")
  
  # Adjust column names in the dataset to match the target age groups
  colnames(nz_data)[3:ncol(nz_data)] <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years",
                                          "20-24 years", "25-29 years", "30-34 years", "35-39 years",
                                          "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                                          "60-64 years", "65-69 years", "70-74 years", "75-79 years",
                                          "80-84 years", "85-89 years", "90+ years", "Total")
  
  # Combine the "85-89 years" and "90+ years" columns into "85+ years"
  nz_data[, `85+ years` := `85-89 years` + `90+ years`]
  
  # Remove the old "85-89 years" and "90+ years" columns
  nz_data <- nz_data[, !c("85-89 years", "90+ years", "Total"), with = FALSE]
  
  # Function to create a summary for a given ethnic group
  summarize_population <- function(group) {
    dt <- nz_data[EthnicGroup == group, .SD, .SDcols = c("Year", age_groups)]
    dt <- melt(dt, id.vars = "Year", variable.name = "Age", value.name = "Count")
    dt <- dcast(dt, Age ~ Year, value.var = "Count")
    setcolorder(dt, c("Age", "2023", "2018", "2013"))
    return(dt)
  }
  
  # Get summaries for total population and Māori
  total <- summarize_population("Total people")
  maori <- summarize_population("Māori")
  
  list(total = as.data.frame(total), minority = as.data.frame(maori))
}

census <- function(country = c("AU", "NZ")) {
  
  country <- match.arg(country, c("AU", "NZ"))
  switch (country, AU = census_AU(), NZ = census_NZ())
}

interpolate_census <- function(data, start, end) {
  
  impute_vec <- function(x) {
    
    n <- length(x)
    nna_idx <- which(!is.na(x))
    for (i in 1:(length(nna_idx)-1)) {
      
      c_idx <- c(nna_idx[i], nna_idx[i+1])
      vals <- x[c_idx]
      lmod <- lm(vals ~ c_idx)
      
      if (i == 1) {
        
        if (i == length(nna_idx) - 1) {
          rng <- 1:n
        } else rng <- 1:nna_idx[i+1]
        
        x[rng] <- predict(lmod, data.frame(c_idx = rng))
        
      } else if (i == length(nna_idx) - 1) {
        
        x[nna_idx[i]:n] <- predict(lmod, data.frame(c_idx = nna_idx[i]:n))
        
      } else {
        
        x[nna_idx[i]:nna_idx[i+1]] <- 
          predict(lmod, data.frame(c_idx = nna_idx[i]:nna_idx[i+1]))
      }
      
    }
    x
  }
  
  oidx <- as.numeric(names(data)[-1]) 
  res <- c()
  for (i in seq_len(nrow(data))) {
    
    ovals <- as.numeric(data[i, -1])
    imp <- rep(NA, end - start + 1)
    imp[oidx - start + 1] <- ovals 
    imp <- impute_vec(imp)
    
    if (any(is.na(imp))) browser()
    
    res <- rbind(
      res,
      cbind(age = data[i, 1], as.data.frame(t(imp)))  
    )
  }
  names(res) <- c("age", as.character(start:end))
  
  res
}

pop_and_dat <- function(country, full = FALSE) {
  
  census <- census(country)
  
  census_tot <- interpolate_census(census[["total"]], 2010, 2022)
  census_min <- interpolate_census(census[["minority"]], 2010, 2022)
  census_maj <- cbind(age = census_tot$age, census_tot[, -1] - census_min[, -1])
  
  melt_and_add <- function(df, majority = 0) {
    
    res <- melt(as.data.table(df), id.vars = "age", variable.name = "year")
    res[, year := as.integer(as.character(year))]
    res[, majority := majority]
  }
  
  pop_dat <- rbind(
    melt_and_add(census_maj, majority = 1),
    melt_and_add(census_min, majority = 0)
  )
  
  src <- if (country == "AU") "aics" else "nzics"
  dat <- merge(
    load_data(src, split_elective = TRUE), 
    load_concepts(c("adm_year"), "anzics", verbose = FALSE), 
    all.x = TRUE
  )
  
  dat[, age := age_grp(age)]
  dat[, diag_grp := floor(apache_iii_diag / 100)]
  dat[diag_grp == 7, diag_grp := 6]
  if (!full) dat[, c("country", "apache_iii_diag") := NULL]
  dat <- setnames(dat, "adm_year", "year")
  
  list(pop_dat = pop_dat, dat = dat)
}
