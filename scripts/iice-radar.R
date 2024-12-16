
ricu:::init_proj()

# functionality for constructing the IICE Radar
iice_radar <- function(lvl = "SA3", grad_cts = FALSE) {
  
  # adjust the report risk by age
  age_adjust <- TRUE
  
  # pull in the information on patients at risk
  pop_dat <- at_risk(lvl)
  
  # pull in the data on patients admitted
  dat <- pop_and_dat("AU")[["dat"]]
  
  # focus on year 2021, which matches the collection year of the census data
  dat <- dat[year == 2021]
  
  # load and merge in information on postal area of patients
  poa_dat <- load_concepts("postcode", "anzics", verbose = FALSE)
  dat <- merge(dat, poa_dat, all.x = TRUE)
  
  # pull in the data on for merging Statistical Areas that share postal codes
  crs <- sa_coarsening(area_map(), "postcode", lvl)
  crs$area <- seq_len(nrow(crs))
  
  # create mapping from Statistical Areas to merged areas
  poa_area <- do.call(
    rbind, Map(
      function(x, y) data.table(postcode = x, area = y),
      crs$coarse, crs$area
    )
  )
  
  # create the mapping from postal areas to merged areas
  lvl_area <- do.call(
    rbind, Map(
      function(x, y) data.table(lvl = x, area = y),
      crs$V1, crs$area
    )
  )
  setnames(lvl_area, "lvl", lvl)
  
  # merge ares information into population data and ICU patient data
  pop_dat <- merge(pop_dat, lvl_area, by = lvl)
  dat <- merge(dat, poa_area, by = "postcode", all.x = TRUE)
  
  dat <- dat[complete.cases(dat)]

  # prepare table for computing risk ratios for ICU admission
  ts_risk <- as.data.table(
    expand.grid(age = unique(dat$age), majority = c(0, 1), area = unique(dat$area))
  )
  
  # merge in admission counts for ICU patient data
  ts_risk <- merge(
    ts_risk, dat[, .N, by = c("age", "majority", "area")],
    all.x = TRUE
  )
  
  # categories with no admissions set to 0
  ts_risk[is.na(N), N := 0]
  
  # compute the risks of admission by area
  ts_risk <- merge(
    ts_risk, pop_dat[, sum(value), by = c("age", "majority", "area")], 
    by = c("age", "majority", "area"), all.x = TRUE
  )
  
  # adjust by age if necessary
  if (age_adjust) {
    
    wgh_dat <- pop_dat[, list(N = sum(value)), by = c("age", "area")][, list(age = age, wgh = N/sum(N)),
                                                by = "area"]
    
    ts_risk <- merge(ts_risk, wgh_dat, by = c("age", "area"), all.x = TRUE)
    ts_risk[, risk := N / V1]
    
    ts_risk <- ts_risk[, list(risk = sum(risk * wgh) / sum(wgh)), 
                       by = c("area", "majority")] 
  } else {

    ts_risk <- ts_risk[, list(risk = sum(N) / sum(V1)), 
                       by = c("area", "majority")]
  }
  
  # merge the risks for majority and minority groups
  ts_rr <- merge(
    ts_risk[majority == 0, c("area", "risk"), with=FALSE],
    ts_risk[majority == 1, c("area", "risk"), with=FALSE],
    by = c("area")
  )
  
  # compute the risk ratio
  ts_rr[, rr := risk.x / risk.y]
  
  # if the risk is 0 for the majority group, set the risk ratio to NA
  ts_rr[is.nan(rr), rr := NA]
  
  # load the shape file for statistical areas
  folder <- "data/abs-data"
  shp <- sf::st_read(file.path(folder, paste0(tolower(lvl), "-shp"), 
                               paste0(tolower(lvl), "-shp.shp")))
  shp <- as.data.table(shp)
  shp <- setnames(shp, paste0(lvl, "_CODE21"), lvl)
  shp <- shp[!grepl("ZZ", get(lvl))]
  shp[[lvl]] <- as.numeric(shp[[lvl]])
  
  # merge area information with risk ratios
  lvl_rr <- merge(lvl_area, ts_rr[, c("area", "rr"), with=FALSE], by = "area")
  
  # merge full information with the shape file
  shp <- merge(shp, lvl_rr, by = lvl, all.x = TRUE)
  
  # choose type of gradient for coloring the areas in IICE Radar
  if (grad_cts) {
    
    shp[, fill_var := pmin(rr, 4)]
    grad <- scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 1, name = "Risk\nRatio")
  } else {
    
    brks <- c(0.1, 0.5, 1, 2)
    nms <- c("< 10%", "10-50%", "50-100%", "100-200%", "> 200%")
    shp[, fill_var := cut(rr-1, breaks = c(-Inf, brks, Inf), labels = nms)]
    shp[, fill_var := factor(fill_var, levels = rev(nms))]
    # clrs <- c("white", "peachpuff", "tomato", "firebrick", "maroon")
    clrs <- c("white", "#FFCCCC", "#FF6666", "red", "black")
    # c("white", "pink", "lightcoral", "red", "darkred")
    names(clrs) <- nms
    grad <- scale_fill_manual(values = clrs, na.translate = FALSE,
                              name = "Excess\nRisk Ratio")
  }

  # plot the radar
  ggplot(shp) +
    geom_sf(aes(fill = fill_var, geometry = geometry)) +
    grad +
    theme_minimal() +
    coord_sf(xlim = c(114, 153)) +
    theme(
      legend.title = element_text(hjust = 0.5) # Center the legend title
    )
}

ggsave("results/iice-radar.png", plot = iice_radar(),
       width = 10, height = 6, bg = "white")
