
ricu:::init_proj()

iice_radar <- function(lvl = "SA3") {
  
  age_adjust <- TRUE
  # pull in the information on patients at risk
  pop_dat <- at_risk(lvl)
  
  # pull in the data on patients admitted
  dat <- pop_and_dat("AU")[["dat"]]
  dat <- dat[year == 2021]
  poa_dat <- load_concepts("postcode", "anzics")
  dat <- merge(dat, poa_dat, all.x = TRUE)
  
  # pull in the data on merging areas
  crs <- sa_coarsening(area_map(), "postcode", lvl)
  crs$area <- seq_len(nrow(crs))
  
  # get the area indicator to both pop_dat and dat
  poa_area <- do.call(
    rbind, Map(
      function(x, y) data.table(postcode = x, area = y),
      crs$coarse, crs$area
    )
  )
  
  lvl_area <- do.call(
    rbind, Map(
      function(x, y) data.table(lvl = x, area = y),
      crs$V1, crs$area
    )
  )
  setnames(lvl_area, "lvl", lvl)
  
  # merge areas into pop_dat, dat
  pop_dat <- merge(pop_dat, lvl_area, by = lvl)
  dat <- merge(dat, poa_area, by = "postcode", all.x = TRUE)
  
  if (sum(is.na(dat$area)) > 0) {
    
    cat("Some areas not mapped. Check.\n")
    dat <- dat[complete.cases(dat)]
  }

  # get risks by age group -> exists
  # get age-adjusted risk ratio -> exists
  ts_risk <- as.data.table(
    expand.grid(age = unique(dat$age), # diag_grp = unique(dat$diag_grp),
                majority = c(0, 1), area = unique(dat$area))
  )
  
  ts_risk <- merge(
    ts_risk, dat[, .N, by = c("age", "majority", "area")],
    all.x = TRUE
  )
  ts_risk[is.na(N), N := 0]
  
  # risk by area
  ts_risk <- merge(
    ts_risk, pop_dat[, sum(value), by = c("age", "majority", "area")], 
    by = c("age", "majority", "area"), all.x = TRUE
  )
  
  if (age_adjust) {
    
    wgh_dat <- pop_dat[, list(N = sum(value)), by = c("age", "area")][, list(age = age, wgh = N/sum(N)),
                                                by = "area"]
    
    ts_risk <- merge(ts_risk, wgh_dat, by = c("age", "area"), all.x = TRUE)
    ts_risk[, risk := N / V1]
    
    ts_risk <- ts_risk[, list(risk = sum(risk * wgh) / sum(wgh)), 
                       by = c("area", "majority")] 
  } else {
    # pooled estimate
    ts_risk <- ts_risk[, list(risk = sum(N) / sum(V1)), 
                       by = c("area", "majority")]
  }
  
  ts_rr <- merge(
    ts_risk[majority == 0, c("area", "risk"), with=FALSE],
    ts_risk[majority == 1, c("area", "risk"), with=FALSE],
    by = c("area")
  )
  
  ts_rr[, rr := risk.x / risk.y]
  ts_rr[is.nan(rr), rr := NA]
  
  # load the shape file for lvl
  folder <- file.path(ricu::data_dir(), "raw", "anzics")
  shp <- sf::st_read(file.path(folder, paste0(tolower(lvl), "-shp"), 
                               paste0(tolower(lvl), "-shp.shp")))
  shp <- as.data.table(shp)
  shp <- setnames(shp, paste0(lvl, "_CODE21"), lvl)
  shp <- shp[!grepl("ZZ", get(lvl))]
  shp[[lvl]] <- as.numeric(shp[[lvl]])
  
  lvl_rr <- merge(lvl_area, ts_rr[, c("area", "rr"), with=FALSE], by = "area")
  
  shp <- merge(shp, lvl_rr, by = lvl, all.x = TRUE)
  
  # plot the radar
  ggplot(shp) +
    geom_sf(aes(fill = pmin(rr, 5), geometry = geometry)) +      # Map fill aesthetic to the 'rr' column
    scale_fill_viridis_c() +       # Use a color scale for continuous data (you can customize the scale)
    theme_minimal() +              # Minimal theme for a clean look
    labs(fill = "RR Value")        # Add a label for the color legend
}
