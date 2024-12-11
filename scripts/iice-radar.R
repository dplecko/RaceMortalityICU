
ricu:::init_proj()

iice_radar <- function(lvl = "SA3", grad_cts = FALSE) {
  
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
  folder <- file.path(ricu::data_dir(), "raw", "anzics-2024")
  shp <- sf::st_read(file.path(folder, paste0(tolower(lvl), "-shp"), 
                               paste0(tolower(lvl), "-shp.shp")))
  shp <- as.data.table(shp)
  shp <- setnames(shp, paste0(lvl, "_CODE21"), lvl)
  shp <- shp[!grepl("ZZ", get(lvl))]
  shp[[lvl]] <- as.numeric(shp[[lvl]])
  
  lvl_rr <- merge(lvl_area, ts_rr[, c("area", "rr"), with=FALSE], by = "area")
  
  shp <- merge(shp, lvl_rr, by = lvl, all.x = TRUE)
  
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
