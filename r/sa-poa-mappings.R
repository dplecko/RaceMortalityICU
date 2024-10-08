
build_tables <- function(lvl = "SA3", ret = c("tbls", "qual")) {
  
  ret <- match.arg(ret, c("tbls", "qual"))
  fl <- paste0(tolower(lvl), "-level.csv")
  raw_data <- readr::read_lines(file.path("data/abs-data", fl))
  
  grps <- c("\" Non-Indigenous", "\" Aboriginal\"", "\" Torres Strait Islander\"",
            "Both Aboriginal and Torres Strait Islander", 
            "Not stated", "\" Overseas", "\" Total\"")
  
  s_ids <- vapply(grps, function(x) which(str_detect(raw_data, x)), numeric(1L))
  num_rows <- sort(s_ids)[2] - sort(s_ids[1]) - 4
  
  cnt_lst <- list()
  for (grp in grps) {
    
    detect_start <- which(str_detect(raw_data, grp))
    grp_cnt <- suppressMessages(suppressWarnings(
      readr::read_csv(file.path("data/abs-data", fl), 
                      skip = detect_start, n_max = num_rows)
    ))
    
    cnt_lst <- c(cnt_lst, list(grp_cnt))
  }
  
  names(cnt_lst) <- c("non-indig", "aborigin", "torres", "both", "unknown", 
                      "visitor", "total")
  
  for (i in seq_along(cnt_lst)) {
    
    dt <- cnt_lst[[i]]
    dt <- as.data.table(dt)
    dt <- dt[-1] # remove the extra row
    last_row <- nrow(dt)
    last_col <- which(names(dt) == "Total") 
    
    col_sums <- as.vector(as.matrix(dt[last_row, 2:(last_col - 1)]))
    row_sums <- dt[-c(last_row)]$Total
    dt <- dt[1:(last_row - 1), seq_len(last_col - 1), with=FALSE]
    
    dt <- setnames(dt, "AGE5P Age in Five Year Groups", paste0(lvl, "_NAME"))
    dt <- merge(dt, unique(area_map()[, c(lvl, paste0(lvl, "_NAME")), with=FALSE]),
                by = paste0(lvl, "_NAME"), all.x = TRUE)
    setcolorder(dt, lvl, before=1)
    
    attr(dt, "row_sums") <- row_sums
    attr(dt, "col_sums") <- col_sums
    
    cnt_lst[[i]] <- dt
  }
  
  #' * perform quality control (!) *
  # browser()
  if (ret == "qual") {
    
    qual <- c()
    for (i in seq_along(cnt_lst)) {
      
      num_dt <- cnt_lst[[i]][, -c(1, 2), with=FALSE]
      row_qual <- rowSums(num_dt) / attr(cnt_lst[[i]], "row_sums") - 1
      col_qual <- colSums(num_dt) / attr(cnt_lst[[i]], "col_sums") - 1
      
      qual <- rbind(
        qual,
        data.frame(tbl = names(cnt_lst)[i], type = "col", val = col_qual),
        data.frame(tbl = names(cnt_lst)[i], type = "row", val = row_qual)
      )
    }
    
    return(qual)
  }
  
  cnt_lst
}

area_map <- function() {
  
  folder <- file.path(ricu::data_dir(), "raw", "anzics")
  if (file.exists("data/area-map.RData")) {
    
    load(file.path("data/area-map.RData"))
  } else {
    
    sa1_seifa <- readxl::read_excel(file.path(folder, "sa1-seifa.xlsx"), 
                                    sheet = 8, skip = 4)
    sa1_seifa <- as.data.table(sa1_seifa)
    sa1_seifa <- setnames(sa1_seifa, "2021 Statistical Area Level 2 (SA2) Code", 
                          "SA2_CODE21")
    sa1_seifa <- setnames(sa1_seifa, "2021 Statistical Area Level 1 (SA1) Code", 
                          "SA1_CODE21")
    sa1_seifa <- setnames(sa1_seifa, "2021 Postal Area (POA) Code", "POA")
    
    sa1_seifa <- sa1_seifa[, c("SA1_CODE21", "SA2_CODE21", "POA"), with=FALSE]
    sa1_seifa <- sa1_seifa[complete.cases(sa1_seifa)]
    
    sas <- sf::st_read(file.path(folder, "sa1-shp/sa1-shp.shp"))
    
    num_cols <- paste0("SA", 1:4, "_CODE21")
    nam_cols <- paste0("SA", 2:4, "_NAME21")
    sa_cols <- c(num_cols, nam_cols)
    sas <- as.data.table(sas[, sa_cols])
    sas <- unique(sas[, 1:7, with=FALSE])
    sas <- sas[SA1_CODE21 != "ZZZZZZZZZZZ"]
    
    for (col in num_cols) sas[, c(col) := as.numeric(get(col))]
    
    areas <- merge(sa1_seifa, sas, by = c("SA1_CODE21", "SA2_CODE21"), all = TRUE)
    areas <- setnames(areas, num_cols, paste0("SA", 1:4))
    areas <- setnames(areas, nam_cols, paste0("SA", 2:4, "_NAME"))
    save(areas, file = file.path("data/area-map.RData"))
  }
  
  areas
}

at_risk <- function(lvl = "SA3") {
  
  tbls <- build_tables(lvl)
  
  for (i in seq_along(tbls)) {
    
    if (i == 1) next
    assert_that(all(tbls[[1]][, 1] == tbls[[i]][, 1]))
  }
  
  maj <- cbind(
    tbls[["non-indig"]][, c(1), with = FALSE],
    tbls[["non-indig"]][, -c(1, 2)] + tbls[["unknown"]][, -c(1, 2)]
  )
  maj$majority <- 1
  
  min <- cbind(
    tbls[["non-indig"]][, c(1), with = FALSE],
    tbls[["aborigin"]][, -c(1, 2)] + tbls[["torres"]][, -c(1, 2)] + 
      tbls[["both"]][, -c(1, 2)]
  )
  min$majority <- 0
  
  ret <- melt(rbind(maj, min), id.vars = c(lvl, "majority"),
              variable.factor = FALSE)
  
  mrg_lvls <- c("85-89 years", "90-94 years", "95-99 years", "100 years and over")
  add_ret <- ret[variable %in% mrg_lvls, list(value = sum(value)), 
                 by = c(lvl, "majority")]
  add_ret[, variable := "85+ years"]
  
  ret <- rbind(
    ret[!(variable %in% mrg_lvls)],
    add_ret
  )
  setnames(ret, "variable", "age")
}

sa_coarsening <- function(areas, lvl, mrg_lvl) {
  
  areas <- area_map()
  
  cls <- unique(areas[, c(lvl, mrg_lvl), with=FALSE])[, list(list(get(mrg_lvl))), 
                                                      by = lvl]
  cls[, coarse := list(list(get(lvl))), by = lvl]
  for (poa in unique(areas[[mrg_lvl]])) {
    
    mrg <- which(unlist(cls[, lapply(V1, function(x) is.element(poa, x))]))
    mrg <- sort(mrg)
    if (length(mrg) > 1) {
      
      # cat(mrg_lvl, poa, "merges", length(mrg), lvl, "s\n")
      
      for (i in 2:length(mrg)) {
        
        nV1 <- unique(do.call(c, cls[mrg]$V1))
        ncoarse <- unique(do.call(c, cls[mrg]$coarse))
        cls[mrg[1], V1 := list(nV1)]
        cls[mrg[1], coarse := list(ncoarse)]
      }
      
      cls <- cls[-mrg[-1]]
    }
  }
  
  cls
}

# # perform quality control
# 
# tbls <- build_tables()
# # quality inspection
# qual <- build_tables(ret = "qual")
# qual[is.nan(qual$val), ]$val <- 0
# qual[qual$val > 1, ]$val <- 1
# qual <- qual[!is.infinite(qual$val), ]
# ggplot(qual, aes(x = val, fill = factor(type))) +
#   geom_density() + theme_bw() +
#   facet_wrap(~ tbl, scales = "free")
# 
# 
# # how are the categories related?
# sum(tbls[["non-indig"]][, -c(1, 2)]) +
# sum(tbls[["aborigin"]][, -c(1, 2)]) +
# sum(tbls[["torres"]][, -c(1, 2)]) +
# sum(tbls[["both"]][, -c(1, 2)]) +
#   sum(tbls[["visitor"]][, -c(1, 2)]) +
#   sum(tbls[["unknown"]][, -c(1, 2)]) -
# sum(tbls[["total"]][, -c(1, 2)])