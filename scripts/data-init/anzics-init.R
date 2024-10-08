
library(readxl)
library(data.table)

# get the data location
folder <- file.path(ricu::data_dir(), "raw", "anzics")

#' * (1) construct the ANZICS main table * 
anzics <- read.csv(file.path(folder, "2112.csv"), na.strings = c("NA", "NULL"))

# create the ICU Stay ID column
anzics <- cbind(anzics, ICUStayID = seq_len(nrow(anzics)))

# fix the time columns
fix_cols <- c("ICU_AD_DTM", "ICU_DS_DTM", "HOSP_AD_DTM", "HOSP_DS_DTM")
for (col in fix_cols) {
  anzics[[col]] <- as.POSIXct(gsub("\\.000", "", anzics[[col]]))
}

#' * (2) construct the d_diagnoses table *
d_diag <- read.csv(file.path(folder, "d_diagnoses.csv"))
fst::write_fst(d_diag, path = file.path(data_dir(), "anzics", "d_diagnoses.fst"))

#' * (3) construct the SEIFA table for POA *
poa_seifa <- read_excel(file.path(folder, "poa-seifa-2021.xlsx"), 
                        sheet = 2, skip = 5)
poa_seifa <- poa_seifa[seq_len(nrow(poa_seifa)-2), ]
poa_seifa <- as.data.table(poa_seifa)
names(poa_seifa) <- c(
  "POA", "irsd", "irsd_decile", "irsad", "irsad_decile",
  "ier", "ier_decile", "ieo", "ieo_decile", "resident_population",
  "caution_indicator", "POA_crosses_state"
)
# fix the column types
val_cols <- setdiff(
  names(poa_seifa), c("POA", "resident_population", "caution_indicator", 
                      "POA_crosses_state")
)
for (col in val_cols) {
  
  poa_seifa[get(col) == "-", c(col) := NA]
  poa_seifa[, c(col) := as.numeric(get(col))]
}
fst::write_fst(poa_seifa, path = file.path(data_dir(), "anzics", "poa_seifa.fst"))

# add the POA column at random (for now)
anzics$POA <- sample(poa_seifa$POA, nrow(anzics), replace = TRUE)
fst::write_fst(anzics, path = file.path(data_dir(), "anzics", "main.fst"))
