
library(readxl)
library(data.table)
library(ricu)
library(fst)

# get the data location in the ricu data directory
folder <- file.path(data_dir(), "anzics")

# load the csv file with the ANZICS APD data
anzics <- read.csv(file.path(folder, "apd-export.csv"), 
                   na.strings = c("NA", "NULL"))

# manually create the ICU Stay ID column
anzics <- cbind(anzics, ICUStayID = seq_len(nrow(anzics)))

# fix the time columns data type
fix_cols <- c("ICU_AD_DTM", "ICU_DS_DTM", "HOSP_AD_DTM", "HOSP_DS_DTM")
for (col in fix_cols) {
  anzics[[col]] <- as.POSIXct(gsub("\\.000", "", anzics[[col]]))
}

# write the main table to ricu-readable data format (fst)
write_fst(anzics, path = file.path(data_dir(), "anzics", "main.fst"))

# load the diagnoses information summary from this repository
d_diag <- read.csv(file.path("data", "d_diagnoses.csv"))

# write the diagnoses table to ricu-readable data format (fst)
write_fst(d_diag, path = file.path(data_dir(), "anzics", "d_diagnoses.fst"))

# load the SEIFA information across postal areas (POA)
poa_seifa <- read_excel(file.path("data", "abs-data", "poa-seifa.xlsx"), 
                        sheet = 2, skip = 5)
poa_seifa <- poa_seifa[seq_len(nrow(poa_seifa)-2), ]
poa_seifa <- as.data.table(poa_seifa)
names(poa_seifa) <- c(
  "postcode", "irsd", "irsd_decile", "irsad", "irsad_decile",
  "ier", "ier_decile", "ieo", "ieo_decile", "resident_population",
  "caution_indicator", "postcode_crosses_state"
)

# fix the column types for the SEIFA by POA table
val_cols <- setdiff(
  names(poa_seifa), c("postcode", "resident_population", "caution_indicator", 
                      "postcode_crosses_state")
)
for (col in val_cols) {
  
  poa_seifa[get(col) == "-", c(col) := NA]
  poa_seifa[, c(col) := as.numeric(get(col))]
}

# write the SEIFA by POA table to ricu-readable data format (fst)
write_fst(poa_seifa, path = file.path(data_dir(), "anzics", "poa_seifa.fst"))
