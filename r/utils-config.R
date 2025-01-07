
proj_root <- function() {
  rprojroot::find_root(rprojroot::has_file(".gitignore"), path = ".")
}

json_file <- function(name, dir, value = NULL, simplifyVector = TRUE,
                      simplifyDataFrame = FALSE, simplifyMatrix = FALSE, null = "null", ...) {
  
  assert_that(dir.exists(dir))
  
  file <- paste0(file.path(dir, name), ".json")
  
  if (!is.null(value)) {
    
    assert_that(is.list(value))
    jsonlite::write_json(value, file, ...)
    
  } else {
    
    if (!file.exists(file)) {
      stop("config file ", basename(file), " does not exists.")
    }
    
    jsonlite::read_json(file, simplifyVector = simplifyVector,
                        simplifyDataFrame = simplifyDataFrame,
                        simplifyMatrix = simplifyMatrix, ...)
  }
}

config <- function(name, value = NULL, ...) {
  json_file(name, file.path(proj_root(), "config"), value, ...)
}
