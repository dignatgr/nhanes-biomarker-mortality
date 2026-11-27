# Build the URL of one NHANES XPT file
build_nhanes_url <- function(start_year, file) {
  paste0(
    "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",
    start_year, "/DataFiles/", file, ".xpt"
  )
}

# Build the URL of one NHANES dat file
build_nchs_nhanes_url <- function(file) {
  paste0(
    "https://ftp.cdc.gov/pub/health_statistics/NCHs/datalinkage/linked_mortality/", file
  )
}

# Download one file into dest_dir, skipping it if missing in the manifest or already there
download_nhanes_file <- function(start_year, file, dest_dir, type = c("nhanes", "nchs")) {
  type <- match.arg(type)
  if (is.na(file) || file == "") return(invisible(NULL))
  
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  url <- switch(type,
                nhanes = build_nhanes_url(start_year, file),
                nchs   = build_nchs_nhanes_url(file)
  )
  
  destfile <- file.path(dest_dir, basename(url))
  if (file.exists(destfile)) return(invisible(destfile))
  download.file(url, destfile, mode = "wb")
  invisible(destfile)
}
