# Build the URL of one NHANES XPT file
build_nhanes_url <- function(start_year, file) {
  paste0(
    "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",
    start_year, "/DataFiles/", file, ".xpt"
  )
}

# Download one file into dest_dir, skipping it if missing in the manifest or already there
download_nhanes_file <- function(start_year, file, dest_dir) {
  if (is.na(file) || file == "") return(invisible(NULL))
  
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  destfile <- file.path(dest_dir, paste0(file, ".xpt"))
  if (file.exists(destfile)) return(invisible(destfile))
  
  download.file(build_nhanes_url(start_year, file), destfile, mode = "wb")
  invisible(destfile)
}