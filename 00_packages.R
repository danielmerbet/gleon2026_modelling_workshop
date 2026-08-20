# =============================================================================
# 00_packages.R
# Install (if needed) and load all packages for the workshop.
# Run this first to verify your setup before the session.
# =============================================================================

required_packages <- c(
  "tidyverse",
  "lubridate",
  "caret",
  "randomForest",
  "reshape2",
  "viridis",
  "patchwork",
  "scales",
  "broom",
  "zoo",       # rolling means for ERA5 forcing
  "httr2",     # Open-Meteo API calls
  "jsonlite",  # parse API JSON responses
  "here",      # robust cross-platform file paths
  "shiny",     # interactive process-model playground (app/)
  "readxl"     # read the meteolestartit.cat Vilar temperature workbook
)

# Install any missing packages
missing <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

# Load all
invisible(lapply(required_packages, library, character.only = TRUE))

message("All packages loaded successfully. R version: ", R.version$major, ".", R.version$minor)
