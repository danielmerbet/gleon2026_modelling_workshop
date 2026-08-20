# =============================================================================
# 01c_prepare_data.R
# Consolidate the new Banyoles  temperature profiles
# into a single tidy CSV.
#
# Source: meteolestartit.cat — temperature soundings ("sondatges") collected
#         and shared by Josep Pascual. Many thanks to Josep for sharing this
#         data with the workshop: https://meteolestartit.cat/
#
# Input:  data/new_data.xlsx
#         One sheet per campaign (sheet name = date, "YYMMDD", with an
#         optional trailing letter for a same-day repeat visit, e.g. "250429b").
#         Each sheet has a fixed depth grid (-0.5 to -100 m) and one
#         temperature column per sampling site (A, B, C, D — or D1/D2 on the
#         one campaign that split site D).
#
# Output: data/validate_temperature.csv
#   date        — campaign date (YYYY-MM-DD)
#   campaign_id — original sheet name (preserves same-day repeat visits)
#   site        — sampling point (A, B, C, D, D1, D2)
#   depth_m     — depth below surface, positive metres
#   temp_C      — water temperature (°C)
# =============================================================================

library(readxl)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

xlsx_path <- "data/new_data.xlsx"
sheets    <- excel_sheets(xlsx_path)

# Campaign sheets are named YYMMDD (+ optional letter suffix, e.g. "250429b").
# This excludes the unrelated "nivell llot" (mud level) sheet.
campaign_sheets <- sheets[str_detect(sheets, "^\\d{6}[a-z]?$")]

parse_campaign <- function(sheet_name) {
  raw <- suppressMessages(read_excel(xlsx_path, sheet = sheet_name, col_names = FALSE))

  # Site columns are always in the 4 columns right after the depth column
  # (labelled A, B, C, D — or A, B, D1, D2 on the one split-D campaign).
  sites <- as.character(unlist(raw[2, 2:5]))

  body <- raw[-(1:2), 1:5]
  names(body) <- c("depth_m", sites)

  date_str <- str_sub(sheet_name, 1, 6)
  date     <- as.Date(date_str, format = "%y%m%d")

  body |>
    mutate(depth_m = suppressWarnings(as.numeric(depth_m))) |>
    filter(!is.na(depth_m)) |>
    pivot_longer(-depth_m, names_to = "site", values_to = "temp_C") |>
    mutate(
      temp_C  = round(suppressWarnings(as.numeric(temp_C)), 2),
      depth_m = abs(depth_m),
      date    = date,
      campaign_id = sheet_name
    ) |>
    filter(!is.na(temp_C)) |>
    select(date, campaign_id, site, depth_m, temp_C)
}

validate_temperature <- map(campaign_sheets, parse_campaign) |>
  list_rbind() |>
  arrange(date, campaign_id, site, depth_m)

write_csv(validate_temperature, "data/validate_temperature.csv")

cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("  Banyoles temperature — meteolestartit.cat\n")
cat("  Data courtesy of Josep Pascual (https://meteolestartit.cat/)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("Campaigns:", n_distinct(validate_temperature$campaign_id), "\n")
cat("Date range:", format(min(validate_temperature$date)), "to",
    format(max(validate_temperature$date)), "\n")
cat("Sites:", paste(sort(unique(validate_temperature$site)), collapse = ", "), "\n")
cat("Rows written: ", nrow(validate_temperature), "-> data/validate_temperature.csv\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

message("\nData saved. Object: validate_temperature")
