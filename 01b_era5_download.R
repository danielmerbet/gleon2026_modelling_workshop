# =============================================================================
# 01b_era5_download.R
# Download ERA5 daily meteorological data for Lake Banyoles from Open-Meteo.
#
# Source: Open-Meteo Historical Weather API (ERA5 reanalysis, free, no key)
#   https://open-meteo.com/en/docs/historical-weather-api
# Location: Lake Banyoles monitoring station — 42.1209°N, 2.7582°E
# Period:   2007-01-01 → 2026-04-30
# Variables downloaded (all daily):
#   temperature_2m_mean        [°C]       — primary process-model forcing
#   temperature_2m_max         [°C]       — daytime peak (swimmer comfort proxy)
#   precipitation_sum          [mm]       — runoff / nutrient loading context
#   windspeed_10m_mean         [m/s]      — wind mixing (mixed layer depth)
#   shortwave_radiation_sum    [MJ/m²]    — heat input to surface layer
#
# Output: data/era5_banyoles.csv
# =============================================================================

library(httr2)
library(jsonlite)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

LAT        <- 42.1209
LON        <- 2.7582
START_DATE <- "2007-01-01"
END_DATE   <- "2026-04-30"

DAILY_VARS <- paste(c(
  "temperature_2m_mean",
  "temperature_2m_max",
  "precipitation_sum",
  "windspeed_10m_mean",
  "shortwave_radiation_sum"
), collapse = ",")

# ─────────────────────────────────────────────────────────────────────────────
# Request
# ─────────────────────────────────────────────────────────────────────────────

message("Requesting ERA5 data from Open-Meteo...")
message("  Location: ", LAT, "°N, ", LON, "°E")
message("  Period:   ", START_DATE, " → ", END_DATE)

resp <- request("https://archive-api.open-meteo.com/v1/archive") |>
  req_url_query(
    latitude   = LAT,
    longitude  = LON,
    start_date = START_DATE,
    end_date   = END_DATE,
    daily      = DAILY_VARS,
    timezone   = "Europe/Madrid",
    models = "era5"
  ) |>
  req_timeout(120) |>
  req_perform()

if (resp_status(resp) != 200) {
  stop("API request failed with status: ", resp_status(resp))
}

# ─────────────────────────────────────────────────────────────────────────────
# Parse response
# ─────────────────────────────────────────────────────────────────────────────

raw  <- resp_body_string(resp)
data <- fromJSON(raw)

era5 <- tibble(
  date              = as.Date(data$daily$time),
  air_temp_C        = data$daily$temperature_2m_mean,
  air_temp_max_C    = data$daily$temperature_2m_max,
  precip_mm         = data$daily$precipitation_sum,
  wind_ms           = data$daily$windspeed_10m_mean,
  radiation_MJ_m2   = data$daily$shortwave_radiation_sum
)

message("Downloaded ", nrow(era5), " days of ERA5 data.")
message("Date range: ", min(era5$date), " to ", max(era5$date))
message("Missing values: ", sum(is.na(era5$air_temp_C)), " in air_temp_C")

# ─────────────────────────────────────────────────────────────────────────────
# Quick sanity check — monthly means
# ─────────────────────────────────────────────────────────────────────────────

monthly_check <- era5 |>
  mutate(month = month(date)) |>
  group_by(month) |>
  summarise(
    air_temp_C      = round(mean(air_temp_C,      na.rm = TRUE), 1),
    precip_mm       = round(mean(precip_mm,        na.rm = TRUE), 1),
    wind_ms         = round(mean(wind_ms,          na.rm = TRUE), 1),
    radiation_MJ_m2 = round(mean(radiation_MJ_m2, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n--- ERA5 monthly climatology (2007–2026) ---\n")
print(monthly_check)

# ─────────────────────────────────────────────────────────────────────────────
# Save
# ─────────────────────────────────────────────────────────────────────────────
write_csv(era5, "data/era5_banyoles.csv")
message("\nSaved: data/era5_banyoles.csv")