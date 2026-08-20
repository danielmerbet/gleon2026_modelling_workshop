# =============================================================================
# utils.R
# Shared helper functions used across workshop scripts.
# Source this file at the top of each script: source("R/utils.R")
# =============================================================================

library(tidyverse)
library(viridis)

# -----------------------------------------------------------------------------
# Theme
# -----------------------------------------------------------------------------

theme_workshop <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(colour = "grey40", size = base_size - 1),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold"),
      legend.position  = "right",
      plot.caption     = element_text(colour = "grey60", size = base_size - 3, hjust = 0)
    )
}

# Month factor with correct ordering
month_factor <- function(x) {
  factor(x, levels = 1:12, labels = month.abb)
}

# -----------------------------------------------------------------------------
# Depth-time heatmap (the workhorse visualisation for this dataset)
# Inputs:
#   df       — long tibble with columns: month, depth_m, <var>
#   var      — string name of the variable column
#   title    — plot title
#   subtitle — plot subtitle
#   units    — axis label units string
#   palette  — viridis option: "viridis", "magma", "plasma", "inferno", "mako"
#   reverse  — logical, reverse colour direction
# -----------------------------------------------------------------------------

plot_heatmap <- function(df, var, title, subtitle = NULL,
                         units = "", palette = "viridis", reverse = FALSE) {
  df |>
    mutate(month_f = month_factor(month)) |>
    ggplot(aes(x = month_f, y = depth_m, fill = .data[[var]])) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_y_reverse(name = "Depth (m)") +
    scale_x_discrete(name = NULL) +
    scale_fill_viridis_c(
      option  = palette,
      direction = if (reverse) -1 else 1,
      name    = units,
      na.value = "grey85"
    ) +
    labs(title = title, subtitle = subtitle,
         caption = "Source: ILEC World Lake Database EUR-56 (1977–1990)") +
    theme_workshop()
}

# -----------------------------------------------------------------------------
# Depth profile plot — one line per month
# -----------------------------------------------------------------------------

plot_profile <- function(df, var, title, subtitle = NULL, units = "",
                         months_highlight = c(2, 6, 8, 10)) {
  df |>
    mutate(month_f = month_factor(month),
           alpha_v = if_else(month %in% months_highlight, 1, 0.25),
           lwd_v   = if_else(month %in% months_highlight, 0.9, 0.4)) |>
    ggplot(aes(x = .data[[var]], y = depth_m,
               group = month_f, colour = month_f,
               alpha = I(alpha_v), linewidth = I(lwd_v))) +
    geom_line() +
    geom_point(data = ~ filter(., month %in% months_highlight), size = 2) +
    scale_y_reverse(name = "Depth (m)") +
    scale_x_continuous(name = units) +
    scale_colour_viridis_d(option = "turbo", name = "Month") +
    labs(title = title, subtitle = subtitle,
         caption = "Source: ILEC World Lake Database EUR-56") +
    theme_workshop()
}

# -----------------------------------------------------------------------------
# Model evaluation metrics: RMSE and R²
# -----------------------------------------------------------------------------

rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

r_squared <- function(obs, pred) {
  ss_res <- sum((obs - pred)^2, na.rm = TRUE)
  ss_tot <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

eval_metrics <- function(obs, pred, label = "model") {
  tibble(
    model  = label,
    n      = sum(!is.na(obs) & !is.na(pred)),
    RMSE   = round(rmse(obs, pred), 3),
    R2     = round(r_squared(obs, pred), 3),
    bias   = round(mean(pred - obs, na.rm = TRUE), 3)
  )
}