# =============================================================================
# Process Model Playground — GLEON Banyoles workshop
#
# An interactive Shiny app: drag sliders to tune the process-model parameters
# and watch the fit to a real ACA campaign update live. If the trained ML
# models are present, the hybrid (process + random-forest) prediction is
# overlaid too.
#
# Run from the project root:
#   shiny::runApp("app/")
# or directly in RStudio
# =============================================================================

library(shiny)
library(tidyverse)
library(zoo)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("../model.R")

# --- data: ACA depth-resolved campaigns + ERA5 forcing -----------------------
load_campaigns <- function() {
  raw <- read_csv("../data/data.csv",
                  locale = locale(encoding = "latin1"),
                  col_types = cols(.default = "c"), show_col_types = FALSE)
  names(raw) <- c("station_code","station_name","site_code","site_name",
                  "utm_x","utm_y","variable","timestamp","value","units",
                  "depth_m","comments")
  prof <- raw |>
    mutate(date = as.Date(as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M:%S")),
           value = suppressWarnings(as.numeric(value)),
           depth_m = suppressWarnings(as.numeric(depth_m)),
           var_en = case_when(
             str_detect(variable, "Temperatura") ~ "temp_C",
             str_detect(variable, "Oxigen")      ~ "DO_mgl",
             TRUE ~ NA_character_)) |>
    filter(!is.na(date), !is.na(value), !is.na(depth_m), !is.na(var_en)) |>
    summarise(value = mean(value), .by = c(date, depth_m, var_en)) |>
    pivot_wider(names_from = var_en, values_from = value) |>
    arrange(date, depth_m)

  era5 <- read_csv("../data/era5_banyoles.csv",
                   show_col_types = FALSE) |>
    arrange(date) |>
    mutate(air_temp_14d = rollmeanr(air_temp_C, k = 14, fill = air_temp_C))

  prof |>
    left_join(era5, by = "date") |>
    mutate(doy = yday(date), month = month(date),
           stratified = as.integer(month %in% 5:10),
           rel_depth = depth_m / 50, depth_x_summer = rel_depth * stratified,
           sin_doy = sin(2*pi*doy/365), cos_doy = cos(2*pi*doy/365)) |>
    filter(depth_m > 0)                     # drop surface-only rows
}

campaigns <- load_campaigns()
campaign_dates <- campaigns |>
  filter(!is.na(temp_C)) |> distinct(date) |> arrange(date) |> pull(date)

# --- optional: trained ML models for the hybrid overlay ----------------------
ml <- readRDS("../data/ml_models.rds")

dp <- default_params()

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  titlePanel("Lake Banyoles — Process Model Playground"),
  tags$p("Tune the physics. Watch the fit. See what the machine-learning step is left to explain."),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("campaign", "Campaign", choices = as.character(campaign_dates),
                  selected = as.character(campaign_dates[length(campaign_dates)])),
      checkboxInput("show_hybrid",
                    if (is.null(ml)) "Hybrid overlay (run Parts 2–3 first)" else "Show hybrid (process + ML)",
                    value = !is.null(ml)),
      hr(),
      strong("Surface heat"),
      sliderInput("k_heat", "Air–water coupling (k_heat)", 0.5, 1.0, dp$k_heat, 0.05),
      sliderInput("solar_offset", "Summer solar warming (°C)", 0, 6, dp$solar_offset, 0.5),
      strong("Stratification"),
      sliderInput("mld_summer", "Summer mixed-layer depth (m)", 2, 12, dp$mld_summer, 1),
      sliderInput("therm_scale", "Thermocline sharpness (m)", 2, 12, dp$therm_scale, 0.5),
      sliderInput("T_deepwater", "Deep-water temperature (°C)", 8, 16, dp$T_deepwater, 0.5),
      strong("Karstic groundwater"),
      sliderInput("T_gw", "Groundwater temperature (°C)", 11, 20, dp$T_gw, 0.5),
      strong("Dissolved oxygen"),
      sliderInput("do_halfdepth", "O2 half-depth (m)", 8, 35, dp$do_halfdepth, 1),
      sliderInput("do_slope", "O2 decline steepness (m)", 2, 12, dp$do_slope, 0.5),
      hr(),
      actionButton("reset", "Reset to calibrated defaults")
    ),
    mainPanel(
      width = 8,
      fluidRow(
        column(6, wellPanel(strong("Temperature RMSE"), textOutput("rmse_temp"))),
        column(6, wellPanel(strong("Dissolved-oxygen RMSE"), textOutput("rmse_do")))
      ),
      plotOutput("profiles", height = "560px"),
      tags$small(textOutput("hint"))
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateSliderInput(session, "k_heat", value = dp$k_heat)
    updateSliderInput(session, "solar_offset", value = dp$solar_offset)
    updateSliderInput(session, "mld_summer", value = dp$mld_summer)
    updateSliderInput(session, "therm_scale", value = dp$therm_scale)
    updateSliderInput(session, "T_deepwater", value = dp$T_deepwater)
    updateSliderInput(session, "T_gw", value = dp$T_gw)
    updateSliderInput(session, "do_halfdepth", value = dp$do_halfdepth)
    updateSliderInput(session, "do_slope", value = dp$do_slope)
  })

  cur_params <- reactive({
    p <- dp
    p$k_heat       <- input$k_heat
    p$solar_offset <- input$solar_offset
    p$mld_summer   <- input$mld_summer
    p$therm_scale  <- input$therm_scale
    p$T_deepwater  <- input$T_deepwater
    p$T_gw         <- input$T_gw
    p$do_halfdepth <- input$do_halfdepth
    p$do_slope     <- input$do_slope
    p
  })

  # observed profile for the chosen campaign
  obs <- reactive({
    campaigns |> filter(date == as.Date(input$campaign))
  })

  # model prediction on a smooth 0–50 m grid (for the fitted lines)
  grid_pred <- reactive({
    d <- obs()[1, ]
    tibble(depth_m = 0:50) |>
      mutate(date = d$date, doy = d$doy, month = d$month,
             air_temp_14d = d$air_temp_14d,
             radiation_MJ_m2 = d$radiation_MJ_m2,
             wind_ms = d$wind_ms,
             stratified = d$stratified,
             rel_depth = depth_m / 50,
             depth_x_summer = rel_depth * stratified,
             sin_doy = sin(2*pi*doy/365), cos_doy = cos(2*pi*doy/365)) |>
      run_process_model(cur_params()) |>
      add_hybrid()
  })

  # append hybrid columns if ML models available
  add_hybrid <- function(df) {
    if (is.null(ml) || !isTRUE(input$show_hybrid)) {
      df$T_hybrid <- NA_real_; df$DO_hybrid <- NA_real_; return(df)
    }
    F <- ml$features
    df |> mutate(
      T_hybrid  = T_pred + predict(ml$rf_temp, newdata = pick(all_of(F))),
      DO_hybrid = pmax(0, DO_pred + predict(ml$rf_do, newdata = pick(all_of(F))))
    )
  }

  # model prediction AT the observed depths (for honest RMSE)
  at_obs <- reactive({
    o <- obs()
    o |> run_process_model(cur_params()) |> add_hybrid()
  })

  output$rmse_temp <- renderText({
    a <- at_obs() |> filter(!is.na(temp_C))
    proc <- sqrt(mean((a$temp_C - a$T_pred)^2))
    txt <- sprintf("process: %.2f °C", proc)
    if (!is.null(ml) && isTRUE(input$show_hybrid)) {
      hy <- sqrt(mean((a$temp_C - a$T_hybrid)^2))
      txt <- sprintf("%s   →   hybrid: %.2f °C", txt, hy)
    }
    txt
  })

  output$rmse_do <- renderText({
    a <- at_obs() |> filter(!is.na(DO_mgl))
    proc <- sqrt(mean((a$DO_mgl - a$DO_pred)^2))
    txt <- sprintf("process: %.2f mg/L", proc)
    if (!is.null(ml) && isTRUE(input$show_hybrid)) {
      hy <- sqrt(mean((a$DO_mgl - a$DO_hybrid)^2))
      txt <- sprintf("%s   →   hybrid: %.2f mg/L", txt, hy)
    }
    txt
  })

  output$profiles <- renderPlot({
    o <- obs(); g <- grid_pred()

    long_obs <- bind_rows(
      o |> transmute(depth_m, value = temp_C, panel = "Temperature (°C)"),
      o |> transmute(depth_m, value = DO_mgl, panel = "Dissolved oxygen (mg/L)")
    )
    long_proc <- bind_rows(
      g |> transmute(depth_m, value = T_pred,  panel = "Temperature (°C)"),
      g |> transmute(depth_m, value = DO_pred, panel = "Dissolved oxygen (mg/L)")
    )
    long_hy <- bind_rows(
      g |> transmute(depth_m, value = T_hybrid,  panel = "Temperature (°C)"),
      g |> transmute(depth_m, value = DO_hybrid, panel = "Dissolved oxygen (mg/L)")
    ) |> filter(!is.na(value))

    ggplot() +
      geom_path(data = long_proc, aes(value, depth_m, colour = "Process"),
                linewidth = 1.2) +
      { if (nrow(long_hy) > 0)
          geom_path(data = long_hy, aes(value, depth_m, colour = "Hybrid"),
                    linewidth = 1.2, linetype = "dashed") } +
      geom_point(data = long_obs, aes(value, depth_m, colour = "Observed"),
                 size = 2.4, alpha = 0.85) +
      scale_y_reverse("Depth (m)", breaks = seq(0, 50, 10)) +
      scale_x_continuous(NULL) +
      scale_colour_manual(NULL,
        values = c("Observed" = "black", "Process" = "#2166ac",
                   "Hybrid" = "#1a9850")) +
      facet_wrap(~panel, scales = "free_x") +
      labs(title = paste("Campaign:", input$campaign)) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top",
            strip.text = element_text(face = "bold"),
            panel.grid.minor = element_blank())
  })

  output$hint <- renderText({
    if (is.null(ml)) {
      "Tip: the gap between the blue line and the black points is the residual. Train the ML models (Parts 2–3) to overlay the hybrid fit."
    } else {
      "The dashed green line is the hybrid: ML has learned the structured residual (karstic warming, anoxia) the physics leaves behind."
    }
  })
}

shinyApp(ui, server)
