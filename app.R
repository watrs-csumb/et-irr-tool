# OpenET wETgraph Shiny app
# Run from this folder with: shiny::runApp()

library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(leaflet)
library(readxl)
library(xml2)
library(openxlsx)
library(shinyFeedback)
library(shinycssloaders)

source("R/openet_utils.R")

models <- c("GEESEBAL", "SSEBOP", "SIMS", "DISALEXI", "PTJPL", "EEMETRIC", "ENSEMBLE")
default_start <- as.Date(format(Sys.Date(), "%Y-01-01"))
default_end <- Sys.Date()

default_setup <- list(
  field_id = "Example",
  api_key = "",
  application_rate_in_hr = 0.1156,
  latitude = 36.1035,
  longitude = -120.10768,
  crop_description = "Almonds",
  field_capacity_in = 4.6,
  initial_water_content_in = 3.5,
  allowable_dryness_in = 3.5,
  permanent_wilting_point_in = 2.4,
  start_date = default_start,
  end_date = default_end,
  selected_model = "ENSEMBLE",
  download_all_models = FALSE,
  count_precip_effective = FALSE
)

plotly_date_layout <- function(p) {
  p %>%
    layout(
      hovermode = "x unified",
      xaxis = list(
        type = "date",
        autorange = TRUE,
        tickmode = "auto",
        title = "",
        hoverformat = "%b %d, %Y",
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        tickformatstops = list(
          list(dtickrange = list(NULL, 86400000), value = "%b %d"),
          list(dtickrange = list(86400000, 604800000), value = "%b %d"),
          list(dtickrange = list(604800000, "M1"), value = "%b %d"),
          list(dtickrange = list("M1", "M12"), value = "%b %Y"),
          list(dtickrange = list("M12", NULL), value = "%Y")
        )
      ),
      yaxis = list(
        tickmode = "auto", nticks = 10,
        showspikes = TRUE, spikemode = "across", spikesnap = "cursor"
      )
    )
}


clean_plotly_hover <- function(p) {
  format_trace_dates <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    # ggplotly often stores Date values as numeric days since 1970-01-01.
    # Plotly hover date formatting treats numbers as milliseconds, which causes
    # the Dec 31, 1969 tooltip bug. Build the display date ourselves instead.
    if (inherits(x, "Date")) {
      d <- x
    } else if (inherits(x, "POSIXt")) {
      d <- as.Date(x)
    } else if (is.numeric(x)) {
      d <- ifelse(abs(x) > 100000,
        as.character(as.Date(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC"))),
        as.character(as.Date(x, origin = "1970-01-01"))
      )
      d <- as.Date(d)
    } else {
      d <- suppressWarnings(as.Date(x))
    }

    if (all(is.na(d))) {
      return(NULL)
    }
    format(d, "%b %d, %Y")
  }

  date_shown <- FALSE
  for (i in seq_along(p$x$data)) {
    # ggplotly stores R Date as integer days since 1970-01-01, but plotly.js
    # expects milliseconds. Multiply by 86400000 to fix the "Dec 31, 1969" bug.
    x_raw <- p$x$data[[i]]$x
    if (is.numeric(x_raw) && length(x_raw) > 0) {
      finite_vals <- x_raw[is.finite(x_raw)]
      if (length(finite_vals) > 0 && all(abs(finite_vals) < 1e5)) {
        p$x$data[[i]]$x <- x_raw * 86400000
      }
    }

    # ribbon traces: ggplotly always sets showlegend=FALSE on geom_ribbon.
    # Re-enable showlegend with a proper label; show name in hover but no value.
    if (isFALSE(p$x$data[[i]]$showlegend) && identical(p$x$data[[i]]$fill, "toself")) {
      p$x$data[[i]]$name <- "Undesired Depletion Zone"
      p$x$data[[i]]$showlegend <- TRUE
      p$x$data[[i]]$hovertemplate <- "Undesired Depletion Zone<extra></extra>"
      next
    }
    # skip any other traces explicitly hidden (none currently, kept for safety)
    if (isFALSE(p$x$data[[i]]$showlegend)) {
      p$x$data[[i]]$hoverinfo <- "skip"
      p$x$data[[i]]$hovertemplate <- "<extra></extra>"
      next
    }
    trace_name <- p$x$data[[i]]$name
    if (is.null(trace_name) || is.na(trace_name) || !nzchar(trace_name)) {
      trace_name <- "Value"
    }
    trace_name <- gsub("<[^>]+>", "", trace_name)
    trace_name <- trimws(trace_name)
    trace_name <- gsub(",\\s*1$", "", trace_name)
    trace_name <- gsub("\\(([^)]+),\\s*1\\)", "(\\1)", trace_name)
    trace_name <- gsub("^\\((.+)\\)$", "\\1", trace_name)
    p$x$data[[i]]$name <- trace_name

    date_labels <- format_trace_dates(p$x$data[[i]]$x)
    if (!date_shown && !is.null(date_labels)) {
      p$x$data[[i]]$customdata <- date_labels
      date_line <- "Date: %{customdata}<br>"
      date_shown <- TRUE
    } else if (!date_shown) {
      date_line <- "Date: %{x}<br>"
      date_shown <- TRUE
    } else {
      date_line <- ""
    }

    p$x$data[[i]]$hovertemplate <- paste0(date_line, trace_name, ": %{y:.2f}<extra></extra>")
  }
  p
}

app_css <- "
body { background-color: #f5f7fb; }
.wet-card { background: #fff; border-radius: 14px; padding: 16px; margin-bottom: 16px; box-shadow: 0 2px 12px rgba(25, 42, 70, .08); }
.metric { background: #fff; border-radius: 14px; padding: 14px 16px; box-shadow: 0 2px 12px rgba(25, 42, 70, .08); min-height: 94px; }
.metric .label { color: #667085; font-size: 12px; text-transform: uppercase; letter-spacing: .05em; }
.metric .value { font-size: 28px; font-weight: 700; margin-top: 5px; }
.warn-box { background: #fff7e6; border: 1px solid #ffd591; border-radius: 10px; padding: 10px 12px; margin-bottom: 14px; }
.red-box { background: #fde8e8; border: 1px solid #f5a0a0; border-radius: 10px; padding: 10px 12px; margin-bottom: 14px; color: #7b1212; font-weight: 500; }
.ok-box { background: #ecfdf3; border: 1px solid #abefc6; border-radius: 10px; padding: 10px 12px; margin-bottom: 14px; }
.help-text { color: #667085; font-size: 13px; }
.well { background-color: #C5D178 !important; border: none !important; box-shadow: none !important; }
.well label, .well h4, .well h5, .well p, .well .help-block, .well .control-label { color: #0d1f33 !important; }
.well input[type='text'], .well input[type='password'], .well input[type='number'], .well select, .well .form-control { background-color: #f0f4d8; color: #0d1f33; border: 1px solid #a8b855; }
.well input[type='text']::placeholder, .well input[type='password']::placeholder { color: #7a8a50; }
.well hr { border-color: #a8b855; }
.well details > summary { cursor: pointer; font-size: 15px; padding: 4px 0; user-select: none; color: #0d1f33; }
.well details > summary:hover { color: #2e86c1; }
.well details { margin-bottom: 2px; }
.well .btn-primary { background-color: #2e86c1; border-color: #2471a3; }
.well .btn-info { background-color: #17a589; border-color: #148a72; }
@media (max-width: 767px) {
  .sidebar { margin-bottom: 16px; }
  .wet-card { padding: 10px; }
  .metric { min-height: auto; padding: 10px 12px; }
  .metric .value { font-size: 22px; }
  .navbar-fixed-top { position: relative; }
}
.shiny-notification { border-radius: 10px; min-width: 300px; max-width: 380px; right: 16px !important; bottom: 16px !important; }
.shiny-notification.shiny-notification-warning { background-color: #c0392b !important; color: #fff !important; border: none !important; }
.shiny-notification.shiny-notification-warning .shiny-notification-close { color: #fff !important; }
@media print {
  .well, .navbar, .navbar-fixed-top, #show_help, .shiny-notification,
  .nav.nav-tabs, button, .btn, .downloadButton, input, select, .selectize-control,
  details { display: none !important; }
  .col-sm-8, .col-sm-4 { width: 100% !important; float: none !important; }
  .wet-card { box-shadow: none !important; border: 1px solid #ddd !important; }
  body { background: #fff !important; }
  #summary_print_header { display: block !important; }
}
#summary_print_header { display: none; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  useShinyFeedback(),
  tags$div(
    style = "position: fixed; top: 12px; right: 18px; z-index: 9999;",
    actionButton("show_help",
      label = "?", title = "How to use this app",
      style = "border-radius: 50%; width: 34px; height: 34px; padding: 0;
               font-weight: 700; font-size: 16px;
               background-color: #2e86c1; color: #fff; border: none;
               box-shadow: 0 2px 6px rgba(0,0,0,0.2); cursor: pointer;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        style = "margin-bottom: 4px;",
        div(
          style = "display: flex; align-items: center; gap: 4px; margin-bottom: 4px;",
          tags$label("Fields", style = "font-weight: 600; font-size: 14px; color: #0d1f33; margin: 0; flex: 1;"),
          actionButton("add_field", tagList(tags$i(class = "fa fa-plus"), " Add field"), class = "btn-xs btn-primary", title = "Add new field", style = "padding: 2px 8px;"),
          actionButton("delete_field", tagList(tags$i(class = "fa fa-trash"), " Delete field"), class = "btn-xs btn-danger", title = "Remove current field", style = "padding: 2px 8px;")
        ),
        selectInput("active_field_key", NULL, choices = c("Example" = "field_1"), width = "100%")
      ),
      hr(),
      tags$details(
        open = NA,
        tags$summary(tags$b("Field Info")),
        br(),
        textInput("field_id", "Field ID", default_setup$field_id),
        textInput("crop", "Crop description", default_setup$crop_description),
        dateRangeInput("date_range", "Date range", start = default_setup$start_date, end = default_setup$end_date),
        fluidRow(
          column(6, numericInput("lat", "Latitude", default_setup$latitude, step = 0.0001)),
          column(6, numericInput("lon", "Longitude", default_setup$longitude, step = 0.0001))
        ),
        actionButton("pick_coords", tagList(tags$i(class = "fa fa-map-marker-alt"), " Pick from map"),
          class = "btn-primary btn-sm btn-block", style = "margin-top: -4px; margin-bottom: 6px;"
        )
      ),
      hr(),
      tags$details(
        tags$summary(tags$b("Soil Properties")),
        br(),
        numericInput("app_rate", "Net application rate, in/hr", default_setup$application_rate_in_hr, step = 0.0001),
        numericInput("initial_water", "Initial water content, in.", default_setup$initial_water_content_in, step = 0.1),
        numericInput("allowable_dryness", HTML("Allowable depletion, in."), default_setup$allowable_dryness_in, step = 0.1),
        numericInput("field_capacity", HTML("Field capacity*, in."), default_setup$field_capacity_in, step = 0.1),
        numericInput("pwp", HTML("Permanent wilting point*, in."), default_setup$permanent_wilting_point_in, step = 0.1),
        tags$p(tags$em("* Can be updated using SSURGO"), style = "font-size: 11px; color: #4a6a8a; margin-top: 6px; margin-bottom: 2px;")
      ),
      hr(),
      tags$details(
        tags$summary(tags$b("SSURGO")),
        br(),
        numericInput("root_depth_ft", "Root zone depth, ft", value = 2.0, min = 0.5, max = 20, step = 0.5),
        actionButton("fetch_ssurgo", "Fetch Soil from SSURGO", class = "btn-primary btn-sm btn-block"),
        br(), br(),
        uiOutput("ssurgo_status")
      ),
      hr(),
      tags$details(
        tags$summary(tags$b("OpenET")),
        br(),
        passwordInput("api_key", "OpenET API key", default_setup$api_key),
        selectInput("openet_model", "ET Model", choices = models, selected = default_setup$selected_model),
        checkboxInput("download_all_models", "Download all ET models", default_setup$download_all_models),
        actionButton("fetch_openet", "Update OpenET data", class = "btn-primary btn-sm btn-block"),
        br(), br(),
        verbatimTextOutput("openet_status")
      ),
      hr(),
      tags$details(
        tags$summary(tags$b("Session")),
        br(),
        p(class = "help-text", "Save your current session (setup, irrigation entries, and ET data) to a file and reload it later."),
        textInput("session_filename", "File name", "", placeholder = "auto-generated"),
        downloadButton("save_session", "Save Session", class = "btn-sm btn-default"),
        br(), br(),
        fileInput("load_session", "Load Session",
          accept = ".rds",
          placeholder = "Choose .rds file"
        ),
        uiOutput("session_status")
      )
    ),
    mainPanel(
      uiOutput("location_warning"),
      tabsetPanel(
        tabPanel(
          "Dashboard",
          br(),
          fluidRow(
            column(3, div(class = "metric", div(class = "label", "Total ETa, in."), div(class = "value", textOutput("m_eta")))),
            column(3, div(class = "metric", div(class = "label", "Applied Water, in."), div(class = "value", textOutput("m_applied")))),
            column(3, div(class = "metric", div(class = "label", "Precipitation, in."), div(class = "value", textOutput("m_precip")))),
            column(3, div(class = "metric", div(class = "label", "Deep Percolation, in."), div(class = "value", textOutput("m_deep"))))
          ),
          br(),
          div(class = "wet-card", h4(textOutput("soil_title")), withSpinner(plotlyOutput("soil_plot", height = 380), type = 6, color = "#2e86c1", size = 0.7)),
          div(
            class = "wet-card", h4(textOutput("eta_title")),
            withSpinner(plotlyOutput("eta_plot", height = 330), type = 6, color = "#2e86c1", size = 0.7)
          ),
          div(class = "wet-card", h4(textOutput("deep_title")), withSpinner(plotlyOutput("deep_plot", height = 330), type = 6, color = "#2e86c1", size = 0.7)),
          div(class = "wet-card", h4(textOutput("kc_title")), withSpinner(plotlyOutput("kc_plot", height = 330), type = 6, color = "#2e86c1", size = 0.7))
        ),
        tabPanel(
          "Irrigation Amounts",
          br(),
          div(
            class = "wet-card",
            p(class = "help-text", "When checked, the soil water balance credits OpenET precipitation in addition to entered irrigation: irrigation + precipitation - ETa."),
            checkboxInput("count_precip_effective", "Count all OpenET precipitation as effective water", value = default_setup$count_precip_effective),
            fluidRow(
              column(3, dateInput("new_irrig_date", "Date", value = default_start)),
              column(3, numericInput("new_irrig_in", "Net water applied, in.", value = 0, step = 0.01)),
              column(4, textInput("new_irrig_note", "Notes / flow meter", value = "")),
              column(2, br(), actionButton("add_irrig", "Add/update"))
            ),
            actionButton("clear_irrig", "Clear applied water"),
            br(), br(),
            DTOutput("irrig_table")
          )
        ),
        tabPanel(
          "Irrigation Explorer",
          br(),
          fluidRow(
            column(3, div(class = "metric", div(class = "label", "Current Soil Water"), div(class = "value", textOutput("sc_cur_swc")))),
            column(3, div(class = "metric", div(class = "label", "Available Before MAD"), div(class = "value", textOutput("sc_available")))),
            column(3, div(class = "metric", div(class = "label", "Projected Daily ET"), div(class = "value", textOutput("sc_proj_et")))),
            column(3, div(class = "metric", div(class = "label", "Days Until Irrigation"), div(class = "value", textOutput("sc_days_to_mad"))))
          ),
          br(),
          div(
            class = "wet-card",
            h4("Forecast Inputs"),
            p(class = "help-text", "Projected ET defaults to the 7-day average from your most recent data. The planning chart below shows soil water over your selected horizon."),
            fluidRow(
              column(
                4,
                numericInput("scenario_et", "Projected daily ET, in./day", value = 0.15, min = 0, step = 0.01),
                actionButton("reset_scenario_et", "Reset to 7-day avg", class = "btn btn-default btn-xs", style = "margin-top: -4px;")
              ),
              column(4, numericInput("scenario_irrig", "Apply irrigation today, in.", value = 0, min = 0, step = 0.10)),
              column(4, sliderInput("scenario_days", "Forecast horizon, days", min = 7, max = 30, value = 14, step = 1))
            )
          ),
          div(
            class = "wet-card",
            h4("Soil Water Depletion — Planning View"),
            withSpinner(plotlyOutput("scenario_plot", height = 400), type = 6, color = "#2e86c1", size = 0.7)
          ),
          uiOutput("forecast_7day_card")
        ),
        tabPanel(
          "Calcs",
          br(),
          downloadButton("download_balance", "Download Excel Results"),
          br(), br(),
          DTOutput("balance_table")
        ),
        tabPanel(
          "OpenET",
          br(),
          DTOutput("openet_table")
        ),
        tabPanel(
          "Map",
          br(),
          div(
            style = "margin-bottom: 8px;",
            radioButtons("map_style", NULL,
              choices = c("Satellite" = "satellite", "Street Map" = "street"),
              selected = "satellite", inline = TRUE
            )
          ),
          leafletOutput("field_map", height = 520)
        ),
        tabPanel(
          "FAQ",
          br(),
          h2("OpenET Irrigation Water Balance Dashboard"),
          p("A tool for estimating daily soil water balance and irrigation scheduling using satellite-based evapotranspiration from OpenET."),
          hr(),
          h4("Frequently Asked Questions"),
          hr(),
          tags$div(
            class = "panel-group", id = "faq-accordion",
            # Q0 - What is ET?
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq0",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "What is ET?"
                  )
                )
              ),
              tags$div(
                id = "faq0", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p(tags$b("Evapotranspiration (ET)"), " is the combined process by which water is transferred from the land surface to the atmosphere through two pathways:"),
                  tags$ul(
                    tags$li(tags$b("Evaporation:"), " Water evaporates directly from soil, open water bodies, and other surfaces."),
                    tags$li(tags$b("Transpiration:"), " Water is taken up by plant roots and released as vapor through tiny pores (stomata) in leaves and stems.")
                  ),
                  p("Together, ET is one of the largest components of the water cycle. Measuring ET accurately is essential for understanding crop water use, scheduling irrigation, and managing water resources."),
                  p(a("Learn more about ET at etdata.org", href = "https://etdata.org/what-is-et/", target = "_blank"))
                )
              )
            ),
            # Q0b - Where does precipitation come from?
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq0b",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "Where does precipitation data come from?"
                  )
                )
              ),
              tags$div(
                id = "faq0b", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p("Both precipitation and reference ET (ETo) used in this app come from ", a(tags$b("gridMET"), href = "https://www.climatologylab.org/gridmet.html", target = "_blank"), ", a gridded surface meteorological dataset that provides daily climate data at ~4 km spatial resolution across the contiguous United States."),
                  tags$ul(
                    tags$li(tags$b("Precipitation (PPT):"), " Daily gridded precipitation from gridMET is used directly in the soil water balance to account for rainfall inputs."),
                    tags$li(tags$b("Reference ET (ETo):"), " Grass reference ET is bias-corrected using nearly 800 quality-controlled weather stations in agricultural areas to account for local differences in wind speed, humidity, solar radiation, and temperature. In California, Spatial CIMIS data from the California Department of Water Resources is used instead of gridMET for reference ET.")
                  ),
                  p(a("gridMET", href = "https://www.climatologylab.org/gridmet.html", target = "_blank"), " precipitation and ETo are part of the reference and ancillary datasets that OpenET uses to support ET model calculations and water balance estimates."),
                  p(a("See OpenET reference & ancillary data methods", href = "https://etdata.org/methods/", target = "_blank"))
                )
              )
            ),
            # Q1
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq1",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "What is OpenET?"
                  )
                )
              ),
              tags$div(
                id = "faq1", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p("OpenET uses satellite-based evapotranspiration (ET) data to provide daily, field-scale ET estimates across the western United States."),
                  p("It combines six independent, peer-reviewed ET models plus an ensemble mean to improve accuracy and reliability. By drawing on satellite imagery, weather data, and land surface information, OpenET delivers consistent ET estimates at the field scale — making it practical for irrigation scheduling, water accounting, and agricultural water management."),
                  p(a("Visit the OpenET website", href = "https://openetdata.org", target = "_blank"))
                )
              )
            ),
            # Q2
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq2",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "What models does OpenET provide?"
                  )
                )
              ),
              tags$div(
                id = "faq2", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p(
                    "OpenET includes six ET models — GEESEBAL, SSEBOP, SIMS, DISALEXI, PT-JPL, and eeMETRIC — as well as an ensemble mean. See the",
                    a("OpenET documentation", href = "https://etdata.org/methods/", target = "_blank"),
                    "for details on each model."
                  ),
                  p(
                    tags$b("The ensemble mean is a good first choice."),
                    " It combines all six models and has proven to be the most consistently accurate option for croplands. Individual models may outperform the ensemble for specific crops or regions, but the ensemble is the recommended starting point before evaluating model-specific performance for your field."
                  ),
                  p(tags$b("Ensemble accuracy for croplands (across validation sites):")),
                  tags$ul(
                    tags$li(tags$b("Growing season:"), " R² = 0.96, bias = −2.0% (39 sites, 177 growing seasons)"),
                    tags$li(tags$b("Monthly:"), " R² = 0.92, bias = −5.8% (46 sites, 1,791 months)"),
                    tags$li(tags$b("Daily:"), " R² = 0.86, bias = −10.0% (55 sites, 5,508 Landsat overpass days)")
                  ),
                  p(a("See full OpenET accuracy details", href = "https://etdata.org/accuracy-known-issues/", target = "_blank"))
                )
              )
            ),
            # Q3
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq3",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "Where do I get an OpenET API key?"
                  )
                )
              ),
              tags$div(
                id = "faq3", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p("Follow these steps to get your free OpenET API key:"),
                  tags$ol(
                    tags$li("Go to ", a("etdata.org", href = "https://etdata.org", target = "_blank"), " and click ", tags$b("Sign Up"), " to create a free account."),
                    tags$li("Once logged in, navigate to your ", tags$b("Account"), " page and find the ", tags$b("API Key"), " section."),
                    tags$li("Click ", tags$b("Generate API Key"), " (or copy your existing key if one has already been created)."),
                    tags$li("Paste the key into the ", tags$b("OpenET API key"), " field in the OpenET section of this app's left panel."),
                    tags$li("Click ", tags$b("Update OpenET data"), " to fetch ET and precipitation data for your field.")
                  ),
                  p(tags$em("Note: API keys are free for individual users. Keep your key private — do not share it publicly."),
                    style = "font-size: 12px; color: #667085;"
                  ),
                  p(a("Go to the OpenET account portal", href = "https://etdata.org/api/", target = "_blank"))
                )
              )
            ),
            # Q4
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq4",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "What is SSURGO?"
                  )
                )
              ),
              tags$div(
                id = "faq4", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p("SSURGO (Soil Survey Geographic Database) is USDA's most detailed soil survey database. It provides soil properties such as field capacity and wilting point at the survey map unit level."),
                  p(a("Explore SSURGO data on the Web Soil Survey", href = "https://websoilsurvey.nrcs.usda.gov", target = "_blank"))
                )
              )
            ),
            # Q5
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq5",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "How are field capacity and permanent wilting point calculated from SSURGO?"
                  )
                )
              ),
              tags$div(
                id = "faq5", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p("When you click ", tags$b("Fetch Soil from SSURGO"), ", the app queries the dominant soil component at your coordinates and retrieves horizon-level data down to your specified root zone depth. FC and PWP are derived from SSURGO moisture retention values using these steps:"),
                  tags$ol(
                    tags$li(
                      tags$b("Read SSURGO moisture values:"),
                      " For each soil horizon, SSURGO provides moisture content at two standard tensions:",
                      tags$ul(
                        style = "margin-top: 4px;",
                        tags$li(tags$b("Field Capacity (FC)"), " — moisture retained at 1/3 bar suction (", tags$code("wthirdbar_r"), ", % by weight). This is the upper limit of plant-available water after free drainage stops."),
                        tags$li(tags$b("Permanent Wilting Point (PWP)"), " — moisture retained at 15 bar suction (", tags$code("wfifteenbar_r"), ", % by weight). Below this point roots can no longer extract water.")
                      )
                    ),
                    tags$li(
                      tags$b("Convert % by weight → volumetric fraction:"),
                      " SSURGO moisture values are gravimetric (g water / g soil). Multiplying by bulk density (", tags$code("dbthirdbar_r"), ", g/cm³) converts them to volumetric fractions (cm³ water / cm³ soil):",
                      tags$p(tags$code("FC_vol = (wthirdbar_r / 100) × dbthirdbar_r"), style = "margin: 4px 0 0 16px;"),
                      tags$p(tags$code("PWP_vol = (wfifteenbar_r / 100) × dbthirdbar_r"), style = "margin: 4px 0 0 16px;")
                    ),
                    tags$li(tags$b("Depth-weighted average:"), " A thickness-weighted average volumetric fraction is computed across all horizons clipped to the root zone depth. Horizons that extend below the root zone are trimmed to it."),
                    tags$li(
                      tags$b("Convert to inches of water:"),
                      " The average volumetric fraction is multiplied by the root zone depth in inches:",
                      tags$p(tags$code("FC (in.) = FC_vol (cm³/cm³) × root zone depth (in.)"), style = "margin: 4px 0 0 16px;"),
                      tags$p(tags$code("PWP (in.) = PWP_vol (cm³/cm³) × root zone depth (in.)"), style = "margin: 4px 0 0 16px;"),
                      " This works because cm³/cm³ is dimensionless — equivalent to inches of water per inch of depth."
                    ),
                    tags$li(tags$b("Available Water Capacity (AWC):"), " The difference between FC and PWP — the water available for plant uptake: ", tags$code("AWC = FC − PWP"), ". SSURGO also reports this directly as ", tags$code("awc_r"), " (cm/cm), which is used as a cross-check."),
                    tags$li(tags$b("Allowable depletion:"), " Set at 50% MAD: ", tags$code("Allowable depletion = FC − 0.5 × AWC"), ". Irrigation is recommended when soil water falls to this level.")
                  )
                )
              )
            ),
            # Q7
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq7",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "How is the soil water balance calculated?"
                  )
                )
              ),
              tags$div(
                id = "faq7", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p(tags$b("Soil water balance")),
                  p("The balance tracks daily soil water content using:"),
                  tags$p(tags$code("SWC = SWC_prev + Irrigation + Effective Precipitation − ETa"), style = "margin-left: 16px;"),
                  p("An irrigation event is suggested when soil water drops to or below the allowable depletion (MAD) threshold. Soil water is capped at field capacity — any water in excess drains as deep percolation."),
                  hr(style = "margin: 10px 0;"),
                  p(tags$b("Deep percolation")),
                  p("Deep percolation is the water that drains below the root zone on days when the computed soil water would exceed field capacity:"),
                  tags$p(tags$code("Deep percolation (in.) = max(0, SWC_prev + Inputs − ETa − Field Capacity)"), style = "margin-left: 16px;"),
                  p("Cumulative deep percolation (Σ Deep Percolation) is the running total over the season. It represents water that was applied or received as precipitation but was not retained in the root zone — it is unavailable to the crop and may carry nutrients below the root zone."),
                  hr(style = "margin: 10px 0;"),
                  p(tags$b("Leaching fraction")),
                  p("The leaching fraction is the cumulative fraction of total applied water (irrigation + effective precipitation) that has drained below the root zone:"),
                  tags$p(tags$code("Leaching Fraction = Σ Deep Percolation / Σ Water Credited"), style = "margin-left: 16px;"),
                  p("where ", tags$em("Water Credited"), " is the sum of net irrigation applied plus any effective precipitation counted. A leaching fraction of 0.10 means 10% of applied water left the root zone. High values indicate over-irrigation or heavy precipitation events that exceed field capacity.")
                )
              )
            ),
            # Q8 - 7-day ETo forecast
            tags$div(
              class = "panel panel-default",
              tags$div(
                class = "panel-heading",
                tags$h5(
                  class = "panel-title",
                  tags$a(
                    "data-toggle" = "collapse", "data-parent" = "#faq-accordion", href = "#faq8",
                    style = "text-decoration: none; color: inherit; display: block;",
                    tags$span(class = "pull-right", style = "font-size: 12px; color: #667085;", "▼"),
                    "Where does the 7-day ETo forecast come from?"
                  )
                )
              ),
              tags$div(
                id = "faq8", class = "panel-collapse collapse",
                tags$div(
                  class = "panel-body",
                  p(HTML('The 7-day forecast in the <b>Irrigation Explorer</b> tab uses reference evapotranspiration (ETo) from <a href="https://open-meteo.com" target="_blank">Open-Meteo</a>, a free and open-source weather API.')),
                  p(tags$b("How it works:")),
                  tags$ul(
                    tags$li("For the US,", tags$b("NOAA's GFS and HRRR models"), " (3–25 km resolution, updated hourly), forecast temperature, humidity, wind speed, and solar radiation — that are used to compute ETo using the ", tags$b("FAO-56 Penman-Monteith equation"), ", the same standard method used by gridMET and state climate offices (e.g. CIMIS in California) and is available for a 7 day forecast."),
                    tags$li("This app multiplies this forecasted ETo by a crop coefficient (Kc), that can be input by the user or estimated using the average ratio of ET and ETo (from OpenET) over the past 7 days.")
                  ),
                  p(tags$b("What is the crop coefficient (Kc)?")),
                  p("Kc accounts for the difference between a reference surface (ETo) and your actual crop. The app auto-estimates Kc from the ratio of your remote sensing based crop ET (ETa from OpenET) to the gridMET ETo over the past 7 days. You can also enter a value manually based on your crop growth stage."),
                  p(tags$em("Note: The forecast is most useful as a short-term planning aid (1-7 days)."),
                    style = "font-size: 12px; color: #667085;"
                  )
                )
              )
            )
          ),
          hr(),
          tags$div(
            style = "margin-top: 24px; padding: 16px 20px; background: #f8f9fa; border-radius: 6px; border-left: 4px solid #2e86c1;",
            h5("About this app", style = "margin-top: 0; color: #2e86c1;"),
            p(
              "This Shiny web application was developed by ",
              tags$b("José M. Rodríguez-Flores"), "[California State University Monterey Bay],",
              " based on an Excel-based irrigation scheduling tool originally created by ",
              tags$b("Jon Chilcote, P.E."), "[USDA-NRCS West National Technology Support Center]"
            ),
            p(
              tags$b("Funding acknowledgment:"), " This tool was developed with support from the USDA-NIFA project ",
              tags$b("CALW-2023-08988"), " — ",
              tags$em("Applications of OpenET and Satellite-Based Evapotranspiration Information to Advance Data-Driven Water Management"), ".",
              " ", a("View project details", href = "https://www.nal.usda.gov/research-tools/food-safety-research-projects/applications-openet-and-satellite-based-evapotranspiration-information-advance-data-driven-water", target = "_blank"), "."
            ),
            p(
              tags$em(""),
              style = "font-size: 12px; color: #667085;"
            ),
            hr(style = "margin: 10px 0;"),
            p(
              "Have suggestions or found a bug? We'd love to hear from you.",
              style = "margin-bottom: 6px; font-size: 13px;"
            ),
            actionButton("open_feedback", tagList(tags$i(class = "fa fa-envelope"), " Send Feedback"),
              class = "btn-sm btn-primary"
            )
          )
        ),
        tabPanel(
          "Summary",
          br(),
          div(
            id = "summary_print_header",
            h4(paste("Printed:", format(Sys.Date(), "%B %d, %Y"))),
            hr()
          ),
          div(
            style = "margin-bottom: 12px;",
            actionButton("print_summary", "Print / Save PDF",
              onclick = "window.print();"
            )
          ),
          div(
            class = "wet-card",
            h4("Fields Overview"),
            DTOutput("summary_fields_table")
          ),
          div(
            class = "wet-card",
            h4("Field Locations"),
            leafletOutput("summary_map", height = 420)
          )
        )
      )
    )
  )
)
server <- function(input, output, session) {
  showModal(modalDialog(
    title = tags$div(
      tags$img(
        src = "https://openetdata.org/static/images/openet-logo.png",
        height = "36px", style = "margin-right: 10px; vertical-align: middle;"
      ),
      tags$span("Welcome to the OpenET Irrigation Water Balance Dashboard",
        style = "vertical-align: middle; font-size: 18px; font-weight: 600;"
      )
    ),
    tags$div(
      p(
        "This tool estimates daily soil water balance and irrigation scheduling using satellite-based evapotranspiration (ET) data from ",
        a("OpenET", href = "https://openetdata.org", target = "_blank"), "."
      ),
      hr(),
      h5("Getting started:"),
      tags$ol(
        tags$li(HTML('<b>Create a free OpenET account</b> and generate an API key at <a href="https://etdata.org" target="_blank">etdata.org</a>.')),
        tags$li(HTML("Enter your <b>Field ID</b>, <b>Crop</b>, <b>Date range</b>, <b>Coordinates</b>, and <b>Soil Properties</b> in the left panel.")),
        tags$li("Optionally click ", tags$b("Fetch Soil from SSURGO"), " to auto-fill soil properties for your location and root zone depth."),
        tags$li("Paste your ", tags$b("OpenET API key"), " into the OpenET section and click ", tags$b("Update OpenET data"), "."),
        tags$li("Enter irrigation events in the ", tags$b("Irrigation Amounts"), " tab and view the water balance on the ", tags$b("Dashboard"), "."),
        tags$li("Open the ", tags$b("Irrigation Explorer"), " tab to project soil water depletion forward in time and find out when your next irrigation is due."),
        tags$li("When done, open the ", tags$b("Session"), " panel and click ", tags$b("Save Session"), " to export your setup, irrigation entries, and ET data to a ", tags$code(".rds"), " file. Reload it any time with ", tags$b("Load Session"), ""),
        tags$li("Use ", tags$b("Download Excel Results"), " in the ", tags$b("Calcs"), " tab to export the full daily water balance table.")
      ),
      hr(),
      p(tags$em("Tip: Hover over any chart to inspect daily values. Zoom in by clicking and dragging. Click on the legend to toggle individual variables on and off."),
        style = "color: #667085; font-size: 13px;"
      )
    ),
    footer = modalButton("Get started"),
    size = "l",
    easyClose = TRUE
  ))

  irrigation_data <- reactiveVal(make_default_irrigation_range(default_start, default_end))
  openet_data <- reactiveVal(make_empty_openet_range(default_start, default_end))
  eto_data <- reactiveVal(NULL)
  openet_status <- reactiveVal("No OpenET API request made yet.")
  openet_location <- reactiveVal(list(latitude = NA_real_, longitude = NA_real_, start_date = NA, end_date = NA))
  ssurgo_status <- reactiveVal("")
  ssurgo_result <- reactiveVal(NULL)
  irrig_version <- reactiveVal(0L)
  irrig_proxy <- dataTableProxy("irrig_table")

  output$openet_status <- renderText(openet_status())
  output$ssurgo_status <- renderUI({
    msg <- ssurgo_status()
    if (!nzchar(msg %||% "")) {
      return(NULL)
    }
    if (grepl("^SSURGO error", msg)) {
      return(div(class = "warn-box", style = "font-size: 12px;", msg))
    }
    if (msg == "ok") {
      s <- ssurgo_result()
      if (!is.null(s)) {
        return(div(
          style = "font-size: 12px; margin-top: 4px;",
          tags$table(
            style = "width: 100%; border-collapse: collapse;",
            tags$tr(tags$td(tags$b("Soil series:"), style = "padding: 2px 6px 2px 0;"), tags$td(s$compname, style = "padding: 2px 0;")),
            tags$tr(tags$td(tags$b("Map unit (mukey):"), style = "padding: 2px 6px 2px 0;"), tags$td(s$mukey, style = "padding: 2px 0;")),
            tags$tr(tags$td(tags$b("Root zone depth:"), style = "padding: 2px 6px 2px 0;"), tags$td(sprintf("%.1f ft", s$rooting_depth_ft), style = "padding: 2px 0;")),
            tags$tr(tags$td(HTML("<b>Field Capacity (FC):</b>"), style = "padding: 2px 6px 2px 0;"), tags$td(sprintf("%.2f in.", s$field_capacity_in), style = "padding: 2px 0;")),
            tags$tr(tags$td(HTML("<b>Perm. Wilting Point (PWP):</b>"), style = "padding: 2px 6px 2px 0;"), tags$td(sprintf("%.2f in.", s$pwp_in), style = "padding: 2px 0;")),
            tags$tr(tags$td(HTML("<b>Avail. Water Capacity (AWC):</b>"), style = "padding: 2px 6px 2px 0;"), tags$td(sprintf("%.2f in.", s$awc_in), style = "padding: 2px 0;")),
            tags$tr(tags$td(HTML("<b>50% MAD (reference):</b>"), style = "padding: 2px 6px 2px 0;"), tags$td(sprintf("%.2f in.", s$allowable_dryness_in), style = "padding: 2px 0;"))
          ),
          hr(style = "margin: 6px 0;"),
          p(style = "color: #388E3C; margin: 0;", icon("check-circle"), " Field capacity and PWP updated."),
          p(
            style = "color: #667085; font-size: 11px; margin: 4px 0 0;",
            tags$em("Allowable depletion and initial water content were not changed \u2014 set those based on management and current field conditions.")
          )
        ))
      }
    }
    # fallback for "Querying SSURGO..." or any other message
    pre(style = "font-size: 11px;", msg)
  })

  # ── Multi-field management ──────────────────────────────────────────────────
  field_counter <- reactiveVal(1L)
  fields_store <- reactiveVal(list(
    field_1 = list(
      setup = list(
        field_id               = default_setup$field_id,
        crop                   = default_setup$crop_description,
        date_range             = c(default_setup$start_date, default_setup$end_date),
        lat                    = default_setup$latitude,
        lon                    = default_setup$longitude,
        app_rate               = default_setup$application_rate_in_hr,
        initial_water          = default_setup$initial_water_content_in,
        allowable_dryness      = default_setup$allowable_dryness_in,
        field_capacity         = default_setup$field_capacity_in,
        pwp                    = default_setup$permanent_wilting_point_in,
        root_depth_ft          = 2.0,
        openet_model           = default_setup$selected_model,
        download_all_models    = default_setup$download_all_models,
        count_precip_effective = default_setup$count_precip_effective
      ),
      irrigation_data = NULL,
      openet_data = NULL,
      eto_data = NULL,
      openet_location = list(latitude = NA_real_, longitude = NA_real_, start_date = NA, end_date = NA),
      openet_status_text = "No OpenET API request made yet.",
      ssurgo_status_text = ""
    )
  ))
  active_field_key <- reactiveVal("field_1")

  field_choices <- function(store) {
    if (length(store) == 0) {
      return(c("Example" = "field_1"))
    }
    ids <- vapply(names(store), function(k) store[[k]]$setup$field_id %||% k, character(1))
    setNames(names(store), ids)
  }

  capture_current_field <- function() {
    list(
      setup = list(
        field_id               = isolate(input$field_id),
        crop                   = isolate(input$crop),
        date_range             = isolate(input$date_range),
        lat                    = isolate(input$lat),
        lon                    = isolate(input$lon),
        app_rate               = isolate(input$app_rate),
        initial_water          = isolate(input$initial_water),
        allowable_dryness      = isolate(input$allowable_dryness),
        field_capacity         = isolate(input$field_capacity),
        pwp                    = isolate(input$pwp),
        root_depth_ft          = isolate(input$root_depth_ft),
        openet_model           = isolate(input$openet_model),
        download_all_models    = isolate(input$download_all_models),
        count_precip_effective = isolate(input$count_precip_effective)
      ),
      irrigation_data = isolate(irrigation_data()),
      openet_data = isolate(openet_data()),
      eto_data = isolate(eto_data()),
      openet_location = isolate(openet_location()),
      openet_status_text = isolate(openet_status()),
      ssurgo_status_text = isolate(ssurgo_status()),
      ssurgo_result_data = isolate(ssurgo_result())
    )
  }

  restore_field <- function(fd) {
    suppress_mismatch_until(as.numeric(Sys.time()) + 1.8)
    prev_mismatch(TRUE) # prevent popup firing on the newly loaded field
    s <- fd$setup
    updateTextInput(session, "field_id", value = s$field_id %||% "")
    updateTextInput(session, "crop", value = s$crop %||% "")
    updateDateRangeInput(session, "date_range",
      start = as.Date(s$date_range[1]), end = as.Date(s$date_range[2])
    )
    updateNumericInput(session, "lat", value = s$lat)
    updateNumericInput(session, "lon", value = s$lon)
    updateNumericInput(session, "app_rate", value = s$app_rate)
    updateNumericInput(session, "initial_water", value = s$initial_water)
    updateNumericInput(session, "allowable_dryness", value = s$allowable_dryness)
    updateNumericInput(session, "field_capacity", value = s$field_capacity)
    updateNumericInput(session, "pwp", value = s$pwp)
    updateNumericInput(session, "root_depth_ft", value = s$root_depth_ft %||% 2.0)
    updateSelectInput(session, "openet_model", selected = s$openet_model %||% "ENSEMBLE")
    updateCheckboxInput(session, "download_all_models", value = isTRUE(s$download_all_models))
    updateCheckboxInput(session, "count_precip_effective", value = isTRUE(s$count_precip_effective))
    if (!is.null(fd$irrigation_data)) irrigation_data(fd$irrigation_data)
    if (!is.null(fd$openet_data)) openet_data(fd$openet_data)
    eto_data(fd$eto_data)
    openet_location(fd$openet_location)
    openet_status(fd$openet_status_text %||% "No OpenET API request made yet.")
    ssurgo_status(fd$ssurgo_status_text %||% "")
    ssurgo_result(fd$ssurgo_result_data)
    irrig_version(irrig_version() + 1L)
  }

  # Switch active field: save current state, then restore selected field
  observeEvent(input$active_field_key,
    {
      new_key <- input$active_field_key
      cur_key <- isolate(active_field_key())
      if (identical(new_key, cur_key)) {
        return()
      }
      store <- fields_store()
      store[[cur_key]] <- capture_current_field()
      fields_store(store)
      active_field_key(new_key)
      if (!is.null(store[[new_key]])) restore_field(store[[new_key]])
    },
    ignoreInit = TRUE
  )

  # Keep store field_id in sync with input immediately (fixes race on field switch).
  observeEvent(input$field_id,
    {
      key <- isolate(active_field_key())
      store <- fields_store()
      if (!is.null(store[[key]])) {
        store[[key]]$setup$field_id <- input$field_id
        fields_store(store)
      }
    },
    ignoreInit = TRUE
  )

  # Update the dropdown label after the user stops typing (debounced 600 ms).
  field_id_d <- debounce(reactive(input$field_id), 600)
  observeEvent(field_id_d(),
    {
      key <- isolate(active_field_key())
      store <- fields_store()
      if (!is.null(store[[key]])) {
        updateSelectInput(session, "active_field_key",
          choices = field_choices(store), selected = key
        )
      }
    },
    ignoreInit = TRUE
  )

  # Add new field
  observeEvent(input$add_field, {
    showModal(modalDialog(
      title = "Add New Field",
      textInput("new_field_name", "Field ID / Name", placeholder = "e.g. West Block"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_field", "Add", class = "btn-primary")
      ),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_add_field, {
    new_name <- trimws(input$new_field_name %||% "")
    if (!nzchar(new_name)) {
      return()
    }
    store <- fields_store()
    cur_key <- isolate(active_field_key())
    store[[cur_key]] <- capture_current_field()
    n <- field_counter() + 1L
    field_counter(n)
    new_key <- paste0("field_", n)
    store[[new_key]] <- list(
      setup = list(
        field_id = new_name, crop = "",
        date_range = c(default_start, default_end),
        lat = default_setup$latitude,
        lon = default_setup$longitude,
        app_rate = default_setup$application_rate_in_hr,
        initial_water = default_setup$initial_water_content_in,
        allowable_dryness = default_setup$allowable_dryness_in,
        field_capacity = default_setup$field_capacity_in,
        pwp = default_setup$permanent_wilting_point_in,
        root_depth_ft = 2.0,
        openet_model = "ENSEMBLE",
        download_all_models = FALSE,
        count_precip_effective = FALSE
      ),
      irrigation_data = make_default_irrigation_range(default_start, default_end),
      openet_data = make_empty_openet_range(default_start, default_end),
      eto_data = NULL,
      openet_location = list(latitude = NA_real_, longitude = NA_real_, start_date = NA, end_date = NA),
      openet_status_text = "No OpenET API request made yet.",
      ssurgo_status_text = ""
    )
    fields_store(store)
    active_field_key(new_key)
    updateSelectInput(session, "active_field_key",
      choices = field_choices(store), selected = new_key
    )
    restore_field(store[[new_key]])
    removeModal()
  })

  # Remove current field
  observeEvent(input$delete_field, {
    if (length(fields_store()) <= 1) {
      showNotification("Cannot remove the only field.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Remove Field",
      p(HTML(paste0('Remove field "<b>', isolate(input$field_id), '</b>"? All unsaved data for this field will be lost.'))),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_field", "Remove", class = "btn-danger")
      ),
      size = "s", easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_delete_field, {
    store <- fields_store()
    del_key <- isolate(active_field_key())
    store[[del_key]] <- NULL
    fields_store(store)
    remaining <- names(store)
    new_key <- remaining[1]
    active_field_key(new_key)
    updateSelectInput(session, "active_field_key",
      choices = field_choices(store), selected = new_key
    )
    restore_field(store[[new_key]])
    removeModal()
  })

  setup_values <- reactive({
    list(
      field_id = input$field_id,
      api_key = input$api_key,
      application_rate_in_hr = input$app_rate,
      latitude = input$lat,
      longitude = input$lon,
      crop_description = input$crop,
      field_capacity_in = input$field_capacity,
      initial_water_content_in = input$initial_water,
      allowable_dryness_in = input$allowable_dryness,
      permanent_wilting_point_in = input$pwp,
      start_date = as.Date(input$date_range[1]),
      end_date = as.Date(input$date_range[2]),
      selected_model = input$openet_model,
      download_all_models = isTRUE(input$download_all_models),
      count_precip_effective = isTRUE(input$count_precip_effective)
    )
  })

  observeEvent(input$date_range,
    {
      req(input$date_range)
      start_date <- as.Date(input$date_range[1])
      end_date <- as.Date(input$date_range[2])
      if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
        return()
      }
      irrigation_data(sync_irrigation_to_range(irrigation_data(), start_date, end_date))
      openet_data(normalize_openet_columns(openet_data(), start_date, end_date))
      updateDateInput(session, "new_irrig_date", value = start_date, min = start_date, max = end_date)
      irrig_version(irrig_version() + 1L)
    },
    ignoreInit = TRUE
  )

  # Input validation
  observe({
    req(input$lat)
    shinyFeedback::feedbackDanger("lat",
      show = input$lat < 24 || input$lat > 50,
      text = "Latitude should be between 24\u00b0 and 50\u00b0"
    )
  })

  observe({
    req(input$lon)
    shinyFeedback::feedbackDanger("lon",
      show = input$lon > -95 || input$lon < -130,
      text = "Longitude should be between -130\u00b0 and -95\u00b0 (western US)"
    )
  })

  observe({
    req(input$field_capacity, input$pwp, input$allowable_dryness)
    fc <- input$field_capacity
    pwp <- input$pwp
    ad <- input$allowable_dryness
    shinyFeedback::feedbackDanger("field_capacity",
      show = isTRUE(fc <= pwp),
      text = "Field capacity must be greater than permanent wilting point"
    )
    shinyFeedback::feedbackWarning("allowable_dryness",
      show = isTRUE(ad < pwp || ad > fc),
      text = "Allowable depletion should be between PWP and field capacity"
    )
    shinyFeedback::feedbackDanger("pwp",
      show = isTRUE(pwp >= fc),
      text = "Permanent wilting point must be less than field capacity"
    )
  })

  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = tags$div(
        tags$img(
          src = "https://openetdata.org/static/images/openet-logo.png",
          height = "36px", style = "margin-right: 10px; vertical-align: middle;"
        ),
        tags$span("Welcome to the OpenET Irrigation Water Balance Dashboard",
          style = "vertical-align: middle; font-size: 18px; font-weight: 600;"
        )
      ),
      tags$div(
        p(
          "This tool estimates daily soil water balance and irrigation scheduling using satellite-based evapotranspiration (ET) data from ",
          a("OpenET", href = "https://openetdata.org", target = "_blank"), "."
        ),
        hr(),
        h5("Getting started:"),
        tags$ol(
          tags$li(HTML('<b>Create a free OpenET account</b> and generate an API key at <a href="https://etdata.org" target="_blank">etdata.org</a>.')),
          tags$li(HTML("Enter your <b>Field ID</b>, <b>Crop</b>, <b>Date range</b>, <b>Coordinates</b>, and <b>Soil Properties</b> in the left panel.")),
          tags$li("Optionally click ", tags$b("Fetch Soil from SSURGO"), " to auto-fill soil properties for your location and root zone depth."),
          tags$li("Paste your ", tags$b("OpenET API key"), " into the OpenET section and click ", tags$b("Update OpenET data"), "."),
          tags$li("Enter irrigation events in the ", tags$b("Irrigation Amounts"), " tab and view the water balance on the ", tags$b("Dashboard"), "."),
          tags$li("Open the ", tags$b("Irrigation Explorer"), " tab to project soil water depletion forward in time and find out when your next irrigation is due."),
          tags$li("When done, open the ", tags$b("Session"), " panel and click ", tags$b("Save Session"), " to export your setup, irrigation entries, and ET data to a ", tags$code(".rds"), " file. Reload it any time with ", tags$b("Load Session"), " — no re-entry needed."),
          tags$li("Use ", tags$b("Download Excel Results"), " in the ", tags$b("Calcs"), " tab to export the full daily water balance table.")
        ),
        hr(),
        p(tags$em("Tip: Hover over any chart to inspect daily values. Zoom in by clicking and dragging. Click on the legend to toggle individual variables on and off."),
          style = "color: #667085; font-size: 13px;"
        )
      ),
      footer = modalButton("Get started"),
      size = "l",
      easyClose = TRUE
    ))
  })

  observeEvent(input$open_feedback, {
    showModal(modalDialog(
      title = "Send Feedback",
      tags$div(
        p("Have a suggestion, found a bug, or want to share how you're using this tool? Fill out the form below."),
        textInput("fb_name", "Your name", placeholder = "e.g. Desert Willow"),
        textInput("fb_org", "Organization (optional)", placeholder = "e.g. North City Farm"),
        textInput("fb_subject", "Subject", placeholder = "e.g. Feature request"),
        textAreaInput("fb_message", "Message", rows = 5, placeholder = "Describe your suggestion or issue..."),
        uiOutput("fb_status")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_feedback", "Send", class = "btn-primary")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  observeEvent(input$submit_feedback, {
    name <- trimws(input$fb_name %||% "")
    org <- trimws(input$fb_org %||% "")
    subject <- trimws(input$fb_subject %||% "")
    message <- trimws(input$fb_message %||% "")
    if (!nzchar(message)) {
      output$fb_status <- renderUI(div(class = "warn-box", "Please enter a message before sending."))
      return()
    }
    body <- utils::URLencode(paste0(
      if (nzchar(name)) paste0("From: ", name, if (nzchar(org)) paste0(" (", org, ")"), "\n\n") else "",
      message
    ), reserved = TRUE)
    subj <- utils::URLencode(if (nzchar(subject)) subject else "OpenET wETgraph Feedback", reserved = TRUE)
    mailto <- paste0("mailto:jrodriguezflores@csumb.edu?subject=", subj, "&body=", body)
    output$fb_status <- renderUI(
      tagList(
        div(
          class = "ok-box", style = "margin-top: 8px;",
          "Your email client should open now. If it doesn't, ",
          a("click here", href = mailto, target = "_blank"), " or email us directly at ",
          a("jrodriguezflores@csumb.edu", href = "mailto:jrodriguezflores@csumb.edu"), "."
        ),
        tags$script(HTML(sprintf("window.location.href = '%s';", mailto)))
      )
    )
  })

  observeEvent(input$fetch_openet, {
    req(input$api_key, input$date_range, input$lat, input$lon)
    tryCatch(
      {
        start_date <- as.Date(input$date_range[1])
        end_date <- as.Date(input$date_range[2])
        fetch_models <- if (isTRUE(input$download_all_models)) models else input$openet_model
        if (!"ENSEMBLE" %in% fetch_models) fetch_models <- unique(c(fetch_models, "ENSEMBLE"))
        n_calls <- length(fetch_models) + 1 + 1L
        withProgress(message = "Fetching OpenET data", detail = "This may take 1\u20135 min per request\u2026", value = 0, {
          et_list <- list()
          for (i in seq_along(fetch_models)) {
            incProgress(0.75 / length(fetch_models), detail = sprintf("%s ET (%d of %d)\u2026", fetch_models[i], i, length(fetch_models)))
            et_list[[fetch_models[i]]] <- fetch_openet_point(input$api_key, input$lon, input$lat, start_date, end_date, model = fetch_models[i], variable = "ET")
          }
          incProgress(0.12, detail = "Precipitation\u2026")
          pr <- fetch_openet_point(input$api_key, input$lon, input$lat, start_date, end_date, model = "ENSEMBLE", variable = "PR")
          {
            incProgress(0.08, detail = "Reference ET (ETo)\u2026")
            eto_result <- tryCatch(
              fetch_openet_point(input$api_key, input$lon, input$lat, start_date, end_date, model = "ENSEMBLE", variable = "ETO"),
              error = function(e) {
                openet_status(paste("ETo fetch warning:", conditionMessage(e)))
                NULL
              }
            )
            eto_data(eto_result)
          }
          out <- combine_openet_models(et_list, pr, start_date, end_date)
          openet_data(out)
          irrigation_data(sync_irrigation_to_range(irrigation_data(), start_date, end_date))
          openet_location(list(latitude = input$lat, longitude = input$lon, start_date = start_date, end_date = end_date))
          incProgress(1)
          openet_status(sprintf("Fetched %s daily rows from %s to %s for %s ET model(s) plus precipitation and ETo.", nrow(out), start_date, end_date, length(fetch_models)))
          irrig_version(irrig_version() + 1L)
        })
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("timed out|Timeout", msg, ignore.case = TRUE)) {
          openet_status(paste0(
            "OpenET API timed out. The server is likely busy or the date range is too long. ",
            "Try a shorter date range (e.g. one season at a time), wait a minute, then retry. ",
            "Technical detail: ", msg
          ))
        } else if (grepl("429", msg)) {
          openet_status("OpenET API rate limit reached (HTTP 429). Wait a few minutes before retrying.")
        } else if (grepl("PROTOCOL_ERROR|HTTP/2|framing layer|stream.*not closed", msg, ignore.case = TRUE)) {
          openet_status("OpenET connection dropped (HTTP/2 protocol error). This is a transient server issue — click 'Update OpenET data' again to retry.")
        } else {
          openet_status(paste("OpenET API error:", msg))
        }
      }
    )
  })

  observeEvent(input$fetch_ssurgo, {
    req(input$lat, input$lon)
    ssurgo_status("Querying SSURGO...")
    tryCatch(
      {
        soil <- fetch_ssurgo_soil_properties(
          latitude = input$lat,
          longitude = input$lon,
          rooting_depth_ft = input$root_depth_ft %||% 4.0
        )
        updateNumericInput(session, "field_capacity", value = soil$field_capacity_in)
        updateNumericInput(session, "pwp", value = soil$pwp_in)
        ssurgo_result(soil)
        ssurgo_status("ok")
      },
      error = function(e) ssurgo_status(paste("SSURGO error:", conditionMessage(e)))
    )
  })

  observeEvent(input$add_irrig, {
    req(input$new_irrig_date)
    s <- setup_values()
    df <- sync_irrigation_to_range(irrigation_data(), s$start_date, s$end_date)
    idx <- match(as.Date(input$new_irrig_date), df$date)
    if (!is.na(idx)) {
      df$net_water_applied_in[idx] <- safe_numeric(input$new_irrig_in) %||% 0
      df$notes[idx] <- input$new_irrig_note %||% ""
      irrigation_data(df)
      irrig_version(irrig_version() + 1L)
    }
    updateNumericInput(session, "new_irrig_in", value = 0)
    updateTextInput(session, "new_irrig_note", value = "")
  })

  observeEvent(input$clear_irrig, {
    df <- irrigation_data()
    df$net_water_applied_in <- 0
    df$notes <- ""
    irrigation_data(df)
    irrig_version(irrig_version() + 1L)
  })

  balance <- reactive({
    s <- setup_values()
    start <- as.Date(s$start_date)
    end <- as.Date(s$end_date)
    validate(
      need(
        !is.na(start) && !is.na(end) && end > start,
        "Please select a valid date range (end date must be after start date)."
      )
    )
    compute_water_balance(irrigation_data(), openet_data(), s)
  })

  observeEvent(input$irrig_table_cell_edit, {
    info <- input$irrig_table_cell_edit
    s <- setup_values()
    display_df <- make_irrigation_display(irrigation_data(), balance(), s)
    row <- as.integer(info$row)
    col <- as.integer(info$col) + 1
    if (row >= 1 && row <= nrow(display_df)) {
      edit_date <- display_df$date[row]
      df <- sync_irrigation_to_range(irrigation_data(), s$start_date, s$end_date)
      idx <- match(edit_date, df$date)
      if (!is.na(idx)) {
        if (col == 2) {
          val <- safe_numeric(info$value)
          if (is.na(val)) val <- 0
          df$net_water_applied_in[idx] <- val
        }
        if (col == 7) df$notes[idx] <- as.character(info$value %||% "")
        irrigation_data(df)
        replaceData(irrig_proxy, make_irrigation_display(df, balance(), s), resetPaging = FALSE, rownames = FALSE)
      }
    }
  })

  metrics <- reactive(summary_metrics(balance()))
  output$m_eta <- renderText(sprintf("%.2f", metrics()$total_eta))
  output$m_applied <- renderText(sprintf("%.2f", metrics()$total_applied))
  output$m_precip <- renderText(sprintf("%.2f", metrics()$total_precip))
  output$m_deep <- renderText(sprintf("%.2f", metrics()$total_deep_perc))

  output$soil_title <- renderText(sprintf("Soil Water Content — %s @ %s", input$crop, input$field_id))
  output$eta_title <- renderText(sprintf("Cumulative ET & Applied Water — %s @ %s", input$crop, input$field_id))
  output$deep_title <- renderText(sprintf("Deep Percolation & Leaching Fraction — %s @ %s", input$crop, input$field_id))
  output$kc_title <- renderText(sprintf("Daily ET, ETo & ET/ETo — %s @ %s", input$crop, input$field_id))

  # Track previous mismatch state to only show notification when it newly becomes mismatched
  prev_mismatch <- reactiveVal(FALSE)
  suppress_mismatch_until <- reactiveVal(0) # timestamp: suppress inline alert until this time

  output$location_warning <- renderUI({
    loc <- openet_location()
    s <- setup_values()
    if (is.na(loc$latitude)) {
      return(div(class = "warn-box", "OpenET has not been refreshed in this session. Click 'Update OpenET data' after setting the date range and coordinates."))
    }
    # Hold off showing the red box during a field switch (timestamp-based, survives multiple re-renders)
    if (as.numeric(Sys.time()) < isolate(suppress_mismatch_until())) {
      invalidateLater(300) # re-check every 300ms until suppress window expires
      return(div(class = "ok-box", "Switching field \u2014 please wait..."))
    }
    coords_same <- isTRUE(all.equal(as.numeric(input$lat), loc$latitude, tolerance = 1e-7)) &&
      isTRUE(all.equal(as.numeric(input$lon), loc$longitude, tolerance = 1e-7))
    dates_same <- identical(as.Date(s$start_date), as.Date(loc$start_date)) &&
      identical(as.Date(s$end_date), as.Date(loc$end_date))
    if (coords_same && dates_same) {
      prev_mismatch(FALSE)
      return(div(class = "ok-box", sprintf(
        "OpenET data match setup: %.5f, %.5f from %s to %s",
        loc$latitude, loc$longitude, as.Date(loc$start_date), as.Date(loc$end_date)
      )))
    }
    what_changed <- if (!coords_same && !dates_same) {
      "Coordinates and date range have changed"
    } else if (!coords_same) {
      "Coordinates have changed"
    } else {
      "Date range has changed"
    }
    msg <- paste0(what_changed, " since the last OpenET fetch. Click \u2018Update OpenET data\u2019 to refresh.")
    if (!isTRUE(prev_mismatch())) {
      showNotification(
        tagList(icon("exclamation-triangle"), " ", msg),
        type = "warning", duration = 8, closeButton = TRUE
      )
      prev_mismatch(TRUE)
    }
    div(class = "red-box", icon("exclamation-triangle"), " ", msg)
  })

  output$eta_plot <- renderPlotly({
    df <- make_excel_plot_balance(balance(), setup_values())
    show_eto <- !is.null(eto_data()) && nrow(eto_data()) > 0
    if (show_eto) {
      eto <- eto_data()
      eto$date <- as.Date(eto$date)
      df <- merge(df, eto[, c("date", "eto_in")], by = "date", all.x = TRUE)
      df$eto_in[is.na(df$eto_in)] <- 0
      df <- df[order(df$date), ]
      df$cumulative_eto_in <- cumsum(df$eto_in)
    }
    p <- ggplot(df, aes(x = date)) +
      geom_line(aes(y = cumulative_eta_in, color = "Σ ETa, in."), linewidth = 1.0) +
      geom_line(aes(y = cumulative_applied_in, color = "Σ Applied Water, in."), linewidth = 1.0, linetype = "dashed") +
      geom_line(aes(y = applied_minus_eta_in, color = "Σ Applied − Σ ETa"), linewidth = 0.8, linetype = "dotdash")
    if (show_eto) {
      p <- p + geom_line(aes(y = cumulative_eto_in, color = "Σ ETo, in."), linewidth = 0.9, linetype = "dotted")
    }
    color_vals <- c(
      "Σ ETa, in." = "#C62828",
      "Σ Applied Water, in." = "#1565C0",
      "Σ Applied − Σ ETa" = "#2E7D32",
      "Σ ETo, in." = "#7B1FA2"
    )
    p <- p +
      scale_color_manual(values = color_vals) +
      labs(x = NULL, y = "Cumulative Water (in.)", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.text = element_text(size = 10))
    plotly_date_layout(clean_plotly_hover(ggplotly(p)))
  })

  output$soil_plot <- renderPlotly({
    df <- make_excel_plot_balance(balance(), setup_values())
    # geom_ribbon with show.legend=FALSE — clean_plotly_hover skips it via isFALSE(showlegend)
    p <- ggplot(df, aes(x = date)) +
      geom_ribbon(aes(ymin = permanent_wilting_point_in, ymax = allowable_dryness_in),
        fill = "#FFCDD2", alpha = 0.30, color = NA
      ) +
      geom_point(aes(y = precip_plot_in, color = "Precipitation, in."), shape = 21, size = 3, fill = "#90CAF9", alpha = 0.85, na.rm = TRUE) +
      geom_point(aes(y = applied_plot_in, color = "Applied Water Event, in."), size = 3, shape = 24, na.rm = TRUE) +
      geom_line(aes(y = field_capacity_in, color = "Field Capacity, in."), linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(y = allowable_dryness_in, color = "Allowable Depletion, in."), linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(y = permanent_wilting_point_in, color = "Perm. Wilting Point, in."), linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(y = soil_water_graph_in, color = "Soil Water Content, in."), linewidth = 1.1) +
      scale_color_manual(values = c(
        "Soil Water Content, in." = "#1565C0",
        "Field Capacity, in." = "#2E7D32",
        "Allowable Depletion, in." = "#F57F17",
        "Perm. Wilting Point, in." = "#C62828",
        "Applied Water Event, in." = "#0288D1",
        "Precipitation, in." = "#42A5F5"
      )) +
      labs(x = NULL, y = "Soil Water (in.)", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.text = element_text(size = 10))
    plotly_date_layout(clean_plotly_hover(ggplotly(p)))
  })

  output$deep_plot <- renderPlotly({
    df <- make_excel_plot_balance(balance(), setup_values())
    plot_ly(df, x = ~date) |>
      add_bars(
        y = ~precip_plot_in, name = "Precipitation, in.",
        marker = list(color = "rgba(144,202,249,0.55)"),
        hovertemplate = "%{x|%b %d, %Y}<br>Precipitation: %{y:.2f} in.<extra></extra>",
        yaxis = "y"
      ) |>
      add_lines(
        y = ~cumulative_deep_percolated_in, name = "\u03a3 Deep Percolation, in.",
        line = list(color = "#1565C0", width = 3.5),
        hovertemplate = "%{x|%b %d, %Y}<br>\u03a3 Deep Percolation: %{y:.2f} in.<extra></extra>",
        yaxis = "y"
      ) |>
      add_lines(
        y = ~leaching_fraction, name = "Leaching Fraction",
        line = list(color = "#C62828", width = 2.5, dash = "dash"),
        hovertemplate = "%{x|%b %d, %Y}<br>Leaching Fraction: %{y:.2f}<extra></extra>",
        connectgaps = FALSE,
        yaxis = "y2"
      ) |>
      layout(
        hovermode = "x unified",
        xaxis = list(
          type = "date", autorange = TRUE, tickmode = "auto",
          title = "",
          hoverformat = "%b %d, %Y",
          showspikes = TRUE, spikemode = "across", spikesnap = "cursor"
        ),
        yaxis = list(
          title = "Inches",
          tickmode = "auto", nticks = 10,
          showspikes = TRUE, spikemode = "across", spikesnap = "cursor"
        ),
        yaxis2 = list(
          title = "Leaching Fraction",
          overlaying = "y", side = "right",
          range = list(0, 1),
          showgrid = FALSE,
          tickformat = ".2f"
        ),
        legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.08),
        font = list(size = 13),
        margin = list(r = 60)
      )
  })

  output$kc_plot <- renderPlotly({
    df <- make_excel_plot_balance(balance(), setup_values())
    eto <- eto_data()
    validate(need(
      !is.null(eto) && nrow(eto) > 0,
      "Plot requires ETo data. Click 'Update OpenET data' to fetch it."
    ))
    eto$date <- as.Date(eto$date)
    df <- merge(df, eto[, c("date", "eto_in")], by = "date", all.x = TRUE)
    df <- df[order(df$date), ]
    # Kc = ET / ETo; suppress where ETo == 0 or NA to avoid Inf / bad values
    df$kc <- ifelse(!is.na(df$eto_in) & df$eto_in > 0.001,
      df$eta_in / df$eto_in, NA_real_
    )
    # cap extreme outliers for display (e.g. very low ETo days)
    df$kc[!is.na(df$kc) & df$kc > 2] <- NA_real_

    plot_ly(df, x = ~date) |>
      add_lines(
        y = ~eta_in, name = "Daily ET, in.",
        line = list(color = "#C62828", width = 2),
        hovertemplate = "ETa: %{y:.2f} in.<extra></extra>",
        yaxis = "y"
      ) |>
      add_lines(
        y = ~eto_in, name = "Daily ETo, in.",
        line = list(color = "#7B1FA2", width = 2),
        hovertemplate = "ETo: %{y:.2f} in.<extra></extra>",
        yaxis = "y"
      ) |>
      add_lines(
        y = ~kc, name = "ET/ETo",
        line = list(color = "#F57F17", width = 2.5, dash = "dash"),
        hovertemplate = "ET/ETo: %{y:.2f}<extra></extra>",
        connectgaps = FALSE,
        yaxis = "y2"
      ) |>
      layout(
        hovermode = "x unified",
        xaxis = list(
          type = "date", autorange = TRUE, tickmode = "auto",
          title = "",
          hoverformat = "%b %d, %Y",
          showspikes = TRUE, spikemode = "across", spikesnap = "cursor"
        ),
        yaxis = list(
          title = "Inches",
          tickmode = "auto", nticks = 10,
          showspikes = TRUE, spikemode = "across", spikesnap = "cursor"
        ),
        yaxis2 = list(
          title = "ET/ETo",
          overlaying = "y", side = "right",
          range = list(0, 2),
          showgrid = FALSE,
          tickformat = ".2f"
        ),
        legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.08),
        font = list(size = 13),
        margin = list(r = 70)
      )
  })

  output$irrig_table <- renderDT(
    {
      df <- make_irrigation_display(irrigation_data(), balance(), setup_values())
      datatable(
        df,
        editable = list(target = "cell", disable = list(columns = c(0, 2, 3, 4, 5))),
        rownames = FALSE,
        colnames = c(
          "Date", "Net Water Applied, in.", "Σ Net Applied, in.",
          "Equiv. Irrig. Hours", "Precip. in.",
          "Soil Water Depletion, in.", "Notes / Flow Meter"
        ),
        options = list(pageLength = 15, scrollX = TRUE, ordering = FALSE)
      ) |>
        formatRound(columns = c("net_water_applied_in", "cumulative_applied_in", "equivalent_irrigation_hours", "precip_openet_in", "soil_water_deficit_in"), digits = 3)
    },
    server = TRUE
  )

  output$balance_table <- renderDT({
    df <- balance()
    # Join ETo if available and compute ET/ETo ratio
    eto <- eto_data()
    if (!is.null(eto) && nrow(eto) > 0) {
      eto$date <- as.Date(eto$date)
      df <- merge(df, eto[, c("date", "eto_in")], by = "date", all.x = TRUE)
      df$et_eto_ratio <- ifelse(!is.na(df$eto_in) & df$eto_in > 0.001,
        df$eta_in / df$eto_in, NA_real_
      )
      df$et_eto_ratio[!is.na(df$et_eto_ratio) & df$et_eto_ratio > 2] <- NA_real_
      df <- df[order(df$date), ]
    }
    pretty_names <- c(
      date = "Date", julian_date = "Julian Date", eta_in = "ETa, in.",
      cumulative_eta_in = "Σ ETa, in.",
      questionable_cumulative_eta_in = "Questionable (0 ETa) Cum. ETa",
      net_water_applied_in = "Inches Applied",
      cumulative_applied_in = "Σ Inches Applied",
      applied_minus_eta_in = "Σ Applied − Σ ETa",
      soil_water_content_in = "Soil Water Content, in.",
      field_capacity_in = "Field Capacity, in.",
      allowable_dryness_in = "Allowable Depletion, in.",
      permanent_wilting_point_in = "Perm. Wilting Point, in.",
      soil_water_graph_in = "Soil Water (chart), in.",
      questionable_soil_water_in = "Questionable Soil Water",
      deep_percolated_water_in = "Deep Percolation Water, in.",
      cumulative_deep_percolated_in = "Σ Deep Percolation, in.",
      leaching_fraction = "Leaching Fraction",
      precip_in = "Precipitation, in.",
      applied_plot_in = "Applied Water Events, in.",
      precip_plot_in = "Precip. Events, in.",
      soil_water_deficit_in = "Soil Water Depletion, in.",
      remaining_storage_capacity_in = "Remaining Storage, in.",
      equivalent_irrigation_hours = "Equiv. Irrig. Hours",
      selected_model = "ET Model",
      notes = "Notes",
      eto_in = "ETo, in.",
      et_eto_ratio = "ET/ETo"
    )
    numeric_cols <- which(vapply(df, is.numeric, logical(1)))
    names(df) <- sapply(names(df), function(n) if (n %in% names(pretty_names)) pretty_names[[n]] else n)
    datatable(df,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatRound(columns = numeric_cols, digits = 4)
  })

  output$openet_table <- renderDT({
    df <- openet_data()
    datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) |>
      formatRound(columns = names(df)[vapply(df, is.numeric, logical(1))], digits = 4)
  })

  # ── Coordinate picker map ─────────────────────────────────────────────────────
  observeEvent(input$pick_coords, {
    lat0 <- isolate(input$lat) %||% default_setup$latitude
    lon0 <- isolate(input$lon) %||% default_setup$longitude
    showModal(modalDialog(
      title = "Pick field coordinates",
      p(class = "help-text", "Click anywhere on the map to set latitude and longitude. The marker will move to your click."),
      uiOutput("picker_coords_display"),
      leafletOutput("picker_map", height = 420),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_coords", "Use these coordinates", class = "btn-primary")
      ),
      size = "l", easyClose = FALSE
    ))
    output$picker_map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.WorldImagery) |>
        setView(lng = lon0, lat = lat0, zoom = 14) |>
        addMarkers(lng = lon0, lat = lat0, layerId = "picked")
    })
    output$picker_coords_display <- renderUI({
      click <- input$picker_map_click
      if (is.null(click)) {
        div(
          style = "font-size: 13px; color: #667085; margin-bottom: 6px;",
          sprintf("Current: %.5f, %.5f — click the map to change.", lat0, lon0)
        )
      } else {
        div(
          style = "font-size: 13px; color: #388E3C; margin-bottom: 6px;",
          icon("check-circle"),
          sprintf(" Selected: %.5f N, %.5f E", click$lat, click$lng)
        )
      }
    })
  })

  observeEvent(input$picker_map_click, {
    click <- input$picker_map_click
    leafletProxy("picker_map") |>
      clearMarkers() |>
      addMarkers(lng = click$lng, lat = click$lat, layerId = "picked")
  })

  observeEvent(input$confirm_coords, {
    click <- input$picker_map_click
    if (!is.null(click)) {
      updateNumericInput(session, "lat", value = round(click$lat, 5))
      updateNumericInput(session, "lon", value = round(click$lng, 5))
    }
    removeModal()
  })

  output$field_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      setView(lng = input$lon, lat = input$lat, zoom = 14) |>
      addMarkers(lng = input$lon, lat = input$lat, popup = paste(input$field_id, "<br>", input$crop))
  })

  observeEvent(input$map_style,
    {
      tile <- if (input$map_style == "satellite") providers$Esri.WorldImagery else providers$OpenStreetMap
      leafletProxy("field_map") |>
        clearTiles() |>
        addProviderTiles(tile)
    },
    ignoreInit = TRUE
  )

  # ── Summary tab ─────────────────────────────────────────────────────────────
  output$summary_fields_table <- renderDT(
    {
      store <- fields_store()
      if (length(store) == 0) {
        return(data.frame(Message = "No fields added yet."))
      }

      rows <- lapply(store, function(fd) {
        s <- fd$setup
        et_total <- NA_real_
        applied_total <- NA_real_
        precip_total <- NA_real_
        cur_swc <- NA_real_
        irrigate_by <- NA_character_
        start_d <- tryCatch(as.Date(s$date_range[[1]]), error = function(e) as.Date(NA))
        end_d <- tryCatch(as.Date(s$date_range[[2]]), error = function(e) as.Date(NA))
        od <- fd$openet_data
        if (!is.null(od) && is.data.frame(od) && nrow(od) > 0 && !is.na(start_d) && !is.na(end_d)) {
          setup_s <- list(
            start_date = start_d, end_date = end_d,
            field_id = s$field_id %||% "", crop_description = s$crop %||% "",
            latitude = s$lat %||% 0, longitude = s$lon %||% 0,
            application_rate_in_hr = s$app_rate %||% 0,
            initial_water_content_in = s$initial_water %||% 0,
            allowable_dryness_in = s$allowable_dryness %||% 0,
            field_capacity_in = s$field_capacity %||% 0,
            permanent_wilting_point_in = s$pwp %||% 0,
            selected_model = s$openet_model %||% "ensemble",
            count_precip_effective = s$count_precip_effective %||% FALSE
          )
          tryCatch(
            {
              bal <- compute_water_balance(fd$irrigation_data, od, setup_s)
              et_total <- round(sum(bal$eta_in, na.rm = TRUE), 2)
              applied_total <- round(sum(bal$net_water_applied_in, na.rm = TRUE), 2)
              precip_total <- round(sum(bal$precip_in, na.rm = TRUE), 2)
              cur_swc <- round(tail(bal$soil_water_content_in, 1), 2)
              mad_in <- round(tail(bal$allowable_dryness_in, 1), 2)
              last_date <- as.Date(tail(bal$date, 1))
              recent <- bal[bal$eta_in > 0, ]
              avg_et <- if (nrow(recent) >= 1) mean(tail(recent$eta_in, 7)) else NA_real_
              if (!is.na(avg_et) && avg_et > 0 && !is.na(cur_swc) && !is.na(mad_in) && cur_swc > mad_in) {
                days_left <- ceiling((cur_swc - mad_in) / avg_et)
                irrigate_by <- format(last_date + days_left, "%Y-%m-%d")
              } else if (!is.na(cur_swc) && !is.na(mad_in) && cur_swc <= mad_in) {
                irrigate_by <- "Now"
              }
              mad_val <- NULL
            },
            error = function(e) NULL
          )
        }
        data.frame(
          Field = s$field_id %||% "",
          Crop = s$crop %||% "",
          From = if (!is.na(start_d)) as.character(start_d) else "",
          To = if (!is.na(end_d)) as.character(end_d) else "",
          Lat = round(s$lat %||% NA_real_, 5),
          Lon = round(s$lon %||% NA_real_, 5),
          `FC (in.)` = round(s$field_capacity %||% NA_real_, 2),
          `PWP (in.)` = round(s$pwp %||% NA_real_, 2),
          `MAD (in.)` = round(s$allowable_dryness %||% NA_real_, 2),
          `ETa (in.)` = et_total,
          `Applied (in.)` = applied_total,
          `Precip (in.)` = precip_total,
          `Cur. SWC (in.)` = cur_swc,
          `Irrigate By` = irrigate_by,
          check.names = FALSE, stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    },
    rownames = FALSE,
    options = list(pageLength = 25, scrollX = TRUE, ordering = FALSE, dom = "t")
  )

  output$summary_map <- renderLeaflet({
    store <- fields_store()
    m <- leaflet() |> addProviderTiles(providers$Esri.WorldImagery)
    lats <- c()
    lons <- c()
    for (fd in store) {
      lat <- fd$setup$lat
      lon <- fd$setup$lon
      if (!is.null(lat) && !is.null(lon) && !is.na(lat) && !is.na(lon)) {
        lats <- c(lats, lat)
        lons <- c(lons, lon)
        popup_txt <- paste0(
          "<b>", fd$setup$field_id %||% "", "</b><br>",
          "Crop: ", fd$setup$crop %||% "", "<br>",
          "Lat: ", round(lat, 5), "  Lon: ", round(lon, 5), "<br>",
          "FC: ", round(fd$setup$field_capacity %||% NA_real_, 2), " in. &nbsp; ",
          "PWP: ", round(fd$setup$pwp %||% NA_real_, 2), " in. &nbsp; ",
          "MAD: ", round(fd$setup$allowable_dryness %||% NA_real_, 2), " in."
        )
        m <- m |> addMarkers(
          lng = lon, lat = lat, popup = popup_txt,
          label = fd$setup$field_id %||% ""
        )
      }
    }
    if (length(lats) > 1) {
      m <- m |> fitBounds(min(lons), min(lats), max(lons), max(lats))
    } else if (length(lats) == 1) {
      m <- m |> setView(lng = lons[1], lat = lats[1], zoom = 13)
    }
    m
  })

  output$download_balance <- downloadHandler(
    filename = function() paste0("openet_wetgraph_", input$field_id, "_", as.Date(input$date_range[1]), "_", as.Date(input$date_range[2]), ".xlsx"),
    content = function(file) write_balance_export(balance(), irrigation_data(), openet_data(), setup_values(), file)
  )

  # ── Download metadata ─────────────────────────────────────────────────────
  # ── Save session ──────────────────────────────────────────────────────────
  # Auto-update filename input when field count changes
  observe({
    n <- length(fields_store())
    default_name <- paste0("fields_", n, "_", format(Sys.time(), "%m_%d_%Y_%H%M"))
    updateTextInput(session, "session_filename", value = default_name)
  })

  output$save_session <- downloadHandler(
    filename = function() {
      nm <- trimws(input$session_filename %||% "")
      if (!nzchar(nm)) nm <- paste0("fields_", length(fields_store()), "_", format(Sys.time(), "%m_%d_%Y_%H%M"))
      # strip .rds if user typed it, we add it back
      nm <- sub("\\.rds$", "", nm, ignore.case = TRUE)
      paste0(nm, ".rds")
    },
    content = function(file) {
      store <- fields_store()
      cur_key <- active_field_key()
      store[[cur_key]] <- capture_current_field()
      saveRDS(list(
        version          = 2L,
        saved_at         = Sys.time(),
        fields_store     = store,
        active_field_key = cur_key,
        field_counter    = field_counter()
      ), file)
    }
  )

  # ── Load session ──────────────────────────────────────────────────────────
  session_msg <- reactiveVal(NULL)
  output$session_status <- renderUI({
    msg <- session_msg()
    if (is.null(msg)) {
      return(NULL)
    }
    div(
      class = if (grepl("^Error", msg)) "warn-box" else "ok-box",
      style = "margin-top: 6px; font-size: 12px;", msg
    )
  })

  observeEvent(input$load_session, {
    req(input$load_session)
    tryCatch(
      {
        sd <- readRDS(input$load_session$datapath)
        if (isTRUE(sd$version >= 2L) && !is.null(sd$fields_store)) {
          # v2: multi-field session
          store <- sd$fields_store
          cur_key <- sd$active_field_key %||% names(store)[1]
          if (!is.null(sd$field_counter)) field_counter(sd$field_counter)
          fields_store(store)
          active_field_key(cur_key)
          updateSelectInput(session, "active_field_key",
            choices = field_choices(store), selected = cur_key
          )
          restore_field(store[[cur_key]])
          session_msg(paste0(
            "Session loaded: ", length(store), " field(s)",
            " (saved ", format(sd$saved_at, "%b %d %Y %H:%M"), ")"
          ))
        } else {
          # v1: legacy single-field session
          s <- sd$setup
          fd <- list(
            setup = list(
              field_id = s$field_id, crop = s$crop,
              date_range = s$date_range, lat = s$lat, lon = s$lon,
              app_rate = s$app_rate, initial_water = s$initial_water,
              allowable_dryness = s$allowable_dryness,
              field_capacity = s$field_capacity, pwp = s$pwp,
              root_depth_ft = s$root_depth_ft %||% 4.0,
              openet_model = s$openet_model,
              download_all_models = s$download_all_models,
              count_precip_effective = s$count_precip_effective
            ),
            irrigation_data = sd$irrigation_data,
            openet_data = sd$openet_data,
            eto_data = sd$eto_data,
            openet_location = sd$openet_location,
            openet_status_text = "Session loaded.",
            ssurgo_status_text = ""
          )
          store <- list(field_1 = fd)
          field_counter(1L)
          fields_store(store)
          active_field_key("field_1")
          updateSelectInput(session, "active_field_key",
            choices = field_choices(store), selected = "field_1"
          )
          restore_field(fd)
          session_msg(paste0("Session loaded: ", s$field_id, " (saved ", format(sd$saved_at, "%b %d %Y %H:%M"), ")"))
        }
      },
      error = function(e) {
        session_msg(paste("Error loading session:", conditionMessage(e)))
      }
    )
  })
  # ── Irrigation Explorer ──────────────────────────────────────────────────────
  scenario_base <- reactive({
    bal <- balance()
    validate(need(nrow(bal) > 0, "Run the water balance first (fetch OpenET data)."))
    last <- tail(bal, 1)
    recent_nonzero <- bal[bal$eta_in > 0, ]
    avg_et <- if (nrow(recent_nonzero) >= 1) round(mean(tail(recent_nonzero$eta_in, 7)), 3) else 0
    list(
      cur_swc   = last$soil_water_content_in,
      fc        = last$field_capacity_in,
      mad       = last$allowable_dryness_in,
      pwp       = last$permanent_wilting_point_in,
      last_date = as.Date(last$date),
      avg_et    = avg_et
    )
  })

  observeEvent(scenario_base(),
    {
      updateNumericInput(session, "scenario_et", value = scenario_base()$avg_et)
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  observeEvent(input$reset_scenario_et, {
    updateNumericInput(session, "scenario_et", value = scenario_base()$avg_et)
  })

  # Open-Meteo 7-day ETo forecast — always fetches on lat/lon change.
  eto_forecast <- reactive({
    req(
      is.numeric(input$lat), is.numeric(input$lon),
      !is.na(input$lat), !is.na(input$lon)
    )
    tryCatch(
      fetch_eto_forecast(input$lat, input$lon),
      error = function(e) NULL
    )
  })

  output$eto_forecast_status <- renderUI({
    fc <- eto_forecast()
    if (is.null(fc)) {
      div(
        style = "margin-top: 4px; color: #888; font-size: 0.85em;",
        icon("exclamation-triangle"), " ETo forecast unavailable"
      )
    } else {
      div(
        style = "margin-top: 4px; color: #388E3C; font-size: 0.85em;",
        icon("check-circle"),
        sprintf(
          " Open-Meteo ETo forecast loaded (%s \u2013 %s)",
          format(min(fc$date), "%b %d"),
          format(max(fc$date), "%b %d, %Y")
        )
      )
    }
  })

  # Estimated Kc from OpenET data: mean(ETa) / mean(ETo) over last 7 non-zero days.
  est_kc <- reactive({
    bal <- balance()
    eto <- eto_data()
    req(nrow(bal) > 0, !is.null(eto), nrow(eto) > 0)
    df <- merge(bal[bal$eta_in > 0, ], eto[, c("date", "eto_in")], by = "date", all.x = TRUE)
    df <- tail(df[!is.na(df$eto_in) & df$eto_in > 0.001, ], 7)
    if (nrow(df) == 0) {
      return(NA_real_)
    }
    round(mean(df$eta_in / df$eto_in, na.rm = TRUE), 3)
  })

  scenario_forecast <- reactive({
    d <- scenario_base()
    n_days <- as.integer(input$scenario_days)
    proj_et <- max(0, input$scenario_et %||% 0)
    irrig_in <- max(0, input$scenario_irrig %||% 0)
    dates <- seq(d$last_date + 1L, by = "day", length.out = n_days)
    swc_no <- numeric(n_days)
    swc_with <- numeric(n_days)
    prev_no <- d$cur_swc
    prev_with <- min(d$fc, d$cur_swc + irrig_in)
    for (i in seq_len(n_days)) {
      prev_no <- max(0, prev_no - proj_et)
      prev_with <- max(0, prev_with - proj_et)
      swc_no[i] <- prev_no
      swc_with[i] <- prev_with
    }
    idx <- which(swc_no <= d$mad)
    days_to_mad <- if (length(idx)) idx[1] else NA_integer_
    list(
      df = data.frame(
        date = dates,
        swc_no = swc_no,
        swc_with = if (irrig_in > 0) swc_with else rep(NA_real_, n_days),
        stringsAsFactors = FALSE
      ),
      fc = d$fc,
      mad = d$mad,
      pwp = d$pwp,
      days_to_mad = days_to_mad,
      irrig_in = irrig_in,
      last_date = d$last_date
    )
  })

  # 7-day weather-based forecast using Open-Meteo ETo × Kc.
  forecast_7day <- reactive({
    fc_data <- eto_forecast()
    req(!is.null(fc_data), nrow(fc_data) > 0)
    d <- scenario_base()
    irrig_in <- max(0, input$scenario_irrig %||% 0)
    kc_val <- max(0.01, input$scenario_kc %||% 1)
    dates <- fc_data$date
    n <- nrow(fc_data)
    swc <- numeric(n)
    prev <- d$cur_swc
    for (i in seq_len(n)) {
      et_day <- max(0, kc_val * fc_data$eto_in[i])
      prev <- max(0, prev - et_day)
      swc[i] <- prev
    }
    idx <- which(swc <= d$mad)
    list(
      df = data.frame(
        date = dates, swc = swc, eto_in = fc_data$eto_in,
        et_est_in = round(kc_val * fc_data$eto_in, 4),
        stringsAsFactors = FALSE
      ),
      fc = d$fc,
      mad = d$mad,
      pwp = d$pwp,
      days_to_mad = if (length(idx)) idx[1] else NA_integer_,
      last_date = d$last_date,
      kc_used = kc_val
    )
  })

  output$forecast_7day_card <- renderUI({
    # Only show forecast if balance data ends within the last 7 days.
    base <- tryCatch(scenario_base(), error = function(e) NULL)
    if (is.null(base)) {
      return(NULL)
    }
    days_stale <- as.integer(Sys.Date() - base$last_date)
    if (days_stale > 7) {
      return(div(
        class = "wet-card",
        h4("Soil Water Depletion and ET using 7-Day ETo Forecast"),
        p(
          class = "help-text", style = "color: #888;",
          icon("info-circle"),
          sprintf(
            " The 7-day ETo forecast is not shown because your data ends %d days ago (%s). Update your end date to today to enable the weather-based forecast.",
            days_stale,
            format(base$last_date, "%b %d, %Y")
          )
        )
      ))
    }
    fc <- eto_forecast()
    if (is.null(fc)) {
      return(div(
        class = "wet-card",
        h4("7-Day ETo Forecast \u2014 Weather-Based View (Open-Meteo)"),
        p(
          class = "help-text", style = "color: #888;",
          icon("exclamation-triangle"), " Open-Meteo forecast unavailable. Check your internet connection."
        )
      ))
    }
    kc_est_val <- tryCatch(est_kc(), error = function(e) NA_real_)
    kc_default <- if (!is.na(kc_est_val)) kc_est_val else 1.0
    # staleness notice for the forecast
    stale_notice <- local({
      days_stale <- as.integer(Sys.Date() - base$last_date)
      last_str <- format(base$last_date, "%b %d, %Y")
      if (days_stale <= 1) {
        div(
          style = "margin-bottom: 8px; font-size: 13px; color: #388E3C;",
          icon("check-circle"),
          sprintf(" Soil water balance data is current through %s.", last_str)
        )
      } else if (days_stale <= 4) {
        div(
          style = "margin-bottom: 8px; font-size: 13px; color: #C62828;",
          icon("exclamation-triangle"),
          sprintf(
            " The forecast starts from the last date in your soil water balance data (%s, %d days ago). Soil water content may not reflect current conditions.",
            last_str, days_stale
          )
        )
      } else {
        div(
          class = "warn-box",
          style = "margin-bottom: 8px;",
          icon("exclamation-triangle"),
          sprintf(
            " The last date in your soil water balance data is %s (%d days ago). The starting soil water content used in this forecast is outdated — update your OpenET end date to today for an accurate projection.",
            last_str, days_stale
          )
        )
      }
    })
    div(
      class = "wet-card",
      h4("7-Day Soil Water Depletion & ET using ETo Forecast"),
      stale_notice,
      uiOutput("eto_forecast_status"),
      br(),
      fluidRow(
        column(
          3,
          numericInput("scenario_kc", "Crop coefficient (Kc)",
            value = kc_default, min = 0.01, max = 3, step = 0.01
          )
        ),
        column(
          5,
          br(),
          actionButton("reset_kc", "Reset to 7-day avg (ET/ETo)",
            class = "btn btn-default btn-xs", style = "margin-top: 4px;"
          )
        )
      ),
      withSpinner(plotlyOutput("forecast_plot", height = 380), type = 6, color = "#F57C00", size = 0.7)
    )
  })

  # Auto-seed Kc when OpenET data loads or changes.
  observeEvent(est_kc(),
    {
      kc_est_val <- tryCatch(est_kc(), error = function(e) NA_real_)
      if (!is.na(kc_est_val)) {
        updateNumericInput(session, "scenario_kc", value = kc_est_val)
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  observeEvent(input$reset_kc, {
    kc_est_val <- tryCatch(est_kc(), error = function(e) NA_real_)
    if (!is.na(kc_est_val)) {
      updateNumericInput(session, "scenario_kc", value = kc_est_val)
    }
  })

  output$forecast_plot <- renderPlotly({
    sc <- forecast_7day()
    df <- sc$df
    y2_max <- max(df$eto_in, na.rm = TRUE) * 2
    p <- plot_ly(df, x = ~date) |>
      add_bars(
        y = ~eto_in, name = "Forecasted ETo",
        yaxis = "y2", opacity = 0.30,
        marker = list(color = "#F57C00", line = list(width = 0)),
        hovertemplate = "ETo: %{y:.2f} in.<extra></extra>"
      ) |>
      add_bars(
        y = ~et_est_in, name = "Est. Crop ET (Kc \u00d7 ETo)",
        yaxis = "y2", opacity = 0.50,
        marker = list(color = "#388E3C", line = list(width = 0)),
        hovertemplate = "Est. ET: %{y:.2f} in.<extra></extra>"
      ) |>
      add_lines(
        y = ~swc, name = "Forecast SWC",
        line = list(color = "#1565C0", width = 4),
        hovertemplate = "SWC: %{y:.2f} in.<extra></extra>"
      ) |>
      add_lines(
        x = ~date, y = rep(sc$fc, nrow(df)), name = "Field Capacity",
        line = list(color = "#388E3C", width = 3, dash = "dash"),
        hovertemplate = paste0("Field Capacity: ", round(sc$fc, 2), " in.<extra></extra>")
      ) |>
      add_lines(
        x = ~date, y = rep(sc$mad, nrow(df)), name = "MAD Threshold",
        line = list(color = "#ffa200", width = 3, dash = "dash"),
        hovertemplate = paste0("MAD: ", round(sc$mad, 2), " in.<extra></extra>")
      ) |>
      add_lines(
        x = ~date, y = rep(sc$pwp, nrow(df)), name = "Perm. Wilting Point",
        line = list(color = "#B71C1C", width = 3, dash = "dash"),
        hovertemplate = paste0("Perm. Wilting Point: ", round(sc$pwp, 2), " in.<extra></extra>")
      )
    if (!is.na(sc$days_to_mad)) {
      irrig_date <- sc$last_date + sc$days_to_mad
      p <- p |> add_lines(
        x = c(irrig_date, irrig_date), y = c(0, sc$fc),
        name = paste0("Irrigate by ", format(irrig_date, "%b %d")),
        line = list(color = "#828282", width = 3),
        hovertemplate = paste0("Irrigate by: ", format(irrig_date, "%b %d, %Y"), "<extra></extra>")
      )
    }
    y_max_fc <- sc$fc * 1.18
    p |> layout(
      hovermode = "x unified",
      xaxis = list(
        type = "date", autorange = TRUE, title = "",
        hoverformat = "%b %d, %Y",
        showspikes = TRUE, spikemode = "across", spikesnap = "cursor"
      ),
      yaxis = list(
        title = "Soil Water Content (SWC), in.",
        range = c(-0.02 * sc$fc, y_max_fc),
        zeroline = FALSE
      ),
      barmode = "overlay",
      yaxis2 = list(
        title = "Inches", overlaying = "y", side = "right",
        range = c(0, y2_max), showgrid = FALSE
      ),
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.12),
      font = list(size = 13),
      margin = list(r = 60)
    )
  })

  output$sc_cur_swc <- renderText({
    sprintf("%.2f in.", scenario_base()$cur_swc)
  })
  output$sc_available <- renderText({
    d <- scenario_base()
    sprintf("%.2f in.", max(0, d$cur_swc - d$mad))
  })
  output$sc_proj_et <- renderText({
    sprintf("%.2f in./day", max(0, input$scenario_et %||% 0))
  })
  output$sc_days_to_mad <- renderText({
    sc <- scenario_forecast()
    if (is.na(sc$days_to_mad)) {
      paste0("> ", input$scenario_days, " days")
    } else {
      irrig_date <- sc$last_date + sc$days_to_mad
      paste0(sc$days_to_mad, " days (", format(irrig_date, "%m/%d/%y"), ")")
    }
  })

  output$scenario_plot <- renderPlotly({
    sc <- scenario_forecast()
    df <- sc$df
    p <- plot_ly(df, x = ~date) |>
      add_lines(
        y = ~swc_no, name = "Projected SWC (no irrigation)",
        line = list(color = "#1565C0", width = 4),
        hovertemplate = "SWC: %{y:.2f} in.<extra></extra>"
      )
    if (sc$irrig_in > 0) {
      p <- p |> add_lines(
        y = ~swc_with,
        name = paste0("SWC w/", round(sc$irrig_in, 2), " in. irrigation"),
        line = list(color = "#1565C0", width = 4, dash = "dot"),
        hovertemplate = "SWC w/ irrigation: %{y:.2f} in.<extra></extra>"
      )
    }
    p <- p |>
      add_lines(
        x = ~date, y = rep(sc$fc, nrow(df)), name = "Field Capacity",
        line = list(color = "#388E3C", width = 4, dash = "dash"),
        hovertemplate = paste0("Field Capacity: ", round(sc$fc, 2), " in.<extra></extra>")
      ) |>
      add_lines(
        x = ~date, y = rep(sc$mad, nrow(df)), name = "MAD Threshold",
        line = list(color = "#ffa200", width = 4, dash = "dash"),
        hovertemplate = paste0("MAD: ", round(sc$mad, 2), " in.<extra></extra>")
      ) |>
      add_lines(
        x = ~date, y = rep(sc$pwp, nrow(df)), name = "Perm. Wilting Point",
        line = list(color = "#B71C1C", width = 4, dash = "dash"),
        hovertemplate = paste0("Perm. Wilting Point: ", round(sc$pwp, 2), " in.<extra></extra>")
      )
    if (!is.na(sc$days_to_mad)) {
      irrig_date <- sc$last_date + sc$days_to_mad
      p <- p |> add_lines(
        x = c(irrig_date, irrig_date), y = c(0, sc$fc),
        name = paste0("Irrigate by ", format(irrig_date, "%b %d")),
        line = list(color = "#828282", width = 3),
        hovertemplate = paste0("Irrigate by: ", format(irrig_date, "%b %d, %Y"), "<extra></extra>")
      )
    }
    y_max <- sc$fc * 1.18
    p |> layout(
      hovermode = "x unified",
      xaxis = list(
        type = "date", autorange = TRUE,
        title = "",
        hoverformat = "%b %d, %Y",
        showspikes = TRUE, spikemode = "across", spikesnap = "cursor"
      ),
      yaxis = list(
        title = "Soil Water Content (SWC), in.",
        range = c(-0.02 * sc$fc, y_max),
        zeroline = FALSE
      ),
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.10),
      font = list(size = 13),
      margin = list(r = 20)
    )
  })
}

shinyApp(ui, server)
