# OpenET wETgraph Shiny app
# Run from this folder with: shiny::runApp()

library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(leaflet)
library(readxl)
library(openxlsx)

source("R/openet_utils.R")

models <- c("GEESEBAL", "SSEBOP", "SIMS", "DISALEXI", "PTJPL", "EEMETRIC", "ENSEMBLE")
default_start <- as.Date("2024-01-01")
default_end <- as.Date("2024-12-31")

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
      yaxis = list(showspikes = TRUE, spikemode = "across", spikesnap = "cursor")
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

    trace_name <- p$x$data[[i]]$name
    if (is.null(trace_name) || is.na(trace_name) || !nzchar(trace_name)) {
      trace_name <- "Value"
    }
    trace_name <- gsub("<[^>]+>", "", trace_name)
    trace_name <- trimws(trace_name)
    trace_name <- gsub(",\\s*1$", "", trace_name)
    trace_name <- gsub("\\(([^)]+),\\s*1\\)", "(\\1)", trace_name)
    p$x$data[[i]]$name <- trace_name

    date_labels <- format_trace_dates(p$x$data[[i]]$x)
    if (!is.null(date_labels)) {
      p$x$data[[i]]$customdata <- date_labels
      date_line <- if (i == 1) "Date: %{customdata}<br>" else ""
    } else {
      date_line <- if (i == 1) "Date: %{x}<br>" else ""
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
.ok-box { background: #ecfdf3; border: 1px solid #abefc6; border-radius: 10px; padding: 10px 12px; margin-bottom: 14px; }
.help-text { color: #667085; font-size: 13px; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  titlePanel("OpenET irrigation water balance dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Initial Setup"),
      textInput("field_id", "Field ID", default_setup$field_id),
      passwordInput("api_key", "OpenET API key", default_setup$api_key),
      dateRangeInput("date_range", "Date range", start = default_setup$start_date, end = default_setup$end_date),
      textInput("crop", "Crop description", default_setup$crop_description),
      fluidRow(
        column(6, numericInput("lat", "Latitude", default_setup$latitude, step = 0.0001)),
        column(6, numericInput("lon", "Longitude", default_setup$longitude, step = 0.0001))
      ),
      numericInput("app_rate", "Net application rate, in/hr", default_setup$application_rate_in_hr, step = 0.0001),
      numericInput("field_capacity", "Field capacity, in.", default_setup$field_capacity_in, step = 0.1),
      numericInput("initial_water", "Initial water content, in.", default_setup$initial_water_content_in, step = 0.1),
      numericInput("allowable_dryness", "Allowable dryness, in.", default_setup$allowable_dryness_in, step = 0.1),
      numericInput("pwp", "Permanent wilting point, in.", default_setup$permanent_wilting_point_in, step = 0.1),
      hr(),
      h4("OpenET"),
      selectInput("openet_model", "Model for charts", choices = models, selected = default_setup$selected_model),
      checkboxInput("download_all_models", "Download all ET models", default_setup$download_all_models),
      actionButton("fetch_openet", "Update OpenET data", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("openet_status")
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
          div(class = "wet-card", h4(textOutput("soil_title")), plotlyOutput("soil_plot", height = 380)),
          div(class = "wet-card", h4(textOutput("eta_title")), plotlyOutput("eta_plot", height = 330)),
          div(class = "wet-card", h4(textOutput("deep_title")), plotlyOutput("deep_plot", height = 330))
        ),
        tabPanel(
          "Irrigation Amounts",
          br(),
          div(
            class = "wet-card",
            p(class = "help-text", "Excel-equivalent rule: Net Water Applied is the input used by the water balance. OpenET precipitation is displayed separately unless the option below is checked."),
            checkboxInput("count_precip_effective", "Count all OpenET precipitation as effective water", value = default_setup$count_precip_effective),
            p(class = "help-text", "When checked, the soil water balance credits OpenET precipitation in addition to entered irrigation: irrigation + precipitation - ETa."),
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
          leafletOutput("field_map", height = 520)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  irrigation_data <- reactiveVal(make_default_irrigation_range(default_start, default_end))
  openet_data <- reactiveVal(make_empty_openet_range(default_start, default_end))
  openet_status <- reactiveVal("No OpenET API request made yet.")
  openet_location <- reactiveVal(list(latitude = NA_real_, longitude = NA_real_, start_date = NA, end_date = NA))
  irrig_version <- reactiveVal(0L)
  irrig_proxy <- dataTableProxy("irrig_table")

  output$openet_status <- renderText(openet_status())

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
      irrigation_data(sync_irrigation_to_range(irrigation_data(), start_date, end_date))
      openet_data(normalize_openet_columns(openet_data(), start_date, end_date))
      updateDateInput(session, "new_irrig_date", value = start_date, min = start_date, max = end_date)
      irrig_version(irrig_version() + 1L)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$fetch_openet, {
    req(input$api_key, input$date_range, input$lat, input$lon)
    tryCatch(
      {
        start_date <- as.Date(input$date_range[1])
        end_date <- as.Date(input$date_range[2])
        fetch_models <- if (isTRUE(input$download_all_models)) models else input$openet_model
        if (!"ENSEMBLE" %in% fetch_models) fetch_models <- unique(c(fetch_models, "ENSEMBLE"))
        withProgress(message = "Fetching OpenET data", value = 0, {
          et_list <- list()
          for (i in seq_along(fetch_models)) {
            incProgress(0.75 / length(fetch_models), detail = paste("Fetching", fetch_models[i], "ET"))
            et_list[[fetch_models[i]]] <- fetch_openet_point(input$api_key, input$lon, input$lat, start_date, end_date, model = fetch_models[i], variable = "ET")
          }
          incProgress(0.15, detail = "Fetching precipitation")
          pr <- fetch_openet_point(input$api_key, input$lon, input$lat, start_date, end_date, model = "ENSEMBLE", variable = "PR")
          out <- combine_openet_models(et_list, pr, start_date, end_date)
          openet_data(out)
          irrigation_data(sync_irrigation_to_range(irrigation_data(), start_date, end_date))
          openet_location(list(latitude = input$lat, longitude = input$lon, start_date = start_date, end_date = end_date))
          incProgress(1)
          openet_status(sprintf("Fetched %s daily rows from %s to %s for %s ET model(s) plus precipitation.", nrow(out), start_date, end_date, length(fetch_models)))
          irrig_version(irrig_version() + 1L)
        })
      },
      error = function(e) openet_status(paste("OpenET API error:", conditionMessage(e)))
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
    compute_water_balance(irrigation_data(), openet_data(), setup_values())
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

  output$soil_title <- renderText(sprintf("Soil Water Content, in. for %s @ %s", input$crop, input$field_id))
  output$eta_title <- renderText(sprintf("Σ Evapotranspiration, in., for %s @ %s %s", input$crop, input$field_id, format(as.Date(input$date_range[1]), "%Y")))
  output$deep_title <- renderText(sprintf("Σ Deep Percolation & Leaching Fraction for %s @ %s %s", input$crop, input$field_id, format(as.Date(input$date_range[1]), "%Y")))

  output$location_warning <- renderUI({
    loc <- openet_location()
    s <- setup_values()
    if (is.na(loc$latitude)) {
      return(div(class = "warn-box", "OpenET has not been refreshed in this session. Click 'Update OpenET data' after setting the date range and coordinates."))
    }
    same <- isTRUE(all.equal(as.numeric(input$lat), loc$latitude, tolerance = 1e-7)) &&
      isTRUE(all.equal(as.numeric(input$lon), loc$longitude, tolerance = 1e-7)) &&
      identical(as.Date(s$start_date), as.Date(loc$start_date)) && identical(as.Date(s$end_date), as.Date(loc$end_date))
    if (same) div(class = "ok-box", sprintf("OpenET data match setup: %.5f, %.5f from %s to %s", loc$latitude, loc$longitude, as.Date(loc$start_date), as.Date(loc$end_date))) else div(class = "warn-box", "Date range or coordinates changed. Click 'Update OpenET data' to refresh the API results.")
  })

  output$eta_plot <- renderPlotly({
    df <- make_excel_plot_balance(balance(), setup_values())
    p <- ggplot(df, aes(x = date)) +
      geom_line(aes(y = cumulative_eta_in, color = "Σ ETa, in."), linewidth = 1.0) +
      geom_line(aes(y = cumulative_applied_in, color = "Σ Applied Water, in."), linewidth = 1.0, linetype = "dashed") +
      geom_line(aes(y = applied_minus_eta_in, color = "Σ Applied − Σ ETa"), linewidth = 0.8, linetype = "dotdash") +
      scale_color_manual(values = c(
        "Σ ETa, in." = "#C62828",
        "Σ Applied Water, in." = "#1565C0",
        "Σ Applied − Σ ETa" = "#2E7D32"
      )) +
      labs(x = NULL, y = "Cumulative Water, in.", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.text = element_text(size = 10))
    plotly_date_layout(clean_plotly_hover(ggplotly(p)))
  })

  output$soil_plot <- renderPlotly({
    df <- make_excel_plot_balance(balance(), setup_values())
    p <- ggplot(df, aes(x = date)) +
      geom_col(aes(y = precip_plot_in, fill = "Precipitation, in."), alpha = 0.55, na.rm = TRUE) +
      geom_point(aes(y = applied_plot_in, color = "Applied Water Event, in."), size = 3, shape = 24, na.rm = TRUE) +
      geom_line(aes(y = field_capacity_in, color = "Field Capacity, in."), linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(y = allowable_dryness_in, color = "Allowable Dryness, in."), linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(y = permanent_wilting_point_in, color = "Perm. Wilting Point, in."), linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(y = soil_water_graph_in, color = "Soil Water Content, in."), linewidth = 1.1) +
      scale_color_manual(values = c(
        "Soil Water Content, in." = "#1565C0",
        "Field Capacity, in." = "#2E7D32",
        "Allowable Dryness, in." = "#F57F17",
        "Perm. Wilting Point, in." = "#C62828",
        "Applied Water Event, in." = "#0288D1"
      )) +
      scale_fill_manual(values = c("Precipitation, in." = "#90CAF9")) +
      labs(x = NULL, y = "Soil Water, in.", color = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.text = element_text(size = 10))
    plotly_date_layout(clean_plotly_hover(ggplotly(p)))
  })

  output$deep_plot <- renderPlotly({
    df <- make_excel_plot_balance(balance(), setup_values())
    p <- ggplot(df, aes(x = date)) +
      geom_col(aes(y = precip_plot_in, fill = "Precipitation, in."),
        alpha = 0.55, na.rm = TRUE
      ) +
      geom_line(aes(
        y = cumulative_deep_percolated_in,
        color = "Σ Deep Percolation, in."
      ), linewidth = 1.2) +
      geom_line(
        aes(
          y = leaching_fraction,
          color = "Leaching Fraction"
        ),
        linewidth = 1.0, linetype = "dashed", na.rm = TRUE
      ) +
      scale_color_manual(values = c(
        "Σ Deep Percolation, in." = "#1565C0",
        "Leaching Fraction" = "#C62828"
      )) +
      scale_fill_manual(values = c("Precipitation, in." = "#90CAF9")) +
      labs(x = NULL, y = "Inches / Fraction", color = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.text = element_text(size = 10))
    plotly_date_layout(clean_plotly_hover(ggplotly(p)))
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
          "Soil Water Deficit, in.", "Notes / Flow Meter"
        ),
        options = list(pageLength = 15, scrollX = TRUE, ordering = FALSE)
      ) |>
        formatRound(columns = c("net_water_applied_in", "cumulative_applied_in", "equivalent_irrigation_hours", "precip_openet_in", "soil_water_deficit_in"), digits = 3)
    },
    server = TRUE
  )

  output$balance_table <- renderDT({
    df <- balance()
    pretty_names <- c(
      date = "Date", julian_date = "Julian Date", eta_in = "ETa, in.",
      cumulative_eta_in = "Σ ETa, in.",
      questionable_cumulative_eta_in = "Questionable (0 ETa) Cum. ETa",
      net_water_applied_in = "Inches Applied",
      cumulative_applied_in = "Σ Inches Applied",
      applied_minus_eta_in = "Σ Applied − Σ ETa",
      soil_water_content_in = "Soil Water Content, in.",
      field_capacity_in = "Field Capacity, in.",
      allowable_dryness_in = "Allowable Dryness, in.",
      permanent_wilting_point_in = "Perm. Wilting Point, in.",
      soil_water_graph_in = "Soil Water (chart), in.",
      questionable_soil_water_in = "Questionable Soil Water",
      deep_percolated_water_in = "Deep Percolation Water, in.",
      cumulative_deep_percolated_in = "Σ Deep Percolation, in.",
      leaching_fraction = "Leaching Fraction",
      precip_in = "Precipitation, in.",
      applied_plot_in = "Applied Water Events, in.",
      precip_plot_in = "Precip. Events, in.",
      soil_water_deficit_in = "Soil Water Deficit, in.",
      remaining_storage_capacity_in = "Remaining Storage, in.",
      equivalent_irrigation_hours = "Equiv. Irrig. Hours",
      selected_model = "ET Model",
      notes = "Notes"
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

  output$field_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      setView(lng = input$lon, lat = input$lat, zoom = 14) |>
      addMarkers(lng = input$lon, lat = input$lat, popup = paste(input$field_id, "<br>", input$crop))
  })

  output$download_balance <- downloadHandler(
    filename = function() paste0("openet_wetgraph_", input$field_id, "_", as.Date(input$date_range[1]), "_", as.Date(input$date_range[2]), ".xlsx"),
    content = function(file) write_balance_export(balance(), irrigation_data(), openet_data(), setup_values(), file)
  )
}

shinyApp(ui, server)
