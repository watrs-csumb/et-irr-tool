# Utility functions for OpenET wETgraph Shiny migration
# Mirrors workbook sheets: Initial_Setup, Irrigation Amounts, OpenET, Calcs, Map

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

excel_date_to_date <- function(x) {
  as.Date(as.numeric(x), origin = "1899-12-30")
}

as_date_flexible <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(excel_date_to_date(x))
  }
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out))) out <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  if (all(is.na(out))) out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  out
}

range_dates <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) stop("Choose a valid date range.")
  seq.Date(start_date, end_date, by = "day")
}

make_default_irrigation_range <- function(start_date, end_date) {
  data.frame(
    date = range_dates(start_date, end_date),
    net_water_applied_in = 0,
    notes = "",
    stringsAsFactors = FALSE
  )
}

make_empty_openet_range <- function(start_date, end_date) {
  data.frame(
    date = range_dates(start_date, end_date),
    GEESEBAL = NA_real_,
    SSEBOP = NA_real_,
    SIMS = NA_real_,
    DISALEXI = NA_real_,
    PTJPL = NA_real_,
    EEMETRIC = NA_real_,
    ENSEMBLE = 0,
    precip_in = 0,
    stringsAsFactors = FALSE
  )
}

sync_irrigation_to_range <- function(irrigation, start_date, end_date) {
  scaffold <- make_default_irrigation_range(start_date, end_date)
  if (is.null(irrigation) || nrow(irrigation) == 0) {
    return(scaffold)
  }
  old <- data.frame(
    date = as_date_flexible(irrigation$date),
    net_water_applied_in = safe_numeric(irrigation$net_water_applied_in),
    notes = as.character(irrigation$notes %||% ""),
    stringsAsFactors = FALSE
  )
  old$net_water_applied_in[is.na(old$net_water_applied_in)] <- 0
  old$notes[is.na(old$notes)] <- ""
  old <- old[!is.na(old$date), ]
  if (nrow(old) > 0) old <- aggregate(cbind(net_water_applied_in) ~ date, old, sum, na.rm = TRUE)
  notes <- data.frame(date = as_date_flexible(irrigation$date), notes = as.character(irrigation$notes %||% ""), stringsAsFactors = FALSE)
  notes <- notes[!is.na(notes$date), ]
  if (nrow(notes) > 0) notes <- aggregate(notes ~ date, notes, function(x) paste(x[nzchar(x)], collapse = "; "))
  out <- merge(scaffold["date"], old, by = "date", all.x = TRUE, sort = TRUE)
  out <- merge(out, notes, by = "date", all.x = TRUE, sort = TRUE)
  out$net_water_applied_in[is.na(out$net_water_applied_in)] <- 0
  out$notes[is.na(out$notes)] <- ""
  out
}

read_setup_from_workbook <- function(path) {
  setup <- readxl::read_excel(path, sheet = "Initial_Setup", col_names = FALSE, .name_repair = "minimal")
  get_cell <- function(row, col) setup[[col]][row]
  calc <- readxl::read_excel(path, sheet = "Calcs", col_names = FALSE, .name_repair = "minimal", n_max = 8)
  irrig <- readxl::read_excel(path, sheet = "Irrigation Amounts", col_names = FALSE, .name_repair = "minimal", n_max = 370)

  first_date <- as_date_flexible(irrig[[1]][2])
  last_date <- as_date_flexible(irrig[[1]][min(nrow(irrig), 368)])
  if (is.na(first_date)) first_date <- as.Date(sprintf("%s-01-01", format(Sys.Date(), "%Y")))
  if (is.na(last_date)) last_date <- as.Date(sprintf("%s-12-31", format(first_date, "%Y")))

  dropdown_index <- safe_numeric(calc[[26]][1] %||% 7)
  model_lookup <- c("GEESEBAL", "SSEBOP", "SIMS", "DISALEXI", "PTJPL", "EEMETRIC", "ENSEMBLE")
  selected_model <- model_lookup[pmin(pmax(dropdown_index, 1), 7)] %||% "ENSEMBLE"
  download_all_models <- as.logical(safe_numeric(calc[[29]][2] %||% 0))
  if (is.na(download_all_models)) download_all_models <- FALSE

  list(
    field_id = as.character(get_cell(1, 2) %||% "Example"),
    api_key = as.character(get_cell(1, 6) %||% ""),
    application_rate_in_hr = safe_numeric(get_cell(3, 2) %||% 0.1156),
    latitude = safe_numeric(get_cell(6, 2) %||% 36.1035),
    longitude = safe_numeric(get_cell(6, 3) %||% -120.10768),
    crop_description = as.character(get_cell(8, 2) %||% "Almonds"),
    field_capacity_in = safe_numeric(get_cell(11, 2) %||% 4.6),
    initial_water_content_in = safe_numeric(get_cell(12, 2) %||% 3.5),
    allowable_dryness_in = safe_numeric(get_cell(13, 2) %||% 3.5),
    permanent_wilting_point_in = safe_numeric(get_cell(14, 2) %||% 2.4),
    start_date = first_date,
    end_date = last_date,
    selected_model = selected_model,
    download_all_models = download_all_models
  )
}

read_irrigation_from_workbook <- function(path, start_date, end_date) {
  raw <- readxl::read_excel(path, sheet = "Irrigation Amounts", col_names = TRUE, .name_repair = "unique")
  if (ncol(raw) < 2) {
    return(make_default_irrigation_range(start_date, end_date))
  }
  names(raw)[1:min(6, ncol(raw))] <- c("date", "net_water_applied_in", "equiv_irrigation_hours", "precip_openet_in", "soil_water_deficit_in", "notes")[1:min(6, ncol(raw))]
  out <- data.frame(
    date = as_date_flexible(raw$date),
    net_water_applied_in = safe_numeric(raw$net_water_applied_in),
    notes = if ("notes" %in% names(raw)) as.character(raw$notes) else "",
    stringsAsFactors = FALSE
  )
  out$net_water_applied_in[is.na(out$net_water_applied_in)] <- 0
  out$notes[is.na(out$notes)] <- ""
  out <- out[!is.na(out$date), ]
  sync_irrigation_to_range(out, start_date, end_date)
}

read_openet_from_workbook <- function(path, start_date, end_date) {
  raw <- readxl::read_excel(path, sheet = "OpenET", col_names = FALSE, .name_repair = "minimal")
  if (nrow(raw) < 3) {
    return(make_empty_openet_range(start_date, end_date))
  }
  pairs <- list(
    GEESEBAL = c(1, 2), SSEBOP = c(3, 4), SIMS = c(5, 6), DISALEXI = c(7, 8),
    PTJPL = c(9, 10), EEMETRIC = c(11, 12), ENSEMBLE = c(13, 14), precip_in = c(15, 16)
  )
  out <- make_empty_openet_range(start_date, end_date)
  for (nm in names(pairs)) {
    ix <- pairs[[nm]]
    if (max(ix) <= ncol(raw)) {
      tmp <- data.frame(date = as_date_flexible(raw[[ix[1]]][-c(1, 2)]), value = safe_numeric(raw[[ix[2]]][-c(1, 2)]))
      tmp <- tmp[!is.na(tmp$date), ]
      if (nrow(tmp) > 0) {
        tmp <- aggregate(value ~ date, tmp, sum, na.rm = TRUE)
        m <- match(out$date, tmp$date)
        out[[nm]][!is.na(m)] <- tmp$value[m[!is.na(m)]]
      }
    }
  }
  out$precip_in[is.na(out$precip_in)] <- 0
  out$ENSEMBLE[is.na(out$ENSEMBLE)] <- 0
  out
}

fetch_openet_point <- function(api_key, longitude, latitude, start_date, end_date,
                               model = "ENSEMBLE", variable = "ET",
                               reference_et = "gridMET", units = "in") {
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Package 'httr2' is required. Install it with install.packages('httr2').")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required. Install it with install.packages('readr').")
  api_key <- trimws(api_key %||% "")
  if (!nzchar(api_key)) stop("Enter an OpenET API key before fetching data.")
  payload <- list(
    date_range = list(as.character(as.Date(start_date)), as.character(as.Date(end_date))),
    interval = "daily",
    geometry = list(as.numeric(longitude), as.numeric(latitude)),
    model = toupper(model),
    variable = toupper(variable),
    reference_et = reference_et,
    units = units,
    file_format = "CSV"
  )
  response <- httr2::request("https://openet-api.org/raster/timeseries/point") |>
    httr2::req_method("POST") |>
    httr2::req_headers(accept = "text/csv, application/json", Authorization = api_key, `Content-Type` = "application/json") |>
    httr2::req_body_json(payload, auto_unbox = TRUE) |>
    httr2::req_timeout(300) |>
    httr2::req_perform()
  body_text <- httr2::resp_body_string(response)
  status <- httr2::resp_status(response)
  if (status < 200 || status >= 300) stop(sprintf("OpenET request failed with HTTP %s: %s", status, body_text))
  parse_openet_response(body_text, variable = variable, model = model)
}

parse_openet_response <- function(body_text, variable = "ET", model = "ENSEMBLE") {
  variable <- toupper(variable)
  value_name <- if (variable == "PR") "precip_in" else if (variable == "ETO") "eto_in" else toupper(model)
  body_text <- trimws(body_text)
  if (!nzchar(body_text)) stop("OpenET returned an empty response.")
  if (startsWith(body_text, "[") || startsWith(body_text, "{")) {
    df <- as.data.frame(jsonlite::fromJSON(body_text, flatten = TRUE))
  } else {
    df <- readr::read_csv(I(body_text), show_col_types = FALSE)
  }
  if (nrow(df) == 0) stop("OpenET returned no rows.")
  nm <- tolower(gsub("[^a-z0-9]+", "_", names(df)))
  names(df) <- nm
  date_col <- intersect(c("date", "time", "datetime", "start_date", "dt", "system_time_start"), nm)[1]
  if (is.na(date_col)) date_col <- nm[grepl("date|time", nm)][1]
  value_candidates <- unique(c(tolower(variable), "value", "et", "et_in", "pr", "precip", "precipitation", "ensemble", tolower(model)))
  value_col <- intersect(value_candidates, nm)[1]
  if (is.na(value_col)) {
    numeric_cols <- nm[vapply(df, function(x) suppressWarnings(any(!is.na(as.numeric(x)))), logical(1))]
    numeric_cols <- setdiff(numeric_cols, c("row", "column", "lat", "latitude", "lon", "longitude", "x", "y"))
    value_col <- tail(numeric_cols, 1)
  }
  if (is.na(date_col) || is.na(value_col)) stop("Could not identify date/value columns in the OpenET response.")
  out <- data.frame(date = as_date_flexible(df[[date_col]]), value = safe_numeric(df[[value_col]]), stringsAsFactors = FALSE)
  out$value[is.na(out$value)] <- 0
  out <- out[!is.na(out$date), ]
  out <- aggregate(value ~ date, out, sum, na.rm = TRUE)
  names(out)[2] <- value_name
  out[order(out$date), ]
}

combine_openet_models <- function(et_list, pr = NULL, start_date, end_date) {
  out <- make_empty_openet_range(start_date, end_date)
  for (nm in names(et_list)) {
    tmp <- et_list[[nm]]
    if (is.null(tmp) || nrow(tmp) == 0) next
    val <- setdiff(names(tmp), "date")[1]
    m <- match(out$date, as.Date(tmp$date))
    out[[toupper(nm)]][!is.na(m)] <- safe_numeric(tmp[[val]][m[!is.na(m)]])
  }
  if (!is.null(pr) && nrow(pr) > 0 && "precip_in" %in% names(pr)) {
    m <- match(out$date, as.Date(pr$date))
    out$precip_in[!is.na(m)] <- safe_numeric(pr$precip_in[m[!is.na(m)]])
  }
  model_cols <- c("GEESEBAL", "SSEBOP", "SIMS", "DISALEXI", "PTJPL", "EEMETRIC", "ENSEMBLE")
  fallback <- rowMeans(out[setdiff(model_cols, "ENSEMBLE")], na.rm = TRUE)
  fallback[is.nan(fallback)] <- 0
  out$ENSEMBLE[is.na(out$ENSEMBLE)] <- fallback[is.na(out$ENSEMBLE)]
  out$ENSEMBLE[is.na(out$ENSEMBLE) | is.nan(out$ENSEMBLE)] <- 0
  out$precip_in[is.na(out$precip_in)] <- 0
  out[, c("date", model_cols, "precip_in")]
}

normalize_openet_columns <- function(openet, start_date, end_date) {
  scaffold <- make_empty_openet_range(start_date, end_date)
  if (is.null(openet) || nrow(openet) == 0) {
    return(scaffold)
  }
  names(openet) <- gsub("[^A-Za-z0-9_]+", "_", names(openet))
  for (m in c("GEESEBAL", "SSEBOP", "SIMS", "DISALEXI", "PTJPL", "EEMETRIC", "ENSEMBLE")) {
    found <- names(openet)[toupper(names(openet)) == m]
    if (length(found) > 0) names(openet)[names(openet) == found[1]] <- m
  }
  lower <- tolower(names(openet))
  if ("precip" %in% lower) names(openet)[lower == "precip"] <- "precip_in"
  if ("precipitation" %in% lower) names(openet)[lower == "precipitation"] <- "precip_in"
  openet$date <- as.Date(openet$date)
  out <- merge(scaffold["date"], openet, by = "date", all.x = TRUE, sort = TRUE)
  for (nm in setdiff(names(scaffold), "date")) if (!nm %in% names(out)) out[[nm]] <- scaffold[[nm]]
  out$precip_in[is.na(out$precip_in)] <- 0
  out$ENSEMBLE[is.na(out$ENSEMBLE)] <- 0
  out[, names(scaffold)]
}

compute_water_balance <- function(irrigation, openet, setup) {
  start_date <- as.Date(setup$start_date)
  end_date <- as.Date(setup$end_date)
  dates <- data.frame(date = range_dates(start_date, end_date))
  openet <- normalize_openet_columns(openet, start_date, end_date)
  selected_model <- toupper(setup$selected_model %||% "ENSEMBLE")
  if (!selected_model %in% names(openet)) selected_model <- "ENSEMBLE"

  openet_daily <- data.frame(date = openet$date, eta_in = safe_numeric(openet[[selected_model]]), precip_in = safe_numeric(openet$precip_in))
  openet_daily$eta_in[is.na(openet_daily$eta_in)] <- 0
  openet_daily$precip_in[is.na(openet_daily$precip_in)] <- 0
  openet_daily <- aggregate(cbind(eta_in, precip_in) ~ date, openet_daily, sum, na.rm = TRUE)

  irrigation <- sync_irrigation_to_range(irrigation, start_date, end_date)
  irrigation_sum <- aggregate(net_water_applied_in ~ date, irrigation, sum, na.rm = TRUE)
  notes_sum <- aggregate(notes ~ date, irrigation, function(x) paste(x[nzchar(x)], collapse = "; "))

  daily <- merge(dates, openet_daily, by = "date", all.x = TRUE, sort = TRUE)
  daily <- merge(daily, irrigation_sum, by = "date", all.x = TRUE, sort = TRUE)
  daily <- merge(daily, notes_sum, by = "date", all.x = TRUE, sort = TRUE)
  daily$eta_in[is.na(daily$eta_in)] <- 0
  daily$precip_in[is.na(daily$precip_in)] <- 0
  daily$net_water_applied_in[is.na(daily$net_water_applied_in)] <- 0
  daily$notes[is.na(daily$notes)] <- ""

  field_capacity <- safe_numeric(setup$field_capacity_in %||% 0)
  allowable_dryness <- safe_numeric(setup$allowable_dryness_in %||% 0)
  permanent_wilting_point <- safe_numeric(setup$permanent_wilting_point_in %||% 0)
  initial_water <- safe_numeric(setup$initial_water_content_in %||% 0)
  app_rate <- safe_numeric(setup$application_rate_in_hr %||% NA_real_)

  n <- nrow(daily)
  swc <- deep_perc <- numeric(n)
  prev <- initial_water
  for (i in seq_len(n)) {
    effective_precip <- if (isTRUE(setup$count_precip_effective)) daily$precip_in[i] else 0
    raw_storage <- prev + daily$net_water_applied_in[i] + effective_precip - daily$eta_in[i]
    deep_perc[i] <- ifelse(raw_storage > field_capacity, raw_storage - field_capacity, 0)
    swc[i] <- ifelse(raw_storage > field_capacity, field_capacity, raw_storage)
    prev <- swc[i]
  }

  cum_eta <- cumsum(daily$eta_in)
  effective_precip_in <- if (isTRUE(setup$count_precip_effective)) daily$precip_in else rep(0, n)
  water_credited_in <- daily$net_water_applied_in + effective_precip_in
  cum_applied <- cumsum(daily$net_water_applied_in)
  cum_water_credited <- cumsum(water_credited_in)
  cum_deep <- cumsum(deep_perc)
  out <- data.frame(
    date = daily$date,
    julian_date = seq_len(n),
    eta_in = daily$eta_in,
    cumulative_eta_in = cum_eta,
    questionable_cumulative_eta_in = ifelse(daily$eta_in == 0, cum_eta, NA_real_),
    net_water_applied_in = daily$net_water_applied_in,
    effective_precip_in = effective_precip_in,
    water_credited_in = water_credited_in,
    cumulative_applied_in = cum_applied,
    cumulative_water_credited_in = cum_water_credited,
    applied_minus_eta_in = cum_water_credited - cum_eta,
    soil_water_content_in = swc,
    field_capacity_in = field_capacity,
    allowable_dryness_in = allowable_dryness,
    permanent_wilting_point_in = permanent_wilting_point,
    soil_water_graph_in = swc,
    questionable_soil_water_in = NA_real_,
    deep_percolated_water_in = deep_perc,
    cumulative_deep_percolated_in = cum_deep,
    leaching_fraction = ifelse(cum_water_credited > 0, cum_deep / cum_water_credited, NA_real_),
    precip_in = daily$precip_in,
    applied_plot_in = ifelse(daily$net_water_applied_in == 0, NA_real_, daily$net_water_applied_in),
    precip_plot_in = ifelse(daily$precip_in == 0, NA_real_, daily$precip_in),
    soil_water_deficit_in = field_capacity - swc,
    remaining_storage_capacity_in = field_capacity - swc,
    equivalent_irrigation_hours = ifelse(!is.na(app_rate) & app_rate > 0, daily$net_water_applied_in / app_rate, NA_real_),
    selected_model = selected_model,
    notes = daily$notes,
    stringsAsFactors = FALSE
  )
  out$questionable_soil_water_in <- ifelse(out$eta_in == 0, out$soil_water_graph_in, NA_real_)
  out
}

make_irrigation_display <- function(irrigation, balance, setup) {
  irrigation <- sync_irrigation_to_range(irrigation, setup$start_date, setup$end_date)
  b <- balance[, c("date", "precip_in", "soil_water_deficit_in", "equivalent_irrigation_hours", "cumulative_applied_in")]
  names(b)[2] <- "precip_openet_in"
  out <- merge(irrigation, b, by = "date", all.x = TRUE, sort = TRUE)
  app_rate <- safe_numeric(setup$application_rate_in_hr)
  if (!is.na(app_rate) && app_rate > 0) out$equivalent_irrigation_hours <- out$net_water_applied_in / app_rate
  out[, c("date", "net_water_applied_in", "cumulative_applied_in", "equivalent_irrigation_hours", "precip_openet_in", "soil_water_deficit_in", "notes")]
}

make_excel_plot_balance <- function(balance, setup) {
  if (is.null(balance) || nrow(balance) == 0) {
    return(balance)
  }
  first <- balance[1, , drop = FALSE]
  first$date <- as.Date(setup$start_date %||% first$date)
  first$julian_date <- 0
  first$eta_in <- 0
  first$cumulative_eta_in <- 0
  first$questionable_cumulative_eta_in <- NA_real_
  first$net_water_applied_in <- 0
  first$effective_precip_in <- 0
  first$water_credited_in <- 0
  first$cumulative_applied_in <- 0
  first$cumulative_water_credited_in <- 0
  first$applied_minus_eta_in <- 0
  first$soil_water_content_in <- safe_numeric(setup$initial_water_content_in %||% first$soil_water_content_in)
  first$soil_water_graph_in <- first$soil_water_content_in
  first$questionable_soil_water_in <- NA_real_
  first$deep_percolated_water_in <- 0
  first$cumulative_deep_percolated_in <- 0
  first$leaching_fraction <- NA_real_
  first$precip_in <- 0
  first$applied_plot_in <- NA_real_
  first$precip_plot_in <- NA_real_
  first$soil_water_deficit_in <- first$field_capacity_in - first$soil_water_content_in
  first$remaining_storage_capacity_in <- first$soil_water_deficit_in
  first$equivalent_irrigation_hours <- 0
  first$notes <- "Initial soil water content"
  out <- rbind(first, balance)
  rownames(out) <- NULL
  out
}

summary_metrics <- function(balance) {
  if (is.null(balance) || nrow(balance) == 0) {
    return(list(total_eta = 0, total_applied = 0, total_precip = 0, total_deep_perc = 0, ending_swc = NA_real_))
  }
  list(
    total_eta = sum(balance$eta_in, na.rm = TRUE),
    total_applied = sum(balance$net_water_applied_in, na.rm = TRUE),
    total_precip = sum(balance$precip_in, na.rm = TRUE),
    total_deep_perc = sum(balance$deep_percolated_water_in, na.rm = TRUE),
    ending_swc = tail(balance$soil_water_content_in, 1)
  )
}

# ---------------------------------------------------------------------------
# SSURGO soil properties via soilDB
# ---------------------------------------------------------------------------

fetch_ssurgo_soil_properties <- function(latitude, longitude, rooting_depth_ft = 4.0) {
  if (!requireNamespace("soilDB", quietly = TRUE)) {
    stop("Package 'soilDB' is required. Install it with install.packages('soilDB').")
  }

  rooting_depth_cm <- rooting_depth_ft * 30.48

  # 1. Resolve mapunit key (mukey) for the coordinate point
  wkt <- sprintf("POINT(%s %s)", as.numeric(longitude), as.numeric(latitude))
  mukey_res <- soilDB::SDA_query(
    sprintf("SELECT mukey FROM SDA_Get_Mukey_from_intersection_with_WktWgs84('%s')", wkt)
  )
  if (is.null(mukey_res) || nrow(mukey_res) == 0 || all(is.na(mukey_res$mukey))) {
    stop(sprintf("No SSURGO data found at coordinates (%.5f, %.5f).", latitude, longitude))
  }
  mukey <- as.character(mukey_res$mukey[1])

  # 2. Fetch horizon data for every component in this mapunit
  hz <- soilDB::SDA_query(sprintf(
    "SELECT c.cokey, c.compname, c.comppct_r,
            h.hzdept_r, h.hzdepb_r,
            h.wthirdbar_r, h.wfifteenbar_r, h.awc_r, h.dbthirdbar_r
     FROM mapunit mu
     INNER JOIN component c ON c.mukey = mu.mukey
     INNER JOIN chorizon h ON h.cokey = c.cokey
     WHERE mu.mukey = '%s'
     ORDER BY c.comppct_r DESC, h.hzdept_r ASC", mukey
  ))
  if (is.null(hz) || nrow(hz) == 0) {
    stop(sprintf("No horizon data found in SSURGO for this location (mukey: %s).", mukey))
  }

  # Use the dominant component (highest comppct_r)
  dominant_cokey <- hz$cokey[which.max(hz$comppct_r)]
  hz <- hz[hz$cokey == dominant_cokey, ]
  hz <- hz[order(hz$hzdept_r), ]

  # Clip horizons to rooting depth
  hz <- hz[!is.na(hz$hzdept_r) & hz$hzdept_r < rooting_depth_cm, ]
  if (nrow(hz) == 0) {
    stop(sprintf("No soil horizons within %.1f ft rooting depth for this location.", rooting_depth_ft))
  }
  hz$hzdepb_r <- pmin(hz$hzdepb_r, rooting_depth_cm)
  hz$thickness_cm <- hz$hzdepb_r - hz$hzdept_r
  hz <- hz[!is.na(hz$thickness_cm) & hz$thickness_cm > 0, ]

  total_cm <- sum(hz$thickness_cm)
  if (total_cm == 0) stop("Zero total horizon thickness within the specified rooting depth.")
  wt <- hz$thickness_cm / total_cm

  # Thickness-weighted average helper
  wavg <- function(x, w) {
    ok <- !is.na(x)
    if (!any(ok)) {
      return(NA_real_)
    }
    sum(x[ok] * (w[ok] / sum(w[ok])))
  }

  # Convert gravimetric % to volumetric (cm3/cm3):
  #   volumetric = (gravimetric_pct / 100) * bulk_density_g_cm3
  # wthirdbar_r / wfifteenbar_r are in % by weight; dbthirdbar_r is g/cm3.
  hz$fc_vol <- ifelse(!is.na(hz$wthirdbar_r) & !is.na(hz$dbthirdbar_r),
    (hz$wthirdbar_r / 100) * hz$dbthirdbar_r, NA_real_
  )
  hz$pwp_vol <- ifelse(!is.na(hz$wfifteenbar_r) & !is.na(hz$dbthirdbar_r),
    (hz$wfifteenbar_r / 100) * hz$dbthirdbar_r, NA_real_
  )

  fc_vol <- wavg(hz$fc_vol, wt)
  pwp_vol <- wavg(hz$pwp_vol, wt)
  awc_vol <- wavg(hz$awc_r, wt) # awc_r is already cm/cm (vol/vol)

  rd_in <- rooting_depth_ft * 12
  awc_in <- awc_vol * rd_in

  # Field capacity and PWP in inches, with graceful fallbacks
  fc_in <- if (!is.na(fc_vol)) {
    fc_vol * rd_in
  } else {
    if (!is.na(pwp_vol)) pwp_vol * rd_in + awc_in else awc_in * 2
  }
  pwp_in <- if (!is.na(pwp_vol)) {
    pwp_vol * rd_in
  } else {
    fc_in - awc_in
  }

  # Sanity check
  if (is.na(fc_in) || fc_in <= 0) {
    stop("Field capacity could not be calculated from SSURGO data for this location.")
  }
  fc_in <- max(fc_in, 0)
  pwp_in <- max(min(pwp_in, fc_in - 0.01), 0)
  awc_in <- max(awc_in, 0)

  # Allowable dryness at 50% MAD (management allowed deficit) — common default
  allowable_in <- max(fc_in - 0.5 * awc_in, pwp_in)

  list(
    compname             = as.character(hz$compname[1]),
    mukey                = mukey,
    rooting_depth_ft     = rooting_depth_ft,
    field_capacity_in    = round(fc_in, 2),
    pwp_in               = round(pwp_in, 2),
    awc_in               = round(awc_in, 2),
    allowable_dryness_in = round(allowable_in, 2)
  )
}

write_balance_export <- function(balance, irrigation, openet, setup, path) {
  wb <- openxlsx::createWorkbook()
  for (s in c("Initial_Setup", "Irrigation Amounts", "OpenET", "Calcs", "Summary")) openxlsx::addWorksheet(wb, s)
  setup_df <- data.frame(
    item = c("Field ID", "Crop", "Start date", "End date", "Latitude", "Longitude", "Application rate in/hr", "Field capacity", "Initial water", "Allowable dryness", "Permanent wilting point", "Selected model"),
    value = c(setup$field_id, setup$crop_description, as.character(setup$start_date), as.character(setup$end_date), setup$latitude, setup$longitude, setup$application_rate_in_hr, setup$field_capacity_in, setup$initial_water_content_in, setup$allowable_dryness_in, setup$permanent_wilting_point_in, setup$selected_model),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "Initial_Setup", setup_df)
  openxlsx::writeData(wb, "Irrigation Amounts", make_irrigation_display(irrigation, balance, setup))
  openxlsx::writeData(wb, "OpenET", openet)
  openxlsx::writeData(wb, "Calcs", balance)
  openxlsx::writeData(wb, "Summary", data.frame(metric = names(summary_metrics(balance)), value = unlist(summary_metrics(balance))))
  for (s in names(wb)) {
    openxlsx::freezePane(wb, s, firstRow = TRUE)
    openxlsx::setColWidths(wb, s, cols = 1:50, widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

# Fetch 7-day ETo forecast from Open-Meteo (free, no API key required).
# Returns a data.frame with columns: date (Date), eto_in (numeric, inches/day).
# On error returns NULL silently.
fetch_eto_forecast <- function(lat, lon) {
  url <- sprintf(
    paste0(
      "https://api.open-meteo.com/v1/forecast",
      "?latitude=%.4f&longitude=%.4f",
      "&daily=et0_fao_evapotranspiration&forecast_days=7&timezone=auto"
    ),
    lat, lon
  )
  resp <- httr2::request(url) |>
    httr2::req_timeout(15) |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  data.frame(
    date = as.Date(body$daily$time),
    eto_in = round(body$daily$et0_fao_evapotranspiration / 25.4, 4),
    stringsAsFactors = FALSE
  )
}
