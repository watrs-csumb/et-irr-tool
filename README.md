# OpenET Irrigation Scheduling Tool

An R Shiny app for field-level irrigation scheduling. Tracks daily soil water balance using **OpenET** remote sensing based ET, **SSURGO** soil properties, and an **Open-Meteo** 7-day ETo forecast to tell you when and how much to irrigate.

---

## Quick Start

```r
install.packages(c(
  "shiny", "ggplot2", "DT", "plotly", "leaflet",
  "readxl", "openxlsx", "httr2", "jsonlite", "readr",
  "soilDB", "shinyFeedback", "shinycssloaders"
))
shiny::runApp()
```

---

## Workflow

1. Enter your **OpenET API key** and set **field info**: coordinates, date range, and crop.
2. *(Optional)* Click **Fetch Soil from SSURGO** to auto-fill field capacity and permanent wilting point.
3. Set **allowable depletion** (MAD threshold) and initial soil water content.
4. Click **Update OpenET data** to fetch ET, precipitation, and ETo.
5. Go to **Irrigation Amounts** and enter net water applied (in.) per irrigation event.
6. Use the **Dashboard** to review the season balance and the **Irrigation Explorer** to plan the next irrigation.

---

## Tabs

### Dashboard
Season overview with four plots and four summary metrics (total ETa, applied water, precipitation, deep percolation).

| Plot | Shows |
|---|---|
| Soil Water | Daily SWC vs. field capacity, MAD threshold, and PWP |
| Cumulative Water | Σ ETa vs. Σ applied water (+ ETo when available) |
| Deep Percolation | Daily drainage and leaching fraction |
| ET / ETo | Daily ETa, ETo, and ET/ETo ratio (crop coefficient proxy) |

### Irrigation Amounts
Log daily irrigation events (date, net water applied, optional note). Check **Count all OpenET precipitation as effective water** to credit precipitation in the water balance.

### Irrigation Explorer
Forward-looking depletion planning — two views:

**Planning View**
Projects soil water from the last OpenET date using a constant ET rate. Adjust inputs and see results immediately:

| Input | Default |
|---|---|
| Projected daily ET (in./day) | 7-day average of recent ETa |
| Apply irrigation today (in.) | 0 — simulate an upcoming event |
| Forecast horizon (days) | 7–30 days |

Shows current SWC, water available before MAD, projected daily ET, and **days until the next irrigation is due**. A vertical marker on the chart flags the irrigation deadline.

**7-Day Weather Forecast View** *(appears automatically when data is current)*
Uses the Open-Meteo FAO-56 Penman-Monteith ETo forecast and a crop coefficient (Kc) to project ET day-by-day. Kc defaults to the 7-day ETa/ETo ratio from your OpenET data; you can adjust it manually or reset to the estimate. Gives a weather-driven irrigation date vs. the flat-ET planning view.

### Calcs
Full daily water balance table. Download as Excel (`.xlsx`).

### OpenET
Raw OpenET API response for all fetched models.

### Map
Field location with Satellite / Street Map toggle.

### FAQ
In-app reference: ET definitions, OpenET models, SSURGO soil calculations, water balance methodology, and ETo forecast source.

---

## Multi-Field Support

Add multiple fields with the **+ Add field** button in the sidebar. Each field has its own inputs, irrigation log, and fetched data. Switch between fields with the dropdown — data is saved per field and persists in the session file.

---

## Data Sources

| Dataset | Source | Key required |
|---|---|---|
| ETa + Precipitation + ETo | [OpenET](https://openetdata.org/) | Yes |
| Soil properties (FC, PWP, AWC) | [USDA NRCS SSURGO](https://www.nrcs.usda.gov/resources/data-and-reports/ssurgo) | No |
| 7-day ETo forecast | [Open-Meteo](https://open-meteo.com/) — FAO-56 PM | No |

---

## Soil Properties (SSURGO)

Click **Fetch Soil from SSURGO** to auto-populate FC and PWP from the dominant soil component at your coordinates. The result box shows soil series, map unit key (mukey), root zone depth, FC, PWP, AWC, and a 50% MAD reference value.

> **FC and PWP are updated automatically. Allowable depletion and initial water content must be set manually** based on your crop, growth stage, and management.

---

## Session Management

Use the **Session** panel to save or load your entire workspace (inputs, irrigation log, and fetched data) as an `.rds` file. The filename defaults to `fields_N_MM_DD_YYYY_HHMM` but can be edited before saving.

---

## Files

```
app.R                  # Shiny UI and server logic
R/openet_utils.R       # API calls, SSURGO, water balance, and export utilities
```

