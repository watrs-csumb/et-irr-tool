# OpenET Irrigation Scheduling Tool — R Shiny App

An interactive irrigation scheduling tool built with R Shiny. It combines **OpenET** satellite-based evapotranspiration data, **SSURGO** soil properties, and **Open-Meteo** 7-day ETo forecasts to track field soil water balance and support irrigation decisions.

---

## Quick Start

Open R or RStudio in this folder and run:

```r
install.packages(c(
  "shiny", "ggplot2", "DT", "plotly", "leaflet",
  "readxl", "openxlsx", "httr2", "jsonlite", "readr",
  "soilDB", "shinyFeedback", "shinycssloaders"
))

shiny::runApp()
```

Or from a terminal:

```bash
R -e "shiny::runApp()"
```

---

## Workflow

1. Enter your **OpenET API key** in the sidebar.
2. Set the **date range**, **coordinates** (lat/lon), **crop**, and **soil-water parameters**.
3. *(Optional)* Click **Fetch Soil from SSURGO** to auto-populate field capacity and permanent wilting point from NRCS SSURGO data.
4. Click **Update OpenET data** to fetch ET, precipitation, and reference ETo from OpenET.
5. Go to **Irrigation Amounts** and enter daily **Net water applied, in.** values.
6. Explore the **Dashboard** plots and use the **Irrigation Explorer** for forward-looking scheduling.

---

## Tabs

### Dashboard
Four interactive plots showing the full season water balance:

| Plot | Description |
|---|---|
| Soil Water | Daily soil water content (in.) vs. field capacity, MAD threshold, and PWP. Precipitation shown as dots. |
| Cumulative Water | Cumulative ETa and applied water (and ETo when available). |
| Deep Percolation | Daily deep percolation and leaching fraction (dual y-axis). |
| Daily ET, ETo & ET/ETo | Daily ETa, ETo, and their ratio (crop coefficient proxy). |

Four summary metrics appear above the plots: total ETa, total applied water, total precipitation, and total deep percolation.

### Irrigation Amounts
Enter daily net water applied (in.) events. Each entry includes date, amount, and an optional note. Precipitation handling is controlled by the **Count all OpenET precipitation as effective water** checkbox.

### Irrigation Explorer
Forward-looking soil water depletion planning and forecast tool. Consists of two charts:

#### Planning View (flat ET projection, full horizon)
Projects soil water depletion from the last day of OpenET data using a constant daily ET rate over a user-selected horizon (7–30 days). Inputs:

| Input | Description |
|---|---|
| Projected daily ET (in./day) | Defaults to 7-day average of recent non-zero OpenET ETa. Editable. |
| Apply irrigation today (in.) | Simulates an irrigation event applied from the current SWC. |
| Forecast horizon (days) | 7–30 day projection window. |

Reference lines: Field Capacity, MAD Threshold, Permanent Wilting Point. A vertical "Irrigate by" marker shows when SWC is projected to hit the MAD threshold.

Four metric boxes: current SWC, available water before MAD, projected daily ET, and days until irrigation.

#### 7-Day ETo Forecast — Weather-Based View (Open-Meteo)
A second chart appears automatically when your data ends within the last 7 days and the Open-Meteo API is reachable. It uses a **7-day daily ETo forecast** (FAO-56 Penman-Monteith) from [Open-Meteo](https://open-meteo.com/) — free, no API key required — to drive a weather-aware SWC projection.

**Crop coefficient (Kc):** The forecast ET per day is computed as `Kc × ETo_forecast`. Kc defaults to the 7-day mean ETa/ETo ratio from your OpenET data (estimated crop coefficient). It can be adjusted manually, and a **Reset to 7-day avg** button restores the estimated value.

If your data end date is more than 7 days in the past, this chart shows an informational message prompting you to update the end date.

### Calcs
Full daily water balance table (downloadable as Excel). Columns include date, ETa, ETo, precipitation, irrigation, soil water content, deep percolation, and cumulative values.

### OpenET
Raw OpenET API response table for all fetched models.

### Map
Field location map with Esri World Imagery / OpenStreetMap toggle.

### FAQ
Embedded answers to common questions about data sources, calculations, and soil parameters.

---

## Data Sources

| Dataset | Source | Access |
|---|---|---|
| Evapotranspiration (ETa) | [OpenET](https://openetdata.org/) — ensemble or individual model | API key required |
| Precipitation | OpenET / gridMET | Fetched alongside ETa |
| Reference ET (ETo) | OpenET / gridMET | Fetched alongside ETa |
| Soil properties (FC, PWP) | [USDA NRCS SSURGO](https://www.nrcs.usda.gov/resources/data-and-reports/ssurgo) via Soil Data Access | No key — auto-queried by lat/lon |
| 7-day ETo forecast | [Open-Meteo](https://open-meteo.com/) — FAO-56 Penman-Monteith | No key required |

---

## Soil Properties (SSURGO)

The **Fetch Soil from SSURGO** button queries the USDA NRCS Soil Data Access (SDA) web service for the dominant soil component at the entered lat/lon and root zone depth.

| Parameter | Auto-populated |
|---|---|
| Field capacity (in.) | ✅ |
| Permanent wilting point (in.) | ✅ |
| MAD threshold (in.) | ❌ — set manually |
| Initial water content (in.) | ❌ — set manually |

The status box reports the soil series name, SSURGO map unit key (mukey), plant-available water capacity (AWC), and a 50% MAD reference value. **50% MAD** is a common starting point (FC − 0.5 × AWC), but the correct value depends on crop type, growth stage, and irrigation system capacity.

---

## Session Management

Use the **Session** panel in the sidebar to:
- **Save Session** — exports all inputs, irrigation entries, and fetched ET data to an `.rds` file.
- **Load Session** — restores a previously saved session in full.

---

## Files

```
app.R                  # Shiny UI and server logic
R/openet_utils.R       # OpenET API, Open-Meteo, SSURGO, water balance, and export utilities
```

