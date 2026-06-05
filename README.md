# OpenET R Shiny app

This is the R Shiny version of the OpenET wETgraph app.

## Run

Open R or RStudio in this folder and run:

```r
install.packages(c(
  "shiny", "ggplot2", "DT", "plotly", "leaflet",
  "readxl", "openxlsx", "httr2", "jsonlite", "readr",
  "soilDB"
))

shiny::runApp()
```

Or from a terminal:

```bash
R -e "shiny::runApp()"
```

## Important workflow

1. Enter the OpenET API key in the sidebar.
2. Select the date range, coordinates, crop, and soil-water parameters.
3. *(Optional)* Click **Fetch Soil from SSURGO** to auto-populate field capacity and permanent wilting point from NRCS SSURGO data for the entered coordinates.
4. Click **Update OpenET data**.
5. Go to **Irrigation Amounts** and enter daily **Net water applied, in.** values.
6. The Dashboard, Calcs, deep percolation, and soil water plots update from those irrigation values.

OpenET precipitation is displayed separately. It is not automatically counted as applied water unless you check **Count all OpenET precipitation as effective water** in the Irrigation Amounts tab.

### Soil properties (SSURGO)

The **Fetch Soil from SSURGO** button queries the USDA NRCS Soil Data Access (SDA) web service for the dominant soil component at the entered lat/lon and root zone depth. It updates:

| Parameter | Updated automatically |
|---|---|
| Field capacity, in. | ✅ |
| Permanent wilting point, in. | ✅ |
| Allowable dryness, in. | ❌ — set manually |
| Initial water content, in. | ❌ — set manually |

The status box reports the soil series name, SSURGO map unit key (mukey), plant-available water capacity (AWC), and a 50% MAD reference value. **50% MAD** (Management Allowed Deficit) is a common starting point — it equals FC minus half the AWC — but the correct value depends on crop type, growth stage, and irrigation system capacity.

## Files

- `app.R`: Shiny user interface and server logic
- `R/openet_utils.R`: OpenET API, SSURGO soil query, irrigation, water-balance, plotting, and export utility functions
