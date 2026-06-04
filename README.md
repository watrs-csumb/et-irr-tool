# OpenET wETgraph R Shiny app

This is the R Shiny version of the OpenET wETgraph app.

## Run

Open R or RStudio in this folder and run:

```r
install.packages(c(
  "shiny", "ggplot2", "DT", "plotly", "leaflet",
  "readxl", "openxlsx", "httr2", "jsonlite", "readr"
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
3. Click **Update OpenET data**.
4. Go to **Irrigation Amounts** and enter daily **Net water applied, in.** values.
5. The Dashboard, Calcs, deep percolation, and soil water plots update from those irrigation values.

OpenET precipitation is displayed separately. It is not automatically counted as applied water unless you include effective precipitation in the applied-water input, matching the workbook logic.

## Files

- `app.R`: Shiny user interface and server logic
- `R/openet_utils.R`: OpenET API, irrigation, water-balance, plotting, and export utility functions
