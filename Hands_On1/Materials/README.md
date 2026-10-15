# Exercise 1 — Dummy ADC CUI

Venky block after the coffee break. Sequential with Exercise 2 (not a separate track).
Three endpoints, seven discrete doses, pencil-and-paper CUI, then the **local** app.

## Project this

`slides/Exercise1_ADC_CUI_Walkthrough.pptx` — teaching chrome (large type, navy / paper). Same 20 × 11.25 canvas as the morning deck, **not** the ISoP title master.

Do **not** project `instructor_solution/answer_key.txt`.

## How to run (from this `Materials` folder)

```r
setwd(".../Hands_On1_20thSept2026/Materials")
shiny::runApp("scripts/03_app.R")
```

Once, if the grid is missing: `source("scripts/01_build_endpoint_grid.R")`.

This is **not** the Lilly / Mirv-S Shiny from the morning. No `brms`, no internet.

## Packages

`dplyr`, `tidyr`, `ggplot2`, `patchwork`, `shiny`, `bslib`, `plotly`

## Rebuild the walkthrough (instructors)

From this `slides/` folder: `python build_pptx.py`
