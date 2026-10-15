# Exercise 2 — Dummy elagolix CUI

Samer block after Exercise 1, same session. Sequential — not a separate track.
Five attributes, 0–400 mg TDD, 6-month horizon, additive CUI, local app.

## Project this

`slides/Exercise2_Elagolix_CUI_Walkthrough.pptx` — teaching chrome (large type, navy / paper). Same 20 × 11.25 canvas as the morning deck, **not** the ISoP title master.

Do **not** project `instructor_solutions/answer_key.txt`. Answers are no longer in the PPTX.

## How to run (from this `Materials` folder)

```r
setwd(".../Hands_On2_20thSept2026/Materials")
shiny::runApp("scripts/03_app.R")
```

Once, if the grid is missing: `source("scripts/01_build_endpoint_grid.R")`.

Horizon stays 6 months. No internet required after packages are installed.

## Packages

`dplyr`, `tidyr`, `ggplot2`, `patchwork`, `shiny`, `bslib`, `plotly`

## Rebuild the walkthrough (instructors)

From this `slides/` folder: `python build_pptx.py`
