# Dummy ADC dose-level E–R lookup for the tutorial.
# Same spirit as hands_on/dummy_data_gen.R logistic E–R, but a 7-dose table
# so the room never runs rxode2 or brms. Not patient-level ImmunoGen / AbbVie data.

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

root <- if (file.exists("scripts/00_cui_functions.R")) {
  getwd()
} else if (file.exists("00_cui_functions.R")) {
  normalizePath("..")
} else {
  stop("Run from exercise_1_adc/")
}

source(file.path(root, "scripts/00_cui_functions.R"))

# Discrete Q3W mg/kg doses from the original dummy ADC app.
# 5.4 mg/kg is the label-like stand-in (approved MIRV is ~6 mg/kg IBW Q3W).
# Rates chosen so:
#   Task A (balanced, multiplicative) peaks at 5.4
#   Task B (efficacy) peaks at 7.4
#   Task C (safety) peaks at 3.2
#   5.4 rates stay easy for the pencil-and-paper slide (0.50 / 0.25 / 0.20).
dose_mgkg <- c(0.8, 1.6, 3.2, 5.4, 6.4, 7.4, 8.0)
# Typical payload Cav (ng/mL) — illustrative, same order as Tu / dummy PK.
cav_ngml <- c(0.18, 0.32, 0.55, 0.85, 1.05, 1.22, 1.35)

grid <- tibble(
  dose_mgkg = dose_mgkg,
  cav_ngml = cav_ngml,
  ORR = c(0.20, 0.32, 0.42, 0.50, 0.56, 0.60, 0.62),
  P_NEU = c(0.04, 0.08, 0.14, 0.25, 0.38, 0.50, 0.60),
  P_DR = c(0.06, 0.10, 0.14, 0.20, 0.30, 0.40, 0.48)
) %>%
  mutate(
    NoNEU = 1 - P_NEU,
    NoDR = 1 - P_DR,
    regimen_label = paste0(dose_mgkg, " mg/kg"),
    label_like = dose_mgkg == 5.4
  )

cat("\n====== Dummy ADC rates by dose ======\n")
print(
  grid %>%
    transmute(
      Dose = regimen_label,
      `Cav ng/mL` = sprintf("%.2f", cav_ngml),
      ORR = sprintf("%.0f%%", 100 * ORR),
      `G3+ NEU` = sprintf("%.0f%%", 100 * P_NEU),
      `Dose red.` = sprintf("%.0f%%", 100 * P_DR),
      NoNEU = sprintf("%.2f", NoNEU),
      NoDR = sprintf("%.2f", NoDR)
    )
)

cat("\n====== Persona peaks (multiplicative default) ======\n")
for (nm in names(persona_weights)) {
  sc <- add_cui(grid, persona_weights[[nm]], "multiplicative")
  pk <- cui_peak(sc)
  sca <- add_cui(grid, persona_weights[[nm]], "additive")
  pka <- cui_peak(sca)
  cat(sprintf(
    "  %-20s  mult %s mg/kg (CUI %.3f)   add %s mg/kg (CUI %.3f)\n",
    nm, pk$dose_mgkg, pk$CUI, pka$dose_mgkg, pka$CUI
  ))
}

dir.create(file.path(root, "data"), showWarnings = FALSE, recursive = TRUE)
write.csv(grid, file.path(root, "data", "endpoint_grid.csv"), row.names = FALSE)

persona_tbl <- imap_dfr(persona_weights, function(w, name) {
  tibble(persona = name, label = persona_labels[[name]], endpoint = names(w), weight = as.numeric(w))
})
write.csv(persona_tbl, file.path(root, "data", "personas.csv"), row.names = FALSE)

cat("\nWrote data/endpoint_grid.csv and data/personas.csv\n")
