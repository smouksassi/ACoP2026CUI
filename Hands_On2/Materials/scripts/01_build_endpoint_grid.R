# Build a dummy elagolix-like E-R lookup for the tutorial.
# Calibrated to published Phase 3 rates (Taylor et al. NEJM 2017; EM-I / EM-II).
# Not patient-level AbbVie data. Not for labeling.

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(readr)
})

root <- if (file.exists("scripts/00_cui_functions.R")) {
  getwd()
} else if (file.exists("00_cui_functions.R")) {
  normalizePath("..")
} else {
  stop("Run from exercise_2_elagolix/ or exercise_2_elagolix/scripts/")
}

source(file.path(root, "scripts/00_cui_functions.R"))

emax <- function(dose, e0, emax, ed50, h = 1) {
  e0 + (emax - e0) * (dose^h) / (ed50^h + dose^h)
}

# Targets at TDD = 0 / 150 / 400 mg (approx. pooled EM-I/II, month 3-6)
# DYSM responders: 21% / 45% / 74%
# NMPP responders: 36.5% / 50% / 56%
# DYSP responders: 31% / 39% / 54%   (150 mg not significant in Phase 3)
# Hot flush:       7% / 24% / 42%
# P(BMD loss >1%): tutorial surrogate used by Winzenborg; steeper than the
#                  published >5% rates (1.5% / 3% / 19%) so the CUI can move.

tdd <- seq(0, 400, by = 10)

grid <- tibble(
  tdd_mg = tdd,
  DYSM = emax(tdd, e0 = 0.210, emax = 0.84, ed50 = 130, h = 1.90),
  NMPP = emax(tdd, e0 = 0.365, emax = 0.63, ed50 = 75, h = 1.30),
  DYSP = emax(tdd, e0 = 0.300, emax = 0.72, ed50 = 310, h = 2.40),
  P_HF = emax(tdd, e0 = 0.070, emax = 0.50, ed50 = 250, h = 1.50),
  P_BMD = emax(tdd, e0 = 0.100, emax = 0.78, ed50 = 320, h = 3.00)
) %>%
  mutate(
    NoHF = 1 - P_HF,
    BMDok = 1 - P_BMD,
    regimen_label = case_when(
      tdd_mg == 0 ~ "Placebo",
      tdd_mg == 150 ~ "150 mg QD",
      tdd_mg == 400 ~ "200 mg BID",
      TRUE ~ NA_character_
    )
  )

# Calibration check at the three published anchors
anchors <- grid %>%
  filter(tdd_mg %in% c(0, 150, 400)) %>%
  transmute(
    TDD = tdd_mg,
    Regimen = regimen_label,
    DYSM = sprintf("%.1f%%", 100 * DYSM),
    NMPP = sprintf("%.1f%%", 100 * NMPP),
    DYSP = sprintf("%.1f%%", 100 * DYSP),
    `Hot flush` = sprintf("%.1f%%", 100 * P_HF),
    `BMD loss >1%` = sprintf("%.1f%%", 100 * P_BMD)
  )

cat("\n====== Dummy grid vs published anchors ======\n")
print(anchors, n = 3)
cat("\nPublished (approx. EM-I/II):\n")
cat("  DYSM  21 / 45 / 74%\n")
cat("  NMPP  37 / 50 / 56%\n")
cat("  DYSP  31 / 39 / 54%  (150 mg NS)\n")
cat("  HF     7 / 24 / 42%\n")
cat("  BMD>5% 1.5 / 3 / 19%  (we use a 1% surrogate for CUI, as in Winzenborg)\n")

dir.create(file.path(root, "data"), showWarnings = FALSE, recursive = TRUE)
write.csv(grid, file.path(root, "data", "endpoint_grid.csv"), row.names = FALSE)

persona_tbl <- imap_dfr(persona_weights, function(w, name) {
  tibble(persona = name, label = persona_labels[[name]], endpoint = names(w), weight = as.numeric(w))
})
write.csv(persona_tbl, file.path(root, "data", "personas.csv"), row.names = FALSE)

cat("\nWrote data/endpoint_grid.csv and data/personas.csv\n")
