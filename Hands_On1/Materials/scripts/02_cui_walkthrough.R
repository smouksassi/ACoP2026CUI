# Exercise 1 walkthrough — run this with the PPTX open.
# Each STEP matches a slide. Students fill the blanks; instructors
# check instructor/answer_key.txt (do not project that file).

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
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

grid_path <- file.path(root, "data", "endpoint_grid.csv")
if (!file.exists(grid_path)) {
  source(file.path(root, "scripts/01_build_endpoint_grid.R"))
}
grid <- read.csv(grid_path)
fig_dir <- file.path(root, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

theme_ex1 <- function() {
  theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", color = "#0B1F3A"),
      plot.subtitle = element_text(color = "#4A4A4A", size = 11),
      legend.position = "bottom"
    )
}

cat("\n============================================================\n")
cat("STEP 1  Clinical question\n")
cat("============================================================\n")
cat("Dummy folate-receptor ADC, solid tumor. Q3W mg/kg doses.\n")
cat("Label-like dose on this grid: 5.4 mg/kg (stand-in for ~6 mg/kg IBW).\n")
cat("Question: which dose maximizes clinical utility,\n")
cat("and does the answer move when efficacy vs safety weights change?\n")

cat("\n============================================================\n")
cat("STEP 2  Attributes (desirability scale 0-1)\n")
cat("============================================================\n")
cat("Benefit (higher is better):\n")
cat("  ORR     objective / overall response probability\n")
cat("Risk, inverted so higher is still better:\n")
cat("  NoNEU   = 1 - P(grade 3+ neutropenia)\n")
cat("  NoDR    = 1 - P(dose reduction)\n")

cat("\n============================================================\n")
cat("STEP 3  Dummy E-R at the seven doses\n")
cat("============================================================\n")
print(
  grid %>%
    transmute(
      Dose = regimen_label,
      ORR, P_NEU, P_DR, NoNEU, NoDR
    ) %>%
    mutate(across(-Dose, ~ round(.x, 2)))
)

er_long <- grid %>%
  select(dose_mgkg, all_of(endpoint_cols)) %>%
  pivot_longer(-dose_mgkg, names_to = "Endpoint", values_to = "Probability") %>%
  mutate(Endpoint = factor(Endpoint, levels = endpoint_cols))

p_er <- ggplot(er_long, aes(dose_mgkg, Probability, color = Endpoint, linetype = Endpoint)) +
  geom_vline(xintercept = 5.4, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.4) +
  scale_color_manual(values = ep_cols[endpoint_cols], breaks = endpoint_cols, name = NULL) +
  scale_linetype_manual(values = ep_lty_gg[endpoint_cols], breaks = endpoint_cols, name = NULL) +
  scale_x_continuous(breaks = grid$dose_mgkg) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "Dummy ADC: endpoint desirability vs dose",
    subtitle = "Tutorial reconstruction — not a label claim   ·   dashed line = 5.4 mg/kg label-like",
    x = "Dose (mg/kg Q3W)  ·  7 discrete levels",
    y = "Desirability  (ORR  |  1 - risk p)"
  ) +
  theme_ex1()
print(p_er)
ggsave(file.path(fig_dir, "01_endpoint_curves.png"), p_er, width = 9, height = 5.2, dpi = 200)

cat("\n============================================================\n")
cat("STEP 4  The equation (write this on the board)\n")
cat("============================================================\n")
cat("Multiplicative CUI  (this exercise default; original dummy ADC app)\n\n")
cat("  CUI(d)  =  prod_i  EP_i(d) ^ w_i     with  w_i already normalized\n\n")
cat("  EP_ORR  = p_ORR\n")
cat("  EP_NoNEU = 1 - p_NEU\n")
cat("  EP_NoDR  = 1 - p_DR\n\n")
cat("One zero desirability kills the product.\n\n")
cat("Additive CUI  (Exercise 2 default; we compute it on the same numbers)\n\n")
cat("  U(d)  =  sum_i  w_i * EP_i(d)\n")

cat("\n============================================================\n")
cat("TASK 0  CUI by hand at 5.4 mg/kg\n")
cat("============================================================\n")
cat("Given:  p_ORR = 0.50    p_NEU = 0.25    p_DR = 0.20\n")
cat("Raw w:  ORR = 2    NoNEU = 1    NoDR = 1\n")
cat("Fill in:\n")
cat("  invert     EP_ORR = ____   EP_NoNEU = ____   EP_NoDR = ____\n")
cat("  normalize  ŵ_ORR  = ____   ŵ_NoNEU  = ____   ŵ_NoDR  = ____\n")
cat("  additive   U      = ____\n")
cat("  product    CUI    = ____\n")
print_cui_by_hand(0.50, 0.25, 0.20, persona_weights$default_balanced, "5.4 mg/kg")

score_persona <- function(persona, form = "multiplicative") {
  w <- persona_weights[[persona]]
  scored <- add_cui(grid, w, form = form)
  peak <- cui_peak(scored)
  tibble(
    persona = persona,
    form = form,
    peak_dose = peak$dose_mgkg,
    peak_cui = round(peak$CUI, 3),
    nearest_label = nearest_label(peak$dose_mgkg),
    cui_32 = round(scored$CUI[scored$dose_mgkg == 3.2], 3),
    cui_54 = round(scored$CUI[scored$dose_mgkg == 5.4], 3),
    cui_74 = round(scored$CUI[scored$dose_mgkg == 7.4], 3)
  )
}

plot_cui <- function(persona, form = "multiplicative", show_endpoints = FALSE) {
  w <- persona_weights[[persona]]
  scored <- add_cui(grid, w, form = form)
  peak <- cui_peak(scored)
  line_levels <- if (isTRUE(show_endpoints)) c(endpoint_cols, "CUI") else "CUI"
  legend_levels <- c(line_levels, "Peak")
  long <- scored %>%
    select(dose_mgkg, all_of(line_levels)) %>%
    pivot_longer(-dose_mgkg, names_to = "Series", values_to = "Value") %>%
    mutate(Series = factor(Series, levels = line_levels))
  lwd <- c(ORR = 1.05, NoNEU = 1.05, NoDR = 1.05, CUI = 1.4)[line_levels]

  ggplot() +
    geom_vline(xintercept = 5.4, linetype = "dashed", color = "grey50") +
    geom_line(
      data = long,
      aes(dose_mgkg, Value, color = Series, linetype = Series, linewidth = Series, group = Series)
    ) +
    geom_point(
      data = scored,
      aes(dose_mgkg, CUI),
      color = ep_cols[["CUI"]],
      size = 2.2,
      show.legend = FALSE
    ) +
    geom_point(
      data = peak,
      aes(dose_mgkg, CUI, color = "Peak"),
      size = 3.2
    ) +
    annotate(
      "text", x = peak$dose_mgkg, y = min(1, peak$CUI + 0.05),
      label = paste0("max at ", peak$dose_mgkg, " mg/kg"),
      color = peak_col, fontface = "bold", size = 3.5
    ) +
    scale_color_manual(
      values = c(ep_cols[line_levels], Peak = peak_col),
      breaks = legend_levels,
      name = NULL
    ) +
    scale_linetype_manual(
      values = c(ep_lty_gg[line_levels], Peak = "blank"),
      breaks = legend_levels,
      name = NULL
    ) +
    scale_linewidth_manual(values = lwd, guide = "none") +
    scale_x_continuous(breaks = grid$dose_mgkg) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.02, 0.08))) +
    labs(
      title = scenario_title(persona, form),
      x = "Dose (mg/kg Q3W)  ·  7 discrete levels",
      y = if (isTRUE(show_endpoints)) "Desirability / CUI" else "CUI"
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        override.aes = list(
          linetype = c(unname(ep_lty_gg[line_levels]), "blank"),
          shape = c(rep(NA, length(line_levels)), 16),
          linewidth = c(unname(lwd), 0.8)
        )
      ),
      linetype = "none"
    ) +
    theme_ex1()
}

cat("\n============================================================\n")
cat("TASK A  Balanced weights  (original ADC app)\n")
cat("============================================================\n")
cat("Weights: ORR=2, NoNEU=1, NoDR=1    form = multiplicative\n")
cat("Fill in: peak dose = ____ mg/kg    label-like?  yes / no\n")
task_a <- score_persona("default_balanced", "multiplicative")
print(task_a)
p_a <- plot_cui("default_balanced", "multiplicative")
print(p_a)
ggsave(file.path(fig_dir, "02_taskA_balanced.png"), p_a, width = 8.5, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("TASK B  Efficacy-priority weights\n")
cat("============================================================\n")
cat("Weights: ORR=8, NoNEU=1, NoDR=1    form = multiplicative\n")
cat("Fill in: peak dose = ____ mg/kg    did the argmax move?  yes / no\n")
task_b <- score_persona("efficacy_priority", "multiplicative")
print(task_b)
p_b <- plot_cui("efficacy_priority", "multiplicative")
print(p_b)
ggsave(file.path(fig_dir, "03_taskB_efficacy.png"), p_b, width = 8.5, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("TASK C  Safety-priority weights\n")
cat("============================================================\n")
cat("Weights: ORR=2, NoNEU=4, NoDR=4    form = multiplicative\n")
cat("Fill in: peak dose = ____ mg/kg    above / at / below label-like?\n")
task_c <- score_persona("safety_priority", "multiplicative")
print(task_c)
p_c <- plot_cui("safety_priority", "multiplicative")
print(p_c)
ggsave(file.path(fig_dir, "04_taskC_safety.png"), p_c, width = 8.5, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("TASK D  Same weights, switch CUI form\n")
cat("============================================================\n")
cat("Balanced weights: multiplicative vs additive.\n")
cat("A product is a veto only if a HIGH-WEIGHT attribute is near zero.\n")
task_d <- bind_rows(
  score_persona("default_balanced", "multiplicative"),
  score_persona("default_balanced", "additive")
)
print(task_d)
p_d <- plot_cui("default_balanced", "multiplicative") + plot_cui("default_balanced", "additive")
print(p_d)
ggsave(file.path(fig_dir, "05_taskD_add_vs_mult.png"), p_d, width = 11, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("DEBRIEF  One sentence to leave with\n")
cat("============================================================\n")
cat("Balanced weights peak at the label-like 5.4 mg/kg; efficacy-priority\n")
cat("walks up to 7.4; safety-priority walks down to 3.2. Write the weights\n")
cat("and the form (product vs sum) before you report a peak.\n")
cat("\nPlots saved under figures/\n")
cat("Open the Shiny app with:  shiny::runApp('scripts/03_app.R')\n")
