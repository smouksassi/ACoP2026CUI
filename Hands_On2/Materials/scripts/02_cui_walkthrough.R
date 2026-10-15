# Exercise 2 walkthrough — run this with the PPTX open.
# Each STEP matches a slide. Students fill the blanks; instructors
# check instructor/answer_key.txt (do not project that file).

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(tibble)
  library(readr)
})

root <- if (file.exists("scripts/00_cui_functions.R")) {
  getwd()
} else if (file.exists("00_cui_functions.R")) {
  normalizePath("..")
} else {
  stop("Run from exercise_2_elagolix/")
}

source(file.path(root, "scripts/00_cui_functions.R"))

grid_path <- file.path(root, "data", "endpoint_grid.csv")
if (!file.exists(grid_path)) {
  source(file.path(root, "scripts/01_build_endpoint_grid.R"))
}
grid <- read.csv(grid_path)
fig_dir <- file.path(root, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

theme_ex2 <- function() {
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
cat("Dummy oral GnRH antagonist, endometriosis-associated pain.\n")
cat("Approved-like regimens: 150 mg QD (TDD 150) and 200 mg BID (TDD 400).\n")
cat("Question: which total daily dose maximizes clinical utility,\n")
cat("and does the answer change when the patient's preferences change?\n")

cat("\n============================================================\n")
cat("STEP 2  Attributes (desirability scale 0-1)\n")
cat("============================================================\n")
cat("Benefit (higher is better, already a responder probability):\n")
cat("  DYSM   dysmenorrhea responder\n")
cat("  NMPP   nonmenstrual pelvic pain responder\n")
cat("  DYSP   dyspareunia responder\n")
cat("Risk, inverted so higher is still better:\n")
cat("  NoHF   = 1 - P(moderate/severe hot flush)\n")
cat("  BMDok  = 1 - P(BMD loss > 1% in 12 months)   [Winzenborg surrogate]\n")

cat("\n============================================================\n")
cat("STEP 3  Dummy E-R at the published anchors\n")
cat("============================================================\n")
print(
  grid %>%
    filter(tdd_mg %in% c(0, 150, 400)) %>%
    transmute(
      TDD = tdd_mg, Regimen = regimen_label,
      DYSM, NMPP, DYSP, NoHF, BMDok
    ) %>%
    mutate(across(-c(TDD, Regimen), ~ round(.x, 3)))
)

er_long <- grid %>%
  select(tdd_mg, all_of(endpoint_cols)) %>%
  pivot_longer(-tdd_mg, names_to = "Endpoint", values_to = "Probability") %>%
  mutate(Endpoint = factor(Endpoint, levels = endpoint_cols))

p_er <- ggplot(er_long, aes(tdd_mg, Probability, color = Endpoint, linetype = Endpoint)) +
  geom_vline(xintercept = c(150, 400), linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1.05) +
  scale_color_manual(values = ep_cols[endpoint_cols], breaks = endpoint_cols, name = NULL) +
  scale_linetype_manual(values = ep_lty_gg[endpoint_cols], breaks = endpoint_cols, name = NULL) +
  scale_x_continuous(breaks = seq(0, 400, by = 50), minor_breaks = seq(0, 400, by = 10), limits = c(0, 410)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "Dummy GnRH antagonist: endpoint desirability vs total daily dose",
    subtitle = "Tutorial reconstruction from published Phase 3 rates — not a label claim",
    x = "Total daily dose (mg)  ·  10 mg grid",
    y = "Desirability  (efficacy p  |  1 - risk p)"
  ) +
  theme_ex2()
print(p_er)
ggsave(file.path(fig_dir, "01_endpoint_curves.png"), p_er, width = 9, height = 5.2, dpi = 200)

cat("\n============================================================\n")
cat("STEP 4  The equation (write this on the board)\n")
cat("============================================================\n")
cat("Additive CUI  (Winzenborg 2021 Eq. 1; this exercise default)\n\n")
cat("                 w_i\n")
cat("  U(d)  =  sum  -----  *  EP_i(d)\n")
cat("                 sum_j w_j\n\n")
cat("  EP_i(d) already on a 0-1 desirability scale.\n")
cat("  Safety endpoints were inverted BEFORE this sum.\n\n")
cat("Multiplicative CUI  (Exercise 1 dummy ADC app)\n\n")
cat("  CUI(d)  =  prod_i  EP_i(d) ^ w_i     with sum w_i = 1\n\n")
cat("One zero desirability kills the product. The sum does not.\n")

score_persona <- function(persona, form = "additive") {
  w <- persona_weights[[persona]]
  scored <- add_cui(grid, w, form = form)
  peak <- cui_peak(scored)
  tibble(
    persona = persona,
    form = form,
    peak_tdd = peak$tdd_mg,
    peak_cui = round(peak$CUI, 3),
    nearest_approved = nearest_approved(peak$tdd_mg),
    cui_150 = round(scored$CUI[scored$tdd_mg == 150], 3),
    cui_400 = round(scored$CUI[scored$tdd_mg == 400], 3)
  )
}

plot_cui <- function(persona, form = "additive", show_endpoints = FALSE) {
  w <- persona_weights[[persona]]
  scored <- add_cui(grid, w, form = form)
  peak <- cui_peak(scored)
  line_levels <- if (isTRUE(show_endpoints)) c(endpoint_cols, "CUI") else "CUI"
  legend_levels <- c(line_levels, "Peak")
  long <- scored %>%
    select(tdd_mg, all_of(line_levels)) %>%
    pivot_longer(-tdd_mg, names_to = "Series", values_to = "Value") %>%
    mutate(Series = factor(Series, levels = line_levels))
  lwd <- c(setNames(rep(1.05, length(endpoint_cols)), endpoint_cols), CUI = 1.4)[line_levels]

  ggplot() +
    geom_vline(xintercept = c(150, 400), linetype = "dashed", color = "grey50") +
    geom_line(
      data = long,
      aes(tdd_mg, Value, color = Series, linetype = Series, linewidth = Series, group = Series)
    ) +
    geom_point(
      data = scored,
      aes(tdd_mg, CUI),
      color = ep_cols[["CUI"]],
      size = 1.1,
      show.legend = FALSE
    ) +
    geom_point(
      data = peak,
      aes(tdd_mg, CUI, color = "Peak", linetype = "Peak"),
      size = 3.2
    ) +
    annotate(
      "text", x = peak$tdd_mg, y = min(1, peak$CUI + 0.04),
      label = paste0("max at ", peak$tdd_mg, " mg"),
      color = peak_col, fontface = "bold", size = 3.5
    ) +
    annotate("text", x = 150, y = 1, label = "150 QD", vjust = -0.4, size = 3, color = "#5C5C5C") +
    annotate("text", x = 400, y = 1, label = "200 BID", vjust = -0.4, size = 3, color = "#5C5C5C") +
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
    scale_x_continuous(
      breaks = seq(0, 400, by = 50),
      minor_breaks = seq(0, 400, by = 10),
      limits = c(0, 410)
    ) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.02, 0.08))) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    labs(
      title = scenario_title(persona, form),
      x = "Total daily dose (mg)  ·  10 mg grid",
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
    theme_ex2()
}

# Overlay endpoints like the Shiny checkbox:
# plot_cui("dce_average", "additive", show_endpoints = TRUE)

cat("\n============================================================\n")
cat("TASK A  Trial-like weights  (DYSP off)\n")
cat("============================================================\n")
cat("Weights: DYSM=1, NMPP=1, DYSP=0, NoHF=1, BMDok=1\n")
cat("Fill in: peak TDD = ____   nearer approved regimen = ____\n")
task_a <- score_persona("trial_equal", "additive")
print(task_a)
p_a <- plot_cui("trial_equal", "additive")
print(p_a)
ggsave(file.path(fig_dir, "02_taskA_trial_equal.png"), p_a, width = 8.5, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("TASK B  Dyspareunia-priority weights\n")
cat("============================================================\n")
cat("Weights: DYSM=1, NMPP=1, DYSP=2, NoHF=1, BMDok=1\n")
cat("Fill in: peak TDD = ____   nearer approved regimen = ____\n")
cat("Did the argmax move from Task A?  yes / no\n")
task_b <- score_persona("dyspareunia_priority", "additive")
print(task_b)
p_b <- plot_cui("dyspareunia_priority", "additive")
print(p_b)
ggsave(file.path(fig_dir, "03_taskB_dyspareunia.png"), p_b, width = 8.5, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("TASK C  DCE average patient  (Poulos 2019 weights)\n")
cat("============================================================\n")
cat("Weights: HF 3.62, DYSP 1.70, NMPP 1.49, DYSM 1.48, BMD 0.49\n")
cat("Fill in: peak TDD = ____   (paper reported ~230 mg)\n")
cat("Is this an approved dose?  yes / no\n")
task_c <- score_persona("dce_average", "additive")
print(task_c)
p_c <- plot_cui("dce_average", "additive")
print(p_c)
ggsave(file.path(fig_dir, "04_taskC_dce.png"), p_c, width = 8.5, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("TASK D  Same weights, switch CUI form\n")
cat("============================================================\n")
cat("DCE average: additive vs multiplicative.\n")
cat("Then bone-sparing: additive vs multiplicative.\n")
cat("A product only vetoes an attribute that is BOTH low AND heavily weighted.\n")
task_d <- bind_rows(
  score_persona("dce_average", "additive"),
  score_persona("dce_average", "multiplicative"),
  score_persona("bone_sparing", "additive"),
  score_persona("bone_sparing", "multiplicative")
)
print(task_d)
p_d <- (plot_cui("dce_average", "additive") + plot_cui("dce_average", "multiplicative")) /
  (plot_cui("bone_sparing", "additive") + plot_cui("bone_sparing", "multiplicative"))
print(p_d)
ggsave(file.path(fig_dir, "05_taskD_add_vs_mult.png"), p_d, width = 11, height = 8, dpi = 200)

cat("\n============================================================\n")
cat("OPTIONAL  Two virtual patients (Winzenborg Figure 4)\n")
cat("============================================================\n")
print(bind_rows(
  score_persona("pain_dominant"),
  score_persona("bone_sparing")
))
p_opt <- plot_cui("pain_dominant") + plot_cui("bone_sparing")
print(p_opt)
ggsave(file.path(fig_dir, "06_virtual_patients.png"), p_opt, width = 11, height = 5, dpi = 200)

cat("\n============================================================\n")
cat("DEBRIEF  One sentence to leave with\n")
cat("============================================================\n")
cat("In this chronic non-oncology setting the CUI peak is weight-sensitive:\n")
cat("trial-like weights favor 150 mg QD; dyspareunia-priority favors 200 mg BID;\n")
cat("DCE-average patient preferences peak at a hypothetical in-between dose.\n")
cat("CUI recommends a dose only after the weights are declared.\n")
cat("\nPlots saved under figures/\n")
cat("Open the Shiny app with:  shiny::runApp('scripts/03_app.R')\n")
