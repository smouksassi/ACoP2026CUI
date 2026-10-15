# Shared CUI helpers for Exercise 2 (elagolix / endometriosis)
# Additive form matches Winzenborg, Soliman, Shebley, CPT:PSP 2021, Eq. 1.
# Multiplicative form is the geometric index used in the dummy ADC app.

endpoint_cols <- c("DYSM", "NMPP", "DYSP", "NoHF", "BMDok")

# One style for walkthrough ggplot and Shiny plotly.
# Legend order: DYSM, NMPP, DYSP, NoHF, BMDok, CUI, Peak.
# Benefit solid; NoHF dashed; BMDok dotted; CUI thick navy.
ep_cols <- c(
  DYSM  = "#1B9E77",
  NMPP  = "#377EB8",
  DYSP  = "#984EA3",
  NoHF  = "#FF7F00",
  BMDok = "#E41A1C",
  CUI   = "#0B1F3A"
)
peak_col <- "#B4532A"
ep_lty_gg <- c(
  DYSM = "solid", NMPP = "solid", DYSP = "solid",
  NoHF = "dashed", BMDok = "dotted", CUI = "solid"
)
ep_lty_plotly <- c(
  DYSM = "solid", NMPP = "solid", DYSP = "solid",
  NoHF = "dash", BMDok = "dot", CUI = "solid"
)
legend_order <- c(endpoint_cols, "CUI", "Peak")
ep_width_plotly <- c(
  DYSM = 3, NMPP = 3, DYSP = 3, NoHF = 3, BMDok = 3, CUI = 4.2
)

normalize_weights <- function(w) {
  w <- unlist(w)
  missing <- setdiff(endpoint_cols, names(w))
  if (length(missing)) stop("Missing weights for: ", paste(missing, collapse = ", "))
  w <- w[endpoint_cols]
  if (any(w < 0)) stop("Weights must be non-negative.")
  if (sum(w) <= 0) stop("At least one weight must be > 0.")
  w / sum(w)
}

# U(d) = sum_i  w_i / sum_j w_j  * EP_i(d)
cui_additive <- function(grid, weights) {
  w <- normalize_weights(weights)
  as.numeric(as.matrix(grid[, endpoint_cols]) %*% w)
}

# CUI(d) = prod_i EP_i(d) ^ w_i   with weights already normalized
cui_multiplicative <- function(grid, weights) {
  w <- normalize_weights(weights)
  ep <- as.matrix(grid[, endpoint_cols])
  ep <- pmin(pmax(ep, 1e-6), 1)
  exp(as.numeric(log(ep) %*% w))
}

add_cui <- function(grid, weights, form = c("additive", "multiplicative")) {
  form <- match.arg(form)
  grid$CUI <- if (form == "additive") {
    cui_additive(grid, weights)
  } else {
    cui_multiplicative(grid, weights)
  }
  grid
}

cui_peak <- function(grid) {
  grid[which.max(grid$CUI), c("tdd_mg", "regimen_label", "CUI")]
}

nearest_approved <- function(tdd_mg) {
  if (tdd_mg < 275) "150 mg QD (TDD 150)" else "200 mg BID (TDD 400)"
}

# Published DCE preference weights (Poulos et al. 2019; used in Winzenborg 2021)
# Hot-flash range 3.58-3.66; we use the midpoint 3.62.
persona_weights <- list(
  trial_equal = c(DYSM = 1, NMPP = 1, DYSP = 0, NoHF = 1, BMDok = 1),
  dyspareunia_priority = c(DYSM = 1, NMPP = 1, DYSP = 2, NoHF = 1, BMDok = 1),
  dce_average = c(DYSM = 1.48, NMPP = 1.49, DYSP = 1.70, NoHF = 3.62, BMDok = 0.49),
  pain_dominant = c(DYSM = 2.0, NMPP = 2.0, DYSP = 2.0, NoHF = 1.0, BMDok = 0.3),
  bone_sparing = c(DYSM = 1.5, NMPP = 1.5, DYSP = 0.5, NoHF = 1.0, BMDok = 3.0)
)

persona_labels <- c(
  trial_equal = "Trial-like: equal DYSM / NMPP / NoHF / BMDok; DYSP off",
  dyspareunia_priority = "Dyspareunia matters: DYSP weight = 2, others = 1",
  dce_average = "DCE average patient (Poulos 2019 weights)",
  pain_dominant = "Pain-dominant patient",
  bone_sparing = "Bone-sparing patient"
)

# Shared titles: Task C is DCE + additive; switching that form is Task D.
scenario_title <- function(persona, form = c("additive", "multiplicative")) {
  form <- match.arg(form)
  if (is.null(persona) || is.na(persona) || identical(persona, "custom")) {
    return(paste0("Custom weights  ·  ", form))
  }
  if (identical(persona, "dce_average") && identical(form, "multiplicative")) {
    return("Task D — DCE average  ·  multiplicative")
  }
  if (identical(persona, "dce_average")) {
    return("Task C — DCE average  ·  additive")
  }
  base <- switch(
    persona,
    trial_equal = "Task A — trial-like",
    dyspareunia_priority = "Task B — dyspareunia priority",
    pain_dominant = "Pain-dominant",
    bone_sparing = "Bone-sparing",
    persona
  )
  paste0(base, "  ·  ", form)
}
