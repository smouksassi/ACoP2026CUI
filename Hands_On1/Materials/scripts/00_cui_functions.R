# Shared CUI helpers for Exercise 1 (dummy ADC / oncology)
# Multiplicative form is the original dummy ADC app:
#   CUI(d) = p_ORR^w * (1-p_NEU)^w * (1-p_DR)^w   with weights normalized
# Additive form is the Exercise 2 default (Winzenborg 2021 Eq. 1), used here
# so students see both on the same three numbers.

endpoint_cols <- c("ORR", "NoNEU", "NoDR")

# Same spirit as Exercise 2: one palette for sliders, plots, tables.
ep_cols <- c(
  ORR   = "#1B9E77",
  NoNEU = "#D95F02",
  NoDR  = "#7570B3",
  CUI   = "#0B1F3A"
)
peak_col <- "#B4532A"
ep_lty_gg <- c(ORR = "solid", NoNEU = "dashed", NoDR = "dotted", CUI = "solid")
ep_lty_plotly <- c(ORR = "solid", NoNEU = "dash", NoDR = "dot", CUI = "solid")
legend_order <- c(endpoint_cols, "CUI", "Peak")
ep_width_plotly <- c(ORR = 3, NoNEU = 3, NoDR = 3, CUI = 4.2)

normalize_weights <- function(w) {
  w <- unlist(w)
  missing <- setdiff(endpoint_cols, names(w))
  if (length(missing)) stop("Missing weights for: ", paste(missing, collapse = ", "))
  w <- w[endpoint_cols]
  if (any(w < 0)) stop("Weights must be non-negative.")
  if (sum(w) <= 0) stop("At least one weight must be > 0.")
  w / sum(w)
}

cui_additive <- function(grid, weights) {
  w <- normalize_weights(weights)
  as.numeric(as.matrix(grid[, endpoint_cols]) %*% w)
}

cui_multiplicative <- function(grid, weights) {
  w <- normalize_weights(weights)
  ep <- as.matrix(grid[, endpoint_cols])
  ep <- pmin(pmax(ep, 1e-6), 1)
  exp(as.numeric(log(ep) %*% w))
}

add_cui <- function(grid, weights, form = c("multiplicative", "additive")) {
  form <- match.arg(form)
  grid$CUI <- if (form == "additive") {
    cui_additive(grid, weights)
  } else {
    cui_multiplicative(grid, weights)
  }
  grid
}

cui_peak <- function(grid) {
  grid[which.max(grid$CUI), c("dose_mgkg", "regimen_label", "CUI")]
}

nearest_label <- function(dose_mgkg) {
  if (dose_mgkg < 4.5) {
    "below label-like (5.4 mg/kg)"
  } else if (dose_mgkg <= 5.8) {
    "5.4 mg/kg (label-like)"
  } else {
    "above label-like (5.4 mg/kg)"
  }
}

# Raw weights: students normalize these. Default matches the original ADC app 0.50 / 0.25 / 0.25.
persona_weights <- list(
  default_balanced = c(ORR = 2, NoNEU = 1, NoDR = 1),
  efficacy_priority = c(ORR = 8, NoNEU = 1, NoDR = 1),
  safety_priority = c(ORR = 2, NoNEU = 4, NoDR = 4),
  equal = c(ORR = 1, NoNEU = 1, NoDR = 1)
)

persona_labels <- c(
  default_balanced = "Task A — balanced (ORR twice each safety)",
  efficacy_priority = "Task B — efficacy priority",
  safety_priority = "Task C — safety priority",
  equal = "Equal raw weights"
)

scenario_title <- function(persona, form = c("multiplicative", "additive")) {
  form <- match.arg(form)
  if (is.null(persona) || is.na(persona) || identical(persona, "custom")) {
    return(paste0("Custom weights  ·  ", form))
  }
  base <- switch(
    persona,
    default_balanced = "Task A — balanced",
    efficacy_priority = "Task B — efficacy priority",
    safety_priority = "Task C — safety priority",
    equal = "Equal weights",
    persona
  )
  paste0(base, "  ·  ", form)
}

# Pencil-and-paper helper: one dose, both CUI forms, printed steps.
cui_by_hand <- function(orr, p_neu, p_dr, weights, digits = 3) {
  w_raw <- unlist(weights)[endpoint_cols]
  w <- normalize_weights(w_raw)
  no_neu <- 1 - p_neu
  no_dr <- 1 - p_dr
  add <- as.numeric(w["ORR"] * orr + w["NoNEU"] * no_neu + w["NoDR"] * no_dr)
  mult <- as.numeric((orr^w["ORR"]) * (no_neu^w["NoNEU"]) * (no_dr^w["NoDR"]))
  list(
    invert = c(ORR = orr, NoNEU = no_neu, NoDR = no_dr),
    w_raw = w_raw,
    w_hat = w,
    additive = add,
    multiplicative = mult
  )
}

print_cui_by_hand <- function(orr, p_neu, p_dr, weights, label = "") {
  out <- cui_by_hand(orr, p_neu, p_dr, weights)
  w <- out$w_hat
  cat("\n----------  CUI by hand", if (nzchar(label)) paste0(" · ", label), "  ----------\n", sep = "")
  cat(sprintf("Given   p_ORR = %.2f    p_NEU = %.2f    p_DR = %.2f\n", orr, p_neu, p_dr))
  cat("Step 1  Invert safety so every EP is 'higher is better'\n")
  cat(sprintf("         EP_ORR   = %.2f\n", out$invert["ORR"]))
  cat(sprintf("         EP_NoNEU = 1 - %.2f = %.2f\n", p_neu, out$invert["NoNEU"]))
  cat(sprintf("         EP_NoDR  = 1 - %.2f = %.2f\n", p_dr, out$invert["NoDR"]))
  cat("Step 2  Normalize weights   ŵ = w / sum(w)\n")
  cat(sprintf("         raw w  =  ORR %.2f   NoNEU %.2f   NoDR %.2f   (sum = %.2f)\n",
              out$w_raw["ORR"], out$w_raw["NoNEU"], out$w_raw["NoDR"], sum(out$w_raw)))
  cat(sprintf("         ŵ      =  ORR %.2f   NoNEU %.2f   NoDR %.2f\n",
              w["ORR"], w["NoNEU"], w["NoDR"]))
  cat("Step 3  Additive     U = Σ  ŵ_i · EP_i\n")
  cat(sprintf("         U = %.2f×%.2f + %.2f×%.2f + %.2f×%.2f = %.3f\n",
              w["ORR"], out$invert["ORR"],
              w["NoNEU"], out$invert["NoNEU"],
              w["NoDR"], out$invert["NoDR"],
              out$additive))
  cat("Step 4  Multiplicative   CUI = Π  EP_i ^ ŵ_i\n")
  cat(sprintf("         CUI = %.2f^%.2f × %.2f^%.2f × %.2f^%.2f = %.3f\n",
              out$invert["ORR"], w["ORR"],
              out$invert["NoNEU"], w["NoNEU"],
              out$invert["NoDR"], w["NoDR"],
              out$multiplicative))
  cat("The two numbers are close here because no EP is near zero.\n")
  invisible(out)
}
