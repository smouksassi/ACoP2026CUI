# Exercise 2 local Shiny app.
#   shiny::runApp("scripts/03_app.R")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(plotly)
})

if (file.exists(file.path(getwd(), "scripts", "00_cui_functions.R"))) {
  root <- getwd()
} else if (file.exists("00_cui_functions.R")) {
  root <- dirname(getwd())
} else {
  stop("Run from exercise_2_elagolix/")
}

source(file.path(root, "scripts", "00_cui_functions.R"))
grid_path <- file.path(root, "data", "endpoint_grid.csv")
if (!file.exists(grid_path)) {
  source(file.path(root, "scripts", "01_build_endpoint_grid.R"))
}
grid <- read.csv(grid_path)

# Locked-tab reconstruction (same numbers as scripts/01_build_endpoint_grid.R).
emax_params <- data.frame(
  Series = c("DYSM", "NMPP", "DYSP", "P_HF", "P_BMD"),
  Used_as = c("DYSM", "NMPP", "DYSP", "NoHF = 1 − P_HF", "BMDok = 1 − P_BMD"),
  E0 = c(0.210, 0.365, 0.300, 0.070, 0.100),
  Emax = c(0.84, 0.63, 0.72, 0.50, 0.78),
  ED50 = c(130, 75, 310, 250, 320),
  h = c(1.90, 1.30, 2.40, 1.50, 3.00),
  stringsAsFactors = FALSE
)
published_anchors <- data.frame(
  tdd_mg = c(0, 150, 400),
  DYSM = c(0.21, 0.45, 0.74),
  NMPP = c(0.365, 0.50, 0.56),
  DYSP = c(0.31, 0.39, 0.54),
  P_HF = c(0.07, 0.24, 0.42),
  P_BMD5 = c(0.015, 0.03, 0.19),
  stringsAsFactors = FALSE
)

preset_choices <- c(
  "Task A — trial-like (DYSP off)" = "trial_equal",
  "Task B — dyspareunia priority" = "dyspareunia_priority",
  "Task C — DCE average patient" = "dce_average",
  "Pain-dominant patient" = "pain_dominant",
  "Bone-sparing patient" = "bone_sparing",
  "Custom (you moved a slider)" = "custom"
)

preset_short <- c(
  trial_equal = "Task A — trial-like",
  dyspareunia_priority = "Task B — dyspareunia priority",
  dce_average = "Task C — DCE average",
  pain_dominant = "Pain-dominant",
  bone_sparing = "Bone-sparing",
  custom = "Custom weights"
)

slider_ids <- c(
  DYSM = "w_dysm", NMPP = "w_nmpp", DYSP = "w_dysp",
  NoHF = "w_nohf", BMDok = "w_bmd"
)

slider_css <- paste(
  vapply(names(slider_ids), function(nm) {
    id <- slider_ids[[nm]]
    col <- ep_cols[[nm]]
    sprintf(
      paste(
        "#%s { border-left: 4px solid %s; padding-left: 8px; }",
        "#%s .control-label { color: %s; font-weight: 700; }",
        "#%s .irs-bar, #%s .irs--shiny .irs-bar {",
        "  background: %s !important; border-color: %s !important; }",
        "#%s .irs-handle, #%s .irs--shiny .irs-handle {",
        "  border-color: %s !important; background: %s !important; }",
        "#%s .irs-handle > i:first-child,",
        "#%s .irs--shiny .irs-handle > i:first-child {",
        "  background: %s !important; }",
        "#%s .irs-single, #%s .irs--shiny .irs-single,",
        "#%s .irs-from, #%s .irs-to {",
        "  background: %s !important; }",
        "#%s .irs-min, #%s .irs-max { color: %s !important; }",
        sep = "\n"
      ),
      id, col,
      id, col,
      id, id, col, col,
      id, id, col, col,
      id, id, col,
      id, id, id, id, col,
      id, id, col
    )
  }, character(1)),
  collapse = "\n"
)

table_css <- paste(
  ".cui-table { width: 100%; font-size: 0.95rem; }",
  ".cui-table th, .cui-table td { text-align: center; padding: 0.4rem 0.55rem; }",
  ".cui-table th:nth-child(1), .cui-table td:nth-child(1),",
  ".cui-table th:nth-child(2), .cui-table td:nth-child(2) { text-align: left; }",
  paste0(
    sprintf(
      ".cui-table th.ep-%s, .cui-table td.ep-%s { color: %s; font-weight: 700; }",
      names(ep_cols), names(ep_cols), unname(ep_cols)
    ),
    collapse = "\n"
  ),
  paste0(
    sprintf(
      ".cui-table th.ep-%s { border-bottom: 3px solid %s; }",
      names(ep_cols), unname(ep_cols)
    ),
    collapse = "\n"
  ),
  sep = "\n"
)

banner_css <- paste(
  ".taskd-banner { display: flex; align-items: center; justify-content: space-between;",
  "  gap: 1rem; flex-wrap: nowrap; font-size: 0.95rem; line-height: 1.25;",
  "  white-space: nowrap; margin-bottom: 0.4rem; }",
  ".taskd-banner .shiny-input-container { margin-bottom: 0; white-space: nowrap; }",
  sep = "\n"
)

series <- lapply(c(endpoint_cols, "CUI"), function(nm) {
  list(
    col = unname(ep_cols[[nm]]),
    width = unname(ep_width_plotly[[nm]]),
    dash = unname(ep_lty_plotly[[nm]])
  )
})
names(series) <- c(endpoint_cols, "CUI")

colored <- function(label, value, key, digits = 2) {
  sprintf(
    "<span style='color:%s;font-weight:700'>%s %s</span>",
    ep_cols[[key]],
    label,
    formatC(value, format = "f", digits = digits)
  )
}

slider_label <- function(key, text = key) {
  HTML(sprintf("<span style='color:%s;font-weight:700'>%s</span>", ep_cols[[key]], text))
}

weight_caption <- function(w) {
  paste0(
    colored("DYSM", w["DYSM"], "DYSM"), " &nbsp;",
    colored("NMPP", w["NMPP"], "NMPP"), " &nbsp;",
    colored("DYSP", w["DYSP"], "DYSP"), " &nbsp;",
    colored("NoHF", w["NoHF"], "NoHF"), " &nbsp;",
    colored("BMDok", w["BMDok"], "BMDok")
  )
}

# Task C = DCE additive; Task D = same weights, switch form; then bone-sparing both forms.
compare_specs <- list(
  list(id = "cmp_dce_add", persona = "dce_average", form = "additive"),
  list(id = "cmp_dce_mult", persona = "dce_average", form = "multiplicative"),
  list(id = "cmp_bone_add", persona = "bone_sparing", form = "additive"),
  list(id = "cmp_bone_mult", persona = "bone_sparing", form = "multiplicative")
)
compare_specs <- lapply(compare_specs, function(spec) {
  spec$title <- scenario_title(spec$persona, spec$form)
  spec
})

shared_xaxis <- function(doses, lock = TRUE) {
  list(
    title = "Total daily dose (mg)  ·  10 mg grid",
    tickmode = "array",
    tickvals = doses,
    ticktext = ifelse(doses %% 50 == 0, as.character(doses), ""),
    range = c(-8, 408),
    zeroline = FALSE,
    fixedrange = lock
  )
}

shared_yaxis <- function(title = "CUI", lock = TRUE, ylim = c(0, 1)) {
  list(
    title = title,
    range = ylim,
    rangemode = "normal",
    zeroline = FALSE,
    fixedrange = lock
  )
}

cui_panel_plot <- function(persona, form, show_ep = FALSE) {
  w <- persona_weights[[persona]]
  dat <- add_cui(grid, w, form = form)
  pk <- cui_peak(dat)
  doses <- sort(unique(dat$tdd_mg))
  p <- plot_ly()
  if (isTRUE(show_ep)) {
    for (nm in endpoint_cols) {
      sty <- series[[nm]]
      p <- add_trace(
        p,
        x = dat$tdd_mg,
        y = dat[[nm]],
        type = "scatter",
        mode = "lines",
        name = nm,
        line = list(color = sty$col, width = sty$width, dash = sty$dash),
        hoverinfo = "none",
        showlegend = FALSE
      )
    }
  }
  p %>%
    add_trace(
      x = dat$tdd_mg,
      y = dat$CUI,
      type = "scatter",
      mode = "lines+markers",
      name = "CUI",
      line = list(color = ep_cols[["CUI"]], width = 3.4),
      marker = list(size = 4, color = ep_cols[["CUI"]]),
      hovertemplate = sprintf(
        "<b>%%{x} mg TDD</b><br><span style='color:%s'><b>CUI</b>    %%{y:.3f}</span><extra></extra>",
        ep_cols[["CUI"]]
      ),
      hoverlabel = list(align = "left", bgcolor = "white"),
      showlegend = FALSE
    ) %>%
    add_trace(
      x = pk$tdd_mg,
      y = pk$CUI,
      type = "scatter",
      mode = "markers",
      name = "Peak",
      marker = list(size = 11, color = peak_col, line = list(color = "white", width = 1)),
      hovertemplate = sprintf(
        "<b>Peak</b><br>%s mg TDD<br>CUI %.3f<br>%s<extra></extra>",
        pk$tdd_mg, pk$CUI, nearest_approved(pk$tdd_mg)
      ),
      showlegend = FALSE
    ) %>%
    layout(
      showlegend = FALSE,
      hovermode = "closest",
      margin = list(l = 48, r = 12, t = 28, b = 48),
      xaxis = shared_xaxis(doses, lock = TRUE),
      yaxis = shared_yaxis("CUI", lock = TRUE),
      shapes = list(
        list(type = "line", x0 = 150, x1 = 150, y0 = 0, y1 = 1,
             yref = "paper", line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1)),
        list(type = "line", x0 = 400, x1 = 400, y0 = 0, y1 = 1,
             yref = "paper", line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1))
      ),
      annotations = list(
        list(x = 150, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
             text = "150 QD", showarrow = FALSE, font = list(size = 10, color = "#5C5C5C")),
        list(x = 400, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
             text = "200 BID", showarrow = FALSE, font = list(size = 10, color = "#5C5C5C")),
        list(
          x = pk$tdd_mg, y = pk$CUI, xref = "x", yref = "y",
          text = sprintf("max at %s mg", pk$tdd_mg),
          showarrow = FALSE, yanchor = "bottom", yshift = 8,
          font = list(size = 12, color = peak_col, family = "Arial")
        )
      )
    ) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c(
      "lasso2d", "select2d", "autoScale2d"
    ))
}

compare_card <- function(spec) {
  w <- persona_weights[[spec$persona]]
  pk <- cui_peak(add_cui(grid, w, form = spec$form))
  card(
    full_screen = TRUE,
    min_height = 300,
    card_header(HTML(paste0(
      "<b>", spec$title, "</b> &nbsp;·&nbsp; raw &nbsp;",
      weight_caption(w),
      " &nbsp;·&nbsp; ",
      colored("peak", pk$CUI, "CUI", digits = 3),
      sprintf(" at %s mg", pk$tdd_mg)
    ))),
    card_body(
      class = "p-0",
      plotlyOutput(spec$id, height = "34vh")
    )
  )
}

ui <- page_sidebar(
  title = "Exercise 2  ·  Elagolix-like CUI  ·  6-month horizon",
  fillable = TRUE,
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D"
  ),
  sidebar = sidebar(
    id = "controls",
    title = NULL,
    width = 300,
    open = TRUE,
    position = "left",
    tags$style(HTML(paste(slider_css, table_css, banner_css, sep = "\n"))),
    selectInput(
      "preset",
      "Weight preset",
      choices = preset_choices,
      selected = "trial_equal"
    ),
    radioButtons(
      "form", "CUI form",
      choices = c("Additive" = "additive", "Multiplicative" = "multiplicative"),
      selected = "additive"
    ),
    sliderInput("w_dysm", slider_label("DYSM"), 0, 4, 1, 0.1),
    sliderInput("w_nmpp", slider_label("NMPP"), 0, 4, 1, 0.1),
    sliderInput("w_dysp", slider_label("DYSP"), 0, 4, 0, 0.1),
    sliderInput("w_nohf", slider_label("NoHF", "No hot flush"), 0, 4, 1, 0.1),
    sliderInput("w_bmd", slider_label("BMDok", "BMD preserved"), 0, 4, 1, 0.1),
    checkboxInput(
      "show_endpoints",
      "Overlay endpoint curves",
      value = TRUE
    )
  ),
  navset_tab(
    id = "main_tabs",
    nav_panel(
      "Explorer",
      value = "explorer",
      card(
        full_screen = TRUE,
        min_height = 560,
        card_header(uiOutput("plot_caption")),
        card_body(
          class = "p-0",
          plotlyOutput("main_plot", height = "62vh")
        )
      ),
      card(
        card_header("CUI at placebo and the two marketed-like doses"),
        htmlOutput("dose_table")
      )
    ),
    nav_panel(
      "Task D · add vs mult",
      value = "compare",
      div(
        class = "taskd-banner",
        tags$span("Task C = DCE average additive. Task D = same weights, switch form, then bone-sparing both forms."),
        checkboxInput("compare_show_endpoints", "Overlay endpoint curves", value = FALSE)
      ),
      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        compare_card(compare_specs[[1]]),
        compare_card(compare_specs[[2]]),
        compare_card(compare_specs[[3]]),
        compare_card(compare_specs[[4]])
      ),
      card(
        card_header("Peak TDD and nearer marketed-like dose"),
        htmlOutput("compare_table")
      )
    ),
    nav_panel(
      "Emax construction",
      value = "emax",
      uiOutput("emax_tab")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$main_tabs, {
    if (input$main_tabs %in% c("compare", "emax")) {
      toggle_sidebar(id = "controls", open = FALSE)
    } else {
      toggle_sidebar(id = "controls", open = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$preset, {
    if (input$preset == "custom") return()
    w <- persona_weights[[input$preset]]
    updateSliderInput(session, "w_dysm", value = unname(w["DYSM"]))
    updateSliderInput(session, "w_nmpp", value = unname(w["NMPP"]))
    updateSliderInput(session, "w_dysp", value = unname(w["DYSP"]))
    updateSliderInput(session, "w_nohf", value = unname(w["NoHF"]))
    updateSliderInput(session, "w_bmd", value = unname(w["BMDok"]))
  })

  observeEvent(
    list(input$w_dysm, input$w_nmpp, input$w_dysp, input$w_nohf, input$w_bmd),
    {
      if (is.null(input$preset) || input$preset == "custom") return()
      target <- persona_weights[[input$preset]]
      cur <- c(
        DYSM = input$w_dysm, NMPP = input$w_nmpp, DYSP = input$w_dysp,
        NoHF = input$w_nohf, BMDok = input$w_bmd
      )
      if (all(abs(cur[names(target)] - target) < 0.06)) return()
      updateSelectInput(session, "preset", selected = "custom")
    },
    ignoreInit = TRUE
  )

  weights <- reactive({
    c(
      DYSM = input$w_dysm, NMPP = input$w_nmpp, DYSP = input$w_dysp,
      NoHF = input$w_nohf, BMDok = input$w_bmd
    )
  })

  scored <- reactive({
    add_cui(grid, weights(), form = input$form)
  })

  peak <- reactive({
    cui_peak(scored())
  })

  output$plot_caption <- renderUI({
    pk <- peak()
    w <- weights()
    HTML(paste0(
      "<span style='font-weight:700'>", scenario_title(input$preset, input$form), "</span>",
      " &nbsp;·&nbsp; raw &nbsp;",
      weight_caption(w),
      " &nbsp;·&nbsp; ",
      colored("peak", pk$CUI, "CUI", digits = 3),
      sprintf(" at %s mg", pk$tdd_mg)
    ))
  })

  output$main_plot <- renderPlotly({
    dat <- scored()
    pk <- peak()
    show_ep <- isTRUE(input$show_endpoints)
    doses <- sort(unique(dat$tdd_mg))

    hover_cui <- sprintf(
      "<b>%s mg TDD</b><br><span style='color:%s'><b>CUI</b>    %.3f</span>",
      dat$tdd_mg, ep_cols[["CUI"]], dat$CUI
    )
    if (show_ep) {
      hover_cui <- paste0(
        hover_cui,
        sprintf("<br><span style='color:%s'>DYSM   %.2f</span>", ep_cols[["DYSM"]], dat$DYSM),
        sprintf("<br><span style='color:%s'>NMPP   %.2f</span>", ep_cols[["NMPP"]], dat$NMPP),
        sprintf("<br><span style='color:%s'>DYSP   %.2f</span>", ep_cols[["DYSP"]], dat$DYSP),
        sprintf("<br><span style='color:%s'>NoHF   %.2f</span>", ep_cols[["NoHF"]], dat$NoHF),
        sprintf("<br><span style='color:%s'>BMDok  %.2f</span>", ep_cols[["BMDok"]], dat$BMDok)
      )
    }

    ylim <- if (show_ep) {
      c(0, 1)
    } else {
      rng <- range(dat$CUI, na.rm = TRUE)
      pad <- max(0.03, diff(rng) * 0.35)
      c(max(0, rng[1] - pad), min(1, rng[2] + pad))
    }

    p <- plot_ly()

    if (show_ep) {
      for (nm in endpoint_cols) {
        sty <- series[[nm]]
        p <- add_trace(
          p,
          x = dat$tdd_mg,
          y = dat[[nm]],
          type = "scatter",
          mode = "lines",
          name = nm,
          line = list(color = sty$col, width = sty$width, dash = sty$dash),
          hoverinfo = "none",
          legendgroup = nm,
          legendrank = match(nm, legend_order)
        )
      }
    }

    p <- add_trace(
      p,
      x = dat$tdd_mg,
      y = dat$CUI,
      type = "scatter",
      mode = "lines+markers",
      name = "CUI",
      line = list(color = series$CUI$col, width = series$CUI$width),
      marker = list(size = 5, color = series$CUI$col),
      hovertemplate = paste0(hover_cui, "<extra></extra>"),
      hoverlabel = list(
        align = "left",
        bgcolor = "white",
        font = list(family = "Consolas, monospace", size = 13)
      ),
      legendgroup = "CUI",
      legendrank = match("CUI", legend_order)
    )

    p <- add_trace(
      p,
      x = pk$tdd_mg,
      y = pk$CUI,
      type = "scatter",
      mode = "markers",
      name = "Peak",
      marker = list(size = 12, color = peak_col, line = list(color = "white", width = 1)),
      hovertemplate = sprintf(
        "<b>Peak</b><br>%s mg TDD<br>CUI %.3f<br>%s<extra></extra>",
        pk$tdd_mg, pk$CUI, nearest_approved(pk$tdd_mg)
      ),
      hoverlabel = list(align = "left", font = list(size = 13)),
      legendgroup = "Peak",
      legendrank = match("Peak", legend_order)
    )

    layout(
      p,
      hovermode = "closest",
      legend = list(
        orientation = "h", y = -0.22, x = 0, font = list(size = 12),
        traceorder = "normal"
      ),
      margin = list(l = 60, r = 20, t = 28, b = 80),
      xaxis = shared_xaxis(doses, lock = FALSE),
      yaxis = shared_yaxis(
        if (show_ep) "Desirability / CUI" else "CUI",
        lock = FALSE,
        ylim = ylim
      ),
      shapes = list(
        list(type = "line", x0 = 150, x1 = 150, y0 = 0, y1 = 1,
             yref = "paper", line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1)),
        list(type = "line", x0 = 400, x1 = 400, y0 = 0, y1 = 1,
             yref = "paper", line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1))
      ),
      annotations = list(
        list(x = 150, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
             text = "150 QD", showarrow = FALSE, font = list(size = 11, color = "#5C5C5C")),
        list(x = 400, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
             text = "200 BID", showarrow = FALSE, font = list(size = 11, color = "#5C5C5C"))
      )
    ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c(
        "lasso2d", "select2d", "autoScale2d"
      ))
  })

  output$dose_table <- renderUI({
    raw <- weights()
    w <- normalize_weights(raw)
    rows <- scored() %>%
      filter(tdd_mg %in% c(0, 150, 400)) %>%
      transmute(
        TDD = as.character(tdd_mg),
        Regimen = regimen_label,
        CUI = sprintf("%.3f", CUI),
        DYSM = sprintf("%.2f", DYSM),
        NMPP = sprintf("%.2f", NMPP),
        DYSP = sprintf("%.2f", DYSP),
        NoHF = sprintf("%.2f", NoHF),
        BMDok = sprintf("%.2f", BMDok)
      )
    extra <- rbind(
      data.frame(
        TDD = "",
        Regimen = "Raw weight (sliders)",
        CUI = "",
        DYSM = sprintf("%.2f", raw["DYSM"]),
        NMPP = sprintf("%.2f", raw["NMPP"]),
        DYSP = sprintf("%.2f", raw["DYSP"]),
        NoHF = sprintf("%.2f", raw["NoHF"]),
        BMDok = sprintf("%.2f", raw["BMDok"]),
        stringsAsFactors = FALSE
      ),
      data.frame(
        TDD = "",
        Regimen = "Normalized weight",
        CUI = "",
        DYSM = sprintf("%.2f", w["DYSM"]),
        NMPP = sprintf("%.2f", w["NMPP"]),
        DYSP = sprintf("%.2f", w["DYSP"]),
        NoHF = sprintf("%.2f", w["NoHF"]),
        BMDok = sprintf("%.2f", w["BMDok"]),
        stringsAsFactors = FALSE
      )
    )
    tab <- rbind(rows, extra)

    hdr <- paste0(
      "<tr>",
      "<th>TDD</th><th>Regimen</th>",
      "<th class='ep-CUI'>CUI</th>",
      "<th class='ep-DYSM'>DYSM</th>",
      "<th class='ep-NMPP'>NMPP</th>",
      "<th class='ep-DYSP'>DYSP</th>",
      "<th class='ep-NoHF'>NoHF</th>",
      "<th class='ep-BMDok'>BMDok</th>",
      "</tr>"
    )
    body <- apply(tab, 1, function(r) {
      paste0(
        "<tr>",
        sprintf("<td>%s</td><td>%s</td>", r[["TDD"]], r[["Regimen"]]),
        sprintf("<td class='ep-CUI'>%s</td>", r[["CUI"]]),
        sprintf("<td class='ep-DYSM'>%s</td>", r[["DYSM"]]),
        sprintf("<td class='ep-NMPP'>%s</td>", r[["NMPP"]]),
        sprintf("<td class='ep-DYSP'>%s</td>", r[["DYSP"]]),
        sprintf("<td class='ep-NoHF'>%s</td>", r[["NoHF"]]),
        sprintf("<td class='ep-BMDok'>%s</td>", r[["BMDok"]]),
        "</tr>"
      )
    })
    HTML(paste0("<table class='cui-table'><thead>", hdr, "</thead><tbody>", paste(body, collapse = ""), "</tbody></table>"))
  })

  lapply(compare_specs, function(spec) {
    local({
      spec <- spec
      output[[spec$id]] <- renderPlotly({
        cui_panel_plot(
          spec$persona,
          spec$form,
          show_ep = isTRUE(input$compare_show_endpoints)
        )
      })
    })
  })

  output$compare_table <- renderUI({
    rows <- lapply(compare_specs, function(spec) {
      w <- persona_weights[[spec$persona]]
      dat <- add_cui(grid, w, form = spec$form)
      pk <- cui_peak(dat)
      data.frame(
        Scenario = spec$title,
        `Peak TDD` = pk$tdd_mg,
        Peak = sprintf("%.3f", pk$CUI),
        Nearer = nearest_approved(pk$tdd_mg),
        `CUI 150` = sprintf("%.3f", dat$CUI[dat$tdd_mg == 150]),
        `CUI 400` = sprintf("%.3f", dat$CUI[dat$tdd_mg == 400]),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    tab <- do.call(rbind, rows)
    hdr <- paste0(
      "<tr>",
      "<th>Scenario</th><th>Peak TDD</th>",
      "<th class='ep-CUI'>Peak CUI</th>",
      "<th>Nearer regimen</th>",
      "<th class='ep-CUI'>CUI at 150</th>",
      "<th class='ep-CUI'>CUI at 400</th>",
      "</tr>"
    )
    body <- apply(tab, 1, function(r) {
      paste0(
        "<tr>",
        sprintf("<td style='text-align:left'>%s</td>", r[["Scenario"]]),
        sprintf("<td>%s</td>", r[["Peak TDD"]]),
        sprintf("<td class='ep-CUI'>%s</td>", r[["Peak"]]),
        sprintf("<td>%s</td>", r[["Nearer"]]),
        sprintf("<td class='ep-CUI'>%s</td>", r[["CUI 150"]]),
        sprintf("<td class='ep-CUI'>%s</td>", r[["CUI 400"]]),
        "</tr>"
      )
    })
    HTML(paste0("<table class='cui-table'><thead>", hdr, "</thead><tbody>", paste(body, collapse = ""), "</tbody></table>"))
  })

  emax_unlocked <- reactiveVal(FALSE)

  try_emax_unlock <- function(key) {
    if (identical(trimws(key), "CUI")) {
      emax_unlocked(TRUE)
    }
  }

  observeEvent(input$emax_key, {
    try_emax_unlock(input$emax_key)
  }, ignoreInit = TRUE)

  observeEvent(input$emax_unlock, {
    try_emax_unlock(input$emax_key)
  })

  output$emax_tab <- renderUI({
    if (!isTRUE(emax_unlocked())) {
      return(card(
        max_height = 280,
        card_header("Locked  ·  instructor notes"),
        card_body(
          p("This tab documents how the dummy Hill Emax grid was built — parameters, published anchors, and why a few curves were made steeper than the papers. Students do not need it for Tasks A–D."),
          passwordInput("emax_key", "Course keyword", placeholder = "3 letters, all caps"),
          actionButton("emax_unlock", "Unlock", class = "btn-primary"),
          if (!is.null(input$emax_key) && nzchar(trimws(input$emax_key)) && !identical(trimws(input$emax_key), "CUI")) {
            tags$p(style = "color:#B4532A;margin-top:0.6rem;", "That is not the keyword.")
          }
        )
      ))
    }

    dummy_at <- grid %>%
      filter(tdd_mg %in% c(0, 150, 400)) %>%
      transmute(
        TDD = tdd_mg,
        DYSM, NMPP, DYSP,
        P_HF, P_BMD
      )

    param_hdr <- paste0(
      "<tr><th>Emax series</th><th>Enters CUI as</th>",
      "<th>E0</th><th>Emax</th><th>ED50 (mg)</th><th>Hill h</th></tr>"
    )
    param_cls <- c(DYSM = "ep-DYSM", NMPP = "ep-NMPP", DYSP = "ep-DYSP", P_HF = "ep-NoHF", P_BMD = "ep-BMDok")
    param_body <- apply(emax_params, 1, function(r) {
      cls <- param_cls[[r[["Series"]]]]
      paste0(
        "<tr>",
        sprintf("<td class='%s'>%s</td>", cls, r[["Series"]]),
        sprintf("<td class='%s'>%s</td>", cls, r[["Used_as"]]),
        sprintf("<td>%s</td>", r[["E0"]]),
        sprintf("<td>%s</td>", r[["Emax"]]),
        sprintf("<td>%s</td>", r[["ED50"]]),
        sprintf("<td>%s</td>", r[["h"]]),
        "</tr>"
      )
    })

    pct <- function(x) sprintf("%.1f%%", 100 * as.numeric(x))
    pub <- published_anchors
    dum <- dummy_at
    cmp_hdr <- paste0(
      "<tr><th>Source</th><th>TDD</th>",
      "<th class='ep-DYSM'>DYSM</th><th class='ep-NMPP'>NMPP</th>",
      "<th class='ep-DYSP'>DYSP</th><th class='ep-NoHF'>Hot flush</th>",
      "<th class='ep-BMDok'>BMD</th></tr>"
    )
    cmp_rows <- list()
    for (i in seq_len(nrow(pub))) {
      tdd <- pub$tdd_mg[i]
      cmp_rows[[length(cmp_rows) + 1]] <- paste0(
        "<tr><td>Taylor NEJM (published)</td>",
        sprintf("<td>%s</td>", tdd),
        sprintf("<td class='ep-DYSM'>%s</td>", pct(pub$DYSM[i])),
        sprintf("<td class='ep-NMPP'>%s</td>", pct(pub$NMPP[i])),
        sprintf("<td class='ep-DYSP'>%s</td>", pct(pub$DYSP[i])),
        sprintf("<td class='ep-NoHF'>%s</td>", pct(pub$P_HF[i])),
        sprintf("<td class='ep-BMDok'>%s (&gt;5%%)</td>", pct(pub$P_BMD5[i])),
        "</tr>"
      )
      d <- dum[dum$TDD == tdd, ]
      cmp_rows[[length(cmp_rows) + 1]] <- paste0(
        "<tr><td>Dummy Emax (this app)</td>",
        sprintf("<td>%s</td>", tdd),
        sprintf("<td class='ep-DYSM'>%s</td>", pct(d$DYSM)),
        sprintf("<td class='ep-NMPP'>%s</td>", pct(d$NMPP)),
        sprintf("<td class='ep-DYSP'>%s</td>", pct(d$DYSP)),
        sprintf("<td class='ep-NoHF'>%s</td>", pct(d$P_HF)),
        sprintf("<td class='ep-BMDok'>%s (&gt;1%%)</td>", pct(d$P_BMD)),
        "</tr>"
      )
    }

    tagList(
      div(
        class = "taskd-banner",
        tags$span("Dummy Hill Emax reconstructed from public Phase 3 rates — not patient-level data, not a label claim.")
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Hill Emax on a 10 mg TDD grid"),
          tags$pre(style = "font-size:1rem;margin:0.4rem 0 0;",
            "E(d)  =  E0  +  (Emax − E0)  ·  d^h / (ED50^h + d^h)\n",
            "TDD    =  0, 10, …, 400 mg\n",
            "NoHF   =  1 − P(hot flush)\n",
            "BMDok  =  1 − P(BMD loss > 1%)     # Winzenborg 1% surrogate, not published >5%"
          )
        )
      ),
      card(
        card_header("Locked parameters  ·  scripts/01_build_endpoint_grid.R"),
        HTML(paste0("<table class='cui-table'><thead>", param_hdr, "</thead><tbody>", paste(param_body, collapse = ""), "</tbody></table>"))
      ),
      card(
        full_screen = TRUE,
        card_header("Dummy curves vs published anchors at 0 / 150 / 400 mg"),
        card_body(class = "p-0", plotlyOutput("emax_plot", height = "48vh"))
      ),
      card(
        card_header("Published (Taylor NEJM 2017, approx. EM-I/II) vs dummy at the same doses"),
        HTML(paste0("<table class='cui-table'><thead>", cmp_hdr, "</thead><tbody>", paste(unlist(cmp_rows), collapse = ""), "</tbody></table>"))
      ),
      card(
        card_header("Why the dummy is not a least-squares fit to the papers"),
        tags$ul(
          tags$li(HTML("<span class='ep-DYSM' style='font-weight:700'>DYSM</span> at 150 mg is steeper than published (~57% vs 45%) so Task A can peak near 150 QD and Task B can still flip to 200 BID after one weight change.")),
          tags$li(HTML("<span class='ep-BMDok' style='font-weight:700'>BMD</span> uses Winzenborg’s 1% loss surrogate, not the published &gt;5% rates (1.5 / 3 / 19%). The 5% curve is too flat for a classroom CUI to move.")),
          tags$li(HTML("<span class='ep-DYSP' style='font-weight:700'>DYSP</span> ED50 is high (310 mg) because 150 mg was not significant in Phase 3; most of the gain sits at 200 BID.")),
          tags$li("Safety is inverted before the CUI: the sliders weight NoHF and BMDok (higher = better), never raw risk."),
          tags$li("Sources: Taylor et al. NEJM 2017; Winzenborg, Soliman, Shebley CPT:PSP 2021 Eq. 1; Poulos et al. Value Health 2019 DCE weights.")
        )
      ),
      card(
        card_header("Tuner targets (instructor/tune_grid.R)  ·  why these ED50 / h values"),
        HTML(paste0(
          "<table class='cui-table'><thead>",
          "<tr><th>Persona</th><th>Form</th><th>Target peak</th><th>Locked dummy peak</th><th>Why</th></tr>",
          "</thead><tbody>",
          "<tr><td>Task A trial-like</td><td>additive</td><td>~150</td><td>160 mg</td><td>near 150 QD when DYSP is off</td></tr>",
          "<tr><td>Task B dyspareunia</td><td>additive</td><td>~400</td><td>400 mg</td><td>one weight change flips the label dose</td></tr>",
          "<tr><td>Task C DCE average</td><td>additive</td><td>200–260 (paper ~230)</td><td>240 mg</td><td>hypothetical in-between dose</td></tr>",
          "<tr><td>Task D DCE</td><td>multiplicative</td><td>right of C</td><td>310 mg</td><td>DCE barely weights BMD, so the product does not veto high dose</td></tr>",
          "<tr><td>Bone-sparing</td><td>add / mult</td><td>≤150</td><td>140 / 170 mg</td><td>both forms stay near 150 QD</td></tr>",
          "</tbody></table>"
        ))
      )
    )
  })

  output$emax_plot <- renderPlotly({
    req(emax_unlocked())
    doses <- sort(unique(grid$tdd_mg))
    risk_map <- c(DYSM = "DYSM", NMPP = "NMPP", DYSP = "DYSP", P_HF = "P_HF", P_BMD = "P_BMD")
    col_map <- c(
      DYSM = unname(ep_cols[["DYSM"]]),
      NMPP = unname(ep_cols[["NMPP"]]),
      DYSP = unname(ep_cols[["DYSP"]]),
      P_HF = unname(ep_cols[["NoHF"]]),
      P_BMD = unname(ep_cols[["BMDok"]])
    )
    lty_map <- c(DYSM = "solid", NMPP = "solid", DYSP = "solid", P_HF = "dash", P_BMD = "dot")
    p <- plot_ly()
    for (nm in names(risk_map)) {
      p <- add_trace(
        p,
        x = grid$tdd_mg,
        y = grid[[risk_map[[nm]]]],
        type = "scatter",
        mode = "lines",
        name = nm,
        line = list(color = col_map[[nm]], width = 3, dash = lty_map[[nm]]),
        legendrank = match(nm, names(risk_map)),
        hovertemplate = sprintf("<b>%s</b><br>%%{x} mg<br>%%{y:.2f}<extra></extra>", nm)
      )
    }
    pub_long <- data.frame(
      tdd_mg = rep(published_anchors$tdd_mg, 4),
      name = rep(c("DYSM", "NMPP", "DYSP", "P_HF"), each = 3),
      y = c(
        published_anchors$DYSM,
        published_anchors$NMPP,
        published_anchors$DYSP,
        published_anchors$P_HF
      ),
      stringsAsFactors = FALSE
    )
    for (nm in unique(pub_long$name)) {
      sl <- pub_long[pub_long$name == nm, ]
      p <- add_trace(
        p,
        x = sl$tdd_mg,
        y = sl$y,
        type = "scatter",
        mode = "markers",
        name = paste0(nm, " published"),
        marker = list(size = 10, color = col_map[[nm]], symbol = "diamond",
                      line = list(color = "white", width = 1)),
        legendrank = match(nm, names(risk_map)) + 10,
        hovertemplate = sprintf("<b>%s published</b><br>%%{x} mg<br>%%{y:.2f}<extra></extra>", nm)
      )
    }
    p <- add_trace(
      p,
      x = published_anchors$tdd_mg,
      y = published_anchors$P_BMD5,
      type = "scatter",
      mode = "markers",
      name = "BMD >5% published",
      marker = list(size = 10, color = col_map[["P_BMD"]], symbol = "diamond-open",
                    line = list(color = col_map[["P_BMD"]], width = 2)),
      hovertemplate = "<b>BMD >5% published</b><br>%{x} mg<br>%{y:.2f}<extra></extra>"
    )
    layout(
      p,
      hovermode = "closest",
      legend = list(orientation = "h", y = -0.22, x = 0, font = list(size = 12), traceorder = "normal"),
      margin = list(l = 56, r = 16, t = 28, b = 80),
      xaxis = shared_xaxis(doses, lock = FALSE),
      yaxis = list(title = "Probability  (raw Emax, before inversion)", range = c(0, 1), zeroline = FALSE),
      shapes = list(
        list(type = "line", x0 = 150, x1 = 150, y0 = 0, y1 = 1,
             yref = "paper", line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1)),
        list(type = "line", x0 = 400, x1 = 400, y0 = 0, y1 = 1,
             yref = "paper", line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1))
      ),
      annotations = list(
        list(x = 150, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
             text = "150 QD", showarrow = FALSE, font = list(size = 11, color = "#5C5C5C")),
        list(x = 400, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
             text = "200 BID", showarrow = FALSE, font = list(size = 11, color = "#5C5C5C"))
      )
    ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
  })
}

shinyApp(ui, server)
