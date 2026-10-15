# Exercise 1 local Shiny app.
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
  stop("Run from exercise_1_adc/")
}

source(file.path(root, "scripts", "00_cui_functions.R"))
grid_path <- file.path(root, "data", "endpoint_grid.csv")
if (!file.exists(grid_path)) {
  source(file.path(root, "scripts", "01_build_endpoint_grid.R"))
}
grid <- read.csv(grid_path)

preset_choices <- c(
  "Task A — balanced (original app)" = "default_balanced",
  "Task B — efficacy priority" = "efficacy_priority",
  "Task C — safety priority" = "safety_priority",
  "Equal raw weights" = "equal",
  "Custom (you moved a slider)" = "custom"
)

slider_ids <- c(ORR = "w_orr", NoNEU = "w_noneu", NoDR = "w_nodr")

slider_css <- paste(
  vapply(names(slider_ids), function(nm) {
    id <- slider_ids[[nm]]
    col <- ep_cols[[nm]]
    sprintf(
      paste(
        "#%s { border-left: 4px solid %s; padding-left: 8px; }",
        "#%s .control-label { color: %s; font-weight: 700; }",
        "#%s .irs-bar, #%s .irs--shiny .irs-bar { background: %s !important; border-color: %s !important; }",
        "#%s .irs-handle, #%s .irs--shiny .irs-handle { border-color: %s !important; background: %s !important; }",
        "#%s .irs-handle > i:first-child, #%s .irs--shiny .irs-handle > i:first-child { background: %s !important; }",
        "#%s .irs-single, #%s .irs--shiny .irs-single { background: %s !important; }",
        sep = "\n"
      ),
      id, col, id, col, id, id, col, col, id, id, col, col, id, id, col, id, id, col
    )
  }, character(1)),
  collapse = "\n"
)

table_css <- paste(
  ".cui-table { width: 100%; font-size: 0.95rem; }",
  ".cui-table th, .cui-table td { text-align: center; padding: 0.4rem 0.55rem; }",
  ".cui-table th:nth-child(1), .cui-table td:nth-child(1),",
  ".cui-table th:nth-child(2), .cui-table td:nth-child(2) { text-align: left; }",
  paste0(sprintf(".cui-table th.ep-%s, .cui-table td.ep-%s { color: %s; font-weight: 700; }",
                 names(ep_cols), names(ep_cols), unname(ep_cols)), collapse = "\n"),
  paste0(sprintf(".cui-table th.ep-%s { border-bottom: 3px solid %s; }",
                 names(ep_cols), unname(ep_cols)), collapse = "\n"),
  ".taskd-banner { display: flex; align-items: center; justify-content: space-between;",
  "  gap: 1rem; flex-wrap: nowrap; font-size: 0.95rem; margin-bottom: 0.4rem; }",
  ".taskd-banner .shiny-input-container { margin-bottom: 0; }",
  sep = "\n"
)

series <- lapply(c(endpoint_cols, "CUI"), function(nm) {
  list(col = unname(ep_cols[[nm]]), width = unname(ep_width_plotly[[nm]]),
       dash = unname(ep_lty_plotly[[nm]]))
})
names(series) <- c(endpoint_cols, "CUI")

colored <- function(label, value, key, digits = 2) {
  sprintf("<span style='color:%s;font-weight:700'>%s %s</span>",
          ep_cols[[key]], label, formatC(value, format = "f", digits = digits))
}
slider_label <- function(key, text = key) {
  HTML(sprintf("<span style='color:%s;font-weight:700'>%s</span>", ep_cols[[key]], text))
}
weight_caption <- function(w) {
  paste0(
    colored("ORR", w["ORR"], "ORR"), " &nbsp;",
    colored("NoNEU", w["NoNEU"], "NoNEU"), " &nbsp;",
    colored("NoDR", w["NoDR"], "NoDR")
  )
}

compare_specs <- lapply(
  list(
    list(id = "cmp_a", persona = "default_balanced", form = "multiplicative"),
    list(id = "cmp_b", persona = "efficacy_priority", form = "multiplicative"),
    list(id = "cmp_c", persona = "safety_priority", form = "multiplicative"),
    list(id = "cmp_d", persona = "default_balanced", form = "additive")
  ),
  function(spec) {
    spec$title <- scenario_title(spec$persona, spec$form)
    spec
  }
)

shared_xaxis <- function(lock = TRUE) {
  list(
    title = "Dose (mg/kg Q3W)  ·  7 discrete levels",
    tickmode = "array",
    tickvals = grid$dose_mgkg,
    ticktext = as.character(grid$dose_mgkg),
    range = c(0.4, 8.4),
    zeroline = FALSE,
    fixedrange = lock
  )
}
shared_yaxis <- function(title = "CUI", lock = TRUE, ylim = c(0, 1)) {
  list(title = title, range = ylim, zeroline = FALSE, fixedrange = lock)
}

cui_panel_plot <- function(persona, form, show_ep = FALSE) {
  w <- persona_weights[[persona]]
  dat <- add_cui(grid, w, form = form)
  pk <- cui_peak(dat)
  p <- plot_ly()
  if (isTRUE(show_ep)) {
    for (nm in endpoint_cols) {
      sty <- series[[nm]]
      p <- add_trace(
        p, x = dat$dose_mgkg, y = dat[[nm]], type = "scatter", mode = "lines+markers",
        name = nm, line = list(color = sty$col, width = sty$width, dash = sty$dash),
        marker = list(size = 7, color = sty$col), hoverinfo = "none", showlegend = FALSE
      )
    }
  }
  p %>%
    add_trace(
      x = dat$dose_mgkg, y = dat$CUI, type = "scatter", mode = "lines+markers",
      name = "CUI",
      line = list(color = ep_cols[["CUI"]], width = 3.4),
      marker = list(size = 8, color = ep_cols[["CUI"]]),
      hovertemplate = sprintf(
        "<b>%%{x} mg/kg</b><br><span style='color:%s'><b>CUI</b>    %%{y:.3f}</span><extra></extra>",
        ep_cols[["CUI"]]
      ),
      showlegend = FALSE
    ) %>%
    add_trace(
      x = pk$dose_mgkg, y = pk$CUI, type = "scatter", mode = "markers", name = "Peak",
      marker = list(size = 12, color = peak_col, line = list(color = "white", width = 1)),
      hovertemplate = sprintf(
        "<b>Peak</b><br>%s mg/kg<br>CUI %.3f<br>%s<extra></extra>",
        pk$dose_mgkg, pk$CUI, nearest_label(pk$dose_mgkg)
      ),
      showlegend = FALSE
    ) %>%
    layout(
      showlegend = FALSE, hovermode = "closest",
      margin = list(l = 48, r = 12, t = 28, b = 48),
      xaxis = shared_xaxis(TRUE), yaxis = shared_yaxis("CUI", TRUE),
      shapes = list(list(
        type = "line", x0 = 5.4, x1 = 5.4, y0 = 0, y1 = 1, yref = "paper",
        line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1)
      )),
      annotations = list(
        list(x = 5.4, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
             text = "label-like", showarrow = FALSE, font = list(size = 10, color = "#5C5C5C")),
        list(x = pk$dose_mgkg, y = pk$CUI, xref = "x", yref = "y",
             text = sprintf("max at %s", pk$dose_mgkg), showarrow = FALSE,
             yanchor = "bottom", yshift = 8,
             font = list(size = 12, color = peak_col))
      )
    ) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

compare_card <- function(spec) {
  w <- persona_weights[[spec$persona]]
  pk <- cui_peak(add_cui(grid, w, form = spec$form))
  card(
    full_screen = TRUE, min_height = 280,
    card_header(HTML(paste0(
      "<b>", spec$title, "</b> &nbsp;·&nbsp; raw &nbsp;",
      weight_caption(w), " &nbsp;·&nbsp; ",
      colored("peak", pk$CUI, "CUI", digits = 3),
      sprintf(" at %s mg/kg", pk$dose_mgkg)
    ))),
    card_body(class = "p-0", plotlyOutput(spec$id, height = "32vh"))
  )
}

ui <- page_sidebar(
  title = "Exercise 1  ·  Dummy ADC CUI  ·  Q3W doses",
  fillable = TRUE,
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1B365D"),
  sidebar = sidebar(
    id = "controls", title = NULL, width = 300, open = TRUE, position = "left",
    tags$style(HTML(paste(slider_css, table_css, sep = "\n"))),
    selectInput("preset", "Weight preset", choices = preset_choices, selected = "default_balanced"),
    radioButtons(
      "form", "CUI form",
      choices = c("Multiplicative" = "multiplicative", "Additive" = "additive"),
      selected = "multiplicative"
    ),
    sliderInput("w_orr", slider_label("ORR", "ORR (response)"), 0, 8, 2, 0.1),
    sliderInput("w_noneu", slider_label("NoNEU", "No grade 3+ neutropenia"), 0, 8, 1, 0.1),
    sliderInput("w_nodr", slider_label("NoDR", "No dose reduction"), 0, 8, 1, 0.1),
    checkboxInput("show_endpoints", "Overlay endpoint curves", value = TRUE)
  ),
  navset_tab(
    id = "main_tabs",
    nav_panel(
      "Explorer", value = "explorer",
      card(
        full_screen = TRUE, min_height = 520,
        card_header(uiOutput("plot_caption")),
        card_body(class = "p-0", plotlyOutput("main_plot", height = "58vh"))
      ),
      card(
        card_header("CUI at selected doses  ·  raw and normalized weights"),
        htmlOutput("dose_table")
      )
    ),
    nav_panel(
      "Tasks A–C · compare", value = "compare",
      div(
        class = "taskd-banner",
        tags$span("Task A = balanced product. Task B = efficacy. Task C = safety. Bottom-right = Task A additive (preview of Exercise 2)."),
        checkboxInput("compare_show_endpoints", "Overlay endpoint curves", value = FALSE)
      ),
      layout_column_wrap(
        width = 1 / 2, heights_equal = "row",
        compare_card(compare_specs[[1]]),
        compare_card(compare_specs[[2]]),
        compare_card(compare_specs[[3]]),
        compare_card(compare_specs[[4]])
      )
    ),
    nav_panel("E–R construction", value = "emax", uiOutput("emax_tab"))
  )
)

server <- function(input, output, session) {

  observeEvent(input$main_tabs, {
    toggle_sidebar(id = "controls", open = identical(input$main_tabs, "explorer"))
  }, ignoreInit = TRUE)

  observeEvent(input$preset, {
    if (input$preset == "custom") return()
    w <- persona_weights[[input$preset]]
    updateSliderInput(session, "w_orr", value = unname(w["ORR"]))
    updateSliderInput(session, "w_noneu", value = unname(w["NoNEU"]))
    updateSliderInput(session, "w_nodr", value = unname(w["NoDR"]))
  })

  observeEvent(
    list(input$w_orr, input$w_noneu, input$w_nodr),
    {
      if (is.null(input$preset) || input$preset == "custom") return()
      target <- persona_weights[[input$preset]]
      cur <- c(ORR = input$w_orr, NoNEU = input$w_noneu, NoDR = input$w_nodr)
      if (all(abs(cur[names(target)] - target) < 0.06)) return()
      updateSelectInput(session, "preset", selected = "custom")
    },
    ignoreInit = TRUE
  )

  weights <- reactive({
    c(ORR = input$w_orr, NoNEU = input$w_noneu, NoDR = input$w_nodr)
  })
  scored <- reactive(add_cui(grid, weights(), form = input$form))
  peak <- reactive(cui_peak(scored()))

  output$plot_caption <- renderUI({
    pk <- peak()
    w <- weights()
    HTML(paste0(
      "<span style='font-weight:700'>", scenario_title(input$preset, input$form), "</span>",
      " &nbsp;·&nbsp; raw &nbsp;", weight_caption(w),
      " &nbsp;·&nbsp; ", colored("peak", pk$CUI, "CUI", digits = 3),
      sprintf(" at %s mg/kg", pk$dose_mgkg)
    ))
  })

  output$main_plot <- renderPlotly({
    dat <- scored()
    pk <- peak()
    show_ep <- isTRUE(input$show_endpoints)
    hover <- sprintf(
      "<b>%s mg/kg</b><br><span style='color:%s'><b>CUI</b>    %.3f</span>",
      dat$dose_mgkg, ep_cols[["CUI"]], dat$CUI
    )
    if (show_ep) {
      hover <- paste0(
        hover,
        sprintf("<br><span style='color:%s'>ORR     %.2f</span>", ep_cols[["ORR"]], dat$ORR),
        sprintf("<br><span style='color:%s'>NoNEU   %.2f</span>", ep_cols[["NoNEU"]], dat$NoNEU),
        sprintf("<br><span style='color:%s'>NoDR    %.2f</span>", ep_cols[["NoDR"]], dat$NoDR)
      )
    }
    p <- plot_ly()
    if (show_ep) {
      for (nm in endpoint_cols) {
        sty <- series[[nm]]
        p <- add_trace(
          p, x = dat$dose_mgkg, y = dat[[nm]], type = "scatter", mode = "lines+markers",
          name = nm, line = list(color = sty$col, width = sty$width, dash = sty$dash),
          marker = list(size = 7, color = sty$col), hoverinfo = "none",
          legendgroup = nm, legendrank = match(nm, legend_order)
        )
      }
    }
    p <- add_trace(
      p, x = dat$dose_mgkg, y = dat$CUI, type = "scatter", mode = "lines+markers",
      name = "CUI",
      line = list(color = series$CUI$col, width = series$CUI$width),
      marker = list(size = 8, color = series$CUI$col),
      hovertemplate = paste0(hover, "<extra></extra>"),
      hoverlabel = list(align = "left", bgcolor = "white",
                        font = list(family = "Consolas, monospace", size = 13)),
      legendgroup = "CUI", legendrank = match("CUI", legend_order)
    )
    p <- add_trace(
      p, x = pk$dose_mgkg, y = pk$CUI, type = "scatter", mode = "markers",
      name = "Peak",
      marker = list(size = 12, color = peak_col, line = list(color = "white", width = 1)),
      hovertemplate = sprintf(
        "<b>Peak</b><br>%s mg/kg<br>CUI %.3f<br>%s<extra></extra>",
        pk$dose_mgkg, pk$CUI, nearest_label(pk$dose_mgkg)
      ),
      legendgroup = "Peak", legendrank = match("Peak", legend_order)
    )
    layout(
      p, hovermode = "closest",
      legend = list(orientation = "h", y = -0.22, x = 0, font = list(size = 12),
                    traceorder = "normal"),
      margin = list(l = 56, r = 16, t = 28, b = 80),
      xaxis = shared_xaxis(FALSE),
      yaxis = shared_yaxis(if (show_ep) "Desirability / CUI" else "CUI", FALSE, c(0, 1)),
      shapes = list(list(
        type = "line", x0 = 5.4, x1 = 5.4, y0 = 0, y1 = 1, yref = "paper",
        line = list(dash = "dash", color = "rgba(90,90,90,0.55)", width = 1)
      )),
      annotations = list(list(
        x = 5.4, y = 1, xref = "x", yref = "paper", yanchor = "bottom",
        text = "5.4 label-like", showarrow = FALSE, font = list(size = 11, color = "#5C5C5C")
      ))
    ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
  })

  output$dose_table <- renderUI({
    raw <- weights()
    w <- normalize_weights(raw)
    rows <- scored() %>%
      filter(dose_mgkg %in% c(3.2, 5.4, 7.4)) %>%
      transmute(
        Dose = regimen_label,
        CUI = sprintf("%.3f", CUI),
        ORR = sprintf("%.2f", ORR),
        NoNEU = sprintf("%.2f", NoNEU),
        NoDR = sprintf("%.2f", NoDR)
      )
    extra <- rbind(
      data.frame(Dose = "Raw weight (sliders)", CUI = "",
                 ORR = sprintf("%.2f", raw["ORR"]),
                 NoNEU = sprintf("%.2f", raw["NoNEU"]),
                 NoDR = sprintf("%.2f", raw["NoDR"])),
      data.frame(Dose = "Normalized weight", CUI = "",
                 ORR = sprintf("%.2f", w["ORR"]),
                 NoNEU = sprintf("%.2f", w["NoNEU"]),
                 NoDR = sprintf("%.2f", w["NoDR"]))
    )
    tab <- rbind(rows, extra)
    hdr <- paste0(
      "<tr><th>Dose</th><th class='ep-CUI'>CUI</th>",
      "<th class='ep-ORR'>ORR</th><th class='ep-NoNEU'>NoNEU</th><th class='ep-NoDR'>NoDR</th></tr>"
    )
    body <- apply(tab, 1, function(r) {
      paste0(
        "<tr>",
        sprintf("<td>%s</td>", r[["Dose"]]),
        sprintf("<td class='ep-CUI'>%s</td>", r[["CUI"]]),
        sprintf("<td class='ep-ORR'>%s</td>", r[["ORR"]]),
        sprintf("<td class='ep-NoNEU'>%s</td>", r[["NoNEU"]]),
        sprintf("<td class='ep-NoDR'>%s</td>", r[["NoDR"]]),
        "</tr>"
      )
    })
    HTML(paste0("<table class='cui-table'><thead>", hdr, "</thead><tbody>",
                paste(body, collapse = ""), "</tbody></table>"))
  })

  lapply(compare_specs, function(spec) {
    local({
      spec <- spec
      output[[spec$id]] <- renderPlotly({
        cui_panel_plot(spec$persona, spec$form, show_ep = isTRUE(input$compare_show_endpoints))
      })
    })
  })

  emax_unlocked <- reactiveVal(FALSE)
  try_unlock <- function(key) {
    if (identical(trimws(key), "CUI")) emax_unlocked(TRUE)
  }
  observeEvent(input$emax_key, try_unlock(input$emax_key), ignoreInit = TRUE)
  observeEvent(input$emax_unlock, try_unlock(input$emax_key))

  output$emax_tab <- renderUI({
    if (!isTRUE(emax_unlocked())) {
      return(card(
        card_header("Locked  ·  instructor notes"),
        card_body(
          p("How the seven-dose dummy table was built. Students do not need this for Tasks 0 and A–D."),
          passwordInput("emax_key", "Course keyword", placeholder = "3 letters, all caps"),
          actionButton("emax_unlock", "Unlock", class = "btn-primary"),
          if (!is.null(input$emax_key) && nzchar(trimws(input$emax_key)) &&
              !identical(trimws(input$emax_key), "CUI")) {
            tags$p(style = "color:#B4532A;margin-top:0.6rem;", "That is not the keyword.")
          }
        )
      ))
    }
    tagList(
      div(class = "taskd-banner",
          tags$span("Dummy dose-level rates in the spirit of hands_on/dummy_data_gen.R — no rxode2 or brms in the room.")),
      card(
        card_header("What the original pipeline did (and why we do not run it live)"),
        tags$ul(
          tags$li("hands_on/dummy_data_gen.R — mirvetuximab-like popPK (Tu et al. BJCP 2024) across 0.8–8 mg/kg, then logistic E–R for ORR, G3+ neutropenia, dose reduction."),
          tags$li("hands_on/dummy_er.R — brms multivariate logistic on payload Cav, then CUI = ORR^0.5 × (1−NEU)^0.25 × (1−DR)^0.25."),
          tags$li("This app uses the same three endpoints and the same product, on a 7-row lookup so laptops stay quiet.")
        )
      ),
      card(
        card_header("Locked rates  ·  scripts/01_build_endpoint_grid.R"),
        HTML(paste0(
          "<table class='cui-table'><thead><tr>",
          "<th>Dose</th><th>Cav ng/mL</th>",
          "<th class='ep-ORR'>ORR</th><th class='ep-NoNEU'>P(NEU)</th><th class='ep-NoDR'>P(DR)</th>",
          "<th class='ep-NoNEU'>NoNEU</th><th class='ep-NoDR'>NoDR</th></tr></thead><tbody>",
          paste(apply(grid, 1, function(r) {
            sprintf(
              "<tr><td>%s</td><td>%s</td><td class='ep-ORR'>%.2f</td><td class='ep-NoNEU'>%.2f</td><td class='ep-NoDR'>%.2f</td><td class='ep-NoNEU'>%.2f</td><td class='ep-NoDR'>%.2f</td></tr>",
              r[["regimen_label"]], r[["cav_ngml"]],
              as.numeric(r[["ORR"]]), as.numeric(r[["P_NEU"]]), as.numeric(r[["P_DR"]]),
              as.numeric(r[["NoNEU"]]), as.numeric(r[["NoDR"]])
            )
          }), collapse = ""),
          "</tbody></table>"
        ))
      ),
      card(
        card_header("Why these rates (tuner targets)"),
        tags$ul(
          tags$li(HTML("5.4 mg/kg is fixed at <b>ORR 0.50 / NEU 0.25 / DR 0.20</b> so Task 0 is computable by hand.")),
          tags$li("Neutropenia rises faster than ORR above 5.4, so efficacy-only weights must walk up the ladder and safety weights walk down."),
          tags$li("Task A product peaks at 5.4; Task B at 7.4; Task C at 3.2; Task A additive is 3.2 (almost tied with 5.4)."),
          tags$li("Sources: Tu et al. BJCP 2024 (popPK structure); original dummy ADC app weights 0.50 / 0.25 / 0.25. Not a label claim.")
        )
      )
    )
  })
}

shinyApp(ui, server)
