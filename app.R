# app.R
# ---------------------------
# Shiny app for the MMC cost-benefit model. Shiny sources the files in R/
# (packages, inputs, model) before this file. Run with shiny::runApp().
# ---------------------------

library(shiny)
library(bslib)
library(reactable)
library(plotly)
library(writexl)

source("app/helpers.R", local = TRUE)
source("app/charts.R", local = TRUE)
source("app/editors.R", local = TRUE)

published <- read_inputs("data/cba_inputs.xlsx")
KR_DRAWS <- 2000      # preference draws behind the intervals on the Results page

rt_theme <- reactableTheme(
  color = ui_ink$primary, backgroundColor = "transparent", borderColor = "#e4e0d5",
  highlightColor = "#f1eee5", cellPadding = "9px 10px",
  style = list(fontFamily = "'Atkinson Hyperlegible Next', system-ui, sans-serif", fontSize = "0.92rem"),
  headerStyle = list(fontWeight = 600, color = ui_ink$secondary, fontSize = "0.82rem", borderBottomColor = "#cdc8bb"))

row_click_js <- function(input_id, key) {
  JS(sprintf("function(rowInfo) { if (!rowInfo) return; Shiny.setInputValue('%s', rowInfo.original.%s, {priority: 'event'}); }",
             input_id, key))
}
sub_line <- function(main, sub) div(div(main), div(class = "cell-sub", sub))

# ── UI ────────────────────────────────────────────────────────────────────────

theme <- bs_theme(version = 5, bg = "#f3f1ea", fg = "#13303b", primary = "#13303b", secondary = "#4c5c63",
                  base_font = font_collection("Atkinson Hyperlegible Next", "system-ui", "sans-serif"),
                  heading_font = font_collection("Atkinson Hyperlegible Next", "system-ui", "sans-serif"),
                  "font-size-base" = "0.9375rem")

overview_panel <- nav_panel(
  "Overview", value = "overview",
  div(class = "overview-intro",
      div(h1("Benefits and costs of MMC demand creation policies"),
          p(class = "lede",
            "We conducted a discrete choice experiment among 406 self-reported uncircumcised, HIV-negative men aged 18\u201345 ",
            "years in fishing communities in Mukono district, Uganda. We use the compensating variation (CV), a Hicksian ",
            "measure of the welfare effects of policy changes, to value five MMC service delivery policies in monetary terms, ",
            "and compare this valuation with the costs of providing the services.")),
      div(p("As was the predominant practice in the study area, the initial state is one in which surgical circumcision ",
            "services are only available at permanent health facilities, 15 km away, two days a week; about 69% of men remain ",
            "uncircumcised. For each policy, the model predicts the uptake of MMC, the CV and the incremental cost, including ",
            "incentive payments."),
          p(class = "small-print",
            "This is an illustrative analysis. Utility parameters are from the conditional logit model; costs are from ",
            "published Ugandan studies, and some inputs are assumptions. ", actionLink("goto_method", "How the model works."),
            " All inputs can be changed on the Inputs page. ",
            tags$a(href = "working_paper.pdf", target = "_blank", rel = "noopener", "Working paper (PDF).")))),
  uiOutput("policy_cards"),
  layout_columns(
    col_widths = 12, class = "mt-3",
    card(card_header(h2("Compensating variation and incremental cost"),
                     p(class = "card-sub", "Per uncircumcised man, 2017 US$, compared with the initial state. Whiskers: 95% Krinsky-Robb intervals (utility parameters)")),
         card_body(uiOutput("ov_bc_ui"))),
    card(card_header(h2("Predicted uptake by service option"),
                     p(class = "card-sub", "Predicted choice probabilities among uncircumcised men")),
         card_body(reactableOutput("shares_table"))))
)

results_panel <- nav_panel(
  "Results", value = "results",
  div(class = "page-head",
      div(h1("Benefits and costs"), uiOutput("res_sub")),
      div(class = "seg", radioButtons("per", NULL, inline = TRUE,
                                      choices = c("Per uncircumcised man" = "man", "Per population" = "pop")))),
  card(card_body(div(style = "overflow-x: auto;", reactableOutput("tbl_results"))),
       card_footer("Intervals: 95% Krinsky-Robb intervals from 2,000 draws of the utility parameters, with costs fixed at the ",
                   "base case. The probabilistic analysis also varies the costs.")),
  card(class = "mt-3",
       card_header(h2("Net benefit"),
                   p(class = "card-sub", "Points: base case. Lines: 95% Krinsky-Robb intervals. The two panels are on different scales.")),
       card_body(uiOutput("res_nb_ui")))
)

oneway_panel <- nav_panel(
  "One-way analysis", value = "oneway",
  card(card_body(
    div(class = "controls",
        selectInput("ow_policy", "Policy", choices = NULL, width = "250px"),
        selectInput("ow_outcome", "Result", choices = setNames(names(outcome_labels), outcome_labels), width = "320px"),
        checkboxGroupInput("ow_groups", "Inputs", inline = TRUE, selected = c("Costs", "Preferences", "Calibration"),
                           choices = c("Costs", "Preferences", "Calibration")),
        div(class = "ctl-wide", sliderInput("ow_n", "Inputs shown", min = 5, max = 30, value = 15, step = 1, width = "220px")),
        actionButton("ow_run", "Run the one-way analysis", class = "btn-primary")),
    p(class = "small-print", style = "margin: 0.8rem 0 0;",
      "Each input is set to its low and then its high value while all other inputs are held at the base case: costs to the ",
      "ranges on the Inputs page, utility parameters to their 95% confidence limits, and the share of men who remain ",
      "uncircumcised in the initial state to 60% and 80%. The opt-out constant has no effect because it is absorbed by the calibration."))),
  uiOutput("ow_stale"),
  card(class = "mt-3", card_header(uiOutput("ow_title")), card_body(uiOutput("ow_plot_ui")))
)

psa_panel <- nav_panel(
  "Probabilistic analysis", value = "psa",
  card(card_body(
    div(class = "controls",
        numericInput("psa_n", "Number of draws", value = 5000, min = 200, max = 50000, step = 500, width = "150px"),
        numericInput("psa_seed", "Random seed", value = 2026, step = 1, width = "130px"),
        actionButton("psa_run", "Run the probabilistic analysis", class = "btn-primary")),
    p(class = "small-print", style = "margin: 0.8rem 0 0;",
      "In each draw, the utility parameters are drawn from their sampling distribution (multivariate normal with the ",
      "robust covariance matrix) and each cost input from its distribution on the Inputs page, and the model is re-run ",
      "for every policy. 5,000 draws take less than a second."))),
  uiOutput("psa_stale"),
  uiOutput("psa_body")
)

inputs_panel <- nav_panel(
  "Inputs", value = "inputs",
  div(class = "inputs-toolbar",
      div(h1("Model inputs", style = "margin-bottom: 0.2rem;"), uiOutput("edit_summary")),
      div(class = "actions",
          actionButton("reset_all", "Reset all inputs", class = "btn-outline-secondary"),
          downloadButton("dl_inputs", "Download inputs (.xlsx)", class = "btn-outline-secondary"),
          div(class = "upload-input", fileInput("ul_inputs", NULL, accept = ".xlsx", buttonLabel = "Upload inputs (.xlsx)")))),
  navset_underline(
    id = "inputs_tab",
    nav_panel("Policies", value = "policies",
              div(class = "tab-head",
                  p(class = "tab-intro", "Each policy is a set of service options; the opt-out is always available, reflecting the voluntary ",
                    "nature of MMC. Select a policy to change its services, or add a new policy."),
                  actionButton("add_policy", "Add a policy", class = "btn-outline-secondary")),
              reactableOutput("tbl_policies")),
    nav_panel("Services", value = "services",
              div(class = "tab-head",
                  p(class = "tab-intro", "Each service option is described by the seven attributes in the choice experiment. ",
                    "Select a service to change it, or add a new service and then include it in a policy."),
                  actionButton("add_option", "Add a service", class = "btn-outline-secondary")),
              div(style = "overflow-x: auto;", reactableOutput("tbl_options"))),
    nav_panel("Costs", value = "costs",
              p(class = "tab-intro", "Provider costs per procedure, the cost of delivering an incentive payment, and the inputs for the ",
                "averted HIV treatment costs. Select a row to edit its base case, its range or its distribution in the PSA."),
              div(style = "overflow-x: auto;", reactableOutput("tbl_costs"))),
    nav_panel("Choice model", value = "settings",
              layout_columns(
                col_widths = breakpoints(sm = 12, lg = c(6, 6)), class = "mt-3",
                card(card_header(h3("Utility parameters")),
                     card_body(
                       radioButtons("set_model", "Conditional logit", inline = FALSE,
                                    choiceNames = list(
                                      tags$span(tags$strong("One price coefficient"), " for out-of-pocket and incentive payments (base case)"),
                                      tags$span(tags$strong("Three price coefficients"), ": out-of-pocket, voucher and cash payments")),
                                    choiceValues = c("cl_one_price_slope", "cl_three_price_slopes"),
                                    selected = published$settings$model),
                       conditionalPanel("input.set_model == 'cl_three_price_slopes'",
                                        selectInput("set_slope", "Price coefficient used as the marginal utility of income",
                                                    choices = slope_labels[-1] |> (\(x) setNames(names(x), x))(), selected = "pricecash")),
                       p(class = "small-print",
                         "The three price coefficients were not statistically different from each other (likelihood ratio test ",
                         "0.44, 2 degrees of freedom, p = 0.80). The coefficient for out-of-pocket payment was not significant, ",
                         "so a CV computed with respect to it has no upper bound."))),
                card(card_header(h3("Calibration and scale")),
                     card_body(
                       sliderInput("set_s0", "Share of men who remain uncircumcised in the initial state", min = 0.40, max = 0.95,
                                   value = published$settings$optout_share, step = 0.005, width = "100%"),
                       p(class = "small-print", "We add a constant to the utility of the opt-out so that the model replicates this share. ",
                         "The base case, 69.2%, is 1 \u2212 2050/6663, as in the policy simulations."),
                       numericInput("set_pop", "Population for the per-population results (uncircumcised men)",
                                    value = published$settings$population, min = 1, step = 100, width = "320px")))),
              card(class = "mt-3", card_header(h3("Conditional logit estimates in use")), card_body(reactableOutput("tbl_coefs")))))
)

method_panel <- nav_panel("How it works", value = "method", uiOutput("method_body"))

ui <- page_navbar(
  id = "nav",
  title = tags$span(class = "brand",
                    tags$span(class = "brand-title", "MMC demand creation: benefits and costs"),
                    tags$span(class = "brand-sub", "A benefit-cost analysis using a discrete choice experiment")),
  window_title = "Medical male circumcision services in Uganda: a benefit-cost analysis",
  theme = theme, fillable = FALSE, lang = "en",
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", href = "app.css"),
              tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
              tags$meta(name = "description", content = "Benefit-cost analysis of medical male circumcision (MMC) service delivery policies in Uganda, with benefits valued by the compensating variation from a discrete choice experiment.")),
    uiOutput("verdict")),
  overview_panel, results_panel, oneway_panel, psa_panel, inputs_panel, method_panel,
  footer = div(class = "app-footer",
               p("Discrete choice experiment among 406 uncircumcised men in Ntenjeru sub-county, Mukono district, Uganda (March\u2013April 2017). ",
                 "Solomon J. Lubinga. An illustrative analysis; see the ",
                 tags$a(href = "working_paper.pdf", target = "_blank", rel = "noopener", "working paper", .noWS = "outside"), "."))
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  rv <- reactiveValues(inputs = published, edit = NULL, psa = NULL, n_new = 0)

  edits <- reactive(input_edits(rv$inputs, published))
  n_ed  <- reactive(n_edits(edits()))
  pol   <- reactive(rv$inputs$policies)
  pcols <- reactive(policy_colours(pol()))
  pop   <- reactive(rv$inputs$settings$population)
  per   <- reactive(if (identical(input$per, "pop")) pop() else 1)

  base_case <- reactive(tryCatch(RunCBA(rv$inputs), error = function(e) e))
  bc <- reactive({
    b <- base_case()
    validate(need(!inherits(b, "error"), paste("The model can't run with these inputs:", if (inherits(b, "error")) conditionMessage(b))))
    b
  })
  kr <- reactive(RunKR(bc(), nsims = KR_DRAWS, seed = 2026))
  tab_man <- reactive(TabulateResults(bc(), kr(), 1))
  tab <- reactive(TabulateResults(bc(), kr(), per()))
  narrow <- function(id) { w <- session$clientData[[paste0("output_", id, "_width")]]; !is.null(w) && w < 620 }

  # ── Verdict ────────────────────────────────────────────────────────────────
  output$verdict <- renderUI({
    b <- base_case()
    status <- if (n_ed() == 0) tags$span("Published inputs") else
      tagList(tags$span(class = "chip chip-edited", plural(n_ed(), "edited input")), actionLink("goto_inputs", "Review"))
    if (inherits(b, "error"))
      return(div(class = "verdict is-error", role = "status",
                 div(class = "verdict-main", div(p(class = "verdict-title", "The model can't run with these inputs."),
                                                 p(class = "verdict-detail", style = "color: inherit;", conditionMessage(b)))),
                 div(class = "verdict-status", status)))
    v <- verdict_text(b, pop())
    col <- pcols()[[v$best]]
    div(class = "verdict", role = "status", `aria-live` = "polite",
        div(class = "verdict-main",
            tags$span(class = "verdict-swatch", style = paste0("background:", col)),
            div(p(class = "verdict-title", v$title), p(class = "verdict-detail", v$detail))),
        div(class = "verdict-status", status))
  })
  observeEvent(input$goto_inputs, nav_select("nav", "inputs"))
  observeEvent(input$goto_method, nav_select("nav", "method"))

  # ── Overview ───────────────────────────────────────────────────────────────
  output$policy_cards <- renderUI({
    b <- bc(); p <- pol(); cols <- pcols()
    card_for <- function(id) {
      i <- match(id, p$policy)
      svc <- rv$inputs$options$label[match(setdiff(p$option_list[[i]], "opt_out"), rv$inputs$options$option)]
      nbc <- b$nb_cv[1, id]
      div(class = "policy-card", style = paste0("--pol:", cols[[id]]),
          div(class = "pc-head", policy_tag(p$label[i], cols[[id]])),
          p(class = "pc-svc", paste(svc, collapse = " \u00b7 ")),
          div(class = "pc-uptake", tags$span(class = "pc-big", fmt_pct(b$uptake[1, id])),
              tags$span(class = "pc-small", sprintf("uptake, %s%s percentage points", if (b$uptake_change[1, id] >= 0) "+" else minus,
                                                    formatC(abs(100 * b$uptake_change[1, id]), format = "f", digits = 1)))),
          tags$dl(class = "pc-stats",
                  tags$dt("Compensating variation (CV)"), tags$dd(fmt_usd(b$cv[1, id])),
                  tags$dt("Incremental cost"), tags$dd(fmt_usd(b$total_cost[1, id])),
                  tags$dt("CV minus cost"), tags$dd(class = if (nbc < 0) "neg" else "pos", fmt_usd(nbc)),
                  tags$dt("Including averted ART costs"), tags$dd(fmt_usd(b$nb_total[1, id]))))
    }
    init <- p$option_list[[1]]
    div(class = "policy-grid",
        div(class = "policy-card pc-initial",
            div(class = "pc-head", tags$span(class = "pol", p$label[1])),
            p(class = "pc-svc", paste(rv$inputs$options$label[match(setdiff(init, "opt_out"), rv$inputs$options$option)], collapse = " \u00b7 ")),
            div(class = "pc-uptake", tags$span(class = "pc-big", fmt_pct(b$uptake[1, 1])), tags$span(class = "pc-small", "uptake of MMC")),
            p(class = "pc-note", "The comparator. All values on the other cards are per uncircumcised man, in 2017 US$, relative to the initial state.")),
        lapply(p$policy[-1], card_for))
  })

  output$ov_bc_ui <- renderUI(plotlyOutput("ov_bc", height = paste0(110 + 62 * (nrow(pol()) - 1), "px")))
  output$ov_bc <- renderPlotly(BenefitCostPlot(tab_man(), 1))

  output$shares_table <- renderReactable({
    b <- bc(); p <- pol(); o <- rv$inputs$options
    used <- o$option[o$option %in% unlist(p$option_list)]
    df <- data.frame(policy = p$label)
    for (op in used) df[[op]] <- vapply(p$policy, function(id) { S <- b$shares[[id]]; if (op %in% colnames(S)) S[1, op] else NA_real_ }, numeric(1))
    bar <- function(value, colour) {
      if (is.na(value)) return(tags$span(class = "share-na", "\u2013"))
      div(class = "share-cell", div(class = "share-bar", style = sprintf("width:%.1f%%;background:%s", 100 * value, colour)),
          tags$span(class = "share-val", fmt_pct(value)))
    }
    cols <- c(list(policy = colDef(name = "Policy", minWidth = 150, sticky = "left",
                                   cell = function(value, index) {
                                     id <- p$policy[index]
                                     if (index == 1) tags$span(class = "pol", value) else policy_tag(value, pcols()[[id]])
                                   })),
              setNames(lapply(used, function(op) colDef(
                name = o$label[match(op, o$option)], minWidth = 92, align = "left",
                cell = function(value) bar(value, if (op == "opt_out") "#b9b3a4" else ui_ink$primary))), used))
    reactable(df, columns = cols, theme = rt_theme, sortable = FALSE, compact = TRUE, wrap = TRUE)
  })

  # ── Results ────────────────────────────────────────────────────────────────
  observe({
    updateRadioButtons(session, "per", choices = c("Per uncircumcised man" = "man",
                                                   setNames("pop", sprintf("Per %s men", format(pop(), big.mark = ",")))),
                       selected = isolate(input$per) %||% "man", inline = TRUE)
  })
  output$res_sub <- renderUI({
    s <- rv$inputs$settings
    p(class = "page-sub", sprintf("%s%s; %s of men stay uncircumcised under the initial policy; 2017 US$. Treatment costs averted: %s per extra circumcision.",
                                  model_labels[[s$model]],
                                  if (s$model == "cl_three_price_slopes") paste0(", ", tolower(slope_labels[[s$money_slope]]), " slope as the marginal utility of income") else "",
                                  fmt_pct(s$optout_share, 1), fmt_usd(bc()$tx_per_mmc, 0)))
  })
  output$tbl_results <- renderReactable({
    t <- tab(); k <- per(); cols <- pcols()
    t$p_nb_cv <- t$p_nb_cv_positive
    reactable(
      t, theme = rt_theme, sortable = FALSE, compact = FALSE,
      columns = list(
        policy = colDef(show = FALSE),
        label = colDef(name = "Policy", minWidth = 190, sticky = "left",
                       cell = function(value, index) policy_tag(value, cols[[t$policy[index]]])),
        uptake = colDef(name = "Uptake of MMC", minWidth = 105,
                        cell = function(value, index) sub_line(fmt_pct(value, 1),
                                                               sprintf("%s%s points", if (t$uptake_change[index] >= 0) "+" else minus,
                                                                       formatC(abs(100 * t$uptake_change[index]), format = "f", digits = 1)))),
        uptake_change = colDef(show = FALSE),
        extra_mmc = colDef(name = "Additional circumcisions", show = k > 1, minWidth = 110,
                           cell = function(value) formatC(value, format = "f", digits = 0, big.mark = ",")),
        cv = colDef(name = "Compensating variation (CV)", minWidth = 140,
                    cell = function(value, index) sub_line(fmt_usd_auto(value, k),
                                                           paste0(fmt_usd_auto(t$cv_lo[index], k), " to ", fmt_usd_auto(t$cv_hi[index], k)))),
        provider_cost = colDef(show = FALSE), transfers = colDef(show = FALSE),
        total_cost = colDef(name = "Incremental cost", minWidth = 120,
                            cell = function(value, index) sub_line(fmt_usd_auto(value, k),
                                                                   if (t$transfers[index] != 0) paste(fmt_usd_auto(t$transfers[index], k), "incentives") else "services only")),
        nb_cv = colDef(name = "CV minus cost", minWidth = 130,
                       cell = function(value, index) sub_line(tags$span(class = if (value < 0) "neg" else "pos", fmt_usd_auto(value, k)),
                                                              paste0("above zero in ", fmt_pct(t$p_nb_cv[index])))),
        tx_savings = colDef(name = "Averted ART costs", minWidth = 125, cell = function(value) fmt_usd_auto(value, k)),
        nb_total = colDef(name = "Net benefit, incl. averted ART", minWidth = 145,
                          cell = function(value, index) sub_line(tags$strong(fmt_usd_auto(value, k)),
                                                                 paste0(fmt_usd_auto(t$nb_total_lo[index], k), " to ", fmt_usd_auto(t$nb_total_hi[index], k)))),
        cost_per_extra_mmc = colDef(name = "Cost per additional circumcision", minWidth = 120,
                                    cell = function(value) fmt_usd(value, 0)),
        cv_lo = colDef(show = FALSE), cv_hi = colDef(show = FALSE), nb_cv_lo = colDef(show = FALSE),
        nb_cv_hi = colDef(show = FALSE), p_nb_cv_positive = colDef(show = FALSE), p_nb_cv = colDef(show = FALSE),
        nb_total_lo = colDef(show = FALSE), nb_total_hi = colDef(show = FALSE), p_nb_total_positive = colDef(show = FALSE)))
  })
  output$res_nb_ui <- renderUI({
    n <- nrow(pol()) - 1
    plotlyOutput("res_nb", height = paste0(if (narrow("res_nb")) 2 * (80 + 52 * n) else 110 + 62 * n, "px"))
  })
  output$res_nb <- renderPlotly(NetBenefitPlot(tab(), per(), narrow("res_nb")))

  # ── One-way ────────────────────────────────────────────────────────────────
  observe({
    p <- pol(); sel <- isolate(input$ow_policy)
    ch <- setNames(p$policy[-1], p$label[-1])
    updateSelectInput(session, "ow_policy", choices = ch, selected = if (!is.null(sel) && sel %in% ch) sel else ch[min(4, length(ch))])
  })
  # runs only when the button is pressed; the inputs shown and their number filter the last run
  ow_state <- reactiveVal(NULL)
  run_owsa <- function() {
    m <- tryCatch(bc(), error = function(e) e)
    if (inherits(m, "error")) return(showNotification("Fix the inputs first: the model can't run with them.", type = "error"))
    pid <- input$ow_policy; oc <- input$ow_outcome
    if (is.null(pid) || !pid %in% pol()$policy) return(showNotification("Choose a policy.", type = "warning"))
    res <- withProgress(message = "Running the one-way analysis", value = 0.5,
                        tryCatch(owsa(m, pid, oc), error = function(e) e))
    if (inherits(res, "error")) return(showNotification(paste("The one-way analysis stopped:", conditionMessage(res)), type = "error", duration = 10))
    ow_state(list(res = res, inputs = rv$inputs, policy = pid, outcome = oc,
                  label = pol()$label[match(pid, pol()$policy)], base = m[[oc]][1, pid]))
  }
  observeEvent(input$ow_run, run_owsa())
  ow <- reactive({ s <- ow_state(); req(s); s$res[s$res$group %in% input$ow_groups, ] })
  output$ow_stale <- renderUI({
    s <- ow_state(); if (is.null(s)) return(NULL)
    if (!identical(s$inputs, rv$inputs) || !identical(s$policy, input$ow_policy) || !identical(s$outcome, input$ow_outcome))
      div(class = "note note-stale", "The inputs or choices have changed since this run. Run the one-way analysis again to update it.")
  })
  output$ow_title <- renderUI({
    s <- ow_state(); if (is.null(s)) return(h2("One-way sensitivity analysis"))
    tagList(h2(sprintf("%s: %s", s$label, tolower(outcome_labels[[s$outcome]]))),
            p(class = "card-sub", sprintf("Base case %s per man. Bars run from the base case to the result with each input at its low (blue) and high (orange) value.",
                                          fmt_usd(s$base))))
  })
  output$ow_plot_ui <- renderUI({
    s <- ow_state()
    if (is.null(s)) return(div(class = "empty-state", p("Choose a policy and a result, then run the one-way analysis to see which inputs move the result most.")))
    n <- min(input$ow_n, nrow(ow()))
    if (n == 0) return(div(class = "empty-state", p("Choose at least one group of inputs.")))
    plotlyOutput("ow_plot", height = paste0(90 + 34 * n, "px"))
  })
  output$ow_plot <- renderPlotly({
    s <- ow_state(); req(s)
    TornadoPlot(head(ow(), input$ow_n), s$base, outcome_labels[[s$outcome]])
  })

  # ── Probabilistic analysis ─────────────────────────────────────────────────
  run_psa <- function(n, seed) {
    if (is.null(n) || is.na(n) || n < 200 || n > 50000) {
      showNotification("Choose between 200 and 50,000 draws.", type = "error"); return()
    }
    res <- tryCatch(RunPSA(bc(), genPSA(bc(), nsims = n, seed = if (is.null(seed) || is.na(seed)) 2026 else seed)), error = function(e) e)
    if (inherits(res, "error")) { showNotification(conditionMessage(res), type = "error"); return() }
    rv$psa <- list(res = res, inputs = rv$inputs, n = n)
  }
  observeEvent(input$psa_run, run_psa(input$psa_n, input$psa_seed))
  output$psa_stale <- renderUI({
    if (!is.null(rv$psa) && !identical(rv$psa$inputs, rv$inputs))
      div(class = "note note-stale", "The inputs have changed since this analysis ran. Run it again to update the results.")
  })
  output$psa_body <- renderUI({
    if (is.null(rv$psa))
      return(card(class = "mt-3", card_body(div(class = "empty-state",
                                                p("Run the probabilistic analysis to characterise the uncertainty in the net benefit of each policy."),
                                                actionButton("psa_run2", "Run with 5,000 draws", class = "btn-primary")))))
    r <- rv$psa$res
    tagList(
      if (mean(!r$valid) > 0) div(class = "note", sprintf("%s of draws had a money coefficient of zero or above, which leaves the CV undefined; they are left out.",
                                                         fmt_pct(mean(!r$valid), 1))),
      card(class = "mt-3", card_header(h2("Net benefit per man"), p(class = "card-sub", sprintf("Means and 95%% intervals over %s draws, 2017 US$", format(r$n, big.mark = ",")))),
           card_body(div(style = "overflow-x: auto;", reactableOutput("psa_table")))),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(7, 5)), class = "mt-3",
        card(card_header(h2("Benefit-cost plane"),
                         p(class = "card-sub", "Each point is one draw, per uncircumcised man. Above the dashed line, the CV exceeds the incremental cost.")),
             card_body(plotlyOutput("psa_plane", height = "460px"))),
        card(card_header(h2("Probability that each policy has the highest net benefit"),
                         p(class = "card-sub", "Share of draws in which the policy has the highest net benefit. The net benefit of the initial state is zero.")),
             card_body(plotlyOutput("psa_best", height = "460px")))))
  })
  observeEvent(input$psa_run2, { updateNumericInput(session, "psa_n", value = 5000); run_psa(5000, input$psa_seed) })
  psa_prob_best <- reactive({
    req(rv$psa); r <- rv$psa$res; p <- rv$psa$inputs$policies
    data.frame(label = p$label, p_cv = probBest(r, "nb_cv"), p_total = probBest(r, "nb_total"))
  })
  output$psa_table <- renderReactable({
    r <- rv$psa$res; p <- rv$psa$inputs$policies; cols <- policy_colours(p)
    s1 <- summarisePSA(r, "nb_cv"); s2 <- summarisePSA(r, "nb_total"); s0 <- summarisePSA(r, "cv")
    pb <- psa_prob_best()[-1, ]
    df <- data.frame(policy = s1$policy, label = p$label[-1], cv = s0$mean, nb_cv = s1$mean, nb_total = s2$mean,
                     p_cv = s1$p_positive, best_cv = pb$p_cv, best_total = pb$p_total)
    reactable(df, theme = rt_theme, sortable = FALSE,
              columns = list(
                policy = colDef(show = FALSE),
                label = colDef(name = "Policy", minWidth = 190, sticky = "left", cell = function(value, index) policy_tag(value, cols[[df$policy[index]]])),
                cv = colDef(name = "Compensating variation (CV)", minWidth = 130, cell = function(value, index) sub_line(fmt_usd(value), paste0(fmt_usd(s0$lo[index]), " to ", fmt_usd(s0$hi[index])))),
                nb_cv = colDef(name = "CV minus cost", minWidth = 130, cell = function(value, index) sub_line(tags$span(class = if (value < 0) "neg" else "pos", fmt_usd(value)), paste0(fmt_usd(s1$lo[index]), " to ", fmt_usd(s1$hi[index])))),
                p_cv = colDef(name = "P(CV > cost)", minWidth = 115, cell = function(value) fmt_pct(value)),
                nb_total = colDef(name = "Net benefit, incl. averted ART", minWidth = 150, cell = function(value, index) sub_line(tags$strong(fmt_usd(value)), paste0(fmt_usd(s2$lo[index]), " to ", fmt_usd(s2$hi[index])))),
                best_cv = colDef(name = "Best on CV minus cost", minWidth = 110, cell = function(value) fmt_pct(value)),
                best_total = colDef(name = "Best, incl. averted ART", minWidth = 110, cell = function(value) fmt_pct(value))))
  })
  output$psa_plane <- renderPlotly({ req(rv$psa); p <- rv$psa$inputs$policies; ScatterPSA(rv$psa$res, p, policy_colours(p)) })
  output$psa_best <- renderPlotly(ProbBestPlot(psa_prob_best()))

  # ── Inputs: summary, reset, download, upload ───────────────────────────────
  output$edit_summary <- renderUI({
    n <- n_ed()
    if (n == 0) p(class = "edit-summary", "All inputs are as published.")
    else p(class = "edit-summary is-edited", plural(n, "input"), " changed from the published model.")
  })
  sync_controls <- function(s) {
    updateRadioButtons(session, "set_model", selected = s$model)
    if (s$model == "cl_three_price_slopes") updateSelectInput(session, "set_slope", selected = s$money_slope)
    updateSliderInput(session, "set_s0", value = s$optout_share)
    updateNumericInput(session, "set_pop", value = s$population)
  }
  observeEvent(input$reset_all, { rv$inputs <- published; sync_controls(published$settings) })
  output$dl_inputs <- downloadHandler(
    filename = function() "mmc_cba_inputs.xlsx",
    content = function(file) write_xlsx(inputs_to_sheets(rv$inputs), file))
  observeEvent(input$ul_inputs, {
    f <- input$ul_inputs$datapath
    new <- tryCatch({ x <- read_inputs(f); RunCBA(x); x }, error = function(e) e)
    if (inherits(new, "error")) {
      showNotification(paste("That file can't be used:", conditionMessage(new)), type = "error", duration = 10); return()
    }
    rv$inputs <- new; sync_controls(new$settings)
    showNotification("Inputs uploaded.", type = "message")
  })

  # ── Inputs: choice model settings ──────────────────────────────────────────
  observeEvent(list(input$set_model, input$set_slope), {
    req(input$set_model)
    slope <- if (input$set_model == "cl_one_price_slope") "price" else (input$set_slope %||% "pricecash")
    if (!identical(rv$inputs$settings$model, input$set_model) || !identical(rv$inputs$settings$money_slope, slope)) {
      rv$inputs$settings$model <- input$set_model; rv$inputs$settings$money_slope <- slope
    }
  })
  observeEvent(input$set_s0, {
    if (!isTRUE(all.equal(rv$inputs$settings$optout_share, input$set_s0))) rv$inputs$settings$optout_share <- input$set_s0
  })
  observeEvent(input$set_pop, {
    v <- input$set_pop
    if (!is.null(v) && !is.na(v) && v >= 1 && v != rv$inputs$settings$population) rv$inputs$settings$population <- round(v)
  })
  output$tbl_coefs <- renderReactable({
    s <- rv$inputs$settings; m <- rv$inputs$models[[s$model]]
    df <- data.frame(parameter = names(m$beta), label = unname(param_labels[names(m$beta)]),
                     estimate = unname(m$beta), se = unname(m$se))
    df$lo <- df$estimate - 1.96 * df$se; df$hi <- df$estimate + 1.96 * df$se
    reactable(df, theme = rt_theme, sortable = FALSE, compact = TRUE,
              rowClass = function(index) if (df$parameter[index] == s$money_slope) "row-money" else "",
              columns = list(
                parameter = colDef(name = "Parameter", minWidth = 110, cell = function(value) tags$span(class = "var-code", value)),
                label = colDef(name = "Attribute", minWidth = 210),
                estimate = colDef(name = "Estimate", align = "right", cell = function(value) fmt_num(value, 4)),
                se = colDef(name = "Robust SE", align = "right", cell = function(value) fmt_num(value, 4)),
                lo = colDef(name = "95% CI", align = "right", minWidth = 140,
                            cell = function(value, index) paste0(fmt_num(value, 4), " to ", fmt_num(df$hi[index], 4))),
                hi = colDef(show = FALSE)))
  })

  # ── Inputs: costs ──────────────────────────────────────────────────────────
  output$tbl_costs <- renderReactable({
    cst <- rv$inputs$costs; ed <- edits()$costs
    df <- cst[, c("parameter", "label", "base", "low", "high", "psa_dist", "price_year", "source")]
    reactable(df, theme = rt_theme, sortable = FALSE, onClick = row_click_js("cost_click", "parameter"),
              rowClass = function(index) paste("clickable", if (isTRUE(ed[[df$parameter[index]]])) "row-edited" else ""),
              columns = list(
                parameter = colDef(show = FALSE),
                label = colDef(name = "Input", minWidth = 260,
                               cell = function(value, index) div(class = "param-name", value,
                                                                  div(class = "param-meta", tags$span(class = "var-code", df$parameter[index]),
                                                                      if (isTRUE(ed[[df$parameter[index]]])) tags$span(class = "chip chip-edited", "Edited")))),
                base = colDef(name = "Base", align = "right", minWidth = 80, cell = function(value) fmt_val(value)),
                low = colDef(name = "Low", align = "right", minWidth = 70, cell = function(value) fmt_val(value)),
                high = colDef(name = "High", align = "right", minWidth = 70, cell = function(value) fmt_val(value)),
                psa_dist = colDef(name = "Distribution", minWidth = 100, cell = function(value) if (value == "pert") "Beta-PERT" else "Fixed"),
                price_year = colDef(name = "US$ of", minWidth = 70, cell = function(value) if (is.na(value)) "\u2013" else value),
                source = colDef(name = "Source", minWidth = 300, style = list(fontSize = "0.84rem", color = ui_ink$secondary))))
  })
  observeEvent(input$cost_click, {
    i <- match(input$cost_click, rv$inputs$costs$parameter); req(!is.na(i))
    rv$edit <- list(type = "cost", i = i)
    showModal(cost_modal(rv$inputs$costs[i, ], isTRUE(edits()$costs[[i]]), rv$inputs$cpi$year))
  })
  ce_vals <- reactive(list(base = input$ce_base, low = input$ce_low, high = input$ce_high))
  ce_errors <- reactive({ req(rv$edit$type == "cost"); check_cost(rv$inputs$costs$parameter[rv$edit$i], ce_vals()) })
  output$ce_feedback <- renderUI(errors_box(ce_errors()))
  output$ce_save_ui <- renderUI(save_button("ce_save", "Save", length(ce_errors()) == 0))
  observeEvent(input$ce_save, {
    req(rv$edit$type == "cost", length(ce_errors()) == 0)
    i <- rv$edit$i; v <- ce_vals()
    rv$inputs$costs$base[i] <- v$base; rv$inputs$costs$low[i] <- v$low; rv$inputs$costs$high[i] <- v$high
    rv$inputs$costs$psa_dist[i] <- input$ce_dist
    if (!is.null(input$ce_year)) rv$inputs$costs$price_year[i] <- as.numeric(input$ce_year)
    removeModal()
  })
  observeEvent(input$ce_reset, {
    i <- rv$edit$i; j <- match(rv$inputs$costs$parameter[i], published$costs$parameter)
    rv$inputs$costs[i, ] <- published$costs[j, ]; removeModal()
  })

  # ── Inputs: services ───────────────────────────────────────────────────────
  output$tbl_options <- renderReactable({
    o <- rv$inputs$options; ed <- edits()$options
    df <- o[o$option != "opt_out", ]
    df$payment <- ifelse(df$price > 0, paste("Pays", fmt_usd(df$price, 0)), ifelse(df$price < 0, paste(fmt_usd(-df$price, 0), df$incentive), "Free"))
    df$cost <- rv$inputs$costs$label[match(df$cost_item, rv$inputs$costs$parameter)]
    df <- df[, c("option", "label", "facility", "distance", "days", "privacy", "method", "payment", "cost")]
    reactable(df, theme = rt_theme, sortable = FALSE, onClick = row_click_js("option_click", "option"),
              rowClass = function(index) paste("clickable", if (isTRUE(ed[[df$option[index]]])) "row-edited" else ""),
              columns = list(
                option = colDef(show = FALSE),
                label = colDef(name = "Service", minWidth = 210, sticky = "left",
                               cell = function(value, index) div(class = "param-name", value,
                                                                  if (isTRUE(ed[[df$option[index]]])) div(tags$span(class = "chip chip-edited", "Edited")))),
                facility = colDef(name = "Facility", minWidth = 95, cell = function(value) if (value == "permanent") "Permanent" else "Outreach camp"),
                distance = colDef(name = "Distance", minWidth = 75, cell = function(value) paste(value, "km")),
                days = colDef(name = "Days a week", minWidth = 80),
                privacy = colDef(name = "Counselling", minWidth = 105, cell = function(value) c(alone = "Alone", "own age" = "Same age", "all ages" = "All ages")[[value]]),
                method = colDef(name = "Method", minWidth = 75, cell = function(value) if (value == "device") "Device" else "Surgery"),
                payment = colDef(name = "Payment", minWidth = 90),
                cost = colDef(name = "Provider cost", minWidth = 220, style = list(fontSize = "0.86rem", color = ui_ink$secondary))))
  })
  open_option <- function(o, is_new) {
    in_initial <- o$option %in% pol()$option_list[[1]]
    rv$edit <- list(type = "option", id = o$option, new = is_new, row = o)
    showModal(option_modal(o, edited = isTRUE(edits()$options[o$option]), can_delete = !is_new && !in_initial,
                           cost_items = procedure_items(rv$inputs$costs), is_new = is_new))
  }
  observeEvent(input$option_click, {
    o <- rv$inputs$options[rv$inputs$options$option == input$option_click, ]; req(nrow(o) == 1)
    open_option(o, FALSE)
  })
  observeEvent(input$add_option, {
    rv$n_new <- rv$n_new + 1
    o <- data.frame(option = paste0("service_", rv$n_new), label = "New service", facility = "permanent", distance = 5,
                    days = 5, privacy = "own age", method = "surgery", incentive = "none", price = 0,
                    cost_item = "cost_surgery_hospital")
    open_option(o, TRUE)
  })
  oe_vals <- reactive(list(label = input$oe_label, facility = input$oe_facility, distance = input$oe_distance,
                           days = input$oe_days, privacy = input$oe_privacy, method = input$oe_method,
                           incentive = input$oe_incentive, amount = input$oe_amount, cost_item = input$oe_cost))
  oe_check <- reactive({ req(rv$edit$type == "option"); check_option(oe_vals()) })
  output$oe_feedback <- renderUI(errors_box(oe_check()$errors, oe_check()$warnings))
  output$oe_save_ui <- renderUI(save_button("oe_save", if (isTRUE(rv$edit$new)) "Add service" else "Save", length(oe_check()$errors) == 0))
  observeEvent(input$oe_save, {
    req(rv$edit$type == "option", length(oe_check()$errors) == 0)
    v <- oe_vals()
    row <- data.frame(option = rv$edit$id, label = trimws(v$label), facility = v$facility, distance = as.numeric(v$distance),
                      days = as.numeric(v$days), privacy = v$privacy, method = v$method, incentive = v$incentive,
                      price = if (v$incentive == "none") v$amount else -v$amount, cost_item = v$cost_item)
    o <- rv$inputs$options
    if (isTRUE(rv$edit$new)) rv$inputs$options <- rbind(o, row) else rv$inputs$options[o$option == rv$edit$id, ] <- row
    removeModal()
    if (isTRUE(rv$edit$new)) showNotification("Service added. Add it to a policy on the Policies tab.", type = "message")
  })
  observeEvent(input$oe_reset, {
    j <- match(rv$edit$id, published$options$option)
    rv$inputs$options[rv$inputs$options$option == rv$edit$id, ] <- published$options[j, ]; removeModal()
  })
  observeEvent(input$oe_delete, {
    id <- rv$edit$id; p <- rv$inputs$policies
    left <- vapply(p$option_list, function(s) length(setdiff(s, c("opt_out", id))), integer(1))
    uses <- vapply(p$option_list, function(s) id %in% s, logical(1))
    if (any(uses & left == 0)) {
      showNotification(paste("Can't delete: it's the only service in", paste(p$label[uses & left == 0], collapse = ", "), "."),
                       type = "error"); return()
    }
    inp <- rv$inputs
    for (i in which(uses)) inp <- set_policy_options(inp, i, setdiff(inp$policies$option_list[[i]], id))
    inp$options <- inp$options[inp$options$option != id, ]
    rv$inputs <- inp; removeModal()
  })

  # ── Inputs: policies ───────────────────────────────────────────────────────
  output$tbl_policies <- renderReactable({
    p <- pol(); o <- rv$inputs$options; ed <- edits()$policies; cols <- pcols()
    df <- data.frame(policy = p$policy, label = p$label,
                     services = vapply(p$option_list, function(s) paste(o$label[match(setdiff(s, "opt_out"), o$option)], collapse = " \u00b7 "), character(1)))
    reactable(df, theme = rt_theme, sortable = FALSE, onClick = row_click_js("policy_click", "policy"),
              rowClass = function(index) paste("clickable", if (isTRUE(ed[[df$policy[index]]])) "row-edited" else ""),
              columns = list(
                policy = colDef(show = FALSE),
                label = colDef(name = "Policy", minWidth = 220,
                               cell = function(value, index) div(class = "param-name",
                                                                  if (index == 1) tags$span(class = "pol", value) else policy_tag(value, cols[[df$policy[index]]]),
                                                                  div(class = "param-meta",
                                                                      if (index == 1) tags$span(class = "chip chip-unused", "Comparator"),
                                                                      if (isTRUE(ed[[df$policy[index]]])) tags$span(class = "chip chip-edited", "Edited")))),
                services = colDef(name = "Services on offer (besides staying uncircumcised)", minWidth = 320)))
  })
  open_policy <- function(i, pl, is_new) {
    rv$edit <- list(type = "policy", i = i, id = pl$policy, new = is_new, row = pl)
    showModal(policy_modal(pl, rv$inputs$options, edited = isTRUE(edits()$policies[pl$policy]),
                           can_delete = !is_new && i > 1, is_initial = i == 1, is_new = is_new))
  }
  observeEvent(input$policy_click, {
    i <- match(input$policy_click, pol()$policy); req(!is.na(i))
    open_policy(i, pol()[i, ], FALSE)
  })
  observeEvent(input$add_policy, {
    if (nrow(pol()) - 1 >= max_policies) {
      showNotification(sprintf("The app shows up to %d policies besides the initial one. Delete one first.", max_policies), type = "warning"); return()
    }
    rv$n_new <- rv$n_new + 1
    pl <- pol()[1, ]; pl$policy <- paste0("policy_", rv$n_new); pl$label <- "New policy"
    open_policy(nrow(pol()) + 1, pl, TRUE)
  })
  pe_errors <- reactive({ req(rv$edit$type == "policy"); check_policy(input$pe_label, input$pe_options) })
  output$pe_feedback <- renderUI(errors_box(pe_errors()))
  output$pe_save_ui <- renderUI(save_button("pe_save", if (isTRUE(rv$edit$new)) "Add policy" else "Save", length(pe_errors()) == 0))
  observeEvent(input$pe_save, {
    req(rv$edit$type == "policy", length(pe_errors()) == 0)
    inp <- rv$inputs; i <- rv$edit$i
    if (isTRUE(rv$edit$new)) {
      row <- rv$edit$row; inp$policies <- rbind(inp$policies[, names(inp$policies) != "option_list"],
                                                row[, names(row) != "option_list"])
      inp$policies$option_list <- c(rv$inputs$policies$option_list, list(NULL))
    }
    inp$policies$label[i] <- trimws(input$pe_label)
    inp <- set_policy_options(inp, i, input$pe_options)
    rv$inputs <- inp; removeModal()
  })
  observeEvent(input$pe_reset, {
    i <- rv$edit$i; j <- match(rv$edit$id, published$policies$policy)
    rv$inputs$policies[i, names(published$policies) != "option_list"] <- published$policies[j, names(published$policies) != "option_list"]
    rv$inputs$policies$option_list[[i]] <- published$policies$option_list[[j]]
    removeModal()
  })
  observeEvent(input$pe_delete, {
    i <- rv$edit$i; req(i > 1)
    rv$inputs$policies <- rv$inputs$policies[-i, ]; removeModal()
  })

  # ── How it works ───────────────────────────────────────────────────────────
  output$method_body <- renderUI(withMathJax(method_content()))
}

# The method page (rendered when opened, so MathJax typesets visible content)
method_content <- function() {
  div(class = "method",
      h1("How the model works"),
      p(class = "lede", "Discrete choice experiments are a potentially powerful tool to examine the monetary valuation of policy ",
        "options for increasing uptake of MMC, and hence a framework for conducting economic evaluations using a benefit-cost ",
        "framework. We value each policy using the compensating variation (CV) and compare it with the incremental cost of ",
        "providing the services. The ", tags$a(href = "working_paper.pdf", target = "_blank", rel = "noopener", "working paper"),
        " describes the methods in full."),
      div(class = "method-grid",
          div(
            h2("1. Indirect utility"),
            p("Respondent \\(n\\) chooses the alternative \\(j\\) that maximizes his utility, \\(U_{nj} = V_j + \\epsilon_{nj}\\), ",
              "with \\(V_j = x_j\\beta\\) and \\(\\epsilon_{nj}\\) independent and identically Gumbel distributed. ",
              "The opt-out (\\emph{neither}) option has \\(V_0 = \\delta_{neither} + \\hat{a}\\). The probability of choosing ",
              "alternative \\(j\\) from the set of options in a policy, \\(S\\), is the logit function:"),
            p("$$P_j(S) = \\frac{e^{V_j}}{\\sum_{k \\in S} e^{V_k}}$$"),
            p("We use the conditional logit model with a single price coefficient for out-of-pocket and incentive payments. ",
              "The model with three price coefficients is a scenario analysis."),
            h2("2. Calibration"),
            p("The conditional logit predicts that 89% of men would choose circumcision if surgical circumcision were only ",
              "available at permanent health facilities. To replicate the observed share of uncircumcised men, \\(S\\) (69.2%), ",
              "we add a constant, \\(\\hat{a}\\), to the utility of the opt-out in the initial state, \\(S_0\\):"),
            p("$$\\hat{a} = \\operatorname{logit}(S) + \\ln \\sum_{k \\in S_0 \\setminus 0} e^{V_k} - \\delta_{neither}$$"),
            h2("3. Compensating variation"),
            p("The CV is a Hicksian measure of the welfare effects of policy changes. For the logit model, with utility linear in ",
              "income, the expected CV is given by the log-sum formula (Small and Rosen 1981):"),
            p("$$CV = \\frac{1}{\\lambda}\\Bigg[\\ln \\sum_{j \\in S_1} e^{V_j^1} - \\ln \\sum_{j \\in S_0} e^{V_j^0}\\Bigg]$$"),
            p("where \\(\\lambda = -\\beta_{price}\\) is the marginal utility of income (0.0272 per US$). A positive CV is a welfare ",
              "gain: the amount a man would be willing to pay for the policy. With one price coefficient, an incentive payment ",
              "of \\(m\\) dollars is valued at \\(m\\), so it cancels against its cost to the payer, as a transfer should. The ",
              "utility of a cash incentive beyond its money value (about $14) does not cancel.")),
          div(
            h2("4. Incremental costs"),
            p("With provider cost \\(c_j\\) per procedure, incentive payment \\(m_j\\) and cost \\(d\\) of delivering it, the ",
              "incremental cost per uncircumcised man is:"),
            p("$$\\Delta C = \\sum_{j \\in S_1} P_j(S_1)(c_j + m_j + d\\,\\mathbf{1}[m_j > 0]) - \\sum_{j \\in S_0} P_j(S_0)(c_j + m_j)$$"),
            h2("5. Averted HIV treatment costs"),
            p("The CV includes the perceived benefits of circumcision to the man, but not the costs of HIV treatment, which ",
              "are not borne by him. One additional circumcision averts \\(1/k\\) HIV infections, each requiring ART at ",
              "\\(c_{ART}\\) per year for \\(L\\) years, starting \\(d\\) years later, discounted at rate \\(r\\):"),
            p("$$B_{tx} = \\frac{c_{ART}}{k}(1+r)^{-d}\\,\\frac{1 - (1+r)^{-L}}{r}$$"),
            h2("6. Net benefit"),
            p("$$NB_{CV} = CV - \\Delta C, \\qquad NB = NB_{CV} + \\Delta U \\cdot B_{tx}$$"),
            p("where \\(\\Delta U\\) is the change in the uptake of MMC. Benefits to female partners are not included."),
            h2("7. Uncertainty"),
            p("We used the Krinsky-Robb method (2,000 draws of the utility parameters from a multivariate normal distribution ",
              "with the robust covariance matrix) for the intervals on the Results page. The probabilistic analysis also draws ",
              "each cost input from a beta-PERT distribution over its low, base and high values. In the one-way analysis, ",
              "each input is varied over its range while all others are held at the base case."))),
      card(class = "mt-4",
           card_header(h2("Data and sources")),
           card_body(
             tags$ul(class = "sources",
                     tags$li("Utility parameters: conditional logit models estimated from a discrete choice experiment among 406 ",
                             "self-reported uncircumcised, HIV-negative men aged 18\u201345 years in Ntenjeru sub-county, Mukono district ",
                             "(4,872 choices; March\u2013April 2017)."),
                     tags$li("Larson B, et al. How much does it cost to improve access to voluntary medical male circumcision among high-risk, ",
                             "low-income communities in Uganda? PLoS One 2015;10(3):e0119484. Costs at fixed sites and outreach camps."),
                     tags$li("Duffy K, et al. Reach and cost-effectiveness of the PrePex device for safe male circumcision in Uganda. ",
                             "PLoS One 2013;8(5):e63134. Cost of device circumcision."),
                     tags$li("Kripke K, et al. Age targeting of voluntary medical male circumcision programs using the Decision Makers' ",
                             "Program Planning Toolkit (DMPPT) 2.0. PLoS One 2016;11(7):e0156909. Circumcisions per HIV infection averted; cost of ART."),
                     tags$li("Small KA, Rosen HS. Applied welfare economics with discrete choice models. Econometrica 1981;49(1):105\u2013130."),
                     tags$li("Krinsky I, Robb AL. On approximating the statistical properties of elasticities. Review of Economics and Statistics 1986;68(4):715\u2013719.")))))
}

shinyApp(ui, server)
