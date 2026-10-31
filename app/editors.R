# app/editors.R
# ---------------------------
# Dialogs for editing a cost input, a service option or a policy, and the
# checks that run before a change is saved.
# ---------------------------

save_button <- function(id, label, ok) {
  b <- actionButton(id, label, class = "btn-primary")
  if (!ok) b <- tagAppendAttributes(b, disabled = NA)
  b
}
errors_box <- function(e, warn = character(0)) {
  tagList(if (length(e)) div(class = "ed-errors", tags$ul(lapply(e, tags$li))),
          if (length(warn)) div(class = "ed-warn", tags$ul(lapply(warn, tags$li))))
}

# Cost input ───────────────────────────────────────────────────────────────────
cost_modal <- function(row, edited, cpi_years) {
  money <- !is.na(row$price_year)
  modalDialog(
    title = div(div(row$label), div(class = "modal-sub", tags$span(class = "var-code", row$parameter))),
    div(class = "ed-section",
        h3("Values"),
        div(class = "ed-grid",
            numericInput("ce_base", "Base case", row$base),
            numericInput("ce_low", "Low, for the one-way analysis", row$low),
            numericInput("ce_high", "High, for the one-way analysis", row$high))),
    div(class = "ed-section ed-grid-2",
        selectInput("ce_dist", "In the probabilistic analysis", width = "100%",
                    choices = c("Beta-PERT over low, base and high" = "pert", "Fixed at the base case" = "fixed"),
                    selected = row$psa_dist),
        if (money) selectInput("ce_year", "Price year of these values", choices = cpi_years,
                               selected = row$price_year, width = "100%")),
    if (money) p(class = "small-print", "Values are converted to 2017 US$ with the US consumer price index."),
    p(class = "small-print", tags$strong("Source: "), row$source),
    uiOutput("ce_feedback"),
    footer = tagList(
      if (edited) actionButton("ce_reset", "Reset to the published value", class = "btn-outline-secondary"),
      modalButton("Cancel"), uiOutput("ce_save_ui", inline = TRUE)),
    size = "l", easyClose = TRUE)
}

check_cost <- function(param, v) {
  e <- character(0)
  if (any(vapply(v, function(x) is.null(x) || is.na(x), logical(1)))) return("Fill in the base, low and high values.")
  if (v$low > v$base || v$base > v$high) e <- c(e, "The values must run low ≤ base ≤ high.")
  if (v$low < 0) e <- c(e, "Values can't be negative.")
  if (param == "discount_rate" && v$high > 0.2) e <- c(e, "Enter the discount rate as a proportion, for example 0.03 for 3%.")
  if (param == "mmc_per_infection_averted" && v$low <= 0) e <- c(e, "The number of circumcisions per infection averted must be above zero.")
  e
}

# Service option ───────────────────────────────────────────────────────────────
option_modal <- function(o, edited, can_delete, cost_items, is_new = FALSE) {
  amount <- if (is.na(o$price)) 0 else abs(o$price)
  modalDialog(
    title = div(div(if (is_new) "New service" else o$label),
                div(class = "modal-sub", "Described by the attributes of the choice experiment")),
    textInput("oe_label", "Name", o$label, width = "100%"),
    div(class = "ed-grid",
        selectInput("oe_facility", "Facility", facility_levels, o$facility),
        selectInput("oe_distance", "Distance from home (km)", c(1, 5, 15), o$distance),
        sliderInput("oe_days", "Days a week offered", min = 1, max = 5, value = o$days, step = 1, ticks = FALSE),
        selectInput("oe_privacy", "Counselling and waiting", privacy_levels, o$privacy),
        selectInput("oe_method", "Method", method_levels, o$method),
        selectInput("oe_incentive", "Incentive", incentive_levels, o$incentive)),
    div(class = "ed-grid-2", style = "margin-top: 0.6rem;",
        numericInput("oe_amount", "Amount (US$)", amount, min = 0, step = 1, width = "100%"),
        selectInput("oe_cost", "Provider cost per procedure", cost_items, o$cost_item, width = "100%")),
    p(class = "small-print", style = "margin-top: 0.4rem;",
      "With no incentive, the amount is what the man pays; with a voucher or cash, it is what he receives. ",
      "The provider costs are edited on the Costs tab."),
    uiOutput("oe_feedback"),
    footer = tagList(
      if (can_delete) actionButton("oe_delete", "Delete this service", class = "btn-outline-danger"),
      if (edited && !is_new) actionButton("oe_reset", "Reset to the published service", class = "btn-outline-secondary"),
      modalButton("Cancel"), uiOutput("oe_save_ui", inline = TRUE)),
    size = "l", easyClose = TRUE)
}

# Errors block saving; warnings flag combinations the experiment didn't show
check_option <- function(v) {
  e <- character(0); w <- character(0)
  if (is.null(v$label) || !nzchar(trimws(v$label))) e <- c(e, "Give the service a name.")
  if (is.null(v$amount) || is.na(v$amount) || v$amount < 0) e <- c(e, "Enter an amount of zero or more.")
  if (length(e)) return(list(errors = e, warnings = w))
  if (v$incentive == "none" && v$amount > 8) w <- c(w, "Men paid at most $8 in the experiment; the model extrapolates beyond that.")
  if (v$incentive != "none" && v$amount > 12) w <- c(w, "Incentives were at most $12 in the experiment; the model extrapolates beyond that.")
  if (v$incentive != "none" && v$amount == 0) w <- c(w, "A voucher or cash incentive of $0 was never shown in the experiment.")
  if (v$facility == "temporary" && as.numeric(v$distance) == 15) w <- c(w, "Outreach camps were 1 or 5 km away in the experiment, never 15 km.")
  if (v$facility == "permanent" && as.numeric(v$distance) == 1) w <- c(w, "Permanent facilities were 5 or 15 km away in the experiment, never 1 km.")
  list(errors = e, warnings = w)
}

# Policy ───────────────────────────────────────────────────────────────────────
policy_modal <- function(pl, options, edited, can_delete, is_initial, is_new = FALSE) {
  svc <- options[options$option != "opt_out", ]
  modalDialog(
    title = div(div(if (is_new) "New policy" else pl$label),
                div(class = "modal-sub", if (is_initial) "The comparator: every other policy is measured against it"
                    else "Men can always choose to stay uncircumcised")),
    textInput("pe_label", "Name", pl$label, width = "100%"),
    checkboxGroupInput("pe_options", "Services on offer", width = "100%",
                       choiceNames = lapply(seq_len(nrow(svc)), function(i)
                         tags$span(tags$strong(svc$label[i]), tags$br(), tags$span(class = "small-print", describe_option(svc[i, ])))),
                       choiceValues = svc$option, selected = setdiff(pl$option_list[[1]], "opt_out")),
    if (is_initial) p(class = "note", "Changing the initial policy changes the calibration: the opt-out constant is set so that ",
                      "the share who stay uncircumcised under this policy matches the share in the settings."),
    uiOutput("pe_feedback"),
    footer = tagList(
      if (can_delete) actionButton("pe_delete", "Delete this policy", class = "btn-outline-danger"),
      if (edited && !is_new) actionButton("pe_reset", "Reset to the published policy", class = "btn-outline-secondary"),
      modalButton("Cancel"), uiOutput("pe_save_ui", inline = TRUE)),
    size = "l", easyClose = TRUE)
}

check_policy <- function(label, opts) {
  e <- character(0)
  if (is.null(label) || !nzchar(trimws(label))) e <- c(e, "Give the policy a name.")
  if (length(opts) == 0) e <- c(e, "Choose at least one service.")
  e
}
