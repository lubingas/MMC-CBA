# app/helpers.R
# ---------------------------
# Constants and helpers for the Shiny app: colours, formatting, labels, edit
# tracking, writing the inputs back to a spreadsheet, and the verdict text.
# ---------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# ── Colours ────────────────────────────────────────────────────────────────────
# Okabe-Ito, in a colour-blind-checked order. Policies take them in the order
# they are listed (the initial policy has none: it is the comparator).
policy_palette <- c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9")
max_policies <- length(policy_palette)
measure_cols <- c(cv = "#0072B2", cost = "#D55E00", tx = "#009E73")

ui_ink <- list(primary = "#13303b", secondary = "#4c5c63", muted = "#6c7a80",
               grid = "#e6e3da", axis = "#cdc8bb", surface = "#fdfcf8")
app_font <- "Atkinson Hyperlegible Next, system-ui, -apple-system, Segoe UI, sans-serif"

policy_colours <- function(policies) {
  ids <- policies$policy[-1]
  setNames(policy_palette[seq_along(ids)], ids)
}
policy_tag <- function(label, colour) {
  tags$span(class = "pol", tags$span(class = "pol-dot", style = paste0("background:", colour)), label)
}

# ── Formatting ─────────────────────────────────────────────────────────────────
minus <- "−"
fmt_usd <- function(x, digits = 2) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return("–")
    s <- formatC(abs(v), format = "f", digits = digits, big.mark = ",")
    paste0(if (v < 0 && round(abs(v), digits) > 0) minus else "", "$", s)
  }, character(1))
}
fmt_usd_auto <- function(x, per) fmt_usd(x, if (per > 1) 0 else 2)
fmt_num <- function(x, digits = 2) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return("–")
    s <- formatC(abs(v), format = "f", digits = digits, big.mark = ",")
    paste0(if (v < 0 && round(abs(v), digits) > 0) minus else "", s)
  }, character(1))
}
fmt_pct <- function(x, digits = 0) {
  vapply(x, function(v) if (is.na(v)) "–" else paste0(formatC(100 * v, format = "f", digits = digits), "%"), character(1))
}
fmt_val <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    format(signif(v, 4), big.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  }, character(1))
}
plural <- function(n, one, many = paste0(one, "s")) paste(format(n, big.mark = ","), if (n == 1) one else many)

# ── Labels ─────────────────────────────────────────────────────────────────────
model_labels <- c(cl_one_price_slope = "One price slope", cl_three_price_slopes = "Three price slopes")
slope_labels <- c(price = "Price (all money)", pricecash = "Cash", pricevoucher = "Voucher", pricenone = "Out-of-pocket")
outcome_labels <- c(nb_total = "Net benefit, including averted ART costs",
                    nb_cv = "Net benefit, CV minus cost",
                    cv = "Compensating variation (CV)")
facility_levels <- c("Permanent (hospital)" = "permanent", "Temporary (outreach camp)" = "temporary")
privacy_levels  <- c("Alone" = "alone", "With men of the same age" = "own age", "With all ages" = "all ages")
method_levels   <- c("Surgery" = "surgery", "Device" = "device")
incentive_levels <- c("None" = "none", "Voucher" = "voucher", "Cash" = "cash")
procedure_items <- function(costs) {
  keep <- grepl("^cost_", costs$parameter) & costs$parameter != "cost_incentive_payment"
  setNames(costs$parameter[keep], costs$label[keep])
}

# Plain-language description of a service option
describe_option <- function(o) {
  if (o$option == "opt_out") return("Stay uncircumcised")
  pay <- if (is.na(o$price) || o$price == 0) "free" else if (o$price > 0) paste0("he pays $", fmt_val(o$price)) else
    paste0("$", fmt_val(-o$price), " ", o$incentive)
  paste0(if (o$method == "device") "Device" else "Surgery", ", ",
         if (o$facility == "permanent") "permanent facility" else "outreach camp", ", ",
         o$distance, " km, ", o$days, " day", if (o$days != 1) "s", " a week, counselling ",
         c(alone = "alone", "own age" = "with men of his age", "all ages" = "with all ages")[[o$privacy]], ", ", pay)
}

# ── Edits ──────────────────────────────────────────────────────────────────────
# Which inputs differ from the published ones, section by section
input_edits <- function(cur, pub) {
  s_keys <- c("model", "money_slope", "optout_share", "population")
  settings <- vapply(s_keys, function(k) !identical(as.character(cur$settings[[k]]), as.character(pub$settings[[k]])), logical(1))
  row_diff <- function(a, b, key, cols) {
    vapply(a[[key]], function(id) {
      j <- match(id, b[[key]])
      if (is.na(j)) return(TRUE)
      i <- match(id, a[[key]])
      !isTRUE(all.equal(lapply(a[i, cols], identity), lapply(b[j, cols], identity), check.attributes = FALSE))
    }, logical(1))
  }
  list(settings = settings,
       options  = row_diff(cur$options, pub$options, "option",
                           c("label", "facility", "distance", "days", "privacy", "method", "incentive", "price", "cost_item")),
       policies = row_diff(cur$policies, pub$policies, "policy", c("label", "options")),
       removed  = length(setdiff(pub$options$option, cur$options$option)) + length(setdiff(pub$policies$policy, cur$policies$policy)),
       costs    = row_diff(cur$costs, pub$costs, "parameter", c("base", "low", "high", "psa_dist", "price_year")))
}
n_edits <- function(ed) sum(ed$settings) + sum(ed$options) + sum(ed$policies) + sum(ed$costs) + ed$removed

# ── Writing inputs ─────────────────────────────────────────────────────────────
inputs_to_sheets <- function(inp) {
  s <- inp$settings
  settings <- data.frame(setting = names(s), value = vapply(s, as.character, character(1)), row.names = NULL)
  cov_sheet <- function(m) data.frame(parameter = rownames(m$vcov), m$vcov, check.names = FALSE, row.names = NULL)
  coefs <- do.call(rbind, lapply(names(inp$models), function(nm) {
    m <- inp$models[[nm]]
    data.frame(model = nm, parameter = names(m$beta), estimate = unname(m$beta), robust_se = unname(m$se), source = m$source)
  }))
  pol <- inp$policies; pol$option_list <- NULL
  c(list(settings = settings, options = inp$options, policies = pol, costs = inp$costs, coefficients = coefs),
    setNames(lapply(inp$models, cov_sheet), paste0("cov_", names(inp$models))),
    list(cpi = inp$cpi))
}

# Keeps policy option lists in step with the "options" text column
set_policy_options <- function(inp, i, ids) {
  ids <- unique(c("opt_out", ids))
  inp$policies$options[i] <- paste(ids, collapse = ", ")
  inp$policies$option_list[[i]] <- ids
  inp
}

# ── Verdict ────────────────────────────────────────────────────────────────────
verdict_text <- function(bc, per) {
  pol <- bc$inputs$policies
  nbt <- bc$nb_total[1, -1]; nbc <- bc$nb_cv[1, -1]
  lab <- setNames(pol$label[-1], pol$policy[-1])
  q <- function(x) paste0("\u2018", x, "\u2019")
  bt <- names(which.max(nbt)); bv <- names(which.max(nbc))
  title <- if (max(nbt) > 0)
    tagList(tags$span(class = "v-pol", lab[[bt]]),
            sprintf(" has the highest net benefit: %s per %s men, including averted HIV treatment costs.",
                    fmt_usd(nbt[[bt]] * per, 0), format(per, big.mark = ",")))
  else "No policy has a positive net benefit, even including averted HIV treatment costs."
  neg <- names(nbc)[nbc < 0]
  detail <- if (max(nbc) > 0)
    paste0("Based on the CV alone, the highest net benefit is for ", q(lab[[bv]]), " (", fmt_usd(nbc[[bv]]), " per man).",
           if (length(neg)) paste0(" The CV of ", paste(q(lab[neg]), collapse = " and "), " is lower than ",
                                   if (length(neg) == 1) "its" else "their", " incremental cost.") else "")
  else paste0("Based on the CV alone, no policy is worth its cost; ", q(lab[[bv]]), " comes closest (",
              fmt_usd(nbc[[bv]]), " per man).")
  list(title = title, detail = detail, best = bt)
}
