# app/charts.R
# ---------------------------
# Plotly charts for the app. Each takes plain data frames and returns a widget.
# ---------------------------

plot_base <- function(p, ...) {
  p |>
    layout(font = list(family = app_font, color = ui_ink$primary, size = 13),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           hoverlabel = list(font = list(family = app_font, size = 13), bgcolor = ui_ink$surface,
                             bordercolor = ui_ink$axis),
           legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom",
                         font = list(size = 12.5)),
           ...) |>
    config(displaylogo = FALSE, responsive = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "zoomIn2d", "zoomOut2d",
                                      "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian"),
           toImageButtonOptions = list(format = "png", scale = 2))
}
axis_style <- function(title = NULL, ...) {
  list(title = list(text = title, font = list(size = 12.5, color = ui_ink$secondary)),
       gridcolor = ui_ink$grid, zerolinecolor = "#8f9a9e", zerolinewidth = 1.2, linecolor = ui_ink$axis,
       tickfont = list(size = 12, color = ui_ink$secondary), automargin = TRUE, ...)
}
money_axis <- function(title, per) axis_style(title, tickprefix = "$", separatethousands = TRUE,
                                              tickformat = if (per > 1) ",.0f" else ",.2~f")

# Policy labels on a categorical y axis, first policy at the top
y_policies <- function(labels) axis_style(NULL, categoryorder = "array", categoryarray = rev(labels),
                                          showgrid = FALSE, ticks = "", ticklabelstandoff = 8)

# Benefit to men (CV, with interval) against the extra cost, per policy
BenefitCostPlot <- function(tab, per, narrow = FALSE) {
  u <- if (per > 1) sprintf("per %s men", format(per, big.mark = ",")) else "per man"
  hv <- function(lab, v, extra = "") paste0("<b>", lab, "</b><br>", v, extra)
  plot_ly(tab, y = ~label) |>
    add_trace(type = "bar", orientation = "h", x = ~cv, name = "Compensating variation (CV)", marker = list(color = measure_cols[["cv"]]),
              error_x = list(type = "data", symmetric = FALSE, array = ~cv_hi - cv, arrayminus = ~cv - cv_lo,
                             color = ui_ink$primary, thickness = 1.3, width = 4),
              hovertext = ~hv(label, paste("CV:", fmt_usd_auto(cv, per)),
                              paste0("<br>95% interval ", fmt_usd_auto(cv_lo, per), " to ", fmt_usd_auto(cv_hi, per))),
              hoverinfo = "text", textposition = "none") |>
    add_trace(type = "bar", orientation = "h", x = ~total_cost, name = "Incremental cost (services and incentives)",
              marker = list(color = measure_cols[["cost"]]),
              hovertext = ~hv(label, paste("Incremental cost:", fmt_usd_auto(total_cost, per)),
                              paste0("<br>of which incentives ", fmt_usd_auto(transfers, per))),
              hoverinfo = "text", textposition = "none") |>
    plot_base(barmode = "group", bargap = 0.32,
              xaxis = money_axis(paste("US$ (2017)", u), per), yaxis = y_policies(tab$label),
              margin = list(l = 8, r = 12, t = 8, b = 8),
              legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom",
                            traceorder = "normal"))
}

# Net benefit with and without averted treatment costs: two panels, shared policies
NetBenefitPlot <- function(tab, per, narrow = FALSE) {
  u <- if (per > 1) sprintf("per %s men", format(per, big.mark = ",")) else "per man"
  dom1 <- if (narrow) c(0, 1) else c(0, 0.46); dom2 <- if (narrow) c(0, 1) else c(0.54, 1)
  ydom1 <- if (narrow) c(0.56, 1) else c(0, 1); ydom2 <- if (narrow) c(0, 0.44) else c(0, 1)
  hv <- function(v, lo, hi, p) paste0("<b>", tab$label, "</b><br>", fmt_usd_auto(v, per), "<br>95% interval ",
                                      fmt_usd_auto(lo, per), " to ", fmt_usd_auto(hi, per),
                                      "<br>Chance above zero: ", fmt_pct(p))
  ya <- y_policies(tab$label)
  p <- plot_ly() |>
    add_trace(data = tab, x = ~nb_cv, y = ~label, type = "scatter", mode = "markers", name = "CV minus cost",
              marker = list(color = measure_cols[["cv"]], size = 10),
              error_x = list(type = "data", symmetric = FALSE, array = ~nb_cv_hi - nb_cv, arrayminus = ~nb_cv - nb_cv_lo,
                             color = measure_cols[["cv"]], thickness = 2.2, width = 0),
              hovertext = hv(tab$nb_cv, tab$nb_cv_lo, tab$nb_cv_hi, tab$p_nb_cv_positive), hoverinfo = "text",
              xaxis = "x", yaxis = "y") |>
    add_trace(data = tab, x = ~nb_total, y = ~label, type = "scatter", mode = "markers",
              name = "Including averted ART costs",
              marker = list(color = measure_cols[["tx"]], size = 10),
              error_x = list(type = "data", symmetric = FALSE, array = ~nb_total_hi - nb_total,
                             arrayminus = ~nb_total - nb_total_lo, color = measure_cols[["tx"]], thickness = 2.2, width = 0),
              hovertext = hv(tab$nb_total, tab$nb_total_lo, tab$nb_total_hi, tab$p_nb_total_positive), hoverinfo = "text",
              xaxis = "x2", yaxis = if (narrow) "y2" else "y")
  lay <- list(
    xaxis = c(money_axis(paste("CV minus cost,", u), per), list(domain = dom1, anchor = "y", rangemode = "tozero")),
    xaxis2 = c(money_axis(paste("Including averted ART costs,", u), per),
               list(domain = dom2, anchor = if (narrow) "y2" else "y", rangemode = "tozero")),
    yaxis = c(ya, list(domain = ydom1)),
    margin = list(l = 8, r = 12, t = 8, b = 8))
  if (narrow) lay$yaxis2 <- c(ya, list(domain = ydom2, anchor = "x2"))
  do.call(plot_base, c(list(p), lay))
}

# Tornado: the outcome with each input at its low and its high value
TornadoPlot <- function(ow, base, outcome_label, per = 1) {
  ow <- ow[nrow(ow):1, ]
  ow$disp <- ifelse(nchar(ow$label) > 46, paste0(substr(ow$label, 1, 44), "…"), ow$label)
  ow$disp <- make.unique(ow$disp, sep = " ")
  und <- is.na(ow$low) | is.na(ow$high)
  lowx <- ifelse(is.na(ow$low), 0, ow$low - base); highx <- ifelse(is.na(ow$high), 0, ow$high - base)
  hv <- function(which, v, r) paste0("<b>", ow$label, "</b><br>", which, ": ", fmt_val(v), "<br>",
                                     outcome_label, ": ", ifelse(is.na(r), "undefined (money coefficient ≥ 0)", fmt_usd_auto(r * per, per)))
  p <- plot_ly(y = ow$disp) |>
    add_trace(type = "bar", orientation = "h", x = lowx * per, base = base * per, name = "Input at its low value",
              marker = list(color = "#0072B2"), hovertext = hv("Low value", ow$low_value, ow$low), hoverinfo = "text",
              textposition = "none") |>
    add_trace(type = "bar", orientation = "h", x = highx * per, base = base * per, name = "Input at its high value",
              marker = list(color = "#D55E00"), hovertext = hv("High value", ow$high_value, ow$high), hoverinfo = "text",
              textposition = "none")
  if (any(und)) p <- p |> add_annotations(x = base * per, y = ow$disp[und], text = "CV undefined at one limit",
                                          showarrow = FALSE, xanchor = "left", xshift = 6,
                                          font = list(size = 12, color = ui_ink$secondary))
  p |> plot_base(barmode = "overlay", bargap = 0.35,
                 xaxis = money_axis(paste0(outcome_label, if (per > 1) sprintf(", per %s men", format(per, big.mark = ",")) else ", per man"), per),
                 yaxis = axis_style(NULL, categoryorder = "array", categoryarray = ow$disp, showgrid = FALSE, ticks = ""),
                 shapes = list(list(type = "line", x0 = base * per, x1 = base * per, y0 = -0.5, y1 = nrow(ow) - 0.5,
                                    line = list(color = ui_ink$primary, width = 1.5))),
                 margin = list(l = 8, r = 16, t = 8, b = 8))
}

# Benefit-cost plane: each draw's extra cost against its CV, per policy
ScatterPSA <- function(psa, pol, cols, per = 1, max_pts = 700) {
  ids <- pol$policy[-1]; labs <- setNames(pol$label[-1], ids)
  v <- which(psa$valid)
  set.seed(1); keep <- if (length(v) > max_pts) sort(sample(v, max_pts)) else v
  p <- plot_ly()
  rng <- c()
  for (id in ids) {
    x <- psa$total_cost[keep, id] * per; y <- psa$cv[keep, id] * per
    rng <- range(c(rng, x, y), finite = TRUE)
    p <- p |> add_trace(x = x, y = y, type = "scatter", mode = "markers", name = labs[[id]],
                        marker = list(color = cols[[id]], size = 5, opacity = 0.5,
                                      line = list(color = ui_ink$surface, width = 0.5)),
                        hovertext = paste0("<b>", labs[[id]], "</b><br>Incremental cost ", fmt_usd_auto(x, per),
                                           "<br>CV ", fmt_usd_auto(y, per)),
                        hoverinfo = "text")
  }
  lim <- c(min(0, rng[1]), rng[2] * 1.04)
  u <- if (per > 1) sprintf(", per %s men", format(per, big.mark = ",")) else ", per man"
  p |> plot_base(
    xaxis = c(money_axis(paste0("Incremental cost", u), per), list(range = lim)),
    yaxis = c(money_axis(paste0("Compensating variation (CV)", u), per), list(range = lim)),
    shapes = list(list(type = "line", x0 = lim[1], y0 = lim[1], x1 = lim[2], y1 = lim[2],
                       line = list(color = ui_ink$primary, width = 1.4, dash = "dash"))),
    annotations = list(
      list(x = lim[1] + 0.06 * diff(lim), y = lim[2] - 0.05 * diff(lim), xanchor = "left", showarrow = FALSE,
           text = "CV exceeds incremental cost", font = list(size = 12.5, color = ui_ink$secondary)),
      list(x = lim[2] - 0.04 * diff(lim), y = lim[1] + 0.06 * diff(lim), xanchor = "right", showarrow = FALSE,
           text = "Incremental cost exceeds CV", font = list(size = 12.5, color = ui_ink$secondary))),
    margin = list(l = 8, r = 12, t = 8, b = 8))
}

# Chance each policy has the largest net benefit (the initial policy counts as zero)
ProbBestPlot <- function(pb) {
  plot_ly(pb, y = ~label) |>
    add_trace(type = "bar", orientation = "h", x = ~p_cv, name = "CV minus cost", marker = list(color = measure_cols[["cv"]]),
              hovertext = ~paste0("<b>", label, "</b><br>Best on CV minus cost in ", fmt_pct(p_cv, 1), " of draws"),
              hoverinfo = "text", textposition = "none") |>
    add_trace(type = "bar", orientation = "h", x = ~p_total, name = "Including averted ART costs",
              marker = list(color = measure_cols[["tx"]]),
              hovertext = ~paste0("<b>", label, "</b><br>Best including treatment costs in ", fmt_pct(p_total, 1), " of draws"),
              hoverinfo = "text", textposition = "none") |>
    plot_base(barmode = "group", bargap = 0.3,
              yaxis = y_policies(pb$label),
              xaxis = axis_style("Share of draws in which it is best", tickformat = ".0%", range = c(0, 1.02)),
              margin = list(l = 8, r = 12, t = 8, b = 8))
}

# Mixed logit: mean and median CV per policy, with the conditional logit CV and the
# incremental cost. Log scale when every value is positive (the means under a
# log-normal price coefficient are far above the medians), linear otherwise.
MixlCVPlot <- function(s, cl_cv) {
  s$cl_cv <- cl_cv
  vals <- c(s$cv_mean, s$cv_median, s$cl_cv, s$total_cost, s$cv_mean_lo, s$cv_mean_hi)
  logx <- all(is.na(vals) | vals > 0)
  hv <- function(what, v) paste0("<b>", s$label, "</b><br>", what, ": ", fmt_usd(v))
  has_kr <- "cv_mean_lo" %in% names(s)
  p <- plot_ly(s, y = ~label) |>
    add_trace(type = "scatter", mode = "markers", x = ~total_cost, name = "Incremental cost",
              marker = list(symbol = "line-ns-open", size = 16, color = measure_cols[["cost"]], line = list(width = 3)),
              hovertext = ~hv("Incremental cost", total_cost), hoverinfo = "text") |>
    add_trace(type = "scatter", mode = "markers", x = ~cl_cv, name = "Conditional logit CV",
              marker = list(symbol = "x-thin-open", size = 10, color = "#6c7a80", line = list(width = 2)),
              hovertext = ~hv("Conditional logit CV", cl_cv), hoverinfo = "text") |>
    add_trace(type = "scatter", mode = "markers", x = ~cv_median, name = "Median CV",
              marker = list(symbol = "circle-open", size = 11, color = measure_cols[["cv"]], line = list(width = 2)),
              hovertext = ~hv("Median CV", cv_median), hoverinfo = "text") |>
    add_trace(type = "scatter", mode = "markers", x = ~cv_mean, name = "Mean CV",
              marker = list(symbol = "circle", size = 11, color = measure_cols[["cv"]]),
              error_x = if (has_kr) list(type = "data", symmetric = FALSE, array = s$cv_mean_hi - s$cv_mean,
                                         arrayminus = s$cv_mean - s$cv_mean_lo, color = measure_cols[["cv"]],
                                         thickness = 1.3, width = 4),
              hovertext = ~hv("Mean CV", cv_mean), hoverinfo = "text")
  plot_base(p, xaxis = c(axis_style(paste0("US$ (2017) per man", if (logx) ", log scale" else ""), tickprefix = "$",
                                    tickformat = ",.2~f"), if (logx) list(type = "log")),
            yaxis = y_policies(s$label), margin = list(l = 8, r = 12, t = 8, b = 8))
}

# Mixed logit: share of men whose CV exceeds the incremental cost per man, for each
# distribution of the price coefficient
MixlSharePlot <- function(d) {
  dist_cols <- c("#0072B2", "#E69F00", "#009E73")
  p <- plot_ly()
  for (i in rev(seq_along(levels(d$spec)))) {        # plotly stacks a group from the bottom up
    x <- d[d$spec == levels(d$spec)[i], ]
    p <- add_trace(p, data = x, type = "bar", orientation = "h", y = ~label, x = ~share, name = levels(d$spec)[i],
                   marker = list(color = dist_cols[i]), textposition = "none", hoverinfo = "text",
                   hovertext = ~paste0("<b>", label, "</b><br>", spec, ": ", fmt_pct(share)))
  }
  plot_base(p, barmode = "group", bargap = 0.28,
            xaxis = axis_style("Share of men whose CV exceeds the cost per man", tickformat = ".0%", range = c(0, 1)),
            yaxis = y_policies(unique(d$label)), margin = list(l = 8, r = 12, t = 8, b = 8),
            shapes = list(list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1, yref = "paper",
                               line = list(color = "#8f9a9e", dash = "dash", width = 1.2)))) |>
    layout(legend = list(traceorder = "reversed"))      # legend in the order of the bars, fixed first
}
