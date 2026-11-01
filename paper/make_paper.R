# paper/make_paper.R
# ---------------------------
# Writes the tables (paper/tables/*.tex), figures (paper/figures/*.pdf) and the
# numbers quoted in the text (paper/numbers.tex) for the working paper, from
# the same model the app runs. Run from the project folder:
#   Rscript paper/make_paper.R
# then compile paper/workingpaper.tex with XeLaTeX (see paper/README).
# ---------------------------

# run the model (main.R reads the inputs, runs the base case, PSA, one-way
# analyses and scenarios, and leaves the results in the workspace)
source("main.R")

tab_dir <- "paper/tables"
fig_dir <- "paper/figures"
dir.create(tab_dir, showWarnings = FALSE); dir.create(fig_dir, showWarnings = FALSE)


### helpers

# numbers with a LaTeX minus sign
num <- function(x, d = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return("--")
    s <- formatC(abs(v), format = "f", digits = d, big.mark = ",")
    if (v < 0 && round(abs(v), d) > 0) paste0("$-$", s) else s
  }, character(1))
}
usd <- function(x, d = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return("--")
    s <- formatC(abs(v), format = "f", digits = d, big.mark = ",")
    paste0(if (v < 0 && round(abs(v), d) > 0) "$-$" else "", "\\$", s)
  }, character(1))
}
pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "\\%")
ci  <- function(lo, hi, f = usd, ...) paste0("(", f(lo, ...), ", ", f(hi, ...), ")")
stars <- function(est, se) {
  p <- 2 * pnorm(-abs(est / se))
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, "\\dag", ""))))
}
esc <- function(x) gsub("\\$", "\\\\$", gsub("%", "\\\\%", gsub("&", "\\\\&", x)))
# column headings over two or three lines: "Incentive (device, US$12 cash)" -> Incentive / (device, / US$12 cash)
stack <- function(x) paste0("\\makecell{", gsub(", ", ",\\\\\\\\", gsub(" \\(", "\\\\\\\\(", esc(x))), "}")

# write a tabular environment
write.tab <- function(file, header, rows, align, pre = NULL, post = NULL) {
  out <- c(sprintf("\\begin{tabular}{%s}", align), "\\toprule", pre, paste(header, collapse = " & "), "\\\\", "\\midrule",
           paste0(vapply(rows, function(r) paste(r, collapse = " & "), character(1)), " \\\\"),
           post, "\\bottomrule", "\\end{tabular}")
  writeLines(out, file.path(tab_dir, file))
}

pol <- inputs$policies
ids <- pol$policy[-1]
short <- setNames(sub(":.*", "", pol$label[-1]), ids)          # "Policy 1", ...
long  <- setNames(sub("^Policy [0-9]: ", "", pol$label[-1]), ids)


### numbers quoted in the text

macros <- list()
mac <- function(name, value) macros[[name]] <<- value
words <- c("One", "Two", "Three", "Four", "Five")

m1 <- inputs$models$cl_one_price_slope; m3 <- inputs$models$cl_three_price_slopes
mac("lambda", formatC(base_case$lambda, format = "f", digits = 4))
mac("lambdaSE", formatC(m1$se[["price"]], format = "f", digits = 4))
mac("aHat", formatC(base_case$a_hat, format = "f", digits = 3))
mac("optoutShare", formatC(100 * base_case$S, format = "f", digits = 1))
uncal <- RunUncalibrated(inputs, base_case$model, base_case$money_slope)
mac("uncalUptake", formatC(100 * uncal$uptake[1, 1], format = "f", digits = 0))
mac("txPerMMC", formatC(base_case$tx_per_mmc, format = "f", digits = 0))
mac("cashConst", formatC(m1$beta[["cash"]] / -m1$beta[["price"]], format = "f", digits = 0))
mac("voucherConst", formatC(m1$beta[["voucher"]] / -m1$beta[["price"]], format = "f", digits = 0))
mac("pctDroppedThree", formatC(100 * mean(!kr_three$valid), format = "f", digits = 1))
mac("costPerMMCmin", formatC(min(results_man$cost_per_extra_mmc), format = "f", digits = 0))
mac("costPerMMCmax", formatC(max(results_man$cost_per_extra_mmc), format = "f", digits = 0))

for (i in seq_along(ids)) {
  id <- ids[i]; w <- paste0("P", words[i]); r <- results_man[results_man$policy == id, ]
  ps <- psa_summary[psa_summary$policy == id, ]
  mac(paste0(w, "Uptake"), formatC(100 * r$uptake, format = "f", digits = 1))
  mac(paste0(w, "UptakeChange"), formatC(100 * r$uptake_change, format = "f", digits = 1))
  mac(paste0(w, "CV"), usd(r$cv)); mac(paste0(w, "CVlo"), usd(r$cv_lo)); mac(paste0(w, "CVhi"), usd(r$cv_hi))
  mac(paste0(w, "Cost"), usd(r$total_cost)); mac(paste0(w, "Transfers"), usd(r$transfers))
  mac(paste0(w, "NBcv"), usd(r$nb_cv)); mac(paste0(w, "NBcvP"), pct(r$p_nb_cv_positive))
  mac(paste0(w, "Tx"), usd(r$tx_savings)); mac(paste0(w, "NBtotal"), usd(r$nb_total))
  mac(paste0(w, "CostPerMMC"), usd(r$cost_per_extra_mmc, 0))
  mac(paste0(w, "PSAnbcv"), usd(ps$nb_cv_mean)); mac(paste0(w, "PSAnbcvP"), pct(ps$nb_cv_p_positive))
  mac(paste0(w, "PSAnbcvLo"), usd(ps$nb_cv_lo)); mac(paste0(w, "PSAnbcvHi"), usd(ps$nb_cv_hi))
  mac(paste0(w, "BestCV"), pct(ps$p_best_nb_cv)); mac(paste0(w, "BestTotal"), pct(ps$p_best_nb_total))
  r3 <- results_three[results_three$policy == id, ]
  mac(paste0(w, "CVthree"), usd(r3$cv)); mac(paste0(w, "CVthreeLo"), usd(r3$cv_lo)); mac(paste0(w, "CVthreeHi"), usd(r3$cv_hi))
}
mac("InitBestCV", pct(psa_summary$p_best_nb_cv[1]))

# one way analyses for policy 5: largest effects
ow5 <- dsa_res[dsa_res$policy == ids[5], ]
top_cv <- head(ow5[ow5$outcome == "nb_cv", ], 3)
top_tot <- head(ow5[ow5$outcome == "nb_total", ], 4)
mac("OWcvTopOne", tolower(top_cv$label[1])); mac("OWcvTopOneLo", usd(top_cv$low[1])); mac("OWcvTopOneHi", usd(top_cv$high[1]))
mac("OWcvTopTwo", tolower(top_cv$label[2])); mac("OWcvTopThree", tolower(top_cv$label[3]))
mac("OWtotTop", paste(tolower(top_tot$label), collapse = "; "))

writeLines(sprintf("\\newcommand{\\mmc%s}{%s}", names(macros), unlist(macros)), "paper/numbers.tex")


### tables

# conditional logit estimates
ll <- function(f) {
  x <- readLines(file.path("data-raw/estimates", f))
  g <- function(k) as.numeric(sub(".*:\\s*", "", grep(k, x, value = TRUE)[1]))
  c(k = g("Estimated parameters"), ll = g("LL\\(final\\)"), aic = g("^AIC"), bic = g("^BIC"))
}
fit1 <- ll("cl_no_corr_output.txt"); fit3 <- ll("01_cl_3_price_coefs_output.txt")
coef_rows <- list(
  c("neither", "Alternative specific constant (\\emph{neither})"),
  c("permhc", "Permanent (vs temporary) health facility"),
  c("dist5", "Distance 5 km (vs 1 km)"), c("dist15", "Distance 15 km (vs 1 km)"),
  c("avail", "Number of week-days for circumcision"),
  c("somepriv", "Privacy: men of your age only (vs by yourself)"),
  c("notpriv", "Privacy: all age groups together (vs by yourself)"),
  c("device", "Device (vs surgical) circumcision"),
  c("voucher", "Voucher (vs no) incentive"), c("cash", "Cash (vs no) incentive"),
  c("price", "Price, all payments"), c("pricenone", "Price, out-of-pocket payment"),
  c("pricevoucher", "Price, voucher payment"), c("pricecash", "Price, cash payment"))
cell <- function(m, p, d = 3) if (p %in% names(m$beta)) paste0(num(m$beta[[p]], d), stars(m$beta[[p]], m$se[[p]]), " (", num(m$se[[p]], d), ")") else ""
rows <- lapply(coef_rows, function(r) c(r[2], cell(m1, r[1], if (grepl("price", r[1])) 4 else 3), cell(m3, r[1], if (grepl("price", r[1])) 4 else 3)))
write.tab("tab_cl.tex", c("Attribute", "One price coefficient", "Three price coefficients"), rows, "lcc",
          post = c("\\midrule",
                   sprintf("Number of parameters & %d & %d \\\\", fit1[["k"]], fit3[["k"]]),
                   sprintf("Log-likelihood & %s & %s \\\\", num(fit1[["ll"]], 1), num(fit3[["ll"]], 1)),
                   sprintf("AIC & %s & %s \\\\", num(fit1[["aic"]], 1), num(fit3[["aic"]], 1)),
                   sprintf("BIC & %s & %s \\\\", num(fit1[["bic"]], 1), num(fit3[["bic"]], 1))))

# service options in the policy scenarios
o <- inputs$options[inputs$options$option != "opt_out", ]
lv <- function(x, map) unname(map[x])
attr_rows <- list(
  c("Type of health facility", lv(o$facility, c(permanent = "Permanent", temporary = "Temporary"))),
  c("Distance (km)", o$distance),
  c("Week-days for circumcision", o$days),
  c("Privacy of counselling and waiting", lv(o$privacy, c(alone = "By yourself", "own age" = "Men of your age", "all ages" = "All age groups"))),
  c("Method of circumcision", lv(o$method, c(surgery = "Surgery", device = "Device"))),
  c("Incentive", lv(o$incentive, c(none = "None", voucher = "Voucher", cash = "Cash"))),
  c("Price (US\\$)", ifelse(o$price == 0, "Free", num(o$price, 0))),
  c("Provider cost (2017 US\\$)", usd(base_case$costs[o$cost_item])))
write.tab("tab_options.tex", c("Attribute", stack(o$label)), attr_rows, paste0("l", strrep("c", nrow(o))))

# composition of the policies
comp <- lapply(seq_len(nrow(pol)), function(i)
  c(esc(pol$label[i]), ifelse(o$option %in% pol$option_list[[i]], "\\checkmark", "")))
write.tab("tab_policies.tex", c("Policy", stack(o$label)), comp, paste0("l", strrep("c", nrow(o))))

# cost inputs
cst <- inputs$costs
money <- !is.na(cst$price_year)
lo <- getCosts(cst, inputs$cpi, inputs$settings$price_year, "low"); hi <- getCosts(cst, inputs$cpi, inputs$settings$price_year, "high")
fmtc <- function(p, v) if (p == "discount_rate") pct(v, 0) else if (grepl("^cost_|art_cost", p)) usd(v) else num(v, 0)
cost_rows <- lapply(seq_len(nrow(cst)), function(i) {
  p <- cst$parameter[i]
  c(esc(cst$label[i]), fmtc(p, base_case$costs[[p]]), paste0(fmtc(p, lo[[p]]), "--", fmtc(p, hi[[p]])),
    if (cst$psa_dist[i] == "pert") "Beta-PERT" else "Fixed", esc(cst$source[i]))
})
write.tab("tab_costs.tex", c("Input", "Base case", "Range", "PSA", "Source"), cost_rows,
          ">{\\raggedright\\arraybackslash}p{4.2cm}rlc>{\\raggedright\\arraybackslash}p{5.2cm}")

# predicted uptake by service option
opts <- inputs$options$option
share_rows <- lapply(pol$policy, function(id) {
  S <- base_case$shares[[id]]
  c(esc(pol$label[pol$policy == id]), vapply(opts, function(op) if (op %in% colnames(S)) pct(S[1, op], 1) else "--", character(1)),
    pct(base_case$uptake[1, id], 1))
})
write.tab("tab_shares.tex", c("Policy", stack(inputs$options$label), "Uptake"), share_rows,
          paste0(">{\\raggedright\\arraybackslash}p{3.4cm}", strrep("c", length(opts) + 1)))

# base-case results per man
two <- function(a, b) paste0("\\makecell{", a, " \\\\ {\\footnotesize ", b, "}}")
res_rows <- lapply(seq_len(nrow(results_man)), function(i) {
  r <- results_man[i, ]
  c(esc(r$label),
    two(pct(r$uptake, 1), paste0("(+", formatC(100 * r$uptake_change, format = "f", digits = 1), ")")),
    two(usd(r$cv), ci(r$cv_lo, r$cv_hi)),
    two(usd(r$total_cost), if (r$transfers > 0) paste0("(", usd(r$transfers), " incentives)") else "(services only)"),
    two(usd(r$nb_cv), ci(r$nb_cv_lo, r$nb_cv_hi)),
    pct(r$p_nb_cv_positive),
    usd(r$tx_savings),
    two(usd(r$nb_total), ci(r$nb_total_lo, r$nb_total_hi)))
})
write.tab("tab_results.tex",
          c("Policy", "\\makecell{Uptake\\\\(change)}", "\\makecell{CV\\\\(95\\% CI)}", "\\makecell{Incremental\\\\cost}",
            "\\makecell{CV $-$ cost\\\\(95\\% CI)}", "P($>$0)", "\\makecell{Averted\\\\ART costs}", "\\makecell{Net benefit\\\\(95\\% CI)}"),
          res_rows, ">{\\raggedright\\arraybackslash}p{2.7cm}ccccccc")

# scenarios: CV minus cost
scen_names <- unique(scen_res$scenario)
scen_lab <- c("Base case" = "Base case: one price coefficient, calibrated",
              "Three price slopes, cash slope" = "Three price coefficients, cash payment",
              "Three price slopes, voucher slope" = "Three price coefficients, voucher payment",
              "Three price slopes, out-of-pocket slope" = "Three price coefficients, out-of-pocket payment",
              "Opt-out not calibrated" = "Opt-out constant not calibrated",
              "Opt-out share 60%" = "60\\% remain uncircumcised in the initial state",
              "Opt-out share 80%" = "80\\% remain uncircumcised in the initial state",
              "Incentives valued at face value only" = "Incentives valued at their money value only")
scen_rows <- lapply(scen_names, function(sn) {
  d <- scen_res[scen_res$scenario == sn, ]
  c(scen_lab[[sn]], usd(d$nb_cv[match(ids, d$policy)]))
})
write.tab("tab_scenarios.tex", c("Scenario", unname(short)), scen_rows, paste0(">{\\raggedright\\arraybackslash}p{6.2cm}", strrep("r", length(ids))))

# PSA
psa_rows <- lapply(seq_len(nrow(psa_summary)), function(i) {
  r <- psa_summary[i, ]
  if (i == 1) return(c(esc(r$label), "--", "--", "--", pct(r$p_best_nb_cv), pct(r$p_best_nb_total)))
  c(esc(r$label), two(usd(r$nb_cv_mean), ci(r$nb_cv_lo, r$nb_cv_hi)), pct(r$nb_cv_p_positive),
    two(usd(r$nb_total_mean), ci(r$nb_total_lo, r$nb_total_hi)), pct(r$p_best_nb_cv), pct(r$p_best_nb_total))
})
write.tab("tab_psa.tex",
          c("Policy", "\\makecell{CV $-$ cost, mean\\\\(95\\% CI)}", "P($>$0)", "\\makecell{Net benefit, mean\\\\(95\\% CI)}",
            "\\makecell{Highest,\\\\CV $-$ cost}", "\\makecell{Highest,\\\\net benefit}"),
          psa_rows, ">{\\raggedright\\arraybackslash}p{3.2cm}ccccc")

### figures (Source Sans Pro, as in the captions)
fam <- "Source Sans Pro"
okabe <- c(blue = "#0072B2", vermillion = "#D55E00", green = "#009E73", orange = "#E69F00", purple = "#CC79A7")
theme_paper <- theme_minimal(base_size = 9, base_family = fam) +
  theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), plot.title = element_blank(), plot.subtitle = element_blank())
# cairo_pdf squeezes the spaces in small text, so draw an SVG and convert it to PDF
# with the rsvg package: install.packages(c("svglite", "rsvg"))
save_pdf <- function(g, f, w, h) {
  svg <- tempfile(fileext = ".svg")
  svglite::svglite(svg, width = w, height = h); print(g); dev.off()
  rsvg::rsvg_pdf(svg, file.path(fig_dir, f))
}

save_pdf(g1 + theme_paper, "fig_cv_cost.pdf", 6.4, 3.4)
save_pdf(g2 + theme_paper + theme(strip.text = element_text(hjust = 0, face = "bold"), panel.spacing.x = unit(1.2, "lines")),
         "fig_net_benefit.pdf", 6.4, 3.2)
save_pdf(g3 + theme_paper + theme(plot.margin = margin(5.5, 14, 5.5, 5.5)), "fig_price_spec.pdf", 6.4, 3.4)

# tornado: policy 5, CV minus cost and net benefit
torn <- do.call(rbind, lapply(c("nb_cv", "nb_total"), function(oc) {
  d <- head(ow5[ow5$outcome == oc & is.finite(ow5$swing), ], 10)
  d$panel <- if (oc == "nb_cv") "CV minus cost" else "Net benefit, including averted ART costs"
  d$lab <- factor(paste(d$panel, d$label), levels = rev(paste(d$panel, d$label)))
  d
}))
gt <- ggplot(torn) +
  geom_segment(aes(y = lab, yend = lab, x = base, xend = low, colour = "Input at its low value"), linewidth = 4) +
  geom_segment(aes(y = lab, yend = lab, x = base, xend = high, colour = "Input at its high value"), linewidth = 4) +
  geom_vline(aes(xintercept = base), colour = "grey20", linewidth = 0.4) +
  facet_wrap(~panel, scales = "free", ncol = 1) +
  scale_y_discrete(labels = function(x) sub("^(CV minus cost|Net benefit, including averted ART costs) ", "", x)) +
  scale_colour_manual(values = unname(okabe[c("blue", "vermillion")]), name = NULL) +
  scale_x_continuous(labels = label_dollar()) +
  labs(x = "US$ per uncircumcised man (2017)", y = NULL) +
  theme_paper + theme(strip.text = element_text(hjust = 0, face = "bold"))
save_pdf(gt, "fig_tornado.pdf", 6.4, 5.2)

# benefit-cost plane from the PSA
set.seed(1)
keep <- sort(sample(which(psa_res$valid), 800))
plane <- do.call(rbind, lapply(seq_along(ids), function(i)
  data.frame(policy = factor(pol$label[i + 1], levels = pol$label[-1]),
             cost = psa_res$total_cost[keep, ids[i]], cv = psa_res$cv[keep, ids[i]])))
lim <- c(0, max(c(plane$cost, plane$cv)) * 1.03)
gp <- ggplot(plane, aes(x = cost, y = cv, colour = policy)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey30") +
  geom_point(size = 0.8, alpha = 0.45) +
  annotate("text", x = lim[1] + 0.03 * diff(lim), y = lim[2] * 0.97, label = "CV exceeds incremental cost",
           hjust = 0, size = 3, family = fam, colour = "grey30") +
  annotate("text", x = lim[2] * 0.97, y = lim[1] + 0.03 * diff(lim), label = "Incremental cost exceeds CV",
           hjust = 1, size = 3, family = fam, colour = "grey30") +
  scale_colour_manual(values = unname(okabe[c("blue", "orange", "green", "vermillion", "purple")]), name = NULL) +
  scale_x_continuous(labels = label_dollar(), limits = lim) + scale_y_continuous(labels = label_dollar(), limits = lim) +
  coord_equal() +
  guides(colour = guide_legend(override.aes = list(size = 2.5, alpha = 1), ncol = 2)) +
  labs(x = "Incremental cost per uncircumcised man (2017 US$)", y = "CV per uncircumcised man (2017 US$)") +
  theme_paper + theme(panel.grid.major.y = element_line(colour = "grey92"))
save_pdf(gp, "fig_bc_plane.pdf", 5.6, 5.4)


### mixed logits
# main_mixl.R must have been run on the estimates at MIXL_DRAWS draws; this reads its outputs
if (!file.exists("output/mixl_results.csv")) stop("Run main_mixl.R first (it writes output/mixl_results.csv)")
mxs <- if (file.exists("output/mixl_settings.csv")) read.csv("output/mixl_settings.csv") else
  data.frame(est_draws = 3000, est_seed = 1, sim_men = 2000, kr_sims = 200)
MIXL_DRAWS <- mxs$est_draws
mx <- read.csv("output/mixl_results.csv")
fitsum <- read.csv(sprintf("estimation/output/fit_summary_R%d_seed1.csv", MIXL_DRAWS))
specs  <- c(Fixed = "mixl_one_price_slope_fixed", Logn = "mixl_one_price_slope_lognormal", Cens = "mixl_one_price_slope_censored")
specs3 <- setNames(sub("one_price_slope", "three_price_slopes", specs), names(specs))
spec_lab <- c(Fixed = "Price fixed", Logn = "Price lognormal", Cens = "Price censored at zero")
mxd <- lapply(specs, function(m) read.csv(sprintf("estimation/output/%s_R%d_seed1_distribution.csv", m, MIXL_DRAWS)))
mxr <- function(m, id) mx[mx$model == m & mx$policy == id, ]
fs  <- function(m) fitsum[fitsum$model == m, ]
co  <- function(d, v, what) d[d$coefficient == v, what]
dol <- function(x, d = 2) usd(x, d)

# numbers quoted in the text
mac("MXdraws", format(MIXL_DRAWS, big.mark = ","))
mac("MXsimMen", format(mxs$sim_men, big.mark = ",")); mac("MXkr", format(mxs$kr_sims, big.mark = ","))
mac("MXstarts", max(fs(specs[["Fixed"]])$starts, 1))
for (sp in names(specs)) {
  f <- fs(specs[[sp]]); f3 <- fs(specs3[[sp]])
  mac(paste0("MXll", sp), num(f$LL, 1)); mac(paste0("MXbic", sp), num(f$BIC, 1))
  mac(paste0("MXgain", sp), num(f$LL_gain_over_CL, 0))
  mac(paste0("MXllThree", sp), num(f3$LL, 1)); mac(paste0("MXbicThree", sp), num(f3$BIC, 1))
}
mac("MXgainPrice", num(max(fs(specs[["Logn"]])$LL, fs(specs[["Cens"]])$LL) - fs(specs[["Fixed"]])$LL, 0))
mac("MXsingleBest", sum(fitsum$starts_at_best[startsWith(fitsum$model, "mixl_")] == 1))
mac("MXstartsOne", fs(specs[["Fixed"]])$starts)
dF <- mxd$Fixed; dL <- mxd$Logn; dC <- mxd$Cens
mac("MXpriceFixed", num(co(dF, "price", "mean"), 4))
mac("MXscale", num(co(dF, "price", "mean") / m1$beta[["price"]], 1))
mac("MXneitherSD", num(co(dF, "neither", "sd"), 2))
mac("MXpermhcMean", num(co(dF, "permhc", "mean"), 2)); mac("MXpermhcSD", num(co(dF, "permhc", "sd"), 2))
mac("MXdeviceMean", num(co(dF, "device", "mean"), 2)); mac("MXdeviceSD", num(co(dF, "device", "sd"), 2))
mac("MXdeviceLike", pct(pnorm(co(dF, "device", "mean") / co(dF, "device", "sd"))))
mac("MXcashMean", num(co(dF, "cash", "mean"), 2)); mac("MXcashSD", num(co(dF, "cash", "sd"), 2))
mac("MXlognMedian", num(co(dL, "price", "median"), 4)); mac("MXlognMean", num(co(dL, "price", "mean"), 4))
mac("MXlognRatio", num(co(dL, "price", "mean") / co(dL, "price", "median"), 1))
mac("MXcensZero", pct(co(dC, "price", "share_zero")))

fx <- mx[mx$model == specs[["Fixed"]], ]; cl <- mx[mx$model == "cl_one_price_slope", ]
fx <- fx[match(ids, fx$policy), ]; cl <- cl[match(ids, cl$policy), ]
for (i in seq_along(ids)) {
  id <- ids[i]; w <- paste0("MXP", words[i]); r <- fx[i, ]
  mac(paste0(w, "Uptake"), formatC(100 * r$uptake, format = "f", digits = 1))
  mac(paste0(w, "CV"), dol(r$cv_mean)); mac(paste0(w, "CVlo"), dol(r$cv_mean_lo)); mac(paste0(w, "CVhi"), dol(r$cv_mean_hi))
  mac(paste0(w, "CVmed"), dol(r$cv_median)); mac(paste0(w, "Cost"), dol(r$total_cost))
  mac(paste0(w, "NB"), dol(r$nb_mean)); mac(paste0(w, "NBlo"), dol(r$nb_mean_lo)); mac(paste0(w, "NBhi"), dol(r$nb_mean_hi))
  mac(paste0(w, "Gain"), pct(r$share_cv_above_cost)); mac(paste0(w, "NBtotal"), dol(r$nb_total_mean))
  mac(paste0(w, "PlugFixed"), dol(r$cv_plugin_mean))
  for (sp in c("Logn", "Cens")) {
    rs <- mxr(specs[[sp]], id)
    mac(paste0(w, "CV", sp), dol(rs$cv_mean, 0)); mac(paste0(w, "CVmed", sp), dol(rs$cv_median))
    mac(paste0(w, "Plug", sp), dol(rs$cv_plugin_mean)); mac(paste0(w, "Gain", sp), pct(rs$share_cv_above_cost))
    mac(paste0(w, "CVlo", sp), dol(rs$cv_mean_lo, 0)); mac(paste0(w, "CVhi", sp), dol(rs$cv_mean_hi, 0))
  }
}
udiff <- 100 * (cl$uptake - fx$uptake)
mac("MXuptakeDiffMin", formatC(min(udiff), format = "f", digits = 0)); mac("MXuptakeDiffMax", formatC(max(udiff), format = "f", digits = 0))
mac("MXtxDrop", pct(1 - fx$tx_savings[5] / cl$tx_savings[5]))
mac("MXgainMin", pct(min(fx$share_cv_above_cost))); mac("MXgainMax", pct(max(fx$share_cv_above_cost)))
med5 <- sapply(specs, function(m) mxr(m, ids[5])$cv_median)
mac("MXmedFiveMin", dol(min(med5))); mac("MXmedFiveMax", dol(max(med5)))
popdiff <- abs(fx$population_cv_mean / fx$cv_mean - 1)    # fx rows are policies 1 to 5
mac("MXpopDiff", pct(max(popdiff)))
mac("MXpopDiffOne", pct(popdiff[1])); mac("MXpopDiffTwo", pct(popdiff[2])); mac("MXpopDiffRest", pct(max(popdiff[3:5])))


### budget perspective (budget comes from main.R)
f1 <- function(x) formatC(x, format = "f", digits = 1)
mac("BreturnMin", f1(min(budget$return_per_dollar))); mac("BreturnMax", f1(max(budget$return_per_dollar)))
mac("BreturnLoMin", f1(min(budget$return_lo)))
mac("BirrMin", pct(min(budget$irr))); mac("BirrMax", pct(max(budget$irr)))
mac("BpaybackMin", f1(min(budget$payback_years))); mac("BpaybackMax", f1(max(budget$payback_years)))
mac("BcoverMin", pct(min(budget$coverage_needed))); mac("BcoverMax", pct(max(budget$coverage_needed)))
mac("BpSavingMin", pct(min(budget$p_saving)))
mac("BartPerYear", usd(base_case$costs[["art_cost_per_year"]] / base_case$costs[["mmc_per_infection_averted"]], 0))
for (i in seq_along(ids)) {
  w <- paste0("BP", c("One", "Two", "Three", "Four", "Five")[i]); b <- budget[budget$policy == ids[i], ]
  mac(paste0(w, "PerMMC"), usd(b$cost_per_extra_mmc, 0)); mac(paste0(w, "Return"), f1(b$return_per_dollar))
  mac(paste0(w, "IRR"), pct(b$irr)); mac(paste0(w, "Payback"), f1(b$payback_years))
  mac(paste0(w, "Saving"), usd(b$net_saving)); mac(paste0(w, "PerThousand"), formatC(1000 * b$uptake_change, format = "f", digits = 0))
}
# the mixed logit with a fixed price coefficient: lower uptake, the same arithmetic
bmx <- budgetView(fx$total_cost, fx$transfers, fx$uptake_change, base_case$costs)
mac("BMXperMMCmin", usd(min(bmx$cost_per_extra_mmc), 0)); mac("BMXperMMCmax", usd(max(bmx$cost_per_extra_mmc), 0))
mac("BMXreturnMin", f1(min(bmx$return_per_dollar))); mac("BMXreturnMax", f1(max(bmx$return_per_dollar)))
mac("BMXirrMin", pct(min(bmx$irr))); mac("BMXirrMax", pct(max(bmx$irr)))
writeLines(sprintf("\\newcommand{\\mmc%s}{%s}", names(macros), unlist(macros)), "paper/numbers.tex")

bud_rows <- lapply(seq_along(ids), function(i) {
  b <- budget[budget$policy == ids[i], ]
  c(esc(pol$label[i + 1]), usd(b$total_cost), formatC(1000 * b$uptake_change, format = "f", digits = 0),
    usd(b$cost_per_extra_mmc, 0), paste0("\\makecell{", f1(b$return_per_dollar), "\\\\(", f1(b$return_lo), ", ", f1(b$return_hi), ")}"),
    f1(b$payback_years), pct(b$irr), pct(b$coverage_needed), pct(b$p_saving))
})
write.tab("tab_budget.tex",
          c("Policy", "\\makecell{Cost now\\\\per man}", "\\makecell{Additional\\\\MMC per\\\\1,000 men}", "\\makecell{Cost per\\\\additional\\\\MMC}",
            "\\makecell{ART costs\\\\averted per\\\\dollar (95\\% CI)}", "\\makecell{Payback\\\\(years)}", "\\makecell{Internal\\\\rate of\\\\return}",
            "\\makecell{ART coverage\\\\needed to\\\\break even}", "\\makecell{P(saves\\\\money)}"),
          bud_rows, ">{\\raggedright\\arraybackslash}p{3.0cm}rrrcrrrr")

# ART costs averted per dollar spent now, by the discount rate the budget applies to them
rho <- seq(0, 0.35, by = 0.0025)
bfig <- do.call(rbind, lapply(seq_along(ids), function(i) {
  b <- budget[budget$policy == ids[i], ]
  data.frame(policy = factor(pol$label[i + 1], levels = pol$label[-1]), rho = rho,
             ratio = sapply(rho, function(r) artSaved(unlist(base_case$costs), r)) / b$cost_per_extra_mmc)
}))
birr <- data.frame(policy = factor(pol$label[-1], levels = pol$label[-1]), irr = budget$irr[match(ids, budget$policy)])
gb <- ggplot(bfig, aes(x = rho, y = ratio, colour = policy)) +
  geom_hline(yintercept = 1, colour = "grey30", linewidth = 0.4) +
  geom_vline(xintercept = base_case$costs[["discount_rate"]], colour = "grey50", linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(data = birr, aes(x = irr, y = 1), size = 2) +
  annotate("text", x = base_case$costs[["discount_rate"]] + 0.004, y = 0.32, label = "base case, 3%", hjust = 0, size = 2.8,
           family = fam, colour = "grey30") +
  scale_colour_manual(values = unname(okabe[c("blue", "orange", "green", "vermillion", "purple")]), name = NULL) +
  scale_x_continuous(labels = label_percent(), breaks = seq(0, 0.35, 0.05)) +
  scale_y_log10(breaks = c(0.25, 0.5, 1, 2, 5, 10, 20), labels = function(x) paste0("$", x)) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(x = "Discount rate applied to the ART costs averted", y = "ART costs averted per dollar spent now (log scale)") +
  theme_paper + theme(panel.grid.major.y = element_line(colour = "grey92"))
save_pdf(gb, "fig_budget.pdf", 6.4, 3.8)

# tables
mlabel <- function(m) {
  pr <- if (grepl("three", m)) "Three" else "One"
  dist <- if (startsWith(m, "cl_")) "--" else c(fixed = "Fixed", lognormal = "Lognormal", censored = "Censored at zero")[[sub(".*_", "", m)]]
  c(if (startsWith(m, "cl_")) "Conditional logit" else "Mixed logit", pr, dist)
}
fit_rows <- lapply(c("cl_one_price_slope", specs, "cl_three_price_slopes", specs3), function(m) {
  f <- fs(m); c(mlabel(m), num(f$LL, 1), f$parameters, num(f$AIC, 1), num(f$BIC, 1))
})
fit_rows[[4]][length(fit_rows[[4]])] <- paste0(fit_rows[[4]][length(fit_rows[[4]])], " \\\\ \\midrule %")
write.tab("tab_mixl_fit.tex", c("Model", "\\makecell{Price\\\\coefficients}", "\\makecell{Distribution of\\\\the price coefficient}",
                                "Log-likelihood", "Parameters", "AIC", "BIC"), fit_rows, "lllrrrr")

cellmx <- function(d, v, what, dg) {
  e <- co(d, v, what); se <- co(d, v, paste0(what, "_robse"))
  if (what == "sd" && d$distribution[d$coefficient == v] == "fixed") return("--")
  two(num(e, dg), paste0("(", num(se, dg), ")"))
}
coef_rows1 <- Filter(function(r) r[1] %in% dF$coefficient, coef_rows)
mx_rows <- lapply(coef_rows1, function(r) {
  dg <- if (r[1] == "price") 4 else 3
  c(r[2], unlist(lapply(mxd, function(d) c(cellmx(d, r[1], "mean", dg), cellmx(d, r[1], "sd", dg)))))
})
write.tab("tab_mixl_coef.tex", c("Attribute", rep(c("Mean", "SD"), 3)), mx_rows, "lcccccc",
          pre = c(paste0("& ", paste(sprintf("\\multicolumn{2}{c}{%s}", spec_lab), collapse = " & "), " \\\\"),
                  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}"),
          post = c("\\midrule",
                   sprintf("Price, median & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
                           num(co(dF, "price", "median"), 4), num(co(dL, "price", "median"), 4), num(co(dC, "price", "median"), 4)),
                   sprintf("Share of men with a price coefficient of zero & \\multicolumn{2}{c}{--} & \\multicolumn{2}{c}{--} & \\multicolumn{2}{c}{%s} \\\\",
                           pct(co(dC, "price", "share_zero"))),
                   sprintf("Log-likelihood & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\",
                           num(fs(specs[["Fixed"]])$LL, 1), num(fs(specs[["Logn"]])$LL, 1), num(fs(specs[["Cens"]])$LL, 1))))

bca_rows <- lapply(seq_along(ids), function(i) {
  r <- fx[i, ]
  c(esc(pol$label[pol$policy == ids[i]]),
    two(pct(r$uptake, 1), paste0("(+", formatC(100 * r$uptake_change, format = "f", digits = 1), ")")),
    two(dol(r$cv_mean), ci(r$cv_mean_lo, r$cv_mean_hi)), dol(r$cv_median), dol(r$total_cost),
    two(dol(r$nb_mean), ci(r$nb_mean_lo, r$nb_mean_hi)),
    two(pct(r$share_cv_above_cost), paste0("(", pct(r$share_cv_above_cost_lo), ", ", pct(r$share_cv_above_cost_hi), ")")),
    two(dol(r$nb_total_mean), ci(r$nb_total_mean_lo, r$nb_total_mean_hi)))
})
write.tab("tab_mixl_bca.tex",
          c("Policy", "\\makecell{Uptake\\\\(change)}", "\\makecell{Mean CV\\\\(95\\% CI)}", "\\makecell{Median\\\\CV}",
            "\\makecell{Incremental\\\\cost}", "\\makecell{Mean CV $-$ cost\\\\(95\\% CI)}", "\\makecell{Men with\\\\CV $>$ cost}",
            "\\makecell{Net benefit with\\\\averted ART costs}"),
          bca_rows, ">{\\raggedright\\arraybackslash}p{2.7cm}ccccccc")

price_rows <- lapply(seq_along(ids), function(i) {
  id <- ids[i]; rF <- fx[i, ]; rL <- mxr(specs[["Logn"]], id); rC <- mxr(specs[["Cens"]], id)
  c(esc(pol$label[pol$policy == id]), dol(cl$cv_mean[i]), dol(rF$cv_mean), dol(rF$cv_median),
    dol(rL$cv_mean, 0), dol(rL$cv_median), dol(rL$cv_plugin_mean), dol(rC$cv_mean), dol(rC$cv_median))
})
write.tab("tab_mixl_price.tex", c("Policy", "CV", "Mean", "Median", "Mean", "Median", "Plug-in", "Mean", "Median"),
          price_rows, ">{\\raggedright\\arraybackslash}p{2.7cm}cccccccc",
          pre = c("& \\makecell{Conditional\\\\logit} & \\multicolumn{2}{c}{Price fixed} & \\multicolumn{3}{c}{Price lognormal} & \\multicolumn{2}{c}{Price censored at zero} \\\\",
                  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-9}"))

# figures
ord <- rev(ids)
f4 <- do.call(rbind, lapply(names(specs), function(sp) {
  r <- mx[mx$model == specs[[sp]], ]; r <- r[match(ids, r$policy), ]
  rbind(data.frame(spec = sp, policy = ids, what = "Mean CV", value = r$cv_mean),
        data.frame(spec = sp, policy = ids, what = "Median CV", value = r$cv_median),
        data.frame(spec = sp, policy = ids, what = "Conditional logit CV", value = cl$cv_mean),
        data.frame(spec = sp, policy = ids, what = "Incremental cost", value = r$total_cost))
}))
f4$spec <- factor(spec_lab[f4$spec], levels = spec_lab)
f4$policy <- factor(f4$policy, levels = ord, labels = setNames(pol$label, pol$policy)[ord])
f4$what <- factor(f4$what, levels = c("Mean CV", "Median CV", "Conditional logit CV", "Incremental cost"))
gm <- ggplot(f4, aes(x = value, y = policy, colour = what, shape = what)) +
  geom_point(size = 2, stroke = 0.9) +
  facet_wrap(~spec, ncol = 1) +
  scale_x_log10(labels = label_dollar(), breaks = c(0.1, 1, 10, 100, 1000)) +
  scale_colour_manual(values = unname(okabe[c("blue", "blue", "orange", "vermillion")]), name = NULL) +
  scale_shape_manual(values = c(16, 1, 4, 124), name = NULL) +
  labs(x = "US$ per uncircumcised man (2017), log scale", y = NULL) +
  theme_paper + theme(strip.text = element_text(hjust = 0, face = "bold"), panel.grid.major.y = element_line(colour = "grey92"))
save_pdf(gm, "fig_mixl_cv.pdf", 6.4, 6.2)

f5 <- do.call(rbind, lapply(names(specs), function(sp) {
  r <- mx[mx$model == specs[[sp]], ]; r <- r[match(ids, r$policy), ]
  data.frame(spec = sp, policy = ids, share = r$share_cv_above_cost)
}))
f5$spec <- factor(spec_lab[f5$spec], levels = spec_lab)
f5$policy <- factor(f5$policy, levels = ord, labels = setNames(pol$label, pol$policy)[ord])
gs <- ggplot(f5, aes(x = share, y = policy, fill = spec)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_vline(xintercept = 0.5, colour = "grey40", linetype = "dashed", linewidth = 0.4) +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = unname(okabe[c("blue", "orange", "green")]), name = NULL, breaks = spec_lab) +
  labs(x = "Share of men whose CV exceeds the incremental cost per man", y = NULL) +
  theme_paper
save_pdf(gs, "fig_mixl_share.pdf", 6.4, 3.6)

cat("Paper tables, figures and numbers written.\n")
