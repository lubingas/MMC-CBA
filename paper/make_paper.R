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

# 2018 reproduction
mac("ReproOutreach", usd(repro$this_model[1])); mac("ReproDevice", usd(repro$this_model[2])); mac("ReproCash", usd(repro$this_model[3]))

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

# reproduction of the 2018 CL estimates of the CV
std3 <- RunUncalibrated(inputs, "cl_three_price_slopes", "pricenone")
unc1 <- RunUncalibrated(inputs, base_case$model, base_case$money_slope)
rp_ids <- c("outreach", "device", "cash")
rep_rows <- lapply(seq_along(rp_ids), function(i) {
  id <- rp_ids[i]
  c(esc(pol$label[pol$policy == id]), usd(repro$paper_2018[i]), usd(repro$this_model[i]),
    usd(std3$cv[1, id]), usd(unc1$cv[1, id]), usd(base_case$cv[1, id]))
})
write.tab("tab_repro.tex",
          c("Policy", "\\makecell{2018 working\\\\paper}", "\\makecell{(a) Options added\\\\one at a time}", "\\makecell{(b) Each policy vs\\\\initial state}",
            "\\makecell{(c) One price\\\\coefficient}", "\\makecell{(d) Calibrated\\\\(base case)}"),
          rep_rows, ">{\\raggedright\\arraybackslash}p{3.0cm}rrrrr")


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

cat("Paper tables, figures and numbers written.\n")
