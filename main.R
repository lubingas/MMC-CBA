# main.R
# ---------------------------
# Benefit-cost analysis of five MMC demand creation policies in Ntenjeru,
# Mukono (Uganda). Benefits are the compensating variation (CV) from the 2017
# discrete choice experiment; costs are provider costs and incentive payments.
# Outputs (tables and figures) go to output/.
# ---------------------------

rm(list = ls())

# load the packages and functions we need
source("R/00_packages.R")
source("R/data_input.R")
source("R/functions.R")
dir.create("output", showWarnings = FALSE)

# read all inputs (settings, service options, policies, costs, utility estimates)
inputs <- read_inputs()
pop <- inputs$settings$population      # results are also reported per this many men


## run model
base_case <- RunCBA(inputs)
cat(sprintf("Model: %s; price coefficient: %s (marginal utility of income = %.4f per US$); opt-out calibrated to %.1f%%\n",
            base_case$model, base_case$money_slope, base_case$lambda, 100 * base_case$S))
cat(sprintf("Averted HIV treatment costs per extra circumcision: US$%.0f\n", base_case$tx_per_mmc))

# uncertainty in the utility parameters only (Krinsky and Robb), for the intervals in the results tables
kr_res <- RunKR(base_case)
cat(sprintf("Krinsky-Robb: %d draws, %.1f%% dropped because the price coefficient had the wrong sign\n",
            kr_res$n, 100 * mean(!kr_res$valid)))

results_man  <- TabulateResults(base_case, kr_res)
results_1000 <- TabulateResults(base_case, kr_res, per = pop)
write.csv(results_man, "output/results_per_man.csv", row.names = FALSE)
write.csv(results_1000, "output/results_per_1000.csv", row.names = FALSE)

show <- function(t, digits = 2) print(format(t, digits = digits, nsmall = digits), row.names = FALSE)
cat("\nPer uncircumcised man (2017 US$)\n")
show(results_man[, c("label", "uptake", "uptake_change", "cv", "cv_lo", "cv_hi", "total_cost", "nb_cv", "tx_savings", "nb_total")])

# the same analysis with three price coefficients (Table 2 of the MDM submission),
# using the cash price coefficient as the marginal utility of income
three_slopes <- RunCBA(inputs, "cl_three_price_slopes", "pricecash")
kr_three <- RunKR(three_slopes)
cat(sprintf("Three price slopes: %.1f%% of draws dropped\n", 100 * mean(!kr_three$valid)))
results_three <- TabulateResults(three_slopes, kr_three)
write.csv(results_three, "output/results_per_man_three_price_slopes.csv", row.names = FALSE)


## PSA
# utility parameters and cost inputs drawn together
nsims <- inputs$settings$n_draws
psa_input <- genPSA(base_case, nsims)
psa_res <- RunPSA(base_case, psa_input)

psa_summary <- data.frame(
  policy = inputs$policies$policy,
  label = inputs$policies$label,
  rbind(NA, setNames(summarisePSA(psa_res, "nb_cv")[, -1], paste0("nb_cv_", c("mean", "median", "lo", "hi", "p_positive")))),
  rbind(NA, setNames(summarisePSA(psa_res, "nb_total")[, -1], paste0("nb_total_", c("mean", "median", "lo", "hi", "p_positive")))),
  p_best_nb_cv = probBest(psa_res, "nb_cv"),
  p_best_nb_total = probBest(psa_res, "nb_total"))
write.csv(psa_summary, "output/psa_summary.csv", row.names = FALSE)
cat("\nProbabilistic analysis (utility parameters and costs), per man\n")
show(psa_summary[, c("label", "nb_cv_mean", "nb_cv_lo", "nb_cv_hi", "nb_cv_p_positive", "nb_total_mean", "nb_total_lo", "nb_total_hi", "p_best_nb_cv", "p_best_nb_total")])


## BUDGET PERSPECTIVE
# what the health budget pays now and saves later, per uncircumcised man, and how
# often the ART costs averted exceed the cost when costs and preferences vary
pol <- inputs$policies$policy[-1]
budget <- data.frame(policy = pol, label = inputs$policies$label[-1],
                     budgetView(base_case$total_cost[1, pol], base_case$transfers[1, pol],
                                base_case$uptake_change[1, pol], base_case$costs))
ok <- psa_res$valid
ret <- psa_res$tx_savings[ok, pol, drop = FALSE] / psa_res$total_cost[ok, pol, drop = FALSE]
budget$p_saving  <- colMeans(ret > 1)
budget$return_lo <- apply(ret, 2, quantile, 0.025)
budget$return_hi <- apply(ret, 2, quantile, 0.975)
write.csv(budget, "output/budget.csv", row.names = FALSE)
cat("\nBudget perspective, per uncircumcised man\n")
show(budget[, c("label", "total_cost", "cost_per_extra_mmc", "return_per_dollar", "return_lo", "return_hi",
                "payback_years", "irr", "coverage_needed", "p_saving")])

## ONE WAY SENSITIVITY ANALYSES
# cost inputs only, every policy
dsa_costs <- owsaCosts(base_case, "nb_total")
write.csv(dsa_costs, "output/owsa_costs_nb_total.csv", row.names = FALSE)
dsa_costs_cv <- owsaCosts(base_case, "nb_cv")
write.csv(dsa_costs_cv, "output/owsa_costs_nb_cv.csv", row.names = FALSE)

# every input (costs, utility parameters, calibration share), for each policy
dsa_res <- do.call(rbind, lapply(inputs$policies$policy[-1], function(p)
  rbind(cbind(policy = p, outcome = "nb_cv", owsa(base_case, p, "nb_cv")),
        cbind(policy = p, outcome = "nb_total", owsa(base_case, p, "nb_total")))))
write.csv(dsa_res, "output/owsa_all_inputs.csv", row.names = FALSE)


## SCENARIOS
scen_res <- RunScenarios(inputs)
write.csv(scen_res, "output/scenarios.csv", row.names = FALSE)


## reproduce the 2018 working paper
# the 2018 CVs (CL, w.r.t. out-of-pocket price, no calibration) added the
# options one at a time to a growing choice set: outreach to {opt-out, surgery};
# the device to {.., outreach}; both cash options to {.., device}
m3 <- inputs$models$cl_three_price_slopes
policymat <- makePolicyMat(inputs$options, names(m3$beta))
logsum <- function(policy) log(sum(exp(drop(m3$beta %*% policymat[, policy]))))
nested <- list(c("opt_out", "surgery_hospital"),
               c("opt_out", "surgery_hospital", "surgery_outreach"),
               c("opt_out", "surgery_hospital", "surgery_outreach", "device_hospital"),
               c("opt_out", "surgery_hospital", "surgery_outreach", "device_hospital",
                 "device_hospital_cash", "surgery_hospital_cash"))
repro <- data.frame(policy = c("Outreach services", "Device circumcision", "Cash incentives"),
                    paper_2018 = c(27.25, 14.77, 44.64),
                    this_model = round(diff(sapply(nested, logsum)) / -m3$beta[["pricenone"]], 2))
cat("\nReproducing the 2018 working paper (CV w.r.t. out-of-pocket price)\n"); print(repro, row.names = FALSE)


## FIGURES
okabe <- c(blue = "#0072B2", vermillion = "#D55E00", green = "#009E73")
lab <- setNames(results_man$label, results_man$policy)
ord <- rev(results_man$policy)
theme_cba <- theme_minimal(base_size = 11) +
  theme(legend.position = "top", legend.justification = "left", panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), plot.title.position = "plot")

# figure 1: value to men (CV) against the extra cost, per man
fig1 <- rbind(
  data.frame(policy = results_man$policy, what = "Value to men (CV)", value = results_man$cv,
             lo = results_man$cv_lo, hi = results_man$cv_hi),
  data.frame(policy = results_man$policy, what = "Extra cost (services and incentives)", value = results_man$total_cost,
             lo = NA, hi = NA))
fig1$policy <- factor(fig1$policy, levels = ord, labels = lab[ord])
fig1$what <- factor(fig1$what, levels = c("Value to men (CV)", "Extra cost (services and incentives)"))
g1 <- ggplot(fig1, aes(y = policy, x = value, fill = what)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68, colour = "white", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.75), height = 0.2,
                 colour = "grey25", linewidth = 0.4, na.rm = TRUE) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = unname(okabe[c("blue", "vermillion")]), name = NULL) +
  scale_x_continuous(labels = label_dollar(), expand = expansion(mult = c(0.02, 0.06))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "US$ per uncircumcised man (2017)", y = NULL,
       title = "Compensating variation and extra cost of each policy",
       subtitle = "Compared with the initial state. Whiskers: 95% Krinsky-Robb intervals for the CV") +
  theme_cba
ggsave("output/fig1_benefit_cost_per_man.png", g1, width = 7.5, height = 4.2, dpi = 200, bg = "white")

# figure 2: net benefit per 1,000 men, with and without averted treatment costs
fig2 <- rbind(
  data.frame(policy = results_1000$policy, what = "CV minus cost", value = results_1000$nb_cv,
             lo = results_1000$nb_cv_lo, hi = results_1000$nb_cv_hi),
  data.frame(policy = results_1000$policy, what = "Plus averted HIV treatment costs", value = results_1000$nb_total,
             lo = results_1000$nb_total_lo, hi = results_1000$nb_total_hi))
fig2$policy <- factor(fig2$policy, levels = ord, labels = lab[ord])
fig2$what <- factor(fig2$what, levels = c("CV minus cost", "Plus averted HIV treatment costs"))
g2 <- ggplot(fig2, aes(y = policy, x = value, colour = what)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.9) +
  geom_point(size = 2.8) +
  facet_wrap(~what, scales = "free_x") +
  scale_colour_manual(values = unname(okabe[c("blue", "green")]), guide = "none") +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "k"), expand = expansion(mult = c(0.06, 0.08))) +
  labs(x = sprintf("Net benefit per %s uncircumcised men, US$ (2017)", format(pop, big.mark = ",")), y = NULL,
       title = "Net benefit of each policy",
       subtitle = "Points: base case. Lines: 95% Krinsky-Robb intervals (utility parameters only). Scales differ") +
  theme_cba + theme(strip.text = element_text(hjust = 0, face = "bold"), panel.spacing.x = unit(1.4, "lines"))
ggsave("output/fig2_net_benefit_per_1000.png", g2, width = 8, height = 4.2, dpi = 200, bg = "white")

# figure 3: the CV under the two specifications of price
fig3 <- rbind(
  data.frame(policy = results_man$policy, model = "One price slope (base case)", value = results_man$cv,
             lo = results_man$cv_lo, hi = results_man$cv_hi),
  data.frame(policy = results_three$policy, model = "Three price slopes (cash slope)", value = results_three$cv,
             lo = results_three$cv_lo, hi = results_three$cv_hi))
fig3$policy <- factor(fig3$policy, levels = ord, labels = lab[ord])
fig3$model <- factor(fig3$model, levels = c("Three price slopes (cash slope)", "One price slope (base case)"))
g3 <- ggplot(fig3, aes(y = policy, x = value, colour = model)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55), height = 0, linewidth = 0.9) +
  geom_point(position = position_dodge(width = 0.55), size = 2.8) +
  scale_colour_manual(values = unname(okabe[c("vermillion", "blue")]), name = NULL) +
  scale_x_continuous(labels = label_dollar(), limits = c(0, NA), expand = expansion(mult = c(0, 0.04))) +
  guides(colour = guide_legend(reverse = TRUE)) +
  labs(x = "Compensating variation, US$ per uncircumcised man (2017)", y = NULL,
       title = "The specification of price decides how precise the CV is",
       subtitle = "Same point estimates; the single price coefficient is estimated far more precisely. Whiskers: 95% intervals") +
  theme_cba + theme(plot.margin = margin(5.5, 16, 5.5, 5.5))
ggsave("output/fig3_cv_by_price_specification.png", g3, width = 7.5, height = 4.2, dpi = 200, bg = "white")
