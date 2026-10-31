# main_mixl.R
# ---------------------------
# The benefit-cost analysis with the uncorrelated mixed logits fitted by
# estimation/mmc_torch.R. Each man has his own utility parameters. The base
# case averages over the posterior distributions of the 406 men in the survey
# (R/mixl.R); the unconditional population distribution is a check.
# Run main.R's model first if you want the conditional logit alongside (it is
# rerun here). Outputs go to output/mixl_*.
# ---------------------------

rm(list = ls())

# load the packages and functions we need
source("R/00_packages.R")
source("R/data_input.R")
source("R/functions.R")
source("R/mixl.R")
dir.create("output", showWarnings = FALSE)

inputs <- read_inputs()
base_case <- RunCBA(inputs)                   # the conditional logit, for comparison

# which estimation run to use (estimation/output/<model>_R<draws>_seed<seed>_*)
EST_DRAWS <- 1000
EST_SEED  <- 1
SIM_MEN   <- 2000                             # simulated men in the benefit-cost analysis
KR_SIMS   <- 200                              # Krinsky-Robb draws (one price coefficient only)

mixl_models <- c("mixl_one_price_slope_fixed", "mixl_one_price_slope_lognormal", "mixl_one_price_slope_censored",
                 "mixl_three_price_slopes_fixed", "mixl_three_price_slopes_lognormal", "mixl_three_price_slopes_censored")
mixl_models <- mixl_models[file.exists(file.path("estimation", "output",
                 sprintf("%s_R%d_seed%d_estimates.csv", mixl_models, EST_DRAWS, EST_SEED)))]
if (!length(mixl_models)) stop("No mixed logit estimates found: run estimation/mmc_torch.R first")


## run model
# posterior (base case) and population (check) for every mixed logit
mixl_fits <- lapply(setNames(mixl_models, mixl_models), function(m) {
  mixl <- readMixl(m, EST_DRAWS, EST_SEED)
  list(posterior  = RunMixlCBA(inputs, mixl, R = SIM_MEN),
       population = RunMixlCBA(inputs, mixl, method = "population", R = SIM_MEN))
})


## Krinsky and Robb
# utility parameters only, the same simulated men in every draw
mixl_kr <- lapply(mixl_fits[grepl("one_price", names(mixl_fits))], function(f) {
  cat("Krinsky-Robb:", f$posterior$model, "\n")
  RunMixlKR(f$posterior, nsims = KR_SIMS)
})


## TABLES
# stack tables that do not all have the same columns
bindFill <- function(l) {
  cols <- unique(unlist(lapply(l, names)))
  do.call(rbind, lapply(l, function(d) { d[setdiff(cols, names(d))] <- NA; d[cols] }))
}
mixl_res <- bindFill(lapply(names(mixl_fits), function(m) {
  t <- TabulateMixl(mixl_fits[[m]]$posterior, mixl_kr[[m]])
  t$population_cv_mean <- mixl_fits[[m]]$population$summary$cv_mean
  t$a_hat <- mixl_fits[[m]]$posterior$a_hat
  t
}))
# the conditional logit in the same layout
cl_res <- data.frame(model = base_case$model, method = "conditional logit",
                     label = inputs$policies$label[-1], policy = inputs$policies$policy[-1],
                     uptake = base_case$uptake[1, -1], uptake_change = base_case$uptake_change[1, -1],
                     cv_mean = base_case$cv[1, -1], cv_median = base_case$cv[1, -1],
                     total_cost = base_case$total_cost[1, -1],
                     nb_mean = base_case$nb_cv[1, -1], nb_median = base_case$nb_cv[1, -1],
                     tx_savings = base_case$tx_savings[1, -1],
                     nb_total_mean = base_case$nb_total[1, -1], nb_total_median = base_case$nb_total[1, -1],
                     row.names = NULL)
mixl_table <- bindFill(list(cl_res, mixl_res))
write.csv(mixl_table, "output/mixl_results.csv", row.names = FALSE)

show <- function(t, digits = 2) print(format(t, digits = digits, nsmall = digits), row.names = FALSE)
cat("\nPer uncircumcised man (2017 US$); mixed logits averaged over the posterior distributions\n")
show(mixl_table[, c("model", "policy", "uptake", "cv_mean", "cv_median", "total_cost",
                    "nb_mean", "nb_median", "share_cv_above_cost", "nb_total_mean")])

# each survey man's expected CV (posterior), one column per policy
for (m in names(mixl_fits))
  write.csv(mixl_fits[[m]]$posterior$cv_by_man, sprintf("output/mixl_cv_by_man_%s.csv", sub("^mixl_", "", m)),
            row.names = FALSE)


## FIGURES
okabe <- c(blue = "#0072B2", vermillion = "#D55E00", green = "#009E73", orange = "#E69F00", grey = "#999999")
lab <- setNames(inputs$policies$label, inputs$policies$policy)
ord <- rev(inputs$policies$policy[-1])
spec_lab <- c(mixl_one_price_slope_fixed = "Price fixed", mixl_one_price_slope_lognormal = "Price lognormal",
              mixl_one_price_slope_censored = "Price censored at zero")
theme_cba <- theme_minimal(base_size = 11) +
  theme(legend.position = "top", legend.justification = "left", panel.grid.minor = element_blank(),
        plot.title.position = "plot", strip.text = element_text(hjust = 0, face = "bold"))

# figure 4: mean and median CV under each price distribution, with the conditional
# logit's CV and the cost per man (one price coefficient; log scale)
one <- mixl_table[mixl_table$model %in% names(spec_lab), ]
fig4 <- rbind(
  data.frame(spec = one$model, policy = one$policy, what = "Mean CV", value = one$cv_mean),
  data.frame(spec = one$model, policy = one$policy, what = "Median CV", value = one$cv_median),
  data.frame(spec = one$model, policy = one$policy, what = "Cost per man", value = one$total_cost),
  do.call(rbind, lapply(names(spec_lab), function(s)
    data.frame(spec = s, policy = cl_res$policy, what = "Conditional logit CV", value = cl_res$cv_mean))))
fig4$spec <- factor(spec_lab[fig4$spec], levels = spec_lab)
fig4$policy <- factor(fig4$policy, levels = ord, labels = lab[ord])
fig4$what <- factor(fig4$what, levels = c("Mean CV", "Median CV", "Conditional logit CV", "Cost per man"))
g4 <- ggplot(fig4, aes(x = value, y = policy, colour = what, shape = what)) +
  geom_point(size = 2.6, stroke = 1) +
  facet_wrap(~spec, ncol = 1) +
  scale_x_log10(labels = label_dollar(), breaks = c(0.1, 1, 10, 100, 1000)) +
  scale_colour_manual(values = unname(okabe[c("blue", "blue", "grey", "vermillion")]), name = NULL) +
  scale_shape_manual(values = c(16, 1, 4, 124), name = NULL) +
  labs(x = "US$ per uncircumcised man (2017), log scale", y = NULL,
       title = "Mixed logit: the mean CV depends on the price distribution, the median much less",
       subtitle = "One price coefficient; men averaged over their posterior distributions") +
  theme_cba
ggsave("output/fig4_mixl_cv_mean_median.png", g4, width = 8, height = 7.5, dpi = 200, bg = "white")

# figure 5: share of men whose CV exceeds the cost per man
fig5 <- one[, c("model", "policy", "share_cv_above_cost")]
fig5$spec <- factor(spec_lab[fig5$model], levels = spec_lab)
fig5$policy <- factor(fig5$policy, levels = ord, labels = lab[ord])
g5 <- ggplot(fig5, aes(x = share_cv_above_cost, y = policy, fill = spec)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_vline(xintercept = 0.5, colour = "grey40", linetype = "dashed", linewidth = 0.4) +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = unname(okabe[c("blue", "orange", "green")]), name = NULL) +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(x = "Share of men whose CV exceeds the cost per man", y = NULL,
       title = "Who gains: for every policy, fewer than half of men value it at more than its cost per man",
       subtitle = "Men with a finite CV; one price coefficient; posterior distributions") +
  theme_cba + theme(panel.grid.major.y = element_blank())
ggsave("output/fig5_mixl_share_gaining.png", g5, width = 8, height = 4.6, dpi = 200, bg = "white")
