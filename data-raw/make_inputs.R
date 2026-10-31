# data-raw/make_inputs.R
# ---------------------------
# Builds data/cba_inputs.xlsx, the single input file the model reads.
# Run once to create the spreadsheet; after that, edit the spreadsheet.
#
# Preference estimates: the two conditional logits estimated in 2018 with the
# CMC choice modelling code (outputs copied unchanged into data-raw/estimates/):
#   cl_three_price_slopes  01_cl_3_price_coefs (1 May 2018), LL -3703.081, the
#                          model in Table 2 of the MDM submission
#   cl_one_price_slope     cl_no_corr (26 Jan 2018), LL -3703.301, the model
#                          behind the January 2018 CV workbook (cl_cv.xlsx)
# ---------------------------

library(openxlsx)
root <- if (file.exists("main.R")) "." else ".."
est_dir <- file.path(root, "data-raw", "estimates")

read_model <- function(est_file, cov_file, model, source) {
  e <- read.csv(file.path(est_dir, est_file), row.names = 1)
  v <- as.matrix(read.csv(file.path(est_dir, cov_file), row.names = 1))
  stopifnot(identical(rownames(e), rownames(v)), identical(rownames(v), colnames(v)))
  list(coef = data.frame(model = model, parameter = rownames(e), estimate = e$est,
                         robust_se = e$robse, source = source),
       cov = data.frame(parameter = rownames(v), v, check.names = FALSE))
}
three <- read_model("01_cl_3_price_coefs_estimates.csv", "01_cl_3_price_coefs_robcovar.csv",
                    "cl_three_price_slopes", "CMC 01_cl_3_price_coefs, 1 May 2018 (MDM submission, Table 2)")
one   <- read_model("cl_no_corr_estimates.csv", "cl_no_corr_robcovar.csv",
                    "cl_one_price_slope", "CMC cl_no_corr, 26 Jan 2018 (January 2018 CV workbook)")

# Service options, described by their attribute levels in the experiment.
# price: US$ the man pays (positive) or receives (negative: the incentive).
options <- data.frame(
  option     = c("opt_out", "surgery_hospital", "surgery_outreach", "device_hospital",
                 "device_hospital_cash", "surgery_hospital_cash"),
  label      = c("Opt-out (neither)", "Surgery (hospital)", "Outreach (surgery)", "Device (hospital)",
                 "Incentive (device, US$12 cash)", "Incentive (surgery, US$8 cash)"),
  facility   = c(NA, "permanent", "temporary", "permanent", "permanent", "permanent"),
  distance   = c(NA, 15, 1, 15, 15, 15),
  days       = c(NA, 2, 5, 2, 2, 2),
  privacy    = c(NA, "all ages", "own age", "all ages", "all ages", "all ages"),
  method     = c(NA, "surgery", "surgery", "device", "device", "surgery"),
  incentive  = c(NA, "none", "none", "none", "cash", "cash"),
  price      = c(NA, 0, 0, 0, -12, -8),
  cost_item  = c(NA, "cost_surgery_hospital", "cost_surgery_outreach", "cost_device_hospital",
                 "cost_device_hospital", "cost_surgery_hospital"))

policies <- data.frame(
  policy  = c("initial", "outreach", "device", "outreach_device", "cash", "cash_outreach"),
  label   = c("Initial state", "Policy 1: outreach services", "Policy 2: device circumcision",
              "Policy 3: outreach and device", "Policy 4: cash incentives", "Policy 5: cash incentives and outreach"),
  options = c("opt_out, surgery_hospital",
              "opt_out, surgery_hospital, surgery_outreach",
              "opt_out, surgery_hospital, device_hospital",
              "opt_out, surgery_hospital, surgery_outreach, device_hospital",
              "opt_out, device_hospital_cash, surgery_hospital_cash",
              "opt_out, surgery_outreach, device_hospital_cash, surgery_hospital_cash"))

costs <- data.frame(
  parameter = c("cost_surgery_hospital", "cost_surgery_outreach", "cost_device_hospital",
                "cost_incentive_payment", "mmc_per_infection_averted", "art_cost_per_year",
                "years_to_art", "years_on_art", "discount_rate"),
  label = c("Surgical circumcision at permanent health facilities, per procedure",
            "Surgical circumcision at outreach camps, per procedure",
            "Device circumcision at permanent health facilities, per procedure",
            "Cost of delivering an incentive payment",
            "Circumcisions per HIV infection averted",
            "Antiretroviral therapy (ART), cost per person per year",
            "Years from circumcision to start of ART (averted infection)",
            "Years on ART per averted infection",
            "Discount rate for averted treatment costs"),
  base  = c(34, 61, 41.90, 0, 12, 497, 5, 20, 0.03),
  low   = c(22.65, 45.75, 30.55, 0, 8, 350, 2, 10, 0.01),
  high  = c(42.50, 72, 45.90, 2, 20, 650, 10, 30, 0.05),
  price_year = c(2012, 2012, 2012, 2017, NA, 2013, NA, NA, NA),
  psa_dist = c("pert", "pert", "pert", "fixed", "pert", "pert", "pert", "pert", "fixed"),
  source = c(
    "Larson et al. 2015 (MUWRP fixed sites, central Uganda). Low: surgical cost in Duffy et al. 2013. High: +25%",
    "Larson et al. 2015 (MUWRP mobile clinic). High: more remote areas in the same study. Low: -25%",
    "Fixed-site surgery (Larson et al. 2015) plus the PrePex premium of $7.90 in Duffy et al. 2013. Low: Duffy's $30.55. High: 35% above surgery",
    "Assumption: administering cash at the facility",
    "Kripke et al. 2016, Uganda DMPPT 2.0, ages 20-34 (12-13), 2014-2028. Range: assumption",
    "Kripke et al. 2016 (Uganda HIV investment case, 2013). Range: assumption",
    "Assumption", "Assumption", "Kripke et al. 2016 used 3%"))

settings <- data.frame(
  setting = c("model", "money_slope", "optout_share", "price_year", "population", "n_draws", "seed"),
  value   = c("cl_one_price_slope", "price", "0.6923", "2017", "1000", "5000", "2026"),
  note    = c("cl_three_price_slopes or cl_one_price_slope",
              "The price coefficient used as the marginal utility of income: pricecash, pricevoucher or pricenone (three slopes); price (one slope)",
              "Share of men who remain uncircumcised in the initial state (surgical circumcision at permanent health facilities only): 1 - 2050/6663, as in the uptake projections",
              "Year of the dollars: the choice experiment was fielded in March-April 2017",
              "Results are also reported per this many uncircumcised men",
              "Krinsky-Robb draws of the preference parameters",
              "Random seed"))

# US CPI-U, annual averages (BLS series CUUR0000SA0)
cpi <- data.frame(year = 2012:2017, value = c(229.594, 232.957, 236.736, 237.017, 240.007, 245.120))

wb <- createWorkbook()
add <- function(name, df) { addWorksheet(wb, name); writeData(wb, name, df); setColWidths(wb, name, seq_along(df), "auto") }
add("settings", settings)
add("options", options)
add("policies", policies)
add("costs", costs)
add("coefficients", rbind(three$coef, one$coef))
add("cov_cl_three_price_slopes", three$cov)
add("cov_cl_one_price_slope", one$cov)
add("cpi", cpi)
saveWorkbook(wb, file.path(root, "data", "cba_inputs.xlsx"), overwrite = TRUE)
