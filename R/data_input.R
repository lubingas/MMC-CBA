# R/data_input.R
# ---------------------------
# Reads data/cba_inputs.xlsx into one list. Every model function takes this
# list (or pieces of it) as an argument; nothing is read from the global
# environment.
# ---------------------------

read_inputs <- function(path = "data/cba_inputs.xlsx") {
  rd <- function(sheet) as.data.frame(readxl::read_excel(path, sheet = sheet))
  settings <- rd("settings")
  s <- setNames(as.list(settings$value), settings$setting)
  for (nm in c("optout_share", "price_year", "population", "n_draws", "seed")) s[[nm]] <- as.numeric(s[[nm]])

  coefs <- rd("coefficients")
  models <- lapply(split(coefs, coefs$model), function(d) {
    v <- rd(paste0("cov_", d$model[1]))
    vc <- as.matrix(v[, -1]); dimnames(vc) <- list(v$parameter, v$parameter)
    list(beta = setNames(d$estimate, d$parameter), se = setNames(d$robust_se, d$parameter),
         vcov = vc[d$parameter, d$parameter], source = d$source[1])
  })

  options <- rd("options")
  policies <- rd("policies")
  policies$option_list <- lapply(strsplit(policies$options, ","), trimws)
  costs <- rd("costs")

  list(settings = s, models = models, options = options, policies = policies,
       costs = costs, cpi = rd("cpi"))
}
