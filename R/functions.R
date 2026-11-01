# R/functions.R
# ---------------------------
# Subroutines for the benefit-cost analysis of MMC demand creation policies.
# Benefits are the compensating variation (CV) from the choice experiment,
# computed with the log-sum formula; costs are the provider costs of the
# services men choose plus any incentive payments.
#
# Every function takes a matrix of utility parameters (betas: one row per
# draw, one column per parameter), so the base case (one row) and the
# simulations (many rows) run through the same code.
# ---------------------------


### helpers

# log(sum(exp(V))) for each row of V, without overflow
rowLogSum <- function(V) {
  m <- apply(V, 1, max)
  m + log(rowSums(exp(V - m)))
}

# adjust a dollar amount from one price year to another using the CPI table
cpi.adjust <- function(x, from, to, cpi) {
  if (is.na(from) || from == to) return(x)
  x * cpi$value[cpi$year == to] / cpi$value[cpi$year == from]
}


### make inputs

# pull out the cost inputs and convert them to the price year of the analysis
# value: "base", "low" or "high"
getCosts <- function(costs, cpi, price_year, value = "base") {
  out <- costs |> pull(all_of(value), parameter)
  for (i in seq_along(out)) out[i] <- cpi.adjust(out[i], costs$price_year[i], price_year, cpi)
  out
}

# random draws of the cost inputs for the PSA: beta-PERT on (low, base, high)
# where psa_dist is "pert", fixed at the base case otherwise.
# returns a matrix with one row per draw and one column per cost input
drawCosts <- function(costs, cpi, price_year, nsims) {
  base <- getCosts(costs, cpi, price_year, "base")
  low  <- getCosts(costs, cpi, price_year, "low")
  high <- getCosts(costs, cpi, price_year, "high")
  draws <- matrix(base, nsims, length(base), byrow = TRUE, dimnames = list(NULL, names(base)))
  pert <- which(costs$psa_dist == "pert" & high > low)
  for (i in pert) {
    a <- low[[i]]; b <- high[[i]]; m <- min(max(base[[i]], a), b)
    draws[, i] <- a + (b - a) * rbeta(nsims, 1 + 4 * (m - a) / (b - a), 1 + 4 * (b - m) / (b - a))
  }
  draws
}

# set up the policy matrix: one column per service option, one row per
# utility parameter, coded as in the estimation (reference levels are a
# temporary facility, 1 km, counselling alone, surgery and no incentive)
makePolicyMat <- function(options, params) {
  policymat <- vapply(seq_len(nrow(options)), function(i) {
    o <- options[i, ]
    if (o$option == "opt_out") return(as.numeric(params == "neither"))
    levels <- c(neither      = 0,
                permhc       = as.numeric(o$facility == "permanent"),
                dist5        = as.numeric(o$distance == 5),
                dist15       = as.numeric(o$distance == 15),
                avail        = as.numeric(o$days),
                somepriv     = as.numeric(o$privacy == "own age"),
                notpriv      = as.numeric(o$privacy == "all ages"),
                device       = as.numeric(o$method == "device"),
                voucher      = as.numeric(o$incentive == "voucher"),
                cash         = as.numeric(o$incentive == "cash"),
                price        = o$price,
                pricenone    = o$price * (o$incentive == "none"),
                pricevoucher = o$price * (o$incentive == "voucher"),
                pricecash    = o$price * (o$incentive == "cash"))
    missing <- setdiff(params, names(levels))
    if (length(missing)) stop("No attribute coding for parameter(s): ", paste(missing, collapse = ", "))
    unname(levels[params])
  }, numeric(length(params)))
  dimnames(policymat) <- list(params, options$option)
  policymat
}


### subroutines

# constant (a_hat) added to the utility of the opt-out so that, when only the
# initial policy is offered, the predicted opt-out share equals the observed
# share S. It has a closed form:
#   a_hat = logit(S) + log(sum over the other initial options of exp(V)) - V(opt-out)
getAhat <- function(betas, policymat, init_policy, S) {
  V <- betas %*% policymat[, init_policy, drop = FALSE]
  others <- setdiff(init_policy, "opt_out")
  qlogis(S) + rowLogSum(V[, others, drop = FALSE]) - V[, "opt_out"]
}

# predicted shares (S_hat) and log-sum for one policy (a set of options)
S_hat <- function(betas, policymat, policy, a_hat) {
  V <- betas %*% policymat[, policy, drop = FALSE]
  V[, "opt_out"] <- V[, "opt_out"] + a_hat
  logsum <- rowLogSum(V)
  list(logsum = logsum, shares = exp(V - logsum))
}

# provider cost per man who chooses each option (one row per draw) and the
# incentive paid to him (+) or by him (-) for each option
getOptionCosts <- function(options, costs) {
  transfer <- setNames(ifelse(is.na(options$price), 0, -options$price), options$option)
  unknown <- setdiff(na.omit(options$cost_item), colnames(costs))
  if (length(unknown)) stop("Unknown cost item(s): ", paste(unknown, collapse = ", "))
  provider <- vapply(seq_len(nrow(options)), function(i) {
    item <- options$cost_item[i]
    (if (is.na(item)) rep(0, nrow(costs)) else costs[, item]) +
      (if (transfer[[i]] > 0) costs[, "cost_incentive_payment"] else 0)
  }, numeric(nrow(costs)))
  provider <- matrix(provider, nrow = nrow(costs), dimnames = list(NULL, options$option))
  list(provider = provider, transfer = transfer)
}

# present value of the HIV treatment costs averted by one extra circumcision
#   (cost of ART per year / circumcisions per infection averted) x sum_{t = d}^{d + L - 1} (1 + r)^(-t)
# where treatment starts d years after the circumcision and lasts L years
tx.savings <- function(costs) {
  r <- costs[, "discount_rate"]; d <- costs[, "years_to_art"]; L <- costs[, "years_on_art"]
  annuity <- ifelse(r == 0, L, (1 + r)^(-d) * (1 - (1 + r)^(-L)) / pmax(r, 1e-12))
  unname(costs[, "art_cost_per_year"] * annuity / costs[, "mmc_per_infection_averted"])
}

# cost inputs as a matrix with one row per draw (a named vector becomes one
# row, repeated nsims times)
asCostMatrix <- function(costs, nsims = 1) {
  if (!is.matrix(costs)) costs <- matrix(costs, nrow = 1, dimnames = list(NULL, names(costs)))
  if (nrow(costs) == 1 && nsims > 1) costs <- costs[rep(1, nsims), , drop = FALSE]
  costs
}


## function to return uptake, CV, costs and net benefit of every policy
## (the equivalent of mtrace in the schizophrenia model)
## INSTRUCTIONS
# 1) betas: matrix of utility parameters, one row per draw
# 2) inputs: the list from read_inputs()
# 3) money_slope: the price coefficient used as the marginal utility of income
# 4) costs: cost inputs, a named vector or a matrix with one row per draw
# 5) S: observed share who stay uncircumcised under the initial policy
# all results are per uncircumcised man, compared with the initial policy
policyTrace <- function(betas, inputs, money_slope, costs, S) {
  policymat <- makePolicyMat(inputs$options, colnames(betas))
  policies <- setNames(inputs$policies$option_list, inputs$policies$policy)
  a_hat <- getAhat(betas, policymat, policies[[1]], S)
  lambda <- -betas[, money_slope]          # marginal utility of income (minus the price coefficient)
  costs <- asCostMatrix(costs, nrow(betas))
  optcosts <- getOptionCosts(inputs$options, costs)
  txsave <- tx.savings(costs)

  # shares, log-sums and costs for each policy
  res <- lapply(policies, function(policy) {
    s <- S_hat(betas, policymat, policy, a_hat)
    list(logsum   = s$logsum,
         uptake   = 1 - s$shares[, "opt_out"],
         provider = rowSums(s$shares * optcosts$provider[, policy, drop = FALSE]),
         transfer = drop(s$shares %*% optcosts$transfer[policy]),
         shares   = s$shares)
  })
  init <- res[[1]]
  collect <- function(f) matrix(sapply(res, f), nrow = nrow(betas), dimnames = list(NULL, names(policies)))

  out <- list(lambda = lambda, a_hat = a_hat,
              uptake        = collect(function(x) x$uptake),
              uptake_change = collect(function(x) x$uptake - init$uptake),
              # compensating variation: CV = (1/lambda) * [log-sum after - log-sum before]
              cv            = collect(function(x) (x$logsum - init$logsum) / lambda),
              provider_cost = collect(function(x) x$provider - init$provider),
              transfers     = collect(function(x) x$transfer - init$transfer))
  out$total_cost <- out$provider_cost + out$transfers
  out$nb_cv      <- out$cv - out$total_cost
  out$tx_savings <- out$uptake_change * txsave
  out$nb_total   <- out$nb_cv + out$tx_savings
  out$shares     <- lapply(res, function(x) x$shares)
  out$tx_per_mmc <- txsave
  out
}


## function to run the base case
## INSTRUCTIONS
# 1) inputs: the list from read_inputs()
# 2) model and money_slope: which conditional logit, and which of its price
#    coefficients is the marginal utility of income (default: the settings sheet)
# 3) S: observed share who stay uncircumcised under the initial policy
# 4) costs: cost inputs (default: base values in the price year of the analysis)
# 5) betas: named values that replace estimates (scenarios only)
RunCBA <- function(inputs,
                   model = inputs$settings$model,
                   money_slope = inputs$settings$money_slope,
                   S = inputs$settings$optout_share,
                   costs = NULL,
                   betas = NULL) {
  m <- inputs$models[[model]]
  if (!money_slope %in% names(m$beta)) stop(model, " has no parameter ", money_slope)
  if (is.null(costs)) costs <- getCosts(inputs$costs, inputs$cpi, inputs$settings$price_year)
  b <- m$beta
  if (!is.null(betas)) b[names(betas)] <- betas
  b <- matrix(b, nrow = 1, dimnames = list(NULL, names(b)))
  res <- policyTrace(b, inputs, money_slope, costs, S)
  res$model <- model; res$money_slope <- money_slope; res$costs <- costs; res$S <- S; res$inputs <- inputs
  res
}

# the same model with the estimated opt-out constant (no calibration), as in
# the 2018 working paper
RunUncalibrated <- function(inputs, model, money_slope) {
  m <- inputs$models[[model]]
  policymat <- makePolicyMat(inputs$options, names(m$beta))
  V <- drop(m$beta %*% policymat[, inputs$policies$option_list[[1]]])
  RunCBA(inputs, model, money_slope, S = exp(V[1]) / sum(exp(V)))   # opt-out share the estimates imply
}


### PSA

## function to generate the PSA draws
# betas: drawn from their sampling distribution (multivariate normal with the
#   robust covariance matrix), as in Krinsky and Robb
# costs: drawn from their distributions (drawCosts) if vary_costs = TRUE,
#   otherwise fixed at the base case
genPSA <- function(model, nsims = model$inputs$settings$n_draws, seed = model$inputs$settings$seed,
                   vary_costs = TRUE) {
  inp <- model$inputs
  m <- inp$models[[model$model]]
  set.seed(seed)
  nsims <- max(nsims, 2)
  betas <- MASS::mvrnorm(nsims, m$beta, m$vcov)
  costs <- if (vary_costs) drawCosts(inp$costs, inp$cpi, inp$settings$price_year, nsims) else model$costs
  list(betas = betas, costs = costs)
}

## function to run the model on the PSA draws
# draws in which the money coefficient has the wrong sign (lambda <= 0) give
# no meaningful CV; they are flagged (valid = FALSE) and left out of summaries
RunPSA <- function(model, psalist) {
  res <- policyTrace(psalist$betas, model$inputs, model$money_slope, psalist$costs, model$S)
  res$valid <- res$lambda > 0
  res$n <- nrow(psalist$betas)
  res$psalist <- psalist
  res
}

# Krinsky-Robb: preference uncertainty only (costs fixed at the base case)
RunKR <- function(model, nsims = model$inputs$settings$n_draws, seed = model$inputs$settings$seed) {
  RunPSA(model, genPSA(model, nsims, seed, vary_costs = FALSE))
}


### budget perspective
# What the health budget pays now and saves later. A policy costs dC per
# uncircumcised man now (provider costs and incentive payments) and raises
# uptake by dU, so it averts dU / k infections, each of which would have cost
# the budget c_ART a year for L years, starting d years from now.

# present value of the ART costs averted by one additional circumcision, at the
# discount rate r (the same formula as tx.savings, with r as an argument)
artSaved <- function(costs, r) {
  d <- costs[["years_to_art"]]; L <- costs[["years_on_art"]]
  annuity <- if (r == 0) L else (1 + r)^(-d) * (1 - (1 + r)^(-L)) / r
  costs[["art_cost_per_year"]] * annuity / costs[["mmc_per_infection_averted"]]
}

## function to return the budget view of every policy
## INSTRUCTIONS
# 1) total_cost, transfers, uptake_change: per uncircumcised man, one value per
#    policy (from RunCBA, or from a mixed logit)
# 2) costs: the cost inputs (a named vector, as in model$costs)
# returns, per policy: the cost per additional circumcision; the ART costs
# averted per dollar spent; the net budget saving; the undiscounted payback
# period (years after the circumcision); the internal rate of return, the
# discount rate at which the ART costs averted just repay the cost; and the
# share of averted infections that must otherwise have been treated for the
# policy to pay for itself
budgetView <- function(total_cost, transfers, uptake_change, costs) {
  if (is.matrix(costs)) costs <- costs[1, ]
  costs <- unlist(costs)
  r <- costs[["discount_rate"]]
  per_mmc <- total_cost / uptake_change                       # cost per additional circumcision
  b_tx <- artSaved(costs, r)
  per_year <- costs[["art_cost_per_year"]] / costs[["mmc_per_infection_averted"]]
  irr <- vapply(per_mmc, function(c) {
    f <- function(rho) artSaved(costs, rho) - c
    if (f(1e-9) <= 0) return(NA_real_)                         # never repaid, even undiscounted
    uniroot(f, c(1e-9, 50), tol = 1e-10)$root
  }, numeric(1))
  payback <- costs[["years_to_art"]] + per_mmc / per_year
  payback[per_mmc / per_year > costs[["years_on_art"]]] <- NA
  data.frame(total_cost = total_cost, transfers = transfers, uptake_change = uptake_change,
             cost_per_extra_mmc = per_mmc, art_saved = uptake_change * b_tx,
             return_per_dollar = b_tx / per_mmc, net_saving = uptake_change * b_tx - total_cost,
             payback_years = payback, irr = irr, coverage_needed = per_mmc / b_tx, row.names = NULL)
}

# the net budget saving per uncircumcised man when the ART costs averted are
# discounted at the budget's own rate rho instead of r
budgetSaving <- function(total_cost, uptake_change, costs, rho) {
  if (is.matrix(costs)) costs <- costs[1, ]
  costs <- unlist(costs)
  uptake_change * artSaved(costs, rho) - total_cost
}


### one way sensitivity analyses

# net benefit of each policy with one cost input at its low or high value
owsaCosts <- function(model, outcome = c("nb_cv", "nb_total")) {
  outcome <- match.arg(outcome)
  inp <- model$inputs
  usa.low  <- getCosts(inp$costs, inp$cpi, inp$settings$price_year, "low")
  usa.high <- getCosts(inp$costs, inp$cpi, inp$settings$price_year, "high")
  rows <- lapply(inp$costs$parameter, function(p) {
    run <- function(v) { costs <- model$costs; costs[p] <- v[p]
      RunCBA(inp, model$model, model$money_slope, model$S, costs = costs)[[outcome]][1, -1] }
    data.frame(parameter = p, label = inp$costs$label[inp$costs$parameter == p],
               policy = names(run(usa.low)), low = run(usa.low), high = run(usa.high),
               base = model[[outcome]][1, -1], row.names = NULL)
  })
  do.call(rbind, rows)
}

param_labels <- c(
  neither = "Opt-out constant", permhc = "Permanent health facility", dist5 = "5 km away (vs 1 km)",
  dist15 = "15 km away (vs 1 km)", avail = "Week-days of service availability",
  somepriv = "Some privacy (men of own age group)", notpriv = "No privacy (all age groups)",
  device = "Device (vs surgical) circumcision", voucher = "Voucher incentive", cash = "Cash incentive",
  price = "Price", pricenone = "Price, out-of-pocket payment", pricevoucher = "Price, voucher payment",
  pricecash = "Price, cash payment")

## function to loop through every input and run the model at its low and high values
## INSTRUCTIONS
# 1) model: a base case from RunCBA()
# 2) policy: the policy to analyse
# 3) outcome: "nb_total", "nb_cv" or "cv"
# 4) s0_range: low and high values of the calibration share
# cost inputs move to their low and high values, utility parameters to their
# 95% confidence limits; a money coefficient of zero or above leaves the CV
# undefined (NA). Returns a table sorted by the size of the effect.
owsa <- function(model, policy, outcome = c("nb_total", "nb_cv", "cv"), s0_range = c(0.6, 0.8)) {
  outcome <- match.arg(outcome)
  inp <- model$inputs
  m <- inp$models[[model$model]]
  run <- function(costs = model$costs, betas = NULL, S = model$S) {
    if (!is.null(betas) && model$money_slope %in% names(betas) && betas[[model$money_slope]] >= 0) return(NA_real_)
    RunCBA(inp, model$model, model$money_slope, S, costs = costs, betas = betas)[[outcome]][1, policy]
  }
  usa.low  <- getCosts(inp$costs, inp$cpi, inp$settings$price_year, "low")
  usa.high <- getCosts(inp$costs, inp$cpi, inp$settings$price_year, "high")

  # cost inputs
  cost_rows <- lapply(seq_len(nrow(inp$costs)), function(i) {
    p <- inp$costs$parameter[i]
    lo <- model$costs; lo[p] <- usa.low[p]
    hi <- model$costs; hi[p] <- usa.high[p]
    data.frame(group = "Costs", input = p, label = inp$costs$label[i],
               low_value = usa.low[[p]], high_value = usa.high[[p]], low = run(costs = lo), high = run(costs = hi))
  })
  # utility parameters
  pref_rows <- lapply(names(m$beta), function(p) {
    lo <- m$beta[[p]] - 1.96 * m$se[[p]]
    hi <- m$beta[[p]] + 1.96 * m$se[[p]]
    data.frame(group = "Preferences", input = p,
               label = if (p %in% names(param_labels)) param_labels[[p]] else p,
               low_value = lo, high_value = hi,
               low = run(betas = setNames(lo, p)), high = run(betas = setNames(hi, p)))
  })
  # calibration share
  cal <- data.frame(group = "Calibration", input = "optout_share",
                    label = "Share who stay uncircumcised, initial state",
                    low_value = s0_range[1], high_value = s0_range[2],
                    low = run(S = s0_range[1]), high = run(S = s0_range[2]))

  out <- do.call(rbind, c(cost_rows, pref_rows, list(cal)))
  out$base <- model[[outcome]][1, policy]
  out$swing <- pmax(abs(out$high - out$base), abs(out$low - out$base), na.rm = TRUE)
  out$swing[is.na(out$low) | is.na(out$high)] <- Inf
  out <- out[order(-out$swing), ]
  rownames(out) <- NULL
  out
}


### scenarios

# net benefit under alternative specifications of the choice model.
# calibration and incentive scenarios use the base-case model.
RunScenarios <- function(inputs) {
  bm <- inputs$settings$model; bs <- inputs$settings$money_slope; S <- inputs$settings$optout_share
  scen <- list(
    "Base case"                               = list(bm, bs, S),
    "Three price slopes, cash slope"          = list("cl_three_price_slopes", "pricecash", S),
    "Three price slopes, voucher slope"       = list("cl_three_price_slopes", "pricevoucher", S),
    "Three price slopes, out-of-pocket slope" = list("cl_three_price_slopes", "pricenone", S),
    "Opt-out not calibrated"                  = list(bm, bs, NA),
    "Opt-out share 60%"                       = list(bm, bs, 0.60),
    "Opt-out share 80%"                       = list(bm, bs, 0.80),
    "Incentives valued at face value only"    = list(bm, bs, S, c(cash = 0, voucher = 0)))
  do.call(rbind, lapply(names(scen), function(nm) {
    a <- scen[[nm]]
    r <- if (is.na(a[[3]])) RunUncalibrated(inputs, a[[1]], a[[2]])
         else RunCBA(inputs, a[[1]], a[[2]], a[[3]], betas = if (length(a) > 3) a[[4]])
    data.frame(scenario = nm, policy = colnames(r$cv)[-1], uptake_change = r$uptake_change[1, -1],
               cv = r$cv[1, -1], total_cost = r$total_cost[1, -1], nb_cv = r$nb_cv[1, -1],
               nb_total = r$nb_total[1, -1], row.names = NULL)
  }))
}


### tables

# mean, median, 95% interval and share above zero of a PSA outcome
summarisePSA <- function(psa, what) {
  M <- psa[[what]][psa$valid, -1, drop = FALSE]
  data.frame(policy = colnames(M), mean = colMeans(M), median = apply(M, 2, median),
             lo = apply(M, 2, quantile, 0.025), hi = apply(M, 2, quantile, 0.975),
             p_positive = colMeans(M > 0), row.names = NULL)
}

# share of PSA draws in which each policy has the largest net benefit (the
# initial policy counts as zero)
probBest <- function(psa, what) {
  M <- cbind(0, psa[[what]][psa$valid, -1, drop = FALSE])
  tabulate(max.col(M, ties.method = "first"), ncol(M)) / nrow(M)
}

## function to tabulate the base case, with PSA intervals if given
# per: report per man (1) or per so many men
TabulateResults <- function(model, psa = NULL, per = 1) {
  pol <- model$inputs$policies
  idx <- -1
  tab <- data.frame(
    policy             = pol$policy[idx],
    label              = pol$label[idx],
    uptake             = model$uptake[1, idx],
    uptake_change      = model$uptake_change[1, idx],
    extra_mmc          = model$uptake_change[1, idx] * per,
    cv                 = model$cv[1, idx] * per,
    provider_cost      = model$provider_cost[1, idx] * per,
    transfers          = model$transfers[1, idx] * per,
    total_cost         = model$total_cost[1, idx] * per,
    nb_cv              = model$nb_cv[1, idx] * per,
    tx_savings         = model$tx_savings[1, idx] * per,
    nb_total           = model$nb_total[1, idx] * per,
    cost_per_extra_mmc = ifelse(model$uptake_change[1, idx] > 0,
                                model$total_cost[1, idx] / model$uptake_change[1, idx], NA),
    row.names = NULL)
  if (!is.null(psa)) {
    for (w in c("cv", "nb_cv", "nb_total")) {
      s <- summarisePSA(psa, w)
      tab[[paste0(w, "_lo")]] <- s$lo * per
      tab[[paste0(w, "_hi")]] <- s$hi * per
      if (w != "cv") tab[[paste0("p_", w, "_positive")]] <- s$p_positive
    }
  }
  tab
}
