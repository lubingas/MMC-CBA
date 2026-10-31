# R/mixl.R
# ---------------------------
# Benefit-cost analysis with a mixed logit (estimation/mmc_torch.R). Each man
# has his own utility parameters, so uptake, CV and costs are averaged over men.
#
# A population of simulated men (draws beta_r, r = 1..R) is shared by two ways
# of averaging:
#   posterior   each of the 406 men in the survey gets his own distribution:
#               the draws reweighted by how likely they make his 12 choices
#               (Train 2009, ch. 11). His CV is the average of CV(beta_r) over
#               his weighted draws, E[ CV | his choices ]; the population
#               figures average over the 406 men. This is the base case.
#   population  every draw counts equally: the unconditional distribution. If
#               the model is right the two agree, so this is a check.
# The CV at each man's posterior-mean coefficients (the plug-in used in the
# 2018 working paper) is also reported.
#
# With a lognormal or censored price coefficient, men who are almost
# insensitive to price have very large CVs, so the mean and the median CV are
# both reported. Men with a price coefficient of exactly zero (censored) have
# no finite CV; they are left out of the CV summaries and their share is reported.
# ---------------------------


### make inputs

# estimates of a mixed logit fitted by estimation/mmc_torch.R
readMixl <- function(model, draws = 1000, seed = 1, dir = file.path("estimation", "output")) {
  tag  <- file.path(dir, sprintf("%s_R%d_seed%d", model, draws, seed))
  est  <- read.csv(paste0(tag, "_estimates.csv"), row.names = 1)
  vc   <- as.matrix(read.csv(paste0(tag, "_robcovar.csv"), row.names = 1, check.names = FALSE))
  dist <- read.csv(paste0(tag, "_distribution.csv"))
  par  <- setNames(est$est, rownames(est))
  list(model = model, draws = draws, seed = seed, par = par, vcov = vc[names(par), names(par)],
       vars = dist$coefficient, dist = setNames(dist$distribution, dist$coefficient))
}

# standard normal draws for the simulated men (modified Latin hypercube):
# one row per man, one column per coefficient
mlhs <- function(R, K, seed) {
  set.seed(seed)
  sapply(seq_len(K), function(k) qnorm((sample(0:(R - 1)) + runif(1)) / R))
}

# coefficients of the simulated men: mean + SD * draw, then -exp() for
# lognormal and min(0, .) for censored coefficients
mixlBetas <- function(mixl, eta, par = mixl$par) {
  vars <- mixl$vars
  sd <- setNames(numeric(length(vars)), vars)
  rand <- vars[mixl$dist != "fixed"]
  sd[rand] <- par[paste0("sd_", rand)]
  b <- sweep(sweep(eta, 2, sd, "*"), 2, par[vars], "+")
  ln <- mixl$dist == "lognormal"; cs <- mixl$dist == "censored"
  b[, ln] <- -exp(b[, ln])
  b[, cs] <- pmin(0, b[, cs])
  colnames(b) <- vars
  b
}

# the choices in the survey, coded as in the model: attributes of service
# options 1 and 2 in each man's tasks (the opt-out has only its constant)
loadChoices <- function(vars, dir = file.path("data", "dce")) {
  ch  <- read.csv(file.path(dir, "choices.csv"))
  des <- read.csv(file.path(dir, "design.csv"))
  d <- merge(ch, des, by = c("block", "task"))
  d <- d[order(d$id, d$task), ]
  code <- function(j) {
    g <- function(a) d[[paste0(a, "_", j)]]
    sapply(vars, function(v) switch(v,
      neither      = rep(0, nrow(d)),
      pricenone    = g("price") * (1 - g("voucher") - g("cash")),
      pricevoucher = g("price") * g("voucher"),
      pricecash    = g("price") * g("cash"),
      g(v)))
  }
  list(X1 = code(1), X2 = code(2), choice = d$choice, id = d$id)
}

# posterior weights: for each man (rows) and simulated man (columns), the
# likelihood of his 12 choices, normalised over the draws
posteriorWeights <- function(betas, cd) {
  V1 <- cd$X1 %*% t(betas)                       # [task, draw]
  V2 <- cd$X2 %*% t(betas)
  V3 <- matrix(betas[, "neither"], nrow(V1), ncol(V1), byrow = TRUE)
  m <- pmax(V1, V2, V3)
  logp <- V1 * (cd$choice == 1) + V2 * (cd$choice == 2) + V3 * (cd$choice == 3) -
    (m + log(exp(V1 - m) + exp(V2 - m) + exp(V3 - m)))
  ll <- rowsum(logp, cd$id)                      # [man, draw]: log-likelihood of his choices
  w <- exp(ll - apply(ll, 1, max))
  w / rowSums(w)
}

# weighted median
wMedian <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o] / sum(w)
  x[which(cumsum(w) >= 0.5)[1]]
}


## function to return uptake, CV, costs and net benefit of every policy with a
## mixed logit (the counterpart of policyTrace)
## INSTRUCTIONS
# 1) betas: coefficients of the simulated men, one row per man (mixlBetas)
# 2) W: posterior weights [survey man, simulated man], or NULL for the population
# 3) inputs, money_slope, costs, S: as in policyTrace
# all results are per uncircumcised man, compared with the initial policy
mixlTrace <- function(betas, W, inputs, money_slope, costs, S) {
  R <- nrow(betas)
  omega <- if (is.null(W)) rep(1 / R, R) else colMeans(W)   # weight of each simulated man
  policymat <- makePolicyMat(inputs$options, colnames(betas))
  policies <- setNames(inputs$policies$option_list, inputs$policies$policy)
  V <- betas %*% policymat                                   # [simulated man, option]

  # calibration: one shift of the opt-out utility, so that the average share
  # who stay uncircumcised under the initial policy is S
  V0 <- V[, policies[[1]], drop = FALSE]
  optout0 <- function(a) { v <- V0; v[, "opt_out"] <- v[, "opt_out"] + a; sum(omega * exp(v[, "opt_out"] - rowLogSum(v))) }
  a_hat <- uniroot(function(a) optout0(a) - S, c(-10, 10), extendInt = "yes", tol = 1e-12)$root
  V[, "opt_out"] <- V[, "opt_out"] + a_hat

  lambda <- -betas[, money_slope]                # marginal utility of income of each simulated man
  ok <- lambda > 0                               # a zero price coefficient has no finite CV
  costs <- asCostMatrix(costs)
  optcosts <- getOptionCosts(inputs$options, costs)
  txsave <- tx.savings(costs)

  # log-sums and choice probabilities of every simulated man, for each policy
  res <- lapply(policies, function(pol) {
    v <- V[, pol, drop = FALSE]; ls <- rowLogSum(v)
    list(logsum = ls, P = exp(v - ls))
  })
  cost <- function(P, pol) {                     # provider cost and transfers, averaged over men
    sh <- colSums(omega * P)
    c(provider = sum(sh * optcosts$provider[1, pol]), transfer = sum(sh * optcosts$transfer[pol]))
  }
  init <- res[[1]]; c0 <- cost(init$P, policies[[1]])
  up0 <- 1 - sum(omega * init$P[, "opt_out"])

  # posterior-mean coefficients of each survey man (the 2018 plug-in)
  if (!is.null(W)) {
    bbar <- W %*% betas
    Vb <- bbar %*% policymat; Vb[, "opt_out"] <- Vb[, "opt_out"] + a_hat
    lsb <- lapply(policies, function(pol) rowLogSum(Vb[, pol, drop = FALSE]))
    lambda_bar <- -bbar[, money_slope]
  }

  cv_by_man <- NULL
  rows <- lapply(names(policies)[-1], function(p) {
    pol <- policies[[p]]; x <- res[[p]]
    cv_r <- (x$logsum - init$logsum) / lambda    # CV of each simulated man
    uptake <- 1 - sum(omega * x$P[, "opt_out"])
    cc <- cost(x$P, pol) - c0
    total <- sum(cc)
    # the CV distribution over men (posterior: the 406 posteriors pooled), leaving
    # out men with no finite CV: mean, median and the share above the cost per man
    w <- omega[ok]
    cv_mean <- sum(w * cv_r[ok]) / sum(w)
    cv_median <- wMedian(cv_r[ok], w)
    gain <- sum(w * (cv_r[ok] > total)) / sum(w)
    man <- NULL; plug_mean <- plug_median <- NA_real_
    if (!is.null(W)) {
      # each survey man's expected CV over his posterior, and the CV at his
      # posterior-mean coefficients (the 2018 plug-in)
      man <- drop(W[, ok, drop = FALSE] %*% cv_r[ok]) / rowSums(W[, ok, drop = FALSE])
      plug <- (lsb[[p]] - lsb[[1]]) / lambda_bar
      plug_mean <- mean(plug); plug_median <- median(plug)
    }
    list(man = man, row = data.frame(
      policy = p, uptake = uptake, uptake_change = uptake - up0,
      cv_mean = cv_mean, cv_median = cv_median, cv_plugin_mean = plug_mean, cv_plugin_median = plug_median,
      share_price_zero = sum(omega[!ok]),
      provider_cost = cc[["provider"]], transfers = cc[["transfer"]], total_cost = total,
      nb_mean = cv_mean - total, nb_median = cv_median - total,
      tx_savings = (uptake - up0) * txsave,
      share_cv_above_cost = gain))
  })
  summary <- do.call(rbind, lapply(rows, `[[`, "row"))
  summary$nb_total_mean   <- summary$nb_mean + summary$tx_savings
  summary$nb_total_median <- summary$nb_median + summary$tx_savings
  if (!is.null(W)) cv_by_man <- sapply(rows, `[[`, "man")
  if (!is.null(cv_by_man)) colnames(cv_by_man) <- names(policies)[-1]
  list(summary = summary, cv_by_man = cv_by_man, a_hat = a_hat, omega = omega,
       shares = lapply(res, function(x) colSums(omega * x$P)))
}


## function to run the base case with a mixed logit
## INSTRUCTIONS
# 1) inputs: the list from read_inputs()
# 2) mixl: estimates from readMixl()
# 3) money_slope: the price coefficient used as the marginal utility of income
#    (default: price, or the cash price coefficient with three price coefficients)
# 4) method: "posterior" (base case) or "population"
# 5) R: simulated men; eta and cd can be passed in to reuse draws and data
RunMixlCBA <- function(inputs, mixl,
                       money_slope = if ("price" %in% mixl$vars) "price" else "pricecash",
                       method = c("posterior", "population"),
                       S = inputs$settings$optout_share,
                       costs = NULL, R = 2000, seed = inputs$settings$seed,
                       par = mixl$par, eta = NULL, cd = NULL) {
  method <- match.arg(method)
  if (is.null(costs)) costs <- getCosts(inputs$costs, inputs$cpi, inputs$settings$price_year)
  if (is.null(eta)) eta <- mlhs(R, length(mixl$vars), seed)
  if (is.null(cd) && method == "posterior") cd <- loadChoices(mixl$vars)
  betas <- mixlBetas(mixl, eta, par)
  W <- if (method == "posterior") posteriorWeights(betas, cd) else NULL
  res <- mixlTrace(betas, W, inputs, money_slope, costs, S)
  res$summary <- cbind(model = mixl$model, method = method,
                       label = inputs$policies$label[match(res$summary$policy, inputs$policies$policy)],
                       res$summary)
  res$model <- mixl$model; res$method <- method; res$money_slope <- money_slope
  res$mixl <- mixl; res$eta <- eta; res$cd <- cd; res$costs <- costs; res$S <- S; res$inputs <- inputs
  res
}

## Krinsky and Robb: the means and SDs drawn from their sampling distribution
## (robust covariance), the same simulated men in every draw
RunMixlKR <- function(fit, nsims = 200, seed = fit$inputs$settings$seed) {
  set.seed(seed)
  pars <- MASS::mvrnorm(nsims, fit$mixl$par, fit$mixl$vcov)
  sims <- lapply(seq_len(nsims), function(i)
    RunMixlCBA(fit$inputs, fit$mixl, fit$money_slope, fit$method, fit$S, fit$costs,
               par = pars[i, ], eta = fit$eta, cd = fit$cd)$summary)
  measures <- c("uptake", "uptake_change", "cv_mean", "cv_median", "total_cost",
                "nb_mean", "nb_median", "nb_total_mean", "nb_total_median", "share_cv_above_cost")
  lapply(setNames(measures, measures), function(v) t(sapply(sims, `[[`, v)))
}

# base case with 95% Krinsky-Robb intervals
TabulateMixl <- function(fit, kr = NULL) {
  s <- fit$summary
  if (!is.null(kr)) for (v in names(kr)) {
    q <- apply(kr[[v]], 2, quantile, c(0.025, 0.975), na.rm = TRUE)
    s[[paste0(v, "_lo")]] <- q[1, ]; s[[paste0(v, "_hi")]] <- q[2, ]
  }
  s
}
