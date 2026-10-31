# =============================================================================
# estimation/mmc_torch.R
# Conditional logit and mixed logit for the MMC choice experiment, fitted with
# R torch.
#
# A basic version of the MMC-Demand torch engine with the same layout: choices
# as arrays [man, task, alternative, attribute], log-softmax over the
# alternatives, autograd gradients, BFGS through optim(), the exact Hessian, and
# robust standard errors clustered by man. Each model is a declaration in
# MODELS and a likelihood per man (ll_cl, ll_mixl), so latent class or
# correlated mixed logit models are another ll_*() function and declaration.
#
# Data (data/dce/), all that the models need:
#   choices.csv  one row per man and task: id (1-406), block, task, choice
#                (1 = service option 1, 2 = service option 2, 3 = neither)
#   design.csv   the attributes of options 1 and 2 in each block and task
#
# Models, each with one price coefficient (base case) or three (out-of-pocket,
# voucher and cash payments):
#   cl_*    conditional logit, as estimated with CMC in 2018 (checked against
#           data-raw/estimates/)
#   mixl_*  mixed logit with independent (uncorrelated) normal coefficients.
#           The price coefficients are
#             fixed      not random: every man has the same marginal utility of income
#             lognormal  -exp(normal): negative for every man
#             censored   min(0, normal): zero for men whose underlying value is
#                        positive. The kink is smoothed (softplus, CENS_TAU)
#   Draws: modified Latin hypercube, as make_draws() in MMC-Demand, so a seed
#   gives the same draws; draw column k belongs to coefficient k in MMC-Demand's
#   order.
#   Starts, as in MMC-Demand: the rule start (means from the conditional logit,
#   lognormal at log of minus the CL estimate, standard deviations 0.5); for a
#   three-price model, the fit of the matching one-price model (nested: with a
#   fixed price it reproduces that model's LL, so the three-price fit cannot end
#   below it); and starts - 1 random starts around the rule start, drawn as
#   random_start() in MMC-Demand. The best endpoint gets the post-estimation.
#
# Fits are saved in estimation/output/fits/ and reloaded when the
# specification, data, draws and seed match, so a rerun only fits what is new.
#
# Run from the project folder:
#   Rscript estimation/mmc_torch.R                     every model, 1000 draws, seed 1
#   Rscript estimation/mmc_torch.R draws=3000 seed=1   as in MMC-Demand's main runs
#   Rscript estimation/mmc_torch.R models=cl           the conditional logits only
#   Rscript estimation/mmc_torch.R models=mixl_one_price_slope_fixed
#   Rscript estimation/mmc_torch.R draws=3000 starts=10   rule, nested and 9 random starts
# Long runs, so they survive a closed terminal:
#   nohup caffeinate -i -s Rscript estimation/mmc_torch.R draws=3000 > estimation/output/run.log 2>&1 &
# =============================================================================

suppressPackageStartupMessages(library(torch))
f64 <- torch_float64()
`%||%` <- function(a, b) if (is.null(a)) b else a
out_dir <- file.path("estimation", "output")
fit_dir <- file.path(out_dir, "fits")
dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)
torch_set_num_threads(max(1, parallel::detectCores() - 1))


## settings
N_DRAWS <- 1000          # draws per man
SEED    <- 1             # draws seed
WHICH   <- "all"         # models to fit: all, cl, mixl, or model names
STARTS  <- 1             # starts per mixed logit: the rule start, plus STARTS - 1 random starts
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=")[[1]]
  if (kv[1] == "draws")  N_DRAWS <- as.integer(kv[2])
  if (kv[1] == "seed")   SEED    <- as.integer(kv[2])
  if (kv[1] == "models") WHICH   <- strsplit(kv[2], ",")[[1]]
  if (kv[1] == "starts") STARTS  <- as.integer(kv[2])
}
CENS_TAU <- 0.005        # softplus smoothing of the censored normal, min(0, x)
N_BIC    <- 4872         # choice tasks (BIC convention of the CMC outputs and MMC-Demand)

# attributes of the service options, as named in design.csv and by CMC
ATTR    <- c("permhc", "dist5", "dist15", "avail", "somepriv", "notpriv", "device", "voucher", "cash", "price")
PRICES3 <- c("pricenone", "pricevoucher", "pricecash")
VARS1   <- c("neither", ATTR)                                  # one price coefficient
VARS3   <- c("neither", setdiff(ATTR, "price"), PRICES3)       # three price coefficients

# a model: its coefficients, which are random, and which are lognormal or censored
spec <- function(vars, random = character(0), lognormal = character(0), censored = character(0))
  list(vars = vars, random = random, ln = vars %in% lognormal, cens = vars %in% censored,
       names = c(vars, if (length(random)) paste0("sd_", random)))

MODELS <- list(
  cl_one_price_slope    = spec(VARS1),
  cl_three_price_slopes = spec(VARS3),
  mixl_one_price_slope_fixed        = spec(VARS1, random = setdiff(VARS1, "price")),
  mixl_one_price_slope_lognormal    = spec(VARS1, random = VARS1, lognormal = "price"),
  mixl_one_price_slope_censored     = spec(VARS1, random = VARS1, censored = "price"),
  mixl_three_price_slopes_fixed     = spec(VARS3, random = setdiff(VARS3, PRICES3)),
  mixl_three_price_slopes_lognormal = spec(VARS3, random = VARS3, lognormal = PRICES3),
  mixl_three_price_slopes_censored  = spec(VARS3, random = VARS3, censored = PRICES3))

# CMC outputs to check the conditional logits against
CMC_FILES <- c(cl_one_price_slope = "cl_no_corr", cl_three_price_slopes = "01_cl_3_price_coefs")


## data
# choices and design -> attribute array A [man, task, alternative, attribute]
# and choice array Y [man, task, alternative] (1 for the chosen alternative)
load_choices <- function(dir = file.path("data", "dce")) {
  files <- file.path(dir, c("choices.csv", "design.csv"))
  ch  <- read.csv(files[1])
  des <- read.csv(files[2])
  d <- merge(ch, des, by = c("block", "task"))
  d <- d[order(d$id, d$task), ]
  ids <- unique(d$id); N <- length(ids); TT <- max(d$task); J <- 3
  A <- array(0, dim = c(N, TT, J, length(ATTR) + 1), dimnames = list(NULL, NULL, NULL, c("neither", ATTR)))
  i <- cbind(match(d$id, ids), d$task)
  for (a in ATTR) {
    A[cbind(i, 1, match(a, dimnames(A)[[4]]))] <- d[[paste0(a, "_1")]]
    A[cbind(i, 2, match(a, dimnames(A)[[4]]))] <- d[[paste0(a, "_2")]]
  }
  A[, , 3, "neither"] <- 1                       # the opt-out: its constant, every attribute 0
  Y <- array(0, dim = c(N, TT, J))
  Y[cbind(i, d$choice)] <- 1
  list(A = A, Y_arr = Y, Y = torch_tensor(Y, dtype = f64), N = N, TT = TT, J = J, n_obs = nrow(d),
       md5 = paste(unname(tools::md5sum(files)), collapse = "|"))
}

# the coded attributes [man, task, alternative, coefficient] of a model (R array).
# The price interactions split price by the type of payment (incentive = none, voucher or cash)
make_X <- function(dat, vars) {
  A <- dat$A
  code <- function(v) switch(v,
    pricenone    = A[, , , "price"] * (1 - A[, , , "voucher"] - A[, , , "cash"]),
    pricevoucher = A[, , , "price"] * A[, , , "voucher"],
    pricecash    = A[, , , "price"] * A[, , , "cash"],
    A[, , , v])
  X <- array(0, dim = c(dim(A)[1:3], length(vars)))
  for (k in seq_along(vars)) X[, , , k] <- code(vars[k])
  X
}


## draws
# modified Latin hypercube sampling, exactly as make_draws() in MMC-Demand:
# column by column, men inside, so column k is the same stream whatever the
# number of columns. Cached for the session
.DRAWS <- new.env()
mlhs_draws <- function(N, R, seed, D = 13) {
  key <- sprintf("%d:%d:%d", N, R, seed)
  if (is.null(.DRAWS[[key]])) {
    set.seed(seed)
    u <- array(0, dim = c(N, R, D))
    for (k in 1:D) for (n in 1:N) u[n, , k] <- (sample(0:(R - 1)) + runif(1)) / R
    .DRAWS[[key]] <- qnorm(u)                    # standard normal, [man, draw, column]
  }
  .DRAWS[[key]]
}


## likelihood
# Each returns the log-likelihood per man. theta is one parameter vector [P],
# or one per man [N, P] (used for the scores)

# conditional logit: the sum over his tasks of log P(chosen alternative)
ll_cl <- function(theta, X, Y) {
  V <- if (theta$dim() == 1) torch_matmul(X, theta) else torch_einsum("ntjk,nk->ntj", list(X, theta))
  (nnf_log_softmax(V, dim = 3) * Y)$sum(dim = c(2, 3))
}

# sign-constrained coefficients, as in MMC-Demand
apply_g <- function(beta, s) {
  if (any(s$ln)) {                               # lognormal: beta = -exp(normal)
    m <- torch_tensor(as.numeric(s$ln), dtype = f64)
    beta <- beta * (1 - m) - torch_exp(beta * m) * m
  }
  if (any(s$cens)) {                             # censored: beta = smoothed min(0, normal)
    cm <- torch_tensor(as.numeric(s$cens), dtype = f64)
    soft_min0 <- -nnf_softplus(-beta, beta = 1 / CENS_TAU, threshold = 20)
    beta <- beta * (1 - cm) + soft_min0 * cm
  }
  beta
}

# mixed logit: log( mean over draws of prod over tasks P(choice) ).
# theta = (means of the K coefficients, SDs of the random ones); s$M places the
# SDs on their coefficients (fixed coefficients get SD 0)
ll_mixl <- function(theta, s, X, Y, eta) {
  if (theta$dim() == 1) theta <- theta$unsqueeze(1)          # same parameters for every man
  K <- length(s$vars)
  mu <- theta[, 1:K]
  sd <- torch_matmul(theta[, (K + 1):length(s$names)], s$M$t())
  beta <- apply_g(mu$unsqueeze(2) + eta * sd$unsqueeze(2), s)   # [man, draw, coefficient]
  V <- torch_einsum("ntjk,nrk->ntjr", list(X, beta))            # utilities
  logp <- (nnf_log_softmax(V, dim = 3) * Y$unsqueeze(4))$sum(dim = c(2, 3))   # [man, draw]
  torch_logsumexp(logp, dim = 2) - log(eta$size(2))
}

# negative LL and its gradient for optim(), computed together and cached.
# llf(theta) returns the log-likelihood per man
make_objective <- function(llf) {
  cache <- new.env()
  update <- function(par) {
    if (!identical(par, cache$par)) {
      th <- torch_tensor(par, dtype = f64, requires_grad = TRUE)
      l <- llf(th)$sum()
      cache$g <- -as.numeric(autograd_grad(l, th)[[1]])
      cache$f <- -l$item(); cache$par <- par
    }
  }
  list(fn = function(p) { update(p); cache$f }, gr = function(p) { update(p); cache$g })
}

# exact Hessian of -LL (autograd twice)
hessian_ll <- function(llf, par) {
  th <- torch_tensor(par, dtype = f64, requires_grad = TRUE)
  g <- autograd_grad(llf(th)$sum(), th, create_graph = TRUE)[[1]]
  H <- -sapply(seq_along(par), function(i) as.numeric(autograd_grad(g[i], th, retain_graph = TRUE)[[1]]))
  (H + t(H)) / 2
}

# per-man scores: each man gets his own copy of theta, so one backward pass
# gives every man's gradient [N, P]
scores_ll <- function(llf, par, N) {
  Th <- torch_tensor(matrix(par, N, length(par), byrow = TRUE), dtype = f64, requires_grad = TRUE)
  as_array(autograd_grad(llf(Th)$sum(), Th)[[1]])
}


## checks
# the mixed logit likelihood written with plain R loops on a few men, and the
# gradient against finite differences, as check_model() in MMC-Demand
checkModel <- function(s, Xa, Y_arr, eta_arr, n_men = 15, n_draws = 20) {
  K <- length(s$vars); TT <- dim(Xa)[2]; idx <- 1:n_men
  set.seed(99)
  par <- rnorm(length(s$names), 0, 0.3)
  e <- eta_arr[idx, 1:n_draws, , drop = FALSE]
  ll_plain <- 0
  for (n in idx) {
    pr <- sapply(1:n_draws, function(r) {
      sdv <- numeric(K); sdv[match(s$random, s$vars)] <- par[-(1:K)]
      b <- par[1:K] + sdv * e[n, r, ]
      b[s$ln] <- -exp(b[s$ln])
      zc <- -b[s$cens] / CENS_TAU                 # the same smoothed min(0, b)
      b[s$cens] <- -CENS_TAU * (pmax(zc, 0) + log1p(exp(-abs(zc))))
      prod(sapply(1:TT, function(t) {
        v <- Xa[n, t, , ] %*% b
        exp(v[Y_arr[n, t, ] == 1]) / sum(exp(v))
      }))
    })
    ll_plain <- ll_plain + log(mean(pr))
  }
  Xs <- torch_tensor(Xa[idx, , , , drop = FALSE], dtype = f64)
  Ys <- torch_tensor(Y_arr[idx, , , drop = FALSE], dtype = f64)
  es <- torch_tensor(e, dtype = f64)
  obj <- make_objective(function(th) ll_mixl(th, s, Xs, Ys, es))
  ll_torch <- -obj$fn(par)
  fd <- sapply(seq_along(par), function(i) {
    h <- 1e-6; ei <- replace(numeric(length(par)), i, h)
    (obj$fn(par + ei) - obj$fn(par - ei)) / (2 * h)
  })
  grad_err <- max(abs(obj$gr(par) - fd) / pmax(1, abs(fd)))
  cat(sprintf("  check on %d men: LL torch %.8f | plain R %.8f | max gradient error %.1e\n",
              n_men, ll_torch, ll_plain, grad_err))
  if (abs(ll_torch - ll_plain) > 1e-6 || grad_err > 1e-4)
    stop("Likelihood check failed: do not trust the fits.")
}


## starts
# random start around the rule start, as random_start() in MMC-Demand: means
# + N(0, 0.5 x scale), SDs uniform on (0.2, 2) x scale ((0.2, 1.5) if
# lognormal), where scale is 1 / SD of the attribute (1 for lognormal)
randomStart <- function(rule, r, mi, seed, uscale, s) {
  set.seed(100000 * seed + 1000 * mi + r)
  K <- length(s$vars)
  sc <- ifelse(s$ln, 1, uscale)
  ri <- match(s$random, s$vars)
  c(rule[1:K] + rnorm(K, 0, 0.5 * sc),
    runif(length(ri), 0.2, ifelse(s$ln[ri], 1.5, 2.0)) * sc[ri])
}

# a three-price model started at the fit of the matching one-price model:
# each price coefficient (and its SD) takes the value of the single one
nestedStart <- function(model, fits) {
  src <- sub("three_price_slopes", "one_price_slope", model, fixed = TRUE)
  if (src == model || is.null(fits[[src]])) return(NULL)
  p1 <- fits[[src]]$est
  nm <- MODELS[[model]]$names
  base <- sub("^sd_", "", nm)
  unname(p1[ifelse(base %in% PRICES3, sub("price(none|voucher|cash)$", "price", nm), nm)])
}


## fit
# BFGS, then the exact Hessian and the robust (sandwich, clustered by man)
# covariance. Saved fits are reloaded when nothing has changed
RunModel <- function(dat, model, R = N_DRAWS, seed = SEED, starts = STARTS, fits = list()) {
  s <- MODELS[[model]]
  is_mixl <- length(s$random) > 0
  Xa <- make_X(dat, s$vars)
  X <- torch_tensor(Xa, dtype = f64)
  K <- length(s$vars)

  file <- file.path(fit_dir, paste0(model, if (is_mixl) sprintf("_R%d_seed%d", R, seed), ".rds"))
  old <- NULL                                    # endpoints of earlier starts, reused
  if (file.exists(file)) {
    f <- readRDS(file)
    keep <- c("vars", "random", "ln", "cens")
    if (identical(f$spec[keep], s[keep]) && identical(f$data_md5, dat$md5)) {
      done <- f$endpoints %||% list(rule = list(par = unname(f$est), ll = f$ll, convergence = f$convergence))
      if (!is_mixl || (sum(startsWith(names(done), "random")) >= starts - 1 &&
                       (is.null(nestedStart(model, fits)) || "nested" %in% names(done)))) {
        cat(sprintf("\n%s: loaded the saved fit\n", model)); return(f)
      }
      old <- done
    }
  }

  # start values: conditional logit means; lognormal means at log(-CL); SDs 0.5
  obj_cl <- make_objective(function(th) ll_cl(th, X, dat$Y))
  cl <- optim(rep(0, K), obj_cl$fn, obj_cl$gr, method = "BFGS", control = list(maxit = 1000, reltol = 1e-14))$par
  if (is_mixl) {
    eta_arr <- mlhs_draws(dat$N, R, seed)[, , 1:K, drop = FALSE]
    eta <- torch_tensor(eta_arr, dtype = f64)
    s$M <- torch_tensor(outer(s$vars, s$random, "==") * 1, dtype = f64)
    cat(sprintf("\n%s (%d draws, seed %d)\n", model, R, seed))
    checkModel(s, Xa, dat$Y_arr, eta_arr)
    llf <- function(th) ll_mixl(th, s, X, dat$Y, eta)
    rule <- cl; rule[s$ln] <- log(pmax(-cl[s$ln], 1e-3))
    rule <- c(rule, rep(0.5, length(s$random)))
    start_list <- list(rule = rule)
    nest <- nestedStart(model, fits)
    if (!is.null(nest)) start_list$nested <- nest
    if (starts > 1) {
      uscale <- apply(Xa, 4, function(x) 1 / sd(as.vector(x)))
      mi <- match(model, names(MODELS))
      for (r in seq_len(starts - 1)) start_list[[sprintf("random%02d", r)]] <- randomStart(rule, r, mi, seed, uscale, s)
    }
  } else {
    cat(sprintf("\n%s\n", model))
    llf <- function(th) ll_cl(th, X, dat$Y)
    start_list <- list(rule = cl)
  }

  # BFGS from every start not already run; the best endpoint is the fit
  t0 <- Sys.time()
  obj <- make_objective(llf)
  endpoints <- old %||% list()
  for (nm in setdiff(names(start_list), names(endpoints))) {
    t1 <- Sys.time()
    o <- tryCatch(optim(start_list[[nm]], obj$fn, obj$gr, method = "BFGS",
                        control = list(maxit = 5000, reltol = 1e-12)),
                  error = function(e) list(par = NA, value = Inf, convergence = NA))
    endpoints[[nm]] <- list(par = o$par, ll = -o$value, convergence = o$convergence)
    if (is_mixl) cat(sprintf("  start %-9s LL %11.4f | convergence %s | %.1f min\n", nm, -o$value,
                             o$convergence, as.numeric(difftime(Sys.time(), t1, units = "mins"))))
  }
  lls <- vapply(endpoints, `[[`, 0, "ll")
  best <- endpoints[[which.max(lls)]]
  opt <- list(par = best$par, value = -best$ll, convergence = best$convergence)
  if (is_mixl) cat(sprintf("  best start: %s; %d of %d starts within 0.01 of the best LL\n",
                           names(endpoints)[which.max(lls)], sum(lls > max(lls) - 0.01), length(lls)))
  par <- setNames(opt$par, s$names)

  H <- hessian_ll(llf, par)
  ev <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  hess_ok <- min(ev) > 0 && max(ev) / min(ev) < 1e10
  if (hess_ok) {
    S <- scores_ll(llf, par, dat$N)
    Hinv <- solve(H)
    vc_rob <- Hinv %*% crossprod(S) %*% Hinv
    dimnames(vc_rob) <- list(s$names, s$names)
    se <- sqrt(diag(Hinv)); robse <- sqrt(diag(vc_rob))
  } else {
    vc_rob <- NULL; se <- robse <- rep(NA_real_, length(par))
  }

  ll <- -opt$value; P <- length(par)
  grad_max <- max(abs(obj$gr(opt$par)))
  s_save <- s; s_save$M <- NULL                  # drop the torch copy before saving
  f <- list(model = model, spec = s_save, est = par, se = se, robse = robse, vc_rob = vc_rob,
            ll = ll, n_par = P, aic = -2 * ll + 2 * P, bic = -2 * ll + P * log(N_BIC),
            grad_max = grad_max, convergence = opt$convergence, hess_ok = hess_ok,
            draws = if (is_mixl) R else NA, seed = if (is_mixl) seed else NA, data_md5 = dat$md5,
            endpoints = endpoints, best_start = names(endpoints)[which.max(lls)],
            starts_at_best = sum(lls > max(lls) - 0.01),
            minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")))
  saveRDS(f, file)
  f
}


## output
# distribution of each coefficient across men: mean, SD and median, and for
# censored coefficients the share of men at zero, with robust SEs (delta method)
coefDistribution <- function(f) {
  s <- f$spec; K <- length(s$vars)
  transform <- function(p) {
    mu <- p[1:K]; sd <- numeric(K); sd[match(s$random, s$vars)] <- abs(p[-(1:K)])
    m <- mu; v <- sd; med <- mu; z <- rep(NA_real_, K)
    ln <- s$ln; cs <- s$cens
    m[ln]   <- -exp(mu[ln] + sd[ln]^2 / 2)
    v[ln]   <- exp(mu[ln] + sd[ln]^2 / 2) * sqrt(exp(sd[ln]^2) - 1)
    med[ln] <- -exp(mu[ln])
    a <- mu[cs] / sd[cs]                         # censored: E and SD of min(0, normal)
    e1 <- mu[cs] * pnorm(-a) - sd[cs] * dnorm(a)
    e2 <- (mu[cs]^2 + sd[cs]^2) * pnorm(-a) - mu[cs] * sd[cs] * dnorm(a)
    m[cs] <- e1; v[cs] <- sqrt(pmax(e2 - e1^2, 0)); med[cs] <- pmin(0, mu[cs]); z[cs] <- pnorm(a)
    c(m, v, med, z)
  }
  est <- transform(f$est)
  se <- rep(NA_real_, length(est))
  if (!is.null(f$vc_rob)) {
    J <- sapply(seq_along(f$est), function(i) {
      h <- 1e-6 * max(1, abs(f$est[i])); e <- replace(numeric(length(f$est)), i, h)
      (transform(f$est + e) - transform(f$est - e)) / (2 * h)
    })
    se <- sqrt(diag(J %*% f$vc_rob %*% t(J)))
  }
  i <- function(b) (b - 1) * K + 1:K
  data.frame(coefficient = s$vars,
             distribution = ifelse(s$ln, "lognormal", ifelse(s$cens, "censored",
                             ifelse(s$vars %in% s$random, "normal", "fixed"))),
             mean = est[i(1)], mean_robse = se[i(1)], sd = est[i(2)], sd_robse = se[i(2)],
             median = est[i(3)], share_zero = est[i(4)], row.names = NULL)
}

# estimates and robust covariance: the conditional logits in the layout of the
# CMC outputs; the mixed logits also get the coefficient distributions
writeFit <- function(f) {
  tag <- paste0(f$model, if (!is.na(f$draws)) sprintf("_R%d_seed%d", f$draws, f$seed))
  est <- data.frame(est = f$est, se = f$se, robse = f$robse, robtrat_0 = f$est / f$robse)
  write.csv(round(est, 6), file.path(out_dir, paste0(tag, "_estimates.csv")))
  if (!is.null(f$vc_rob)) write.csv(f$vc_rob, file.path(out_dir, paste0(tag, "_robcovar.csv")))
  if (!is.na(f$draws))
    write.csv(coefDistribution(f), file.path(out_dir, paste0(tag, "_distribution.csv")), row.names = FALSE)
}

# largest differences from the CMC run of 2018
checkCMC <- function(f, dir = file.path("data-raw", "estimates")) {
  if (!f$model %in% names(CMC_FILES)) return(invisible(NULL))
  file <- file.path(dir, paste0(CMC_FILES[[f$model]], "_estimates.csv"))
  if (!file.exists(file)) return(invisible(NULL))
  cmc <- read.csv(file, row.names = 1)[names(f$est), ]
  ll_cmc <- as.numeric(sub(".*:", "", grep("LL\\(final\\)",
              readLines(sub("_estimates.csv", "_output.txt", file)), value = TRUE)))
  cat(sprintf("  vs CMC 2018: LL %.3f (CMC %.3f); largest difference in estimates %.4f, in robust SEs %.4f\n",
              f$ll, ll_cmc, max(abs(f$est - cmc$est)), max(abs(f$robse - cmc$robse))))
}


## run model
dat <- load_choices()
cat(sprintf("%d men, %d choice tasks, %d alternatives | mixed logits: %d draws, seed %d\n",
            dat$N, dat$n_obs, dat$J, N_DRAWS, SEED))

run <- names(MODELS)
if (!identical(WHICH, "all"))
  run <- run[run %in% WHICH | sub("_.*", "", run) %in% WHICH]
# a mixed logit is compared with the conditional logit with the same price coefficients
run <- union(names(MODELS)[startsWith(names(MODELS), "cl_")], run)
# a three-price mixed logit starts from the matching one-price fit, so that is fitted (or loaded) first
run <- names(MODELS)[names(MODELS) %in% union(run, sub("three_price_slopes", "one_price_slope", run, fixed = TRUE))]

fits <- list()
for (m in run) {
  f <- RunModel(dat, m, fits = fits)
  fits[[m]] <- f
  writeFit(f)
  cat(sprintf("  LL %.3f, %d parameters, AIC %.1f, BIC %.1f | max |gradient| %.1e | Hessian %s | %.1f min\n",
              f$ll, f$n_par, f$aic, f$bic, f$grad_max, if (f$hess_ok) "OK" else "NOT positive definite",
              f$minutes))
  if (is.na(f$draws)) {
    print(round(data.frame(est = f$est, robse = f$robse, robtrat_0 = f$est / f$robse), 4))
    checkCMC(f)
  } else {
    d <- coefDistribution(f)
    num <- sapply(d, is.numeric)
    d[num] <- lapply(d[num], round, 4)
    print(d, row.names = FALSE)
  }
}

# fit summary; LL gain over the conditional logit with the same price coefficients
summ <- do.call(rbind, lapply(fits, function(f) data.frame(
  model = f$model, draws = f$draws, seed = f$seed, LL = round(f$ll, 3), parameters = f$n_par,
  AIC = round(f$aic, 1), BIC = round(f$bic, 1),
  LL_gain_over_CL = round(f$ll - fits[[if (identical(f$spec$vars, VARS1)) "cl_one_price_slope" else "cl_three_price_slopes"]]$ll, 3),
  best_start = f$best_start %||% "rule", starts = length(f$endpoints %||% 1),
  starts_at_best = f$starts_at_best %||% 1,
  max_gradient = signif(f$grad_max, 2), converged = f$convergence == 0, hessian_ok = f$hess_ok,
  minutes = round(f$minutes, 1))))
write.csv(summ, file.path(out_dir, sprintf("fit_summary_R%d_seed%d.csv", N_DRAWS, SEED)), row.names = FALSE)
cat("\nFit summary\n"); print(summ, row.names = FALSE)
