# =============================================================================
# estimation/mmc_torch_cl.R
# Conditional logit for the MMC choice experiment, fitted with R torch.
#
# A very basic version of the MMC-Demand torch engine, cut down to the
# conditional logit but with the same layout: choices as arrays
# [man, task, alternative, attribute], log-softmax over the alternatives,
# autograd gradients, BFGS through optim(), the exact Hessian, and robust
# standard errors clustered by man. The likelihood is written per man, so a
# latent class or mixed logit model is another ll_*() function; the rest
# (objective, Hessian, scores, output) stays as it is.
#
# Data (data/dce/), all that the model needs:
#   choices.csv  one row per man and task: id (1-406), block, task, choice
#                (1 = service option 1, 2 = service option 2, 3 = neither)
#   design.csv   the attributes of options 1 and 2 in each block and task
#                (the Ngene design, 3 blocks of 12 tasks)
#
# The two models estimated with CMC in 2018 (outputs in data-raw/estimates/):
#   cl_one_price_slope     one price coefficient (base case)   CMC LL -3703.301
#   cl_three_price_slopes  out-of-pocket, voucher and cash     CMC LL -3703.081
# Writes estimation/output/<model>_estimates.csv and <model>_robcovar.csv, in
# the layout of the CMC outputs, and checks them against CMC.
#
# Run from the project folder:  Rscript estimation/mmc_torch_cl.R
# =============================================================================

suppressPackageStartupMessages(library(torch))
f64 <- torch_float64()
out_dir <- file.path("estimation", "output")
dir.create(out_dir, showWarnings = FALSE)


## settings
# attributes of the service options, as named in design.csv and by CMC
ATTR <- c("permhc", "dist5", "dist15", "avail", "somepriv", "notpriv", "device", "voucher", "cash", "price")

# the coefficients of each model, in the order of the CMC outputs
MODELS <- list(
  cl_one_price_slope    = c("neither", ATTR),
  cl_three_price_slopes = c("neither", setdiff(ATTR, "price"), "pricenone", "pricevoucher", "pricecash"))

# CMC outputs to check against
CMC_FILES <- c(cl_one_price_slope = "cl_no_corr", cl_three_price_slopes = "01_cl_3_price_coefs")


## data
# choices and design -> attribute array A [man, task, alternative, attribute]
# and choice array Y [man, task, alternative] (1 for the chosen alternative)
load_choices <- function(dir = file.path("data", "dce")) {
  ch  <- read.csv(file.path(dir, "choices.csv"))
  des <- read.csv(file.path(dir, "design.csv"))
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
  list(A = A, Y = torch_tensor(Y, dtype = f64), N = N, TT = TT, J = J, n_obs = nrow(d))
}

# the coded attributes X [man, task, alternative, coefficient] of a model.
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
  torch_tensor(X, dtype = f64)
}


## likelihood
# log-likelihood per man: the sum over his tasks of log P(chosen alternative).
# theta is one coefficient vector [K], or one per man [N, K] (used for the scores)
ll_cl <- function(theta, X, Y) {
  V <- if (theta$dim() == 1) torch_matmul(X, theta) else torch_einsum("ntjk,nk->ntj", list(X, theta))
  (nnf_log_softmax(V, dim = 3) * Y)$sum(dim = c(2, 3))
}

# negative LL and its gradient for optim(), computed together and cached
make_objective <- function(ll, X, Y) {
  cache <- new.env()
  update <- function(par) {
    if (!identical(par, cache$par)) {
      th <- torch_tensor(par, dtype = f64, requires_grad = TRUE)
      l <- ll(th, X, Y)$sum()
      cache$g <- -as.numeric(autograd_grad(l, th)[[1]])
      cache$f <- -l$item(); cache$par <- par
    }
  }
  list(fn = function(p) { update(p); cache$f }, gr = function(p) { update(p); cache$g })
}

# exact Hessian of -LL (autograd twice)
hessian_ll <- function(ll, par, X, Y) {
  th <- torch_tensor(par, dtype = f64, requires_grad = TRUE)
  g <- autograd_grad(ll(th, X, Y)$sum(), th, create_graph = TRUE)[[1]]
  H <- -sapply(seq_along(par), function(i) as.numeric(autograd_grad(g[i], th, retain_graph = TRUE)[[1]]))
  (H + t(H)) / 2
}

# per-man scores: each man gets his own copy of theta, so one backward pass
# gives every man's gradient [N, K]
scores_ll <- function(ll, par, X, Y) {
  N <- X$size(1)
  Th <- torch_tensor(matrix(par, N, length(par), byrow = TRUE), dtype = f64, requires_grad = TRUE)
  as_array(autograd_grad(ll(Th, X, Y)$sum(), Th)[[1]])
}


## fit
# BFGS from zero, then the exact Hessian and the robust (sandwich, clustered by man) covariance
RunCL <- function(dat, model) {
  vars <- MODELS[[model]]
  X <- make_X(dat, vars)
  obj <- make_objective(ll_cl, X, dat$Y)
  opt <- optim(rep(0, length(vars)), obj$fn, obj$gr, method = "BFGS",
               control = list(maxit = 1000, reltol = 1e-14))
  par <- setNames(opt$par, vars)

  H <- hessian_ll(ll_cl, par, X, dat$Y)
  S <- scores_ll(ll_cl, par, X, dat$Y)
  Hinv <- solve(H)
  vc_rob <- Hinv %*% crossprod(S) %*% Hinv
  dimnames(vc_rob) <- list(vars, vars)

  ll <- -opt$value; P <- length(par)
  list(model = model, est = par, se = sqrt(diag(Hinv)), robse = sqrt(diag(vc_rob)), vc_rob = vc_rob,
       ll = ll, n_par = P, aic = -2 * ll + 2 * P, bic = -2 * ll + P * log(dat$n_obs),
       grad_max = max(abs(obj$gr(opt$par))), convergence = opt$convergence)
}

# estimates and robust covariance, in the layout of the CMC outputs
writeFit <- function(fit) {
  est <- data.frame(est = fit$est, se = fit$se, robse = fit$robse, robtrat_0 = fit$est / fit$robse)
  write.csv(round(est, 6), file.path(out_dir, paste0(fit$model, "_estimates.csv")))
  write.csv(fit$vc_rob, file.path(out_dir, paste0(fit$model, "_robcovar.csv")))
}

# largest differences from the CMC run of 2018
checkCMC <- function(fit, dir = file.path("data-raw", "estimates")) {
  f <- file.path(dir, paste0(CMC_FILES[[fit$model]], "_estimates.csv"))
  if (!file.exists(f)) return(invisible(NULL))
  cmc <- read.csv(f, row.names = 1)[names(fit$est), ]
  ll_cmc <- as.numeric(sub(".*:", "", grep("LL\\(final\\)",
              readLines(sub("_estimates.csv", "_output.txt", f)), value = TRUE)))
  cat(sprintf("  vs CMC 2018: LL %.3f (CMC %.3f); largest difference in estimates %.4f, in robust SEs %.4f\n",
              fit$ll, ll_cmc, max(abs(fit$est - cmc$est)), max(abs(fit$robse - cmc$robse))))
}


## run model
dat <- load_choices()
cat(sprintf("%d men, %d choice tasks, %d alternatives\n", dat$N, dat$n_obs, dat$J))

for (m in names(MODELS)) {
  t0 <- Sys.time()
  fit <- RunCL(dat, m)
  cat(sprintf("\n%s: LL %.3f, %d parameters, AIC %.1f, BIC %.1f (max |gradient| %.1e, %.1f s)\n",
              m, fit$ll, fit$n_par, fit$aic, fit$bic, fit$grad_max, as.numeric(Sys.time() - t0, units = "secs")))
  print(round(data.frame(est = fit$est, robse = fit$robse, robtrat_0 = fit$est / fit$robse), 4))
  checkCMC(fit)
  writeFit(fit)
}
