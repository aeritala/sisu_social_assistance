posterior_predict_generalized_gamma <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  beta <- brms::get_dpar(prep, "beta", i = i)
  delta <- brms::get_dpar(prep, "delta", i = i)
  generalized_gamma_rng(mu, beta, delta)
}

log_lik_generalized_gamma <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  beta <- brms::get_dpar(prep, "beta", i = i)
  delta <- brms::get_dpar(prep, "delta", i = i)
  y <- prep$data$Y[i]
  generalized_gamma_lpdf(y, mu, beta, delta)
}

generalized_gamma <- custom_family(
  name = "generalized_gamma",
  dpars = c("mu", "beta", "delta"),
  links = c("log", "identity", "identity"),
  lb = c(0, 0, 0),
  ub = c(NA, NA, NA),
  type = "real",
  loop = FALSE,
  log_lik = log_lik_generalized_gamma,
  posterior_predict = posterior_predict_generalized_gamma
)

stan_funs <- readLines(file.path("models",
                                 "functions",
                                 "generalized-gamma_regpar-alpha.stan"))
stanvars <- stanvar(scode = stan_funs, block = "functions")