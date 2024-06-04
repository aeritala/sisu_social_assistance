posterior_predict_generalized_pareto <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  ymin <- brms::get_dpar(prep, "ymin", i = i)
  generalized_pareto_rng(ymin, mu, sigma)
}

log_lik_generalized_pareto <- function(i, prep) {
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  mu <- brms::get_dpar(prep, "mu", i = i)
  ymin <- brms::get_dpar(prep, "ymin", i = i) 
  y <- prep$data$Y[i]
  generalized_pareto_lpdf(y, ymin, mu, sigma)
}

generalized_pareto <- custom_family(
  name = "generalized_pareto",
  dpars = c("ymin", "mu", "sigma"),
  links = c("identity", "log", "log"),
  lb = c(NA, NA, 0),
  ub = c(NA, NA, NA),
  type = "real",
  posterior_predict = posterior_predict_generalized_pareto,
  log_lik = log_lik_generalized_pareto
)

stan_funs <- readLines(file.path("models",
                                 "functions",
                                 "gpareto_regpar-k.stan"))

stanvars <- stanvar(scode = stan_funs, block = "functions")
