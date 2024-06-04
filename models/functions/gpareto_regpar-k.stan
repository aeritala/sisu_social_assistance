real generalized_pareto_lpdf(real y, real ymin, real mu, real sigma) {
  // generalised Pareto log pdf
  // int N = rows(y);
  real inv_mu = inv(mu);
  if (mu < 0 && (y - ymin) / sigma > -inv_mu) {
    reject("mu < 0 and (y-ymin) / sigma > -1/mu; found mu, sigma =", mu, sigma);
  }
  if (sigma <= 0) {
    reject("sigma<=0; found sigma = ", sigma);
  }
  if (abs(mu) > 1e-15) {
    return -(1 + inv_mu) * (log1p((y - ymin) * (mu / sigma))) - log(sigma);
  } else {
    return -(y - ymin) / sigma - log(sigma);
  } // limit mu->0
}
real generalized_pareto_rng(real ymin, real mu, real sigma) {
  // generalised Pareto rng
  if (sigma <= 0) {
    reject("sigma<=0; found  sigma = ", sigma);
  }
  if (abs(mu) > 1e-15) {
    return ymin + (uniform_rng(0, 1) ^ -mu - 1) * sigma / mu;
  } else {
    return ymin - sigma * log(uniform_rng(0, 1));
  } // limit mu->0
}
