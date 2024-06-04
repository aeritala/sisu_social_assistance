 real generalized_pareto_lpdf(real y, real mu, real ymin, real k) {
  // generalised Pareto log pdf
  real sigma = (mu - ymin)*(1 - k);
  real inv_k = inv(k);
  if (k < 0 && (y - ymin) / sigma > -inv_k) {
   reject("k < 0 and max(y-ymin) / sigma > -1/k; found k, sigma =", k, sigma);
  }
  if (sigma <= 0) {
    reject("sigma<=0; found sigma = ", sigma);
  }
  if (abs(k) > 1e-15) {
    return -(1 + inv_k) * (log1p((y - ymin) * (k / sigma))) - log(sigma);
  } else {
    return -(y - ymin) / sigma - log(sigma);
  } // limit k->0
 }
 real generalized_pareto_rng(real mu, real ymin, real k) {
  // generalised Pareto rng
  real sigma = (mu - ymin)*(1 - k);
  if (sigma <= 0) {
    reject("sigma<=0; found sigma = ", sigma);
  }
  if (abs(k) > 1e-15) {
    return ymin + (uniform_rng(0, 1) ^ -k - 1) * sigma / k;
  } else {
    return ymin - sigma * log(uniform_rng(0, 1));
  } // limit k->0
 }
