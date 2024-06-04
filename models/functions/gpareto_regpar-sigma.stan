 real generalized_pareto_lpdf(real y, real ymin, real k, real mu) {
  // generalised Pareto log pdf
  // int N = rows(y);
  real inv_k = inv(k);
  if (k < 0 && (y - ymin) / mu > -inv_k) {
   reject("k < 0 and max(y-ymin) / mu > -1/k; found k, mu =", k, mu);
  }
  if (mu <= 0) {
    reject("mu<=0; found mu = ", mu);
  }
  if (abs(k) > 1e-15) {
    return -(1 + inv_k) * (log1p((y - ymin) * (k / mu))) - log(mu);
  } else {
    return -(y - ymin) / mu - log(mu);
  } // limit k->0
 }
 real generalized_pareto_rng(real ymin, real k, real mu) {
  // generalised Pareto rng
  if (mu <= 0) {
    reject("mu<=0; found mu = ", mu);
  }
  if (abs(k) > 1e-15) {
    return ymin + (uniform_rng(0, 1) ^ -k - 1) * mu / k;
  } else {
    return ymin - mu * log(uniform_rng(0, 1));
  } // limit k->0
 }
