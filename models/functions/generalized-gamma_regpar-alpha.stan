real generalized_gamma_lpdf(vector x, vector mu, real beta, real delta) {
  if (beta <= 0) {
    reject("beta<=0; found beta = ", beta);
  }
  if (delta <= 0) {
    reject("delta<=0; found delta = ", delta);
  }
  return rows(x) * log(delta) - delta * log(beta) * sum(mu) - sum(lgamma(mu)) +
    delta * sum(mu .* log(x)) - sum(log(x) + pow(x ./ beta, delta));
}

vector generalized_gamma_rng(vector mu, vector beta, vector delta) {
  vector[rows(mu)] y = to_vector(gamma_rng(mu, 1));
  return beta .* pow(y, 1 ./ delta);
}
