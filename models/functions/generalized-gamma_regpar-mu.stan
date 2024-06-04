real generalized_gamma_lpdf(vector x, vector mu, real alpha, real delta) {
  if (alpha <= 0) {
    reject("alpha <= 0; found alpha = ", alpha);
  }
  if (delta <= 0) {
    reject("delta <= 0; found delta = ", delta);
  }
  vector[rows(x)] beta;
  beta = exp(log(mu) + lgamma(alpha) - lgamma(alpha + 1 / delta));
  return rows(x) * (log(delta) - lgamma(alpha)) - delta * alpha * sum(log(beta)) +
    delta * alpha * sum(log(x)) - sum(log(x) + pow(x ./ beta, delta));
}
vector generalized_gamma_rng(vector mu, vector alpha, vector delta) {
  vector[rows(mu)] beta;
  beta = exp(log(mu) + lgamma(alpha) - lgamma(alpha + 1 ./ delta));
  vector[rows(mu)] y = to_vector(gamma_rng(alpha, 1));
  return beta .* pow(y, 1 ./ delta);
}
