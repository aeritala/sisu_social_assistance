
n <- 10000
shape <- 0.1
mu_gamma <- 0.1/10
rate <- shape/mu_gamma
rate
{
  set.seed(1)
  sigma <- rgamma(n, shape = shape, rate = rate)
  xi <- runif(n, 0, 0.5)
  #xi <- rbeta(n, 0.99, 1)
  t <- sigma/xi
  mu <- rnorm(n, 7.5, 0.2) + 15*rnorm(n, 0, 0.15)
}

plot(density(mu - t), xlim = c(-20,20))
abline(v = 1, col = "red")
mean(mu-t > 1)
mean(mu-t > 0)
summary(t)
summary(mu)
summary(mu-t)
