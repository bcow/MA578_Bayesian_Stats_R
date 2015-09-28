# MA 578: R session, 09/17/15
# y_i | theta ~ Poisson(theta * x_i)

# [ Prior Elicitation ]
# assume conjugacy: theta ~ Gamma(alpha, beta)
# "on average, 4 cars per hour" -> E[theta] = alpha / beta = 4
# "most likely between 1 to 6 cars per hour" -> P(1 <= theta <= 6) = .95
# find beta numerically by finding an approximate root to 'f'
f <- function (beta)
  pgamma(6, 4 * beta, beta) - pgamma(1, 4 * beta, beta) - .95
beta <- seq(1, 10, length=100)
plot(beta, f(beta), type='l')
abline(h=0, lty=2)
abline(v=3)
# so, beta ~= 3, and alpha = 4 * beta = 12
alpha <- 12; beta <- 3

# [ Bayesian Analysis ]
# data:
x <- c(1,2,4); y <- c(2,4,16)
# check posterior graphically
t <- seq(0, 8, length=100) # theta
plot(t, dgamma(t, alpha + sum(y), beta + sum(x)), type='l',
     xlab=expression(theta), ylab='density')
# 95% posterior interval
qgamma(c(.025, .975), alpha + sum(y), beta + sum(x))

# describing posterior by sampling:
ts <- rgamma(1000, alpha + sum(y), beta + sum(x))
hist(ts)
mean(ts)
quantile(ts, c(.025, .975))

# compare to prior
plot(t, dgamma(t, alpha + sum(y), beta + sum(x)), type='l',
     xlab=expression(theta), ylab='density')
lines(t, dgamma(t, alpha, beta), lty=2) # prior
lines(t, dgamma(t, sum(y) + 1, sum(x)), lty=3) # likelihood

# posterior predictive
help(dnbinom) # check negative binomial distribution
xt <- 3 # x tilde
yt <- 0:20
plot(yt, dnbinom(yt, alpha + sum(y), 1 - xt / (xt + beta + sum(x))),
     type='h', xlab=expression(tilde(y)), ylab=expression(P(tilde(y)~"|"~y)))
# 95% posterior predictive interval
qnbinom(c(.025, .975), alpha + sum(y), 1 - xt / (xt + beta + sum(x)))

# using sampling:
ys <- rnbinom(1000, alpha + sum(y), 1 - xt / (xt + beta + sum(x)))
mean(ts)
mean(ys)
quantile(ys, c(.025, .975))
# conditional sampling: sample theta | y, then y.tilde | theta
ys <- rpois(1000, rgamma(1000, alpha + sum(y), beta + sum(x)))
# or, in two steps,
ts <- rgamma(1000, alpha + sum(y), beta + sum(x))
ys <- rpois(1000, xt * ts)
mean(ys) # compare to marginal sample from negative binomial
quantile(ys, c(.025, .975))

