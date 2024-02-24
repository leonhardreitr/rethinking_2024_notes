library(rethinking)
library(ggdag)
library(tidyverse)

data("UCBAdmissions")

dag_coords <-
  tibble(
    name = c("G", "D", "A", "u"),
    x = c(1, 2, 3, 3),
    y = c(2, 3, 2, 3.7)
  )
dag <-
  dagify(
    D ~ G + u,
    A ~ G + D + u,
    coords = dag_coords
  )

ggdag(dag) + theme_dag()

set.seed(12)
N <- 2e3
G <- sample(1:2, size = N, replace = T)
u <- rbern(N, .1)

D <- rbern(N, ifelse(G == 1, u * 1, .75)) + 1

p_u0 <- matrix(c(.1, .1, .1, .3), nrow = 2)
p_u1 <- matrix(c(.3, .3, .5, .5), nrow = 2)

pu <- list(p_u0, p_u1)

p <- sapply(1:N, function(i) {
  pu[[1 + u[i]]][D[i], G[i]]
})

A <- rbern(N, p)
A

dat <- list(A = A, D = D, G = G)

# total effect
m1 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G],
    a[G] ~ normal(0, 1)
  ),
  data = dat, chains = 4, cores = parallel::detectCores()
)

summary(m1)


# total effect
m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ normal(0, 1)
  ),
  data = dat, chains = 4, cores = parallel::detectCores()
)

summary(m2)

post2 <- extract.samples(m2)
dens(inv_logit(post2$a[, 1, 1]), lwd = 3, col = 4, xlim = c(0, 0.5), xlab = "probability admission")
dens(inv_logit(post2$a[, 2, 1]), lwd = 3, col = 4, lty = 2, add = TRUE)
dens(inv_logit(post2$a[, 1, 2]),
  lwd = 3, col = 2,
  add = TRUE
)
dens(inv_logit(post2$a[, 2, 2]), lwd = 3, col = 2, add = TRUE, lty = 2)

dat$D2 <- ifelse(dat$D == 2, 1, 0)
dat$N <- length(dat$D)
dat$b <- c(1, 1)
dat$g <- c(1, 0)



m3 <- ulam(
  alist(
    # Model A
    A ~ bernoulli(p),
    logit(p) <- a[G, D] + b[G] * u[i],
    matrix[G, D]:a ~ normal(0, 1),

    # Model D
    D2 ~ bernoulli(q),
    logit(q) <- delta[G] + g[G] * u[i],
    delta[G] ~ normal(0, 1),

    # declare u
    vector[N]:u ~ normal(0, 1)
  ),
  data = dat, chains = 4, cores = parallel::detectCores()
)

summary(m3)


post2 <- extract.samples(m3)
dens(inv_logit(post2$a[, 1, 1]), lwd = 3, col = 5, xlim = c(0, 0.5), xlab = "probability admission")
dens(inv_logit(post2$a[, 2, 1]), lwd = 3, col = 4, lty = 2, add = TRUE)
dens(inv_logit(post2$a[, 1, 2]),
  lwd = 3, col = 2,
  add = TRUE
)
dens(inv_logit(post2$a[, 2, 2]), lwd = 3, col = 2, add = TRUE, lty = 2)


# Poison ------------------------------------------------------------------

data("Kline")

dag <-
  dagify(
    C ~ L + P,
    T ~ P + L + C,
    P ~ L
  )

d <- Kline
d$P <- scale(log(d$population))
d$contact_id <- ifelse(d$contact == "high", 2, 1)
dat <- list(T = d$total_tools, P = d$P, C = d$contact_id)

# intercept only

m4 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(3, 0.5)
  ),
  data = dat, chains = 4, log_lik = T
)

m5 <- ulam(
  alist(
    T ~ dpois(lambda),
    log(lambda) <- a[C] + b[C] * P,
    a[C] ~ dnorm(3, 0.5),
    b[C] ~ dnorm(0, .2)
  ),
  data = dat, chains = 4, log_lik = T
)

compare(m4, m5, func = PSIS)

k <- PSIS(m5, pointwise = TRUE)$k
plot(dat$P, dat$T,
  xlab = "log population (std)", ylab = "total tools",
  col = ifelse(dat$C == 1, 4, 2), lwd = 4 + 4 * normalize(k),
  ylim = c(0, 75), cex = 1 + normalize(k)
)

# set up the horizontal axis values to compute predictions at
P_seq <- seq(from = -1.4, to = 3, len = 100)

# predictions for C=1 (low contact)
lambda <- link(m5, data = data.frame(P = P_seq, C = 1))
lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)
lines(P_seq, lmu, lty = 2, lwd = 1.5)
shade(lci, P_seq, xpd = TRUE, col = col.alpha(4, 0.3))

# predictions for C=2 (high contact)
lambda <- link(m5, data = data.frame(P = P_seq, C = 2))
lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)
lines(P_seq, lmu, lty = 1, lwd = 1.5)
shade(lci, P_seq, xpd = TRUE, col = col.alpha(2, 0.3))

identify(dat$P, dat$T, d$culture)


plot(d$population, d$total_tools,
  xlab = "population", ylab = "total tools",
  col = ifelse(dat$C == 1, 4, 2), lwd = 4 + 4 * normalize(k),
  ylim = c(0, 75), cex = 1 + normalize(k)
)
P_seq <- seq(from = -5, to = 3, length.out = 100)
# 1.53 is sd of log(population)
# 9 is mean of log(population)
pop_seq <- exp(P_seq * 1.53 + 9)
lambda <- link(m5, data = data.frame(P = P_seq, C = 1))
lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)
lines(pop_seq, lmu, lty = 2, lwd = 1.5)
shade(lci, pop_seq, xpd = TRUE, col = col.alpha(4, 0.3))

lambda <- link(m5, data = data.frame(P = P_seq, C = 2))
lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)
lines(pop_seq, lmu, lty = 1, lwd = 1.5)
shade(lci, pop_seq, xpd = TRUE, col = col.alpha(2, 0.3))

identify(d$population, d$total_tools, d$culture)

dat2 <- list(T = d$total_tools, P = d$population, C = d$contact_id)

m11.11 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- exp(a[C]) * P^b[C] / g,
    a[C] ~ dnorm(1, 1),
    b[C] ~ dexp(1),
    g ~ dexp(1)
  ),
  data = dat2, chains = 4, cores = 4, log_lik = TRUE
)

precis(m11.11, 2)

plot(d$population, d$total_tools,
  xlab = "population", ylab = "total tools",
  col = ifelse(dat$C == 1, 4, 2), lwd = 4 + 4 * normalize(k),
  ylim = c(0, 75), cex = 1 + normalize(k)
)
P_seq <- seq(from = -5, to = 3, length.out = 100)

pop_seq <- exp(P_seq * 1.53 + 9)
lambda <- link(m11.11, data = data.frame(P = pop_seq, C = 1))
lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)
lines(pop_seq, lmu, lty = 2, lwd = 1.5)
shade(lci, pop_seq, xpd = TRUE, col = col.alpha(4, 0.3))

lambda <- link(m11.11, data = data.frame(P = pop_seq, C = 2))
lmu <- apply(lambda, 2, mean)
lci <- apply(lambda, 2, PI)
lines(pop_seq, lmu, lty = 1, lwd = 1.5)
shade(lci, pop_seq, xpd = TRUE, col = col.alpha(2, 0.3))

identify(d$population, d$total_tools, d$culture)
