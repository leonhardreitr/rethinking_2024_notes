library(rethinking)
library(ellipse)
library(tidybayes.rethinking)
data("bangladesh")
str(bangladesh)

d <- bangladesh

dat <- list(
  C = d$use.contraception,
  D = as.integer(d$district),
  U = ifelse(d$urban == 1, 1, 0)
)

m1 <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D] + b[D] * U,
    save > vector[61]:a <<- abar * za * sigma,
    save > vector[61]:b <<- bbar * zb * tau,
    vector[61]:za ~ normal(0, 1),
    vector[61]:zb ~ normal(0, 1),
    c(abar, bbar) ~ normal(0, 1),
    c(sigma, tau) ~ exponential(1)
  ),
  data = dat, cores = detectCores(), chains = 4
)

precis(m1)

m2 <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D] + b[D] * U,
    vector[61]:a ~ normal(abar, sigma),
    vector[61]:b ~ normal(bbar, tau),
    c(abar, bbar) ~ normal(0, 1),
    c(sigma, tau) ~ exponential(1)
  ),
  data = dat, cores = detectCores(), chains = 4
)

precis(m2)

mCDUcov <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D] + b[D] * U,
    # define effects using other parameters
    transpars > vector[61]:a <<- v[, 1],
    transpars > vector[61]:b <<- v[, 2],
    # priors - centered correlated varying effects
    matrix[61, 2]:v ~ multi_normal(abar, Rho, sigma),
    vector[2]:abar ~ normal(0, 1),
    corr_matrix[2]:Rho ~ lkj_corr(4),
    vector[2]:sigma ~ exponential(1)
  ),
  data = dat, chains = 4, cores = 4
)

precis(mCDUcov, 3, pars = c("Rho", "sigma"))


mCDUcov_nc <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D] + b[D] * U,
    # define effects using other parameters
    # this is the non-centered Cholesky machine
    transpars > vector[61]:a <<- abar[1] + v[, 1],
    transpars > vector[61]:b <<- abar[2] + v[, 2],
    transpars > matrix[61, 2]:v <-
      compose_noncentered(sigma, L_Rho, Z),
    # priors - note that none have parameters inside them
    # that is what makes them non-centered
    matrix[2, 61]:Z ~ normal(0, 1),
    vector[2]:abar ~ normal(0, 1),
    cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky(4),
    vector[2]:sigma ~ exponential(1),
    # convert Cholesky to Corr matrix
    gq > matrix[2, 2]:Rho <<- Chol_to_Corr(L_Rho)
  ),
  data = dat, chains = 4, cores = 4
)

precis(mCDUcov_nc, 3, pars = c("Rho", "sigma"))
precis(mCDUnc, 3, pars = c("sigma", "tau"))
