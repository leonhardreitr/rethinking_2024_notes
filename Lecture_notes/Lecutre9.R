library(rethinking)
library(ggdag)
library(tidyverse)

data("UCBAdmissions")

dag_coords <-
  tibble(name = c("G", "D", "A"),
         x    = c(1, 2, 3),
         y    = c(2, 3, 2))
dag <- 
  dagify(
    D ~ G,
    A ~ G + D, coords = dag_coords)

ggdag(dag) + theme_dag()

# turn DAG to generative model
n <- 1000
  # sim Gender
  G <- rbern(n,prob = .5)
  
  # sim Departments tendencies
  D <- rbern(n, if_else(G == 1, .3,.8)) + 1
  
  # sim accept
  accept_rate <- matrix(c(.3,.2,.3,.3), nrow = 2)
  A <- rbern(n, accept_rate[D,G])

table(G,D)


# PPS for Logits ----------------------------------------------------------
set.seed(99)
n <- 1e4
a <- rnorm(n,0,1.5)
b <- rnorm(n,0,1.5)

xseq <- seq(from = -3, to = 3, length.out = 1000)
p <- sapply(xseq, function(x)
            inv_logit(a + b*x))

plot(NULL, xlim = c(-2.5,2.5), ylim = c(0,1),
     xlab = "x value", ylab = "Probability")
for (i in 1:10) {
  lines(xseq, p[i,], lwd = 3, col = rethink_palette[5])
}

# total effect
d_sim <- list(A = A, D = D, G = G)
m1 <- ulam(
  alist(A ~ bernoulli(p),
        logit(p) <- a[G],
        a[G] ~ normal(0,1)),
  data = d_sim, chains = 4, cores = 8
)

precis(m1)


# Binomial Regression -----------------------------------------------------

data("UCBadmit")
d <- UCBadmit

dat <- list(
  A = d$admit,
  N = d$applications,
  G = if_else(d$applicant.gender == "female", 1, 2),
  D = as.integer(d$dept)
)

mG <- ulam(
  alist(
    A ~ binomial(N,p),
    logit(p) <- a[G],
    a[G] ~ normal(0,1)
  ), data = dat, chains = 4, cores = 4
)

summary(mG)


mGD <- ulam(
  alist(
    A ~ binomial(N,p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data = dat, chains = 4, cores = 4
)

summary(mGD)

traceplot(mGD)
WAIC(mGD)

# direct effect
post1 <- extract.samples(mG)
PrA_G1 <- inv_logit(post1$a[,1])
PrA_G2 <- inv_logit(post1$a[,2])
diff_prob <- PrA_G1 - PrA_G2
dens(diff_prob, lwd = 4, col = 2, xlab = "Gender Contrast", main = "Males advantaged")

# total effect
post2 <- extract.samples(mGD)
PrA <- inv_logit(post2$a)
diff_prob_D <- sapply(1:6, function(i)
  PrA[,1,i] - PrA[,2,i])

plot(NULL, xlim = c(-.2,.3), ylim = c(0,25))
for (i in 1:6) {
  dens(diff_prob_D[,i], lwd = 4, col = 1+i, add = T)
}
 
mGD |> spread_draws(intercept[condition])


# Simulate an intervention ------------------------------------------------

total_apps <- sum(dat$N)

apps_per_d <- sapply(1:6, function(i)
  sum(dat$N[dat$D==i]))

# sim only XX

p_G1 <- link(mGD, data = list(
  D = rep(1:6, times = apps_per_d),
  N = rep(1, total_apps),
  G = rep(1, total_apps)
))

# sim only XY

p_G2 <- link(mGD, data = list(
  D = rep(1:6, times = apps_per_d),
  N = rep(1, total_apps),
  G = rep(2, total_apps)
))

dens(p_G1 - p_G2, lwd = 4, col = 2)


# Survival Analysis -------------------------------------------------------

data("AustinCats")
d <- AustinCats

d_slim <- list(
  days = d$days_to_event,
  adopted = ifelse(d$out_event == "Adoption",1,0),
  color_id = ifelse(d$color == "Black",1,2)
)

meow <- 
  ulam(
    alist(
      days|adopted == 1 ~ exponential(lambda),
      days|adopted == 0 ~ costum(exponential_lccdf(!Y|lambda)),
      lambda <- 1.0/mu,
      log(mu) <- a[color_id],
      a[color_id] ~ normal(0,1)
    ), data = d_slim, chains = 4, cores = parallel::detectCores()
  )















