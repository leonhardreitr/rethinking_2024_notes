# Week 3 Homework ---------------------------------------------------------

# Leonhard Reiter
# leonhard.reiter@univie.ac.at

library(rethinking)
library(tidyverse)
library(ggdag)
library(cowplot)
library(brms)

theme_set(theme_cowplot())
data("foxes")
d <- foxes

glimpse(d)

# 1 Use the backdoor criterion and estimate the total causal influence of A on F. What effect would increasing the area of a territory have on the amount of food inside it?
coords <- list(
  x = c(A = 2, F = .5, G = 3, W = 2),
  y = c(A = 2, F = 0, G = 0, W = -1)
)

fox_dag <-
  dagify(F ~ A,
    G ~ F,
    W ~ G + F,
    coords = coords
  )

ggdag(fox_dag) + theme_dag() + labs(
  title = "We only need to adjust for Area!"
)

d <-
  d |>
  mutate(
    A = standardize(area),
    F = standardize(avgfood),
    W = standardize(weight),
    G = standardize(groupsize)
  )


#### Prior stuff ####
# once again: Not a biologist, but i will try my best

n <- 10
a <- rnorm(n, 0, .2)
b <- runif(n, 0, .5)

plot(
  NULL,
  xlim = range(d$A), ylim = range(d$F),
  xlab = "Area", ylab = "Average Food",
  main = "Looks okay?"
)

for (i in 1:n) {
  abline(a[i], b[i], lwd = 3, col = 1)
}

m1 <- quap(
  alist(
    F ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

summary(m1)[2, 1]

b1 <-
  brm(
    data = d,
    family = gaussian,
    F ~ 1 + A,
    prior = c(
      prior(normal(0, .2), class = Intercept),
      prior(normal(0, .5), class = b),
      prior(exponential(1), class = sigma)
    ),
    chains = 4, core = 8
  )

bayestestR::describe_posterior(b1)

cat(
  "Area seems to have a huge effect, with it being",
  round(summary(m1)[2, 1], digits = 2)
)

# 2. Infer the total causal effect of adding food F to a territory on the weight W of foxes. Can you calculate the causal effect by simulating an intervention on food?

m2 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bF * F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

summary(m2)

b2 <-
  brm(
    data = d,
    family = gaussian,
    W ~ 1 + F,
    prior = c(
      prior(normal(0, .2), class = Intercept),
      prior(normal(0, .5), class = b),
      prior(exponential(1), class = sigma)
    ),
    chains = 4, core = 8
  )

bayestestR::describe_posterior(b2)

cat(
  "Seems to have only a small effect, with it being",
  round(summary(m2)[2, 1], digits = 2)
)

# 3.
# So i guess i need to adjust for G? To block the mediation

m3 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + bF * F + bG * G,
    a ~ dnorm(0, 0.2),
    c(bF, bG) ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)

summary(m3)


b3 <- 
  brm(
    data = d,
    family = gaussian,
    W ~ 1 + F + G,
    prior = c(
      prior(normal(0, .2), class = Intercept),
      prior(normal(0, .5), class = b),
      prior(exponential(1), class = sigma)
    ),
    chains = 4, core = 8
  )

bayestestR::describe_posterior(b3)

# So average food has a positive effect on weight and average group size has a negative So
# they basically cancel each other out
# lets check this by estimating the direct effect of food on group size

m4 <- quap(
  alist(
    G ~ dnorm(mu, sigma),
    mu <- a + bF * F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data = d
)
summary(m4)

b4 <- 
  brm(
    data = d,
    family = gaussian,
    G ~ 1 + F,
    prior = c(
      prior(normal(0, .2), class = Intercept),
      prior(normal(0, .5), class = b),
      prior(exponential(1), class = sigma)
    ),
    chains = 4, core = 8
  )

bayestestR::describe_posterior(b4)

# Seems to hold up, food has a strong effect on Group size
# So basicly more food more foxes, but not necessary more foxes more foxes
# Becaues if there is a lot of food, but also many foxes, they effect will be
# "eaten"