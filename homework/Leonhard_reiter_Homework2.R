# Home Work week 2 --------------------------------------------------------

# Leonhard Reiter
# leonhard.reiter@univie.ac.at
library(tidyverse)
library(rethinking)
library(tinytable)
library(ggdag)
library(patchwork)

theme_set(theme_bw())

data("Howell1")

# select young people
d <- Howell1 |> 
  filter(age < 13)

dag_coords <-
  tibble(name = c("A", "H", "W"),
         x    = c(1, 3, 2),
         y    = c(2, 2, 1))
dag <- 
dagify(
  H ~ A,
  W ~ A + H, coords = dag_coords)

ggdag(dag) + theme_dag()

# our generative model
sim_model <- function(A, bAH = 5, bAW = .1, bHW = .5) {
    N <- length(A)
    H <- rnorm(N, bAH*A, 2)
    W <- rnorm(N, bHW*H + bAW*A,2)
   data.frame(A,H,W)
  }

d <- sim_model(runif(20,1,12))

# fake people plot
p1 <- 
d |> 
  ggplot(aes(A,W)) +
  geom_smooth(se = F, col = "black") + geom_point(shape = 1, size = 3) +
  labs(x = "Age",
       y = "Weight")

p2 <- 
  d |> 
  ggplot(aes(A,H)) +
  geom_smooth(se = F, col = "black") + geom_point(shape = 1, size = 3) +
  labs(x = "Age",
       y = "Height")

p3 <- 
  d |> 
  ggplot(aes(H,W)) +
  geom_smooth(se = F, col = "black") + geom_point(shape = 1, size = 3) +
  labs(x = "Height",
       y = "Weight")

((p1+p2) / p3) + ggtitle("Probably too linear")

# Estimate the total causal effect of each year of growth on weight
# make up some priors
# I am not a developmental psychologist, just a normal one, so i will go 
# with weakly informative prior

d <- Howell1 |> 
  filter(age < 13)

n <- 11
a <- rnorm(n, 4.5,1)
b <- runif(n,0,12)

plot(
  NULL,
  xlim = range(d$age), ylim = range(d$weight),
  xlab = "Age", ylab = "Weight"
)

for (i in 1:n) {
  abline(a[i], b[i], lwd = 3, col = 1)
}

# ok not perfect, some kids that are too heavy or too light, but it will do
# time to model

mod <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*A,
    a ~ dnorm(4.5,1),
    b ~ dnorm(0,12),
    sigma ~ dexp(1)
  ), data = list(W = d$weight, A = d$age)
)

precis(mod)

# okay f it, fire up brms

library(brms)

mod_brms <- 
  brm(
    data = d,
    family = gaussian,
    weight ~ 1 + age,
    prior = c(
      prior(normal(4.5,1), class = Intercept),
      prior(normal(0,12), class = b),
      prior(exponential(1), class = sigma)
    ),
    chains = 4, cores = 5, iter = 2000, warmup = 800
  )

bayestestR::describe_posterior(mod_brms)
