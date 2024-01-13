library(rethinking)
library(tidyverse)
library(brms)
theme_set(theme_black())

data("Howell1")

ggplot(Howell1, aes(x = height, y = weight)) +
  geom_smooth(col = "white", se = F, method = "lm") +
  geom_point(shape = 1, col = "white")

d <- Howell1 |> 
  filter(age > 18)

d |> 
  ggplot(aes(x = height, y = weight)) +
  geom_smooth(col = "white", se = F, method = "lm") +
  geom_point(shape = 1, col = "white")

sim_weight <- function(H, b, sd){
  U <- rnorm(length(H),0, sd)
  W <- b*H + U
  return(W)
}
H <- runif(200, 130, 175)
sim_weight(H, .5, .5) -> W

tibble(H = H, W = W) |> 
  ggplot(aes(H,W)) +
  geom_point(shape = 1, col = "yellow")

# prior predictive check
n <- 1e3
a <- rnorm(n, 0, 10)
b <- rnorm(n,0, 1)

p <- 
  ggplot() +
  xlim(130, 170) + 
  ylim(50, 90)

for (j in 1:50) {
  p <- p + geom_abline(intercept = a[j], slope = b[j],
                       color = "white", size = 1,
                       alpha = .75)
}

print(p)


# validate model
set.seed(93)
H <- runif(10,130,170)
W <- sim_weight(H,.5,5)

b1 <- brm(
  W ~ H,
  family = gaussian,
  data = tibble(H = H, W = W)
)

bayestestR::describe_posterior(b1)

b2 <- brm(
  weight ~ height,
  family = gaussian,
  data = d
)

summary(b2)
conditional_effects(b2)
d |> 
  ggplot(aes(x = height, y = weight)) +
  geom_abline(intercept = fixef(b2)[1], 
              slope     = fixef(b2)[2], 
              col = "white") +
  geom_point(shape = 1, size = 2, color = "royalblue")
