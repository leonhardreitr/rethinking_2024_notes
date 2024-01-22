library(rethinking)
library(tidyverse)
library(easystats)

# Ockhams Razor goes brr brr

# Struggle against causation
# How to design estimators

# Struggle against data
# How to use estimators

theme_set(theme_abyss())

data(milk)
d <- milk

glimpse(d)

d |> 
  ggplot(aes(mass, neocortex.perc)) +
  geom_point(shape = 1, size = 3, color = "white") +
  geom_smooth(se = F, col = "white", formula = y ~ x) 

# LOOCV

# Fit a function
# Drop one oberservations
# Fit line to remaining
# Predict dropped line
# Repeat by dropping another point
# Score is error on dropped

# Score in (Fit) vs. Predictive Accuracy

library(ggdag)
dagify(H ~ t,
       f~ t,
       H ~ f,
       H ~ h2)  |> dagitty::impliedConditionalIndependencies()


library(brms)

data("WaffleDivorce")

m1 <- 
  brm(
    data = WaffleDivorce,
    family = student(),
    Divorce ~ 1 + MedianAgeMarriage + Marriage,
    prior = c(
      prior(normal(0,.2), class = Intercept),
      prior(normal(0,.5), class = b)
    )
  )

model_parameters(m1) 
