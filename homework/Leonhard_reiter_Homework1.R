########### Home work week 1 ###############
# Leonhard Reiter
# leonhard.reiter@univie.ac.at
library(tidyverse)
library(rethinking)
library(tinytable)

# 1. Suppose the globe tossing data (Lecture 2, Chapter 2) had turned
# out to be 3 water and 11 land. Construct the posterior distribution
n <- 11
w <- 3
l  <- 11

(
  d <-
    tibble(p_grid = seq(from = 0, to = 1, length.out = n),
           # note the flat uniform prior
           prior  = 1) %>% 
    mutate(likelihood = dbinom(w, size = n, prob = p_grid)) %>% 
    mutate(posterior = (likelihood * prior) / sum(likelihood * prior))
) |> tt()


# 2. Using the posterior distribution from 1,
# compute the posterior predictive distribution for the next 5 tosses of the same globe
# Define the function to simulate 5 additional tosses
p <- rbeta(1e4,3+1,11+1)
w_simu <- rbinom(1e4, size = 5, prob = p)
simplehist(w_simu)
