library(rethinking)
library(tidyverse)
library(tinytable)
options(digits = 3) # how many significant digits to print by default


N <- 100
s <- rnorm(N)
h <- rnorm(N, s*0.5)

d <- rbern(N, .5)
hstar <- h
hstar[d == 1] <- NA

tibble(s, h, d) |> 
  mutate(d = as.character(d)) |> 
  ggplot(aes(s,h)) +
  geom_point(shape = 1, aes(col = d)) +
  geom_smooth(method = "lm", se = F, aes( col = d)) +
  scale_color_manual("Did dog ate?", values = c("red1", "darkred"),
                     labels = c("No", "Yes")) +
  hrbrthemes::theme_ipsum()
  
  
data("Primates301")
d <- Primates301
d
dd <- d |> 
complete(brain)
d[complete()]