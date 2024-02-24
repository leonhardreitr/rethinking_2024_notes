library(rethinking)
library(ggdag)
library(tidyverse)

data("Trolley")
d <- Trolley
glimpse(d)

simplehist(d$response,col = "red3", main = "meh")


# x -> R <- S and also Education, Age and Gender

# ordered = cumulative
dat <- list(R = d$response, A = d$action, I = d$intention, C = d$contact, G = d$male)
m1 <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bA*A + bI*I + bC*C,
    c(bA,bI,bC) ~ normal(0,.5),
    alpha ~ normal(0,1)
  ), data = dat, cores = detectCores(), chains = 4
)

precis(m1,2)



m2 <- ulam(
  alist(
    R ~ dordlogit(phi, alpha),
    phi <- bA[G]*A + bI[G]*I + bC[G]*C,
    bA[G] ~ normal(0,.5),
    bI[G] ~ normal(0,.5), 
    bC[G] ~ normal(0,.5),
    alpha ~ normal(0,1)
  ), data = dat, cores = detectCores(), chains = 4
)

precis(m2,2)



edu_lev <- c(6,1,8,4,7,2,5,3)
library(gtools) # provides rdirichlet

nf <- 40
a <- 10
delta <- rdirichlet( nf , a=rep(a,7) )
delta <- rdirichlet( nf , a=1:7 )
ani.record(reset=TRUE)
for ( f in 1:nf) {
  
  plot( NULL , xlim=c(1,7) , ylim=c(0,0.4) , xlab="index" , ylab="probability" )
  
  if ( f > 1 ) {
    start <- max(f-3,1)
    for ( i in start:(f-1) )
      lines( 1:7 , delta[i,] , type="l" , lwd=4 , col=grau(0.25*i/f) )
  }
  
  lines( 1:7 , delta[f,] , type="b" , lwd=4 , col=2 )
  
  ani.record()
  
}

oopts = ani.options(interval = 0.3)
ani.replay()

# ani.saveqz(dpi=150)
# convert -alpha remove -background white -delay 20 -loop 0 frame*.png a_out.gif
# convert -delay 3 a_out.gif a_out.gifedu_ew <- edu_lev[d$edu]
dat$E <- edu_ew
dat$a <- rep(2,7)

mRXE <- ulam(
  alist(
    R ~ ordered_logistic( phi , alpha ),
    phi <- bE*sum( delta_j[1:E] ) + bA*A + bI*I + bC*C,
    alpha ~ normal( 0 , 1 ),
    c(bA,bI,bC,bE) ~ normal( 0 , 0.5 ),
    vector[8]: delta_j <<- append_row( 0 , delta ),
    simplex[7]: delta ~ dirichlet( a )
  ), data=dat , chains=4 , cores=4 )

precis(mRXE,2)
