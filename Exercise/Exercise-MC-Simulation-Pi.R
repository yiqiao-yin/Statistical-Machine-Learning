################### MONTE CARLO SIMULATION OF PI #########################

## Define a function to output a plot of pi
piR.plot <- function(N) {
  x <- runif(N)
  y <- runif(N)
  d <- sqrt(x^2 + y^2)
  label <- ifelse(d < 1, 1, 0)
  plot(x,y,col=label+1,main=paste0("Simulation of Pi Using N=",N))
}

## Plot Monte Carlo Simulation of Pi
library(animation)
saveGIF({
  for (i in c(100,200,500,1000,2000,5000,10000,15000,20000)) {piR.plot(i)}
}, movie.name = "brownian_motion.gif", interval = 0.3, nmax = 30, 
ani.width = 500)
