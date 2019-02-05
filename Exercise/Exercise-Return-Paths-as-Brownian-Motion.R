##################### BEGIN SCRIPT #######################

# This script let us create two GIF:
# (1) Simple 2D brownian motion;
# (2) 2D random walk using stocks

# Library
library(animation)

#################### BROWNIAN MOTION #######################

## Plot brownian motion
saveGIF({
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, movie.name = "brownian_motion.gif", interval = 0.1, nmax = 30, 
ani.width = 600)

################# WEEK/MONTH RETURNS BROWNIAN MOTION STYLE ##################

# Define a function that calculates returns
All.Indice.3D.Enter <- function(
  a,b,c,d, e,f,g,h
) {
  # Data
  data.list <- list(
    a,b,c,d, e,f,g,h
  )
  all <- matrix(NA, nrow = 8, ncol = 4)
  
  # Update Momentum:
  for (i in c(1:nrow(all))){all[i,1] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-5),4])-1}
  for (i in c(1:nrow(all))){all[i,2] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25),4])-1}
  for (i in c(1:nrow(all))){all[i,3] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-25*3),4])-1}
  for (i in c(1:nrow(all))){all[i,4] <- (data.frame(data.list[i])[nrow(data.frame(data.list[i])),4])/(data.frame(data.list[i])[(nrow(data.frame(data.list[i]))-252),4])-1}
  
  # Update column names:
  colnames(all) <- c("Pre 5-Days",
                     "Pre 30-Days", 
                     "Pre Quarter",
                     "Pre Year")
  df <- data.frame(all)
  df
} # End of function

# Stock gif
library(quantmod)
getSymbols(c("AAPL", "MSFT", "GOOGL", "NVDA",     "AMZN", "GS", "LMT", "BA"),
           to = paste0("2018-",01,"-01")); head(AAPL); tail(AAPL)

# Test
data <- All.Indice.3D.Enter(AAPL, MSFT, GOOGL, NVDA,    AMZN, GS, LMT, BA)
rownames(data) <- c("AAPL", "MSFT", "GOOGL", "NVDA",     "AMZN", "GS", "LMT", "BA"); data

# Write GIF
saveGIF({
  for (month in as.character(rep(1:12))) {
    getSymbols(c("AAPL", "MSFT", "GOOGL", "NVDA",     "AMZN", "GS", "LMT", "BA"),
               to = paste0("2018-",month,"-01")) # ; head(AAPL); tail(AAPL)
    data <- All.Indice.3D.Enter(AAPL, MSFT, GOOGL, NVDA,    AMZN, GS, LMT, BA)
    rownames(data) <- c("AAPL", "MSFT", "GOOGL", "NVDA",     "AMZN", "GS", "LMT", "BA"); data
    plot(data$Pre.5.Days~data$Pre.30.Days,
         xlim = c(-.3,.3), ylim = c(-0.1,0.1),
         main = paste0("Week/Month Returns Using Data Up to 2018-",month,"-01"),
         xlab = "Last Month Returns", ylab = "Last Week Returns",
         data = data[,c(1,2)], pch = 2)
    with(data[,c(1,2)], text(data$Pre.5.Days~data$Pre.30.Days,
                             labels = row.names(data[, c(1,2)]), pos = 4))
    if (as.numeric(month) %in% c(2)) {
      txt <- "Invisible Hand as Boundary!!"; txt.color = "green";
      mtext(text = txt, side = c(3,4), line = 0.5, cex = 1, font = 1, col = txt.color)}
    if (as.numeric(month) %in% c(5)) {
      txt <- "Invisible Hand as Boundary!!"; txt.color = "green";
      mtext(text = txt, side = 1, line = 0.5, cex = 1, font = 1, col = txt.color)}
    if (as.numeric(month) %in% c(11)) {
      txt <- "Invisible Hand as Boundary!!"; txt.color = "green";
      mtext(text = txt, side = 2, line = 0.5, cex = 1, font = 1, col = txt.color)}
  }
}, movie.name = paste0("2018-",month,"-01-BM-style", ".gif"),
interval = 0.8, nmax = 30, 
ani.width = 600)


#################### END SCRIPT ########################