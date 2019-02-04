################## MASS: STEP AIC VARIABLE SELECTION ########################

# Load package
library(MASS); library(leaps)

# Define function
step.AIC.VS <- function(
  x = x,
  y = y,
  cutoff = 0.9,
  detect.number.interaction = 10,
  direction = "exhaustive") {
  
  # Data
  all <- data.frame(cbind(y,x))
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Compare the performance of the different models
  detect.number.interaction <- detect.number.interaction
  model <- regsubsets(
    train.y~., 
    data = train, 
    nvmax = detect.number.interaction, 
    method = direction, # method=c("exhaustive","backward", "forward", "seqrep")
    really.big = T
  )
  sum <- summary(model); 
  #apply(cbind(1:5), 1, function(i) which(sum$outmat[i,] == sum$outmat[1,1]))
  selected.variable <- rownames(data.frame(which(sum$outmat[detect.number.interaction,] == sum$outmat[1,1])))
  selected.variable = selected.variable[-1]
  
  # Clean up (in case there are NA output)
  if (length(selected.variable) > 0) {
    selected.variable = selected.variable
  } else {
    selected.variable = colnames(x)
  }
  
  # Output
  return(list(
    Summary = sum,
    Final.Variable = selected.variable
  ))
} # End of function

######################## Data: Discrete ##########################

# Set seed
set.seed(1)

# Create data
m <- 20
n <- 200

# Explanatory variable:
x = data.frame(cbind(
  matrix(runif(m*n, min = 0, max = 1),nrow=n)
))
x <- round(x, 0)

# Response variable:
y <- (x$X1 + x$X2) %% 2
#y <- x$X1^2 + x$X2^2 %% 1
#y <- x$X1 * x$X2 %% 2
#y <- ifelse(exp(x$X1 * x$X2) %% 1 > .5, 1, 0)
#y <- ifelse(sin(x$X1 * x$X2) %% 1 > .5, 1, 0)
#y <- (x$X1 + x$X2 + x$X3) %% 2
#y <- (x$X1 * x$X2 + x$X3 * x$X4) %% 2
#y <- (sin(x$X1 * x$X2) + cos(x$X3 * x$X4)) %% 1; y <- ifelse(y > .5, 1, 0)

# Data frame:
df <- data.frame(cbind(y,x))
all = df; print(c("Dim = (row x col) = ", dim(all)))
all[1:5,1:3]; dim(all)

######################### Run ##################################

# Run
Result <- step.AIC.VS(
  x = all[,-1],
  y = all[, 1],
  cutoff = 0.9,
  detect.number.interaction = 3,
  direction = "exhaustive")
Result$Summary
Result$Final.Variable
