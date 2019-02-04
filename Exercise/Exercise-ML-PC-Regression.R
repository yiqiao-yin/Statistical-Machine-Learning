####################### GLM: PC LOGISTIC #########################

# Define function:
principle.component.regression <- function(
  x = x,
  y = y,
  cutoff = 0.9) {
  
  # Data
  all <- data.frame(cbind(y,x))
  
  # Split
  X <- princomp(all[,-1])
  all <- data.frame(cbind(all[,1], X$scores))
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Model Fitting:
  model <- glm(
    train.y ~.,
    data=train.x,
    family = gaussian
  )
  summary(model)
  
  # Training error
  preds.train <- predict(model, train.x)
  preds.train <- data.frame(preds.train)
  mse.train <- sum((preds.train - as.matrix(train.y))^2)/nrow(train.x)
  
  # Make prediction:
  preds <- predict(model, test.x) # nrow(test.x)
  preds <- data.frame(preds); dim(preds)
  mse <- sum((preds - as.matrix(test.y))^2)/nrow(test.x)
  
  # Final output:
  return(
    list(
      Train.MSE = mse.train,
      Test.MSE = mse
    )
  )
} # End of function

######################## Data: Continuous ##########################

# Set seed
set.seed(1)

# Create data
m <- 12
n <- 100

# I-score Parameter:
# Explanatory variable:
x = data.frame(cbind(
  matrix(rnorm(m*n, mean=0, sd=1),nrow=n)
))

# Response variable:
#y <- x$X1 + x$X2
#y <- (1/3) * x$X1 + (1/5) * x$X2
#y <- x$X1^2 + x$X2^2
#y <- x$X1 * x$X2
#y <- exp(x$X1 * x$X2)
#y <- sin(x$X1 * x$X2)
#y <- x$X1 + x$X2 + x$X3
#y <- x$X1 * x$X2 + x$X3 * x$X4
#y <- sin(x$X1 * x$X2) + cos(x$X3 * x$X4)
#y <- sin(x$X1 * x$X2) + exp(x$X3 * x$X4)
y <- 1/(1+exp(-x$X1-x$X2-x$X3))

# Binary response
#y <- ifelse(y > mean(y), 1, 0)

# Data frame:
df <- data.frame(cbind(y,x))
all = df; print(c("Dim = (row x col) = ", dim(all)))
all[1:5,1:3]; dim(all)

######################### RUN FUNCTION ############################

# Try:
model <- principle.component.regression(
  x = all[, -1], 
  y = all[, 1], 
  cutoff = 0.9)
model$Training.Accuracy
model$Testing.Accuracy