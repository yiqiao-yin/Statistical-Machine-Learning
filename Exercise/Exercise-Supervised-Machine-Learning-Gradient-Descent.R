###################### GRAD DESCENT: CLASSIFICATION ###########################

# Library
library(pROC)

# Define function:
grad.descent.classification <- function(
  x = x,
  y = y,
  cutoff = 0.9, 
  cutoff.coefficient = 1,
  alpha = 0.01,
  num_iters = 1000) {
  
  # Data
  all <- data.frame(cbind(y,x))
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- data.frame(train[,-1]); colnames(train.x) <- colnames(train)[-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- data.frame(test[,-1]); dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Gradient Descent:
  # squared error cost function
  cost <- function(X, y, theta) {
    sum( (as.matrix(X) %*% as.matrix(theta) - y)^2 ) / (2*length(y))
  }
  
  # learning rate and iteration limit
  #alpha <- 0.01
  #num_iters <- 1000
  
  # keep history
  cost_history <- double(num_iters)
  theta_history <- list(num_iters)
  
  # initialize coefficients
  number.of.coeff <- ncol(train.x)
  theta <- matrix(rep(0,number.of.coeff+1), nrow=(number.of.coeff+1))
  
  # add a column of 1's for the intercept coefficient
  X <- cbind(1, train.x)
  
  # gradient descent
  for (i in 1:num_iters) {
    error <- (as.matrix(X) %*% as.matrix(theta) - train.y)
    delta <- t(X) %*% as.matrix(error) / length(train.y)
    theta <- theta - alpha * delta
    cost_history[i] <- cost(X, train.y, theta)
    theta_history[[i]] <- theta
  } # Finished Gradient Descent
  
  # Make prediction on training:
  preds.train.prob <- as.matrix(X) %*% as.matrix(theta)
  preds.mean.train <- mean(preds.train.prob)
  preds.train <- ifelse(preds.train.prob > cutoff.coefficient*preds.mean.train, 1, 0)
  table.train <- as.matrix(
    cbind(preds.train,train.y)
  )
  tab.train <- table(table.train[,1], table.train[,2])
  percent.train <- sum(diag(tab.train))/sum(tab.train)
  
  # ROC
  actuals <- train.y
  scores <- preds.train.prob
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj$auc
  
  # Make prediction on testing:
  colnames(test.x) <- colnames(train.x)
  X.new <- cbind(1, test.x)
  preds.prob <- as.matrix(X.new) %*% as.matrix(theta)
  preds.mean <- mean(preds.prob)
  preds <- ifelse(preds.prob > cutoff.coefficient*preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds,test.y)
  )
  dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table)
  
  # ROC
  actuals <- test.y
  scores <- preds
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, preds.prob)
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Weights = theta,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      y.hat = preds,
      y.truth = test.y,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Gini = auc*2 - 1
    )
  )
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

####################### RUN FUNCTION #############################

# Run
Result <- grad.descent.classification(
  x = all[, -1],
  y = all[, 1],
  cutoff = 0.9, 
  cutoff.coefficient = 1,
  alpha = 0.01,
  num_iters = 1000
)

# View
Result$Weights
Result$AUC
Result$Prediction.Table
Result$Testing.Accuracy