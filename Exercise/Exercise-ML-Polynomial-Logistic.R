##################### GLM: POLY LOG/REG ##########################

library(mda); library(pROC)

# Define function:
poly.logistic <- function(
  x = x,
  y = y,
  cutoff = .9, 
  degree = 3) {
  
  # Data
  all <- data.frame(cbind(y, x))
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  model <- polyreg(
    x = train.x,
    y = train.y
    #degree = degree
  )
  sum <- summary(model)
  
  # Make prediction on training:
  preds.train <- model$fitted.values
  preds.mean.train <- mean(preds.train)
  preds.train <- ifelse(preds.train > preds.mean.train, 1, 0)
  table.train <- as.matrix(
    cbind(preds.train,train.y)
  )
  tab.train <- table(table.train[,1], table.train[,2])
  percent.train <- sum(diag(tab.train))/sum(tab.train)
  
  # ROC
  actuals <- train.y
  scores <- preds.train
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc.train <- auc(roc_obj) 
  
  # Make prediction on testing:
  preds <- predict(model, test.x) # nrow(test.x)
  preds.mean <- mean(preds)
  preds <- ifelse(preds > preds.mean, 1, 0)
  table <- as.matrix(
    cbind(c(preds),test.y)
  )
  dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table)
  
  # ROC
  actuals <- test.y
  scores <- predict(model, test.x)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- auc(roc_obj)
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, predict(model, test.x))
  colnames(truth.vs.pred.prob) <- c("True Probability", "Predicted Probability")
  
  # Final output:
  return(
    list(
      Summary = sum,
      Training.Accuracy = percent.train,
      Training.AUC = auc.train,
      y.hat = preds,
      y.truth = test.y,
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      AUC = auc,
      Truth.vs.Predicted.Probabilities = truth.vs.pred.prob
      #AUC.Plot = plot(
      #  1 - spec, sens, type = "l", col = "red", 
      #  ylab = "Sensitivity", xlab = "1 - Specificity")
    )
  )
} # End of function

######################## Data: Discrete ##########################

# Set seed
set.seed(1)

# Create data
m <- 12
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

###################### RUN FUNCTION ############################

# Run:
Log.Result <- poly.logistic(
  x = all[, -1],
  y = all[, 1],
  cutoff = 0.9, 
  degree = 3)
Log.Result$Training.Accuracy; Log.Result$Training.AUC;
Log.Result$Testing.Accuracy; Log.Result$AUC