###################### iRF: ITERATIVE RANDOM FOREST ############################

# Library
library("iRF"); library("pROC")

# Define function:
iter.Random.Forest <- function(
  x = x,
  y = y,
  cutoff = .9,
  num.tree = 5,
  num.iter = 5
) {
  
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
  
  # Modeling fitting:
  model <- iRF(
    x = as.matrix.data.frame(train.x),
    y = as.factor(train.y),
    xtest = as.matrix.data.frame(test.x),
    ytest = as.factor(test.y),
    n.iter = num.iter,
    ntree = num.tree,
    verbose = FALSE
  )
  sum <- model$rf.list[[num.iter]]
  
  # Make prediction on training:
  preds.train <- sum$predicted  # colMeans(model$yhat.train)
  preds.train[is.na(preds.train) == TRUE] <- 0
  #preds.mean.train <- mean(preds.train)
  #preds.train <- ifelse(preds.train > preds.mean.train, 1, 0)
  table.train <- as.matrix(
    cbind(preds.train,train.y)
  )
  tab.train <- table(table.train[,1], table.train[,2]); tab.train
  percent.train <- sum(diag(tab.train))/sum(tab.train); percent.train
  
  # ROC
  actuals <- train.y
  scores <- as.numeric(preds.train)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc.train <- roc_obj$auc
  
  # Make prediction on testing:
  preds <- sum$test$predicted # colMeans(model$yhat.test)
  #preds.mean <- mean(preds)
  #preds <- ifelse(preds > preds.mean, 1, 0)
  table <- as.matrix(
    cbind(preds,test.y)
  ); dim(table); head(table)
  
  # Compute accuracy:
  table <- table(table[,1],table[,2]); table
  percent <- sum(diag(table))/sum(table); percent
  
  # ROC
  actuals <- test.y
  scores <- sum$test$votes[,2] # colMeans(model$yhat.test)
  roc_obj <- roc(response = actuals, predictor =  scores)
  auc <- roc_obj$auc
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, sum$test$votes[,2])
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
      Gini = auc*2 - 1, 
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

####################### RUN FINCTION #############################


# Run
model.result <- iter.Random.Forest(
  x = all[, -1],
  y = all[, 1],
  cutoff = .9,
  num.tree = 5,
  num.iter = 5
)
model.result$Training.Accuracy; model.result$Training.AUC
model.result$Prediction.Table
model.result$Testing.Accuracy; model.result$AUC; model.result$Gini
