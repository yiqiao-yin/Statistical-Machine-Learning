######################### KERAS: NEURO NETWORK FCT #############################

# Package
library(keras); library(pROC)

# Define function
keras.nn <- function(
  x = x,
  y = y,
  cutoff = .8,
  validation_split = 0.8,
  batch_size = 128,
  l1.units = 256,
  l2.units = 128,
  l3.units = 64,
  epochs = 10
) {
  
  # Data
  all <- data.frame(cbind(y,x))
  
  # Setup
  x_train <- as.matrix(all[1:(cutoff*nrow(all)), ])
  y_train <- as.matrix(all[1:(cutoff*nrow(all)), 1])
  x_test <- as.matrix(all[(cutoff*nrow(all)+1):nrow(all), ])
  y_test <- as.matrix(all[(cutoff*nrow(all)+1):nrow(all), 1])
  dim(x_train); dim(y_train); dim(x_test); dim(y_test)
  
  # To prepare this data for training we one-hot encode the 
  # vectors into binary class matrices using the Keras to_categorical() function
  y_train <- to_categorical(y_train, 2)
  y_test <- to_categorical(y_test, 2)
  dim(x_train); dim(y_train); dim(x_test); dim(y_test)
  
  # Defining the Model
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = l1.units, activation = 'relu', input_shape = c(ncol(x_train))) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = l2.units, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = l3.units, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 2, activation = 'softmax')
  summary(model)
  
  # Next, compile the model with appropriate loss function, optimizer, and metrics:
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  # Training and Evaluation
  history <- model %>% fit(
    x_train, y_train, 
    epochs = epochs, 
    batch_size = batch_size, 
    validation_split = cutoff
  ); plot(history)
  
  # Evaluate the model's performance on the test data:
  # model %>% evaluate(x_test, y_test)
  
  # Generate predictions on new data:
  y_test_hat <- model %>% predict_classes(x_test)
  y_test <- as.matrix(all[(cutoff*nrow(all)+1):nrow(all), 1])
  confusion.matrix <- table(y_test_hat, y_test)
  test.acc <- sum(diag(confusion.matrix))/sum(confusion.matrix)
  all.error <- plyr::count(y_test - cbind(y_test_hat))
  
  # Return
  return(
    list(
      x_train = x_train,
      y_train = y_train,
      x_test = x_test,
      y_test = y_test,
      Training.Plot = plot(history),
      Confusion.Matrix = confusion.matrix,
      Testing.Accuracy = test.acc,
      All.Types.of.Error = all.error
    )
  )
} # End of function

######################## Data: Discrete ##########################

# Set seed
set.seed(1)

# Create data
m <- 200
n <- 10000

# Explanatory variable:
x = data.frame(cbind(
  matrix(runif(m*n, min = 0, max = 1),nrow=n)
))
x <- round(x, 0)

# Response variable:
#y <- (x$X1 + x$X2) %% 2
#y <- x$X1^2 + x$X2^2 %% 1
#y <- x$X1 * x$X2 %% 2
#y <- ifelse(exp(x$X1 * x$X2) %% 1 > .5, 1, 0)
#y <- ifelse(sin(x$X1 * x$X2) %% 1 > .5, 1, 0)
#y <- (x$X1 + x$X2 + x$X3) %% 2
#y <- (x$X1 * x$X2 + x$X3 * x$X4) %% 2
y <- (sin(x$X1 * x$X2) + cos(x$X3 * x$X4)) %% 1; y <- ifelse(y > .5, 1, 0)

# Data frame:
df <- data.frame(cbind(y,x))
all = df; print(c("Dim = (row x col) = ", dim(all)))
all[1:5,1:3]; dim(all)

######################## RUN NEURAL NETWEORK USING KERAS ######################

# Run
Result <- keras.nn(
  x = all[, -1], 
  y = all[, 1],
  cutoff = .8,
  batch_size = 128,
  l1.units = 256,
  l2.units = 128,
  l3.units = 64,
  epochs = 10
)

# View
Result$Confusion.Matrix; 
Result$Testing.Accuracy;
Result$All.Types.of.Error
