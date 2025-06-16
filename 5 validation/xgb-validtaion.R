library(Metrics)
library(Matrix)
library(xgboost)


data <- read.csv("data.csv", encoding = "UTF-8")
validation <- read.csv("validation1.csv", encoding = "UTF-8")
L <- nrow(data)


rmse_test <- matrix(0, nrow = 1, ncol = 1000)
r2_test <- matrix(0, nrow = 1, ncol = 1000)
pred <- matrix(NA, nrow = nrow(validation), ncol = 1000)

for (a in 1:1000) {
  set.seed(a)
  par <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
  train <- data[par == 1, ]
  test <- data[par == 2, ]
  
  print(paste("--- Iteration a =", a, "---")) 
  

  print(paste("Original nrow(train):", nrow(train))) 
  if (nrow(train) == 0) {
    print("WARNING: train dataset is empty for this iteration!")

  }
  
  train_matrix <- sparse.model.matrix(TCF ~ . - 1, data = train, na.action = na.pass)
  train_label <- train$TCF
  
  print(paste("nrow(train_matrix):", nrow(train_matrix)))
  print(paste("length(train_label):", length(train_label)))
  

  if (nrow(train_matrix) != length(train_label)) {
    print("ERROR: Mismatch in train_matrix rows and train_label length!")

  }
  

  print(paste("Original nrow(validation):", nrow(validation)))
  validation_matrix <- sparse.model.matrix(TCF ~ . - 1, data = validation, na.action = na.pass)
  validation_label <- validation$TCF
  
  print(paste("nrow(validation_matrix):", nrow(validation_matrix)))
  print(paste("length(validation_label):", length(validation_label)))
  

  if (nrow(validation_matrix) != length(validation_label)) {
    print("ERROR: Mismatch in validation_matrix rows and validation_label length!")

  }
  

  if (nrow(train) > 0) { 
    dtrain <- xgb.DMatrix(data = train_matrix, label = train_label, missing = NA)
    dvalidation <- xgb.DMatrix(data = validation_matrix, label = validation_label, missing = NA)
    

    set.seed(a)
    xgb <- xgboost(data = dtrain, max_depth = 2, eta = 0.1, verbose = 0,
                   objective = 'reg:squarederror', nround = 300)
    

    ptest <- predict(xgb, dvalidation)
    rmse_test[1, a] <- rmse(validation$TCF, ptest)
    
    R2b <- data.frame(ptest = ptest, TCF = validation$TCF)
    lb <- lm(TCF ~ ptest, data = R2b)
    r2_test[1, a] <- summary(lb)$r.squared
    
    pred[, a] <- ptest
  } else {

    rmse_test[1, a] <- NA
    r2_test[1, a] <- NA

    print(paste("Skipping training for iteration a =", a, "due to empty train set."))
  }
  
  print(paste('over：', a))
}


ad <- "/validation/"
write.csv(rmse_test, paste0(ad, "rmse_test.csv"), row.names = FALSE)
write.csv(r2_test, paste0(ad, "r2_test.csv"), row.names = FALSE)
write.csv(pred, paste0(ad, "pred_test.csv"), row.names = FALSE)

print("over")

