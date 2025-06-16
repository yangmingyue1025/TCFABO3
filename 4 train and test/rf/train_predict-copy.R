library(Metrics)
library(randomForest)

data <- read.csv("data.csv", encoding = "UTF-8")
L <- dim(data)[1]

# create the output matrices
rmse_train <- matrix(0, nrow = 1, ncol = 1000)
r2_train <- matrix(0, nrow = 1, ncol = 1000)
rmse_test <- matrix(0, nrow = 1, ncol = 1000)
r2_test <- matrix(0, nrow = 1, ncol = 1000)
pred <- matrix(0, nrow = L, ncol = 1000)
pred2 <- matrix(0, nrow = L, ncol = 1000)

a<- 1
for (a in c(1:1000)){
  
  set.seed(a)
  par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
  train <- data[par==1,]
  test <- data[par==2,]
  
  set.seed(a)
  rf <- randomForest(TCF~., train,
                     ntree = 250, maxnodes = 27)
  
  ### training and testing results ###
  ptrain <- predict(rf, train)
  rmse_train[1,a] <- rmse(train$TCF,ptrain) # RMSE of the training data set
  
  R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
  R2a[,1] <- ptrain
  R2a[,2] <- train$TCF
  R2a <- as.data.frame(R2a)
  names(R2a)[1] <- "ptrain"
  names(R2a)[2] <- "TCF"
  la <- lm(TCF~.,R2a)
  r2_train[1,a] <- as.numeric(summary(la)["r.squared"]) # R2 of the training data set
  
  ptest <- predict(rf, test)
  rmse_test[1,a] <- rmse(test$TCF,ptest) # RMSE of the testing data set
  
  R2b <- matrix(0, nrow = length(ptest), ncol = 2)
  R2b[,1] <- ptest
  R2b[,2] <- test$TCF
  R2b <- as.data.frame(R2b)
  names(R2b)[1] <- "ptest"
  names(R2b)[2] <- "TCF"
  lb <- lm(TCF~.,R2b)
  r2_test[1,a] <- as.numeric(summary(lb)["r.squared"]) # R2 of the testing data set
  
  
  # prediction of the testing set (did not involve in the current model training)
  pp <- as.matrix(ptest)
  k=1
  for (k in c(1:L)){
    if (k %in% row.names(pp)) {pred[k,a] = pp[row.names(pp) == k]}
    else {pred[k,a] =NA}
    
    k = k+1
  }
  
  pp <- as.matrix(ptrain)
  k=1
  for (k in c(1:L)){
    if (k %in% row.names(pp)) {pred2[k,a] = pp[row.names(pp) == k]}
    else {pred2[k,a] =NA}
    
    k = k+1
  }
  
  print(paste('over：', a))
  a = a+1
}

ad <- "/rf/"     # change the ##output file path## into the real output file path

write.csv(rmse_train, paste(ad, "rmse_train.csv"))
write.csv(r2_train, paste(ad, "r2_train.csv"))
write.csv(rmse_test, paste(ad, "rmse_test.csv"))
write.csv(r2_test, paste(ad, "r2_test.csv"))
write.csv(pred, paste(ad, "pred_test.csv"))
write.csv(pred2, paste(ad, "pred_train.csv"))

