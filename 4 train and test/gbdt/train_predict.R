library(Metrics)
library(gbm)


data <- read.csv("data.csv", encoding = "UTF-8")
L <- dim(data)
str(data)
# create the output matrices
rmse_train <- matrix(0, nrow = 1, ncol = 1000)
r2_train <- matrix(0, nrow = 1, ncol = 1000)
rmse_test <- matrix(0, nrow = 1, ncol = 1000)
r2_test <- matrix(0, nrow = 1, ncol = 1000)
pred <- matrix(0, nrow = L, ncol = 1000)
pred2 <- matrix(0, nrow = L, ncol = 1000)


for (a in c(1:1000)){
  
  set.seed(a)
  par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
  train <- data[par==1,]
  test <- data[par==2,]
  
  set.seed(a)
  gbm1 <- gbm(TCF~., data=train,                                                                   
              var.monotone= rep(0,3),
              distribution="gaussian",        # see the help for other choices  
              n.trees=600,                  # number of trees  
              shrinkage=0.05,            # shrinkage or learning rate, 0.001 to 0.1 usually work
              interaction.depth=3,            # 1: additive model, 2: two-way interactions, etc.  
              bag.fraction = 0.5,             # subsampling fraction, 0.5 is probably best  
              train.fraction = 1,           # fraction of data for training, first train.fraction*N used for training  
              n.minobsinnode = 3,            # minimum total weight needed in each node  
              # cv.folds = 10,                # do 10-fold cross-validation  
              keep.data=TRUE,                 # keep a copy of the dataset with the object  
              verbose=FALSE,                  # don't print out progress  
              n.cores=3
  )
  
  ptrain <- predict(gbm1, train)
  rmse_train[1,a] <- rmse(train$TCF,ptrain) # RMSE of the training data set
  
  R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
  R2a[,1] <- ptrain
  R2a[,2] <- train$TCF
  R2a <- as.data.frame(R2a)
  names(R2a)[1] <- "ptrain"
  names(R2a)[2] <- "TCF"
  la <- lm(TCF~.,R2a)
  r2_train[1,a] <- as.numeric(summary(la)["r.squared"]) # R2 of the training data set
  
  ptest <- predict(gbm1, test)
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
  p <- predict(gbm1, test)
  pp <- data.frame(p, row.names = row.names(test))
  pp <- as.matrix(pp)
  k=1
  for (k in c(1:L)){
    if (k %in% row.names(pp)) {pred[k,a] = pp[row.names(pp) == k]}
    else {pred[k,a] =NA}
    
    k = k+1
  }
  
  # prediction of the training set (did not involve in the current model training)
  p <- predict(gbm1, train)
  pp <- data.frame(p, row.names = row.names(train))
  pp <- as.matrix(pp)
  k=1
  for (k in c(1:L)){
    if (k %in% row.names(pp)) {pred2[k,a] = pp[row.names(pp) == k]}
    else {pred2[k,a] =NA}
    
    k = k+1
  }
  
  print(paste('over：', a))
  a = a+1
}
ad <- "/gbdt/"                      # change the ##output file path## into the real output file path

write.csv(rmse_train, paste(ad, "rmse_train.csv"))
write.csv(r2_train, paste(ad, "r2_train.csv"))
write.csv(rmse_test, paste(ad, "rmse_test.csv"))
write.csv(r2_test, paste(ad, "r2_test.csv"))
write.csv(pred, paste(ad, "pred_test.csv"))
write.csv(pred2, paste(ad, "pred_train.csv"))

