#install.packages("xgboost")
install.packages("ggplot2")
install.packages("Ckmeans.1d.dp")

library("xgboost")
library("Matrix")

data <- read.csv("your data.csv")
L <- length(data)
L
data
# Importance extraction-matrix, 1000 trials
im1 <- matrix(0, nrow = (L-1), ncol = 1)
im1[,1] <- colnames(data[1:(L-1)]) 
colnames(im1)[1]<-"Feature"
im1

a = 1

for (a in c(1:1000)){
  # train and test
  set.seed(a)
  par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
  train <- data[par==1,]
  test <- data[par==2,]
  train_matrix <- sparse.model.matrix(TCF ~ .-1, data = train)
  test_matrix <- sparse.model.matrix(TCF ~ .-1, data = test)
  train_label <- train$TCF
  test_label <-  test$TCF
  train_fin <- list(data=train_matrix,label=train_label)
  test_fin <- list(data=test_matrix,label=test_label) 
  dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label) 
  dtest <- xgb.DMatrix(data = test_fin$data, label = test_fin$label)
  set.seed(a)
  xgb <- xgboost(data = dtrain,max_depth= 3, eta=0.2,  verbose = 0,
                 objective='reg:squarederror', nround=300) # 公式需要变化
  #feature importance
  importance <- xgb.importance(train_matrix@Dimnames[[2]], model = xgb)
  xgb.ggplot.importance(importance)
  impor <- as.data.frame(importance)
  impor
  im2 <- matrix(0, nrow = nrow(impor), ncol = 2)
  im2[,1] <- impor[,1]
  im2[,2] <- impor[,5]
  colnames(im2)[2]<-"Importance"
  colnames(im2)[1]<-"Feature"
  im1 <- merge(im1,im2, by = "Feature", all = TRUE)
  im1[is.na(im1)]<-0
}

write.csv(im1, "xgb_im1.csv")

