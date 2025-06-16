# install.packages("Metrics")
library("xgboost")
library("Matrix")
library("Metrics")
library(data.table)


dire = "/Users/yangmingyue/Desktop/ABO365/特征组合筛选/特征组合文件/" 
file_list = list.files(path = dire, pattern = "*.csv$",recursive = TRUE,full.names = TRUE)
file_name_list <- list.files(dire, pattern = NULL, all.files = FALSE, full.names = FALSE, recursive =  FALSE) 

feature <- read.csv("/Users/yangmingyue/Desktop/ABO365/特征组合筛选/feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"
L <- dim(feature)[1]
TCF <- data[,ncol(data)]


para <- read.csv("/Users/yangmingyue/Desktop/ABO365/特征组合筛选/xgb/r2_te.csv", encoding = "UTF-8")


rmse_train <- matrix(0, nrow = L, ncol = 1000)
r2_train <- matrix(0, nrow = L, ncol = 1000)
rmse_test <- matrix(0, nrow = L, ncol = 1000)
r2_test <- matrix(0, nrow = L, ncol = 1000)

b = 1
for (b in c(1:L)){
  

  max_depth = para[b,3]
  eta = para[b,4]

  name <- substr(file_name_list[b], 1, nchar(file_name_list[b])-4) #获取对应".csv"文件的名称
  name <- (feature[b,1])
  file_name = paste(name,"csv", sep = ".")
  data <- read.csv(paste(dire,file_name, sep = "/" ), encoding = 'UTF-8') #导入对应特征组合的数据
  
  a = 1
  for (a in c(1:1000)){

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
    
    #模型训练
    set.seed(a)
    xgb <- xgboost(data = dtrain,max_depth=max_depth, eta=eta, verbose = 0,
                   objective='reg:squarederror', nround=300) # 公式需要变化这次应该用的300（223）
    

    ptrain <- predict(xgb, dtrain)
    rmse_train[b,a] <- rmse(train$TCF,ptrain) # 训练集 RMSE
    R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
    R2a[,1] <- ptrain
    R2a[,2] <- train$TCF
    R2a <- as.data.frame(R2a)
    names(R2a)[1] <- "ptrain"
    names(R2a)[2] <- "TCF"
    la <- lm(TCF~.,R2a)
    r2_train[b,a] <- as.numeric(summary(la)["r.squared"])
    
    

    ptest <- predict(xgb, dtest)
    rmse_test[b,a] <- rmse(test$TCF,ptest) # 测试集 RMSE
    R2b <- matrix(0, nrow = length(ptest), ncol = 2)
    R2b[,1] <- ptest
    R2b[,2] <- test$TCF
    R2b <- as.data.frame(R2b)
    names(R2b)[1] <- "ptest"
    names(R2b)[2] <- "TCF"
    lb <- lm(TCF~.,R2b)
    r2_test[b,a] <- as.numeric(summary(lb)["r.squared"])
    
    r2_test
    

    a = a+1
  }
  b = b+1
}

write.csv(rmse_train,"/Users/yangmingyue/Desktop/ABO365/特征组合筛选/xgb/超参数优化后计算结果/rmse_train.csv")
write.csv(r2_train,"/Users/yangmingyue/Desktop/ABO365/特征组合筛选/xgb/超参数优化后计算结果/r2_train.csv")
write.csv(rmse_test,"/Users/yangmingyue/Desktop/ABO365/特征组合筛选/xgb/超参数优化后计算结果/rmse_test.csv")
write.csv(r2_test,"/Users/yangmingyue/Desktop/ABO365/特征组合筛选/xgb/超参数优化后计算结果/r2_test.csv")


