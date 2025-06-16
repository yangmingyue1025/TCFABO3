# install.packages("Metrics")
library("xgboost")
library("Matrix")
library("Metrics")


dire = "/Users/yangmingyue/Desktop/ABO365/特征组合筛选/特征组合文件/"  
file_list = list.files(path = dire, pattern = "*.csv$",recursive = TRUE,full.names = TRUE)
file_name_list <- list.files(dire, pattern = NULL, all.files = FALSE, full.names = FALSE, recursive =  FALSE) 

feature <- read.csv("/Users/yangmingyue/Desktop/ABO365/特征组合筛选/feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"
L <- dim(feature)[1]

ad <- "/Users/yangmingyue/Desktop/ABO365/特征组合筛选/xgb/计算结果/"

for (tt in c(1:30)){

  b = 1
  
  aver1_tr <- matrix(0, nrow = 6, ncol = 10)   # 每一行代表一个max_depth，每一列代表eta
  aver2_tr <- matrix(0, nrow = 6, ncol = 10)
  aver1_te <- matrix(0, nrow = 6, ncol = 10)
  aver2_te <- matrix(0, nrow = 6, ncol = 10)
  
  for (b in c(1:L)){
    

    name <- substr(file_name_list[b], 1, nchar(file_name_list[b])-4) 
    name <- (feature[b,1])
    file_name = paste(name,"csv", sep = ".")
    data <- read.csv(paste(dire,file_name, sep = "/" ), encoding = 'UTF-8')
    
    nn <- 1
    
    for (max_depth in c(1,2,3,4,5,6)){
      
      mm <- 1
      
      for (eta in seq(from = 0.05, to = 0.5, by = 0.05)){
        

        rmse_train <- matrix(0, nrow = 1, ncol = 10)
        r2_train <- matrix(0, nrow = 1, ncol = 10)
        rmse_test <- matrix(0, nrow = 1, ncol = 10)
        r2_test <- matrix(0, nrow = 1, ncol = 10)
        
        t <- 1
        rmse_tr <- 0
        rmse_te <- 0
        r2_tr <- 0
        r2_te <- 0
        
        for (t in c(1:10)){
          

          set.seed(tt)
          par1 <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
          train1 <- data[par1==1,]
          test1 <- data[par1==2,]
          

          par <- sample(10, nrow(train1),replace = TRUE, prob = rep(0.1,10))
          
          train <- train1[par != t,]
          test <- train1[par == t,]
          
          train_matrix <- sparse.model.matrix(TCF ~ .-1, data = train)
          test_matrix <- sparse.model.matrix(TCF ~ .-1, data = test)
          train_label <- train$TCF
          test_label <-  test$TCF
          train_fin <- list(data=train_matrix,label=train_label)
          test_fin <- list(data=test_matrix,label=test_label) 
          dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label) 
          dtest <- xgb.DMatrix(data = test_fin$data, label = test_fin$label)
          

          set.seed(tt)
          xgb <- xgboost(data = dtrain,max_depth=max_depth, eta=eta,
                         objective='reg:squarederror', nround=300, verbose = 0) 

          ptrain <- predict(xgb, dtrain)
          rmse_train[1,t] <- rmse(train$TCF,ptrain) 
          R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
          R2a[,1] <- ptrain
          R2a[,2] <- train$TCF
          R2a <- as.data.frame(R2a)
          names(R2a)[1] <- "ptrain"
          names(R2a)[2] <- "TCF"
          la <- lm(TCF~.,R2a)
          r2_train[1,t] <- as.numeric(summary(la)["r.squared"])
          

          ptest <- predict(xgb, dtest)
          rmse_test[1,t] <- rmse(test$TCF,ptest) 
          R2b <- matrix(0, nrow = length(ptest), ncol = 2)
          R2b[,1] <- ptest
          R2b[,2] <- test$TCF
          R2b <- as.data.frame(R2b)
          names(R2b)[1] <- "ptest"
          names(R2b)[2] <- "TCF"
          lb <- lm(TCF~.,R2b)
          r2_test[1,t] <- as.numeric(summary(lb)["r.squared"])
          
          t <- t + 1
        }
        
        rmse_tr <- mean(rmse_train[1, ])
        rmse_te <- mean(rmse_test[1, ])
        r2_tr <- mean(r2_train[1, ])
        r2_te <- mean(r2_test[1, ])
        
        aver1_tr[nn,mm] <- rmse_tr
        aver2_tr[nn,mm] <- r2_tr
        aver1_te[nn,mm] <- rmse_te
        aver2_te[nn,mm] <- r2_te
        
        mm <- mm + 1
        
      }
      
      nn <- nn + 1
      
    }
    
    write.csv(aver1_tr,paste(ad,tt,name,"rmse_train.csv"))
    write.csv(aver2_tr,paste(ad,tt,name,"r2_train.csv"))
    write.csv(aver1_te,paste(ad,tt,name,"rmse_test.csv"))
    write.csv(aver2_te,paste(ad,tt,name,"r2_test.csv"))
    
    b <- b + 1
    
  }
  
  tt <- tt + 1
}

