# install.packages("Metrics")
library(Metrics)
library(randomForest)

data <- read.csv("after_importance.csv", encoding = "UTF-8")
str(data)
feature <- read.csv("feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"
L <- dim(feature)[1]

ad <- "/result/"
aver1_tr <- matrix(0, nrow = 12, ncol = 20)   # rows for maxnodes，columns for ntree
aver2_tr <- matrix(0, nrow = 12, ncol = 20)
aver1_te <- matrix(0, nrow = 12, ncol = 20)
aver2_te <- matrix(0, nrow = 12, ncol = 20)

b <- 1
for (tt in c(1:30)){ # 30 times loop
  
  for (b in c(1:L)){
    

    "formula" = as.character(feature[b,1])
    formula <- as.formula(formula)
    name <- (feature[b,1])
    

    mm <- 1 # count for maxnodes
    for (maxnodes in seq(from = 3, to = 36, by = 3)){
      
      nn <- 1 #count for ntree
      for (ntree in seq(from = 50, to = 1000, by = 50)){
        
        # 10-fold cross-validation + grid search
        # 127 possible combinations * 30 times loop
        rmse_train <- matrix(0, nrow = 1, ncol = 10)
        r2_train <- matrix(0, nrow = 1, ncol = 10)
        rmse_test <- matrix(0, nrow = 1, ncol = 10)
        r2_test <- matrix(0, nrow = 1, ncol = 10)
        
        t <- 1
        # 10-fold cross-validation
        for (t in c(1:10)){
          set.seed(tt)
          par1 <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
          train1 <- data[par1==1,]
          test1 <- data[par1==2,]
          
          par <- sample(10, nrow(train1),replace = TRUE, prob = rep(0.1,10))
          
          train <- train1[par != t,]
          test <- train1[par == t,]
          
          set.seed(tt)
          rf <- randomForest(formula, train, ntree = ntree, maxnodes = maxnodes)
          
          # result of training
          ptrain <- predict(rf, train)
          rmse(train$TCF,ptrain)
          # RMSE of training
          rmse_train[1, t] <- rmse(train$TCF,ptrain)
          # R2 of training
          R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
          R2a[,1] <- ptrain
          R2a[,2] <- train$TCF
          R2a <- as.data.frame(R2a)
          names(R2a)[1] <- "ptrain"
          names(R2a)[2] <- "TCF"
          la <- lm(TCF~.,R2a)
          r2_train[1, t] <- as.numeric(summary(la)["r.squared"])
          # result of testing
          ptest <- predict(rf, test)
          # RMSE of testing
          rmse_test[1, t] <- rmse(test$TCF,ptest)
          # R2 of testing
          R2b <- matrix(0, nrow = length(ptest), ncol = 2)
          R2b[,1] <- ptest
          R2b[,2] <- test$TCF
          R2b <- as.data.frame(R2b)
          names(R2b)[1] <- "ptest"
          names(R2b)[2] <- "TCF"
          lb <- lm(TCF~.,R2b)
          r2_test[1, t] <- as.numeric(summary(lb)["r.squared"])
          t <- t + 1
          
        }
        
        aver1_tr[mm, nn] <- mean(rmse_train[1, ])
        aver2_tr[mm, nn] <- mean(r2_train[1, ])
        aver1_te[mm, nn] <- mean(rmse_test[1, ])
        aver2_te[mm, nn] <- mean(r2_test[1, ])  
        nn <- nn + 1
      }
      mm <- mm + 1
    }
    write.csv(aver1_tr,paste(ad,tt,name,"rmse_train.csv"))
    write.csv(aver2_tr,paste(ad,tt,name,"r2_train.csv"))
    write.csv(aver1_te,paste(ad,tt,name,"rmse_test.csv"))
    write.csv(aver2_te,paste(ad,tt,name,"r2_test.csv"))
    b <- b + 1
  }
  print(paste('over:',tt,'.',b,'.',name))
}

