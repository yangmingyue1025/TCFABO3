#install.packages("ipred")
library(Metrics)
library(ipred)

data <- read.csv('/after_importance.csv')
str(data)

feature <- read.csv("feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"
L <- dim(feature)[1]

ad <- "/result/"

tt <- 1
for (tt in c(1:30)){

  # 127*30
  
  b = 1
  
  for (b in c(1:L)){
    

    "formula" = as.character(feature[b,1])
    formula <- as.formula(formula)
    
    name <- (feature[b,1])
    
    nn <- 1
    
    aver1_tr <- matrix(0, nrow = 12, ncol = 1)   # 每一行代表一个nbagg
    aver2_tr <- matrix(0, nrow = 12, ncol = 1)
    aver1_te <- matrix(0, nrow = 12, ncol = 1)
    aver2_te <- matrix(0, nrow = 12, ncol = 1)
    
    for (nbagg in c(25, 50, 75, 100 ,125, 150, 175, 200, 225, 250, 275, 300)){
      
        

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

        set.seed(tt)
        mybag <- bagging(formula, data = train, nbagg = nbagg)
        

        ptrain <- predict(mybag, train)
        rmse_train[1,t] <- rmse(train$TCF,ptrain) 
        R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
        R2a[,1] <- ptrain
        R2a[,2] <- train$TCF
        R2a <- as.data.frame(R2a)
        names(R2a)[1] <- "ptrain"
        names(R2a)[2] <- "TCF"
        la <- lm(TCF~.,R2a)
        r2_train[1,t] <- as.numeric(summary(la)["r.squared"])
        

        ptest <- predict(mybag, test)
        rmse_test[1,t] <- rmse(test$TCF,ptest) 
        R2b <- matrix(0, nrow = length(ptest), ncol = 2)
        R2b[,1] <- ptest
        R2b[,2] <- test$TCF
        R2b <- as.data.frame(R2b)
        names(R2b)[1] <- "ptest"
        names(R2b)[2] <- "TCF"
        lb <- lm(TCF~.,R2b)
        r2_test[1,t] <- as.numeric(summary(lb)["r.squared"])
        r2_test
        t <- t + 1
      }
      
      rmse_tr <- mean(rmse_train[1, ])
      rmse_te <- mean(rmse_test[1, ])
      r2_tr <- mean(r2_train[1, ])
      r2_te <- mean(r2_test[1, ])
      
      aver1_tr[nn,1] <- rmse_tr
      aver2_tr[nn,1] <- r2_tr
      aver1_te[nn,1] <- rmse_te
      aver2_te[nn,1] <- r2_te
      
      nn <- nn + 1
        
      }
      
    write.csv(aver1_tr,paste(ad,tt,name,"rmse_train.csv"))
    write.csv(aver2_tr,paste(ad,tt,name,"r2_train.csv"))
    write.csv(aver1_te,paste(ad,tt,name,"rmse_test.csv"))
    write.csv(aver2_te,paste(ad,tt,name,"r2_test.csv"))
      
    }
  
  tt <- tt + 1
  
}

