# install.packages("Metrics")
library(Metrics)
library(gbm)

data <- read.csv("after_importance.csv", encoding = "UTF-8")
str(data)
feature <- read.csv("feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"
L <- dim(feature)[1]

ad <- "/result/"
aver1_tr <- matrix(0, nrow = 7, ncol = 20)   # 每一行代表一个maxnodes，每一列代表一个ntree
aver2_tr <- matrix(0, nrow = 7, ncol = 20)
aver1_te <- matrix(0, nrow = 7, ncol = 20)
aver2_te <- matrix(0, nrow = 7, ncol = 20)

b <- 1

for (b in c(1:127)){
  

  "formula" = as.character(feature[b,1])
  formula <- as.formula(formula)
  LL <- length(strsplit(as.character(feature[b,1]), "\\+")[[1]])
  LL
  name <- (feature[b,1])
  
  # 网格搜索
  
  mm <- 1 
  
  for (shrinkage in c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1)){
    
    nn <- 1 
    
    for (ntree in seq(from = 50, to = 1000, by = 50)){
      
      tt <- 1
      
      rmse_tr <- matrix(0, nrow = 1, ncol = 30)
      rmse_te <- matrix(0, nrow = 1, ncol = 30)
      r2_tr <- matrix(0, nrow = 1, ncol = 30)
      r2_te <- matrix(0, nrow = 1, ncol = 30)
      
      for (tt in c(1:30)){

        
        rmse_train <- matrix(0, nrow = 1, ncol = 10)
        r2_train <- matrix(0, nrow = 1, ncol = 10)
        rmse_test <- matrix(0, nrow = 1, ncol = 10)
        r2_test <- matrix(0, nrow = 1, ncol = 10)
        
        t <- 1

        for (t in c(1:10)){
          set.seed(tt)
          par1 <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
          train1 <- data[par1==1,]
          test1 <- data[par1==2,]
          

          par <- sample(10, nrow(train1),replace = TRUE, prob = rep(0.1,10))
          
          train <- train1[par != t,]
          test <- train1[par == t,]
          
          set.seed(tt)
          gbm1 <- gbm(formula, data=train,                                                                   
                      var.monotone= rep(0,LL),
                      distribution="gaussian",        # see the help for other choices  
                      n.trees=ntree,                  # number of trees  
                      shrinkage=shrinkage,            # shrinkage or learning rate, 0.001 to 0.1 usually work
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
          rmse(train$TCF,ptrain)

          rmse_train[1, t] <- rmse(train$TCF,ptrain)

          R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
          R2a[,1] <- ptrain
          R2a[,2] <- train$TCF
          R2a <- as.data.frame(R2a)
          names(R2a)[1] <- "ptrain"
          names(R2a)[2] <- "TCF"
          la <- lm(TCF~.,R2a)
          r2_train[1, t] <- as.numeric(summary(la)["r.squared"])

          ptest <- predict(gbm1, test)
       
          rmse_test[1, t] <- rmse(test$TCF,ptest)
   
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
        
        rmse_tr[1, tt] <- mean(rmse_train[1, ])
        rmse_te[1, tt] <- mean(rmse_test[1, ])
        r2_tr[1, tt] <- mean(r2_train[1, ])
        r2_te[1, tt] <- mean(r2_test[1, ])
        
        tt <- tt + 1
        
      }
      
      aver1_tr[mm, nn] <- mean(rmse_tr[1, ])
      aver2_tr[mm, nn] <- mean(r2_tr[1, ])
      aver1_te[mm, nn] <- mean(rmse_te[1, ])
      aver2_te[mm, nn] <- mean(r2_te[1, ])
      
      nn <- nn + 1
      
    }
    
    mm <- mm + 1
    
  }
  
  write.csv(aver1_tr,paste(ad,name,"rmse_train.csv"))
  write.csv(aver2_tr,paste(ad,name,"r2_train.csv"))
  write.csv(aver1_te,paste(ad,name,"rmse_test.csv"))
  write.csv(aver2_te,paste(ad,name,"r2_test.csv"))
  
  b <- b + 1
  
}

