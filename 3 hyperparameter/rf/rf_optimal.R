library(Metrics)
library(randomForest)
data <- read.csv("after_importance.csv", encoding = "UTF-8")

feature <- read.csv("feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"

para <- read.csv("r2_te.csv", encoding = "UTF-8")
L <- dim(feature)[1]


rmse_train <- matrix(0, nrow = L, ncol = 1000)
r2_train <- matrix(0, nrow = L, ncol = 1000)
rmse_test <- matrix(0, nrow = L, ncol = 1000)
r2_test <- matrix(0, nrow = L, ncol = 1000)


b = 1
for (b in c(1:L)){
  

  "formula" = as.character(feature[b,1])
  formula <- as.formula(formula)
  

  maxnodes = para[b, 3]
  ntree = para[b, 4]
  
  a = 1
  for (a in c(1:1000)){
    

    set.seed(a)
    par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
    train <- data[par==1,]
    test <- data[par==2,]
    

    set.seed(a)
    rf <- randomForest(formula, train, ntree = ntree, maxnodes = maxnodes)
    

    ptrain <- predict(rf, train)
    rmse_train[b,a] <- rmse(train$TCF,ptrain) 
    R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
    R2a[,1] <- ptrain
    R2a[,2] <- train$TCF
    R2a <- as.data.frame(R2a)
    names(R2a)[1] <- "ptrain"
    names(R2a)[2] <- "TCF"
    la <- lm(TCF~.,R2a)
    r2_train[b,a] <- as.numeric(summary(la)["r.squared"])
    
    

    ptest <- predict(rf, test)
    rmse_test[b,a] <- rmse(test$TCF,ptest)
    R2b <- matrix(0, nrow = length(ptest), ncol = 2)
    R2b[,1] <- ptest
    R2b[,2] <- test$TCF
    R2b <- as.data.frame(R2b)
    names(R2b)[1] <- "ptest"
    names(R2b)[2] <- "TCF"
    lb <- lm(TCF~.,R2b)
    r2_test[b,a] <- as.numeric(summary(lb)["r.squared"])
    
    
    

    a = a+1
  }
  b = b+1
}

dire = "/your folder for optimal/"

write.csv(rmse_train,paste(dire,"rmse_train.csv"))
write.csv(r2_train,paste(dire,"r2_train.csv"))
write.csv(rmse_test,paste(dire,"rmse_test.csv"))
write.csv(r2_test,paste(dire,"r2_test.csv"))

