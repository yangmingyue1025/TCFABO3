# install.packages("Metrics")
library(Metrics)
library(gbm)

data <- read.csv("after_importance.csv", encoding = "UTF-8")

feature <- read.csv("feature_combination.csv", encoding = "UTF-8")
names(feature)[1] <- "feature"

para <- read.csv("r2_te.csv", encoding = "UTF-8")
L <- dim(feature)[1]
str(para)

# Defined output matrix
rmse_train <- matrix(0, nrow = L, ncol = 1000)
r2_train <- matrix(0, nrow = L, ncol = 1000)
rmse_test <- matrix(0, nrow = L, ncol = 1000)
r2_test <- matrix(0, nrow = L, ncol = 1000)

b = 1
for (b in c(1:L)){
  
  # TCF~features
  "formula" = as.character(feature[b,1])
  formula <- as.formula(formula)
  LL <- length(strsplit(as.character(feature[b,1]), "\\+")[[1]])
  # Optimum hyperparameters
  shrinkage = para[b,3]
  ntree = para[b,4]
  
  a = 1
  for (a in c(1:1000)){  # 1000 times loop
    
    set.seed(a)
    par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
    train <- data[par==1,]
    test <- data[par==2,]
    
    #gbdt
    set.seed(a)
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
    
    # result of training
    ptrain <- predict(gbm1, train)
    rmse_train[b,a] <- rmse(train$TCF,ptrain)
    R2a <- matrix(0, nrow = length(ptrain), ncol = 2)
    R2a[,1] <- ptrain
    R2a[,2] <- train$TCF
    R2a <- as.data.frame(R2a)
    names(R2a)[1] <- "ptrain"
    names(R2a)[2] <- "TCF"
    la <- lm(TCF~.,R2a)
    r2_train[b,a] <- as.numeric(summary(la)["r.squared"])
    
    
    # result of testing
    ptest <- predict(gbm1, test)
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

ad = "/your folder for optimal/"

write.csv(rmse_train,paste(ad,"rmse_train.csv"))
write.csv(r2_train,paste(ad,"r2_train.csv"))
write.csv(rmse_test,paste(ad,"rmse_test.csv"))
write.csv(r2_test,paste(ad,"r2_test.csv"))

