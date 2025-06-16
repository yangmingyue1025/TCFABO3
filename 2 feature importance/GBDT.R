# GBDT
# install.packages('gbm')
library(gbm)  

data <- read.csv("your data.csv")
str(data)
L <- length(data)
# Importance extraction-matrix, 1000 trials
im1 <- matrix(0, nrow = (L-1), ncol = 1000)
im2 <- matrix(0, nrow = (L-1), ncol = 1000)

a = 1
for (a in c(1:1000)){
  # train and test
  set.seed(a)
  par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
  train <- data[par==1,]
  test <- data[par==2,]
  # Gradient Boosting Decision Tree
  
  set.seed(a)
  # fit initial model  
  gbm1 <- gbm(TCF~., data=train, 
              var.monotone= rep(0,L-1),    # 0: no monotone restrictions 
              distribution="gaussian",        # see the help for other choices  
              n.trees=1000,                     # number of trees  
              shrinkage=0.01,                   # shrinkage or learning rate, 0.001 to 0.1 usually work  
              interaction.depth=3,             # 1: additive model, 2: two-way interactions, etc.  
              bag.fraction = 0.5,              # subsampling fraction, 0.5 is probably best  
              train.fraction = 1,           # fraction of data for training, first train.fraction*N used for training  
              n.minobsinnode = 2,             # minimum total weight needed in each node  
              cv.folds = 10,                     # do 10-fold cross-validation  
              keep.data=TRUE,                  # keep a copy of the dataset with the object  
              verbose=FALSE,                   # don't print out progress  
              n.cores=3)                        
  
  ## check performance using 10-fold cross-validation  
  best.iter <- gbm.perf(gbm1,method="cv")  
  # print(best.iter) 
  gbdt <- summary(gbm1,n.trees=best.iter, plotit = 'FALSE',order = 'FALSE')
  im1[,a] <- gbdt$rel.inf
  
  
  a = a+1
  
}


# train
p1 <- predict(gbm1,train)
p1
plot(train$TCF,p1,xlim = c(-100, 80), ylim = c(-100, 80)) 
x <- seq(-100, 80,80)
lines(x,x,col = "red")
# test
p2 <- predict(gbm1,test)
p2
plot(test$TCF,p2,xlim = c(-100, 80), ylim = c(-100, 80)) 
x <- seq(-100, 80,80)
lines(x,x,col = "red")


im2[,1] <- as.character(gbdt$var)

write.csv(im1, "your result1.csv")
write.csv(im2, "your result2.csv")



