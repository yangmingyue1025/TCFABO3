pb <- txtProgressBar(min = 0, max = 1000, style = 3)
library(randomForest)
data <- read.csv("your data.csv")
L <- length(data)
#Importance extraction-matrix, 1000 trials
im1 <- matrix(0, nrow = (L-1), ncol = 1000)
im2 <- matrix(0, nrow = (L-1), ncol = 1000)
b = 1
for (b in c(1:1000)){
  a <- round(b)
  # train and test
  set.seed(a)
  par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
  train <- data[par==1,]
  test <- data[par==2,]
  set.seed(a)
  rf <- randomForest(TCF~., train, 
                     ntree = 300, maxnodes = 21,
                     importance = TRUE, proximity=TRUE) # change it
  im1[,a] <- rf$importance[,1]
  im2[,a] <- rf$importance[,2]
  Sys.sleep(0.1)
  setTxtProgressBar(pb, b)
  b = b+1
  
}



# train
p1 <- predict(rf,train)
p1
plot(train$TCF,p1,xlim = c(-100, 100), ylim = c(-100, 100)) 
x <- seq(-100, 100,100)
lines(x,x,col = "red")
# test
p2 <- predict(rf,test)
p2
plot(test$TCF,p2,xlim = c(-100, 100), ylim = c(-100, 100)) 
x <- seq(-100, 100,100)
lines(x,x,col = "red")


write.csv(im1, "rf_im1.csv")
write.csv(im2, "rf_im2.csv")

