# install.packages("Metrics")
library("xgboost")
library("Matrix")
library("Metrics")
library(data.table)
data <- read.csv("data.csv", encoding = "UTF-8")
head(data)                                                                                       
str(data)
L <- dim(data)[1]

poss <- read.csv("possible_set.csv", encoding = "UTF-8")
str(poss)
names(poss)[1]<- "m"
names(poss)[2]<- "Vi"
names(poss)[3]<- "tt"
names(poss)[4]<- "pm"

str(poss)
#test <- poss
Lposs <- dim(poss)[1]



pred <- matrix(0, nrow = Lposs , ncol = 1000)


a = 1
b <- 1
for (a in c(1:1000)){   

  set.seed(a)
  par <- sample(2, nrow(data),replace = TRUE, prob = c(0.8,0.2))
  train <- data[par==1,]
  test <- data[par==2,]
  

  train_matrix <- sparse.model.matrix(TCF ~ .-1, data = train)
  poss_matrix <- sparse.model.matrix(TCF ~ .-1, data = poss)
  
  train_label <- train$TCF
  train_fin <- list(data=train_matrix,label=train_label)
  dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label) 
  
  poss_label <- poss$TCF
  poss_fin <- list(data=poss_matrix,label=poss_label)  
  dposs <- xgb.DMatrix(data = poss_fin$data, label = poss_fin$label)
  
  xgb <- xgboost(data = dtrain,max_depth=2, eta=0.1, verbose = 0,
                 objective='reg:squarederror', nround=300) 
  
  
  

  p <- predict(xgb, dposs)
  pp <- as.matrix(p)
  
  pred[,b] <- pp

  a = a+1
  b = b+1
}


pred1 <- matrix(0, nrow = Lposs , ncol = 1)

ll = 1
for (ll in c(1:Lposs)){
  pred1[ll,] <- mean(pred[ll,])
}


write.csv(pred1,"xgb_possible_pred1000.csv")

