#Neural Network
library(nnet)
index <- sample(c(TRUE,FALSE),nrow(mtcars),replace=TRUE,prob=c(0.6,0.4))
train <- mtcars[index,]
test <- mtcars[!index,]
model <- nnet(mpg~.,data=train,size=3,linout=TRUE,skip=TRUE)
value <- predict(model,test)
value
mean((value-test[,1])^2)
##challenge
Boston
names(Boston)
model <- nnet(medv~.,data=Boston,size = 10,linout = TRUE, skip = TRUE)
pred <- predict(model,newdata=Boston)
#RMSE
sqrt(sum((pred - Boston$medv)^2)/length(pred))
sqrt(mean((pred-Boston$medv)^2))