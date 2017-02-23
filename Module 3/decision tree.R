#Decision Tree Classifier
library(rpart) #load the library
#split the data into training and test set
index <- sample(c(TRUE,FALSE),nrow(iris),replace=TRUE, prob=c(0.6,0.4))
train <- iris[index,] #training sample
test <- iris[!index,] #test sample
#build the tree model
model <- rpart(Species~.,data=train,method="class")
?rpart
library(rpart.plot)
rpart.plot(model) #plot the decision tree
rpart.plot(model,type=0) #can try different types
#Make Prediction
class <- predict(model,newdata=test,type="class")
class
#verify model prediction
mean(class==test[,5])
table(class,test[,5])
#Challenge: use Decision Tree classifier for sonar dataset
library(mlbench)
data(Sonar)
n <- dim(Sonar)[1]
index <- sample(1:n,0.6*n,replace=FALSE)
train <- Sonar[index,]
test <- Sonar[-index,]
model <- rpart(Class~.,data=train,method="class")
rpart.plot(model)