############knn
library(class) #load the library
#60% data to be used as training set 
index <- sample(c(TRUE,FALSE),nrow(iris),
                replace=TRUE,prob=c(0.6,0.4))

#construct a training dataset
train <- iris[index,] 
#construct a test dataset
test <- iris[!index,] 
#use 3-nearest neighbor classifier to classify the species of flowers in test group
class <- knn(train[,1:4],test[,1:4],train[,5],k=3)  #set k=3 
#can try 5-nearest neighbor classifier
class <- knn(train[,1:4],test[,1:4],train[,5],k=5)  #set k=5
mean(class==test[,5]) # Accuracy
table(class,test[,5]) # Confusion Matrix
#challenge:use knn classifier for sonar dataset
library(mlbench)
data(Sonar)
#alternatively
data(Sonar,package="mlbench")
n <- dim(Sonar)[1]
index <- sample(1:n,0.6*n,replace=FALSE) #60% data to be used as training set 
train <- Sonar[index,]
test <- Sonar[-index,]
#3-nearest neighbor classifier
class <- knn(train[,1:60],test[,1:60],train[,61],k=3)
mean(class==test[,61]) # Accuracy
table(class,test[,61]) # Confusion Matrix