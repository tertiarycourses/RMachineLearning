#Gaussian Navie Bayes
library(e1071)
#Splitting the data set into training and testing data set
index <- sample(c(TRUE,FALSE),nrow(iris),replace=TRUE,prob=c(0.6,0.4))
train <- iris[index,]
test <- iris[!index,]
#Making the naive bayes model
#only use Petal.Length variable
model <- naiveBayes(Species~Petal.Length,data=train)
#use all variables
model <- naiveBayes(Species~.,data=train)
#Making the prediction
class <- predict(model,test)
class
mean(class==test[,5])
table(class,test[,5])
#challenge:Use GNB classifier for sonar dataset
library(mlbench)
data(Sonar)
n <- dim(Sonar)[1]
index <- sample(1:n,0.6*n,replace=FALSE)
train <- Sonar[index,]
test <- Sonar[-index,]
model <- naiveBayes(Class~.,data=train)
class <- predict(model,newdata=test)
class
mean(class==test[,61])
table(class,test[,61])