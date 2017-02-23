##########svm
library(e1071)
#two-class classification
#remove the "setosa" group
myiris <- iris[!iris$Species == "setosa",]
plot(myiris)
plot(myiris$Sepal.Length, myiris$Sepal.Width,col=myiris$Species)
plot(myiris$Petal.Length, myiris$Petal.Width,col=myiris$Species) #separatable
#split the dataset into a training sample and a test sample
index <- sample(c(TRUE,FALSE),nrow(myiris),replace=TRUE,prob=c(0.6,0.4))
#choose the variable "Petal.Length", "Petal.Width" and "Species"
train <- myiris[index,c("Petal.Length", "Petal.Width", "Species")]
test <- myiris[!index,c("Petal.Length", "Petal.Width", "Species")]
plot(train$Petal.Length,train$Petal.Width, pch = 19, col = train$Species,  xlab = "Petal Length", ylab =  "Petal Width")
legend(x=3.5, y = 2.4, legend = c("Versicolor", "Virginica"), 
       col = c("green", "red"), bty = "n", pch = 19, cex = 1.5)
#linear svm
model <- svm(Species~.,data=train,kernal="linear",cost=0.1,scale=TRUE)
plot(model,train)
model$index
summary(model)
#try other kernals
model <- svm(Species~.,data=train,kernal="radial",scale=TRUE,cost=1,gamma=0.5)
#choose the best parameter
para <- tune(svm,Species~.,data=train,ranges =list(gamma = 2^(-1:1), cost = 2^(2:4)))
summary(para)
para$best.parameters$gamma #best gamma
para$best.parameters$cost  #best cost
#model with best parameters
model <- svm(Species~.,data=train,kernal="radial",scale=TRUE,cost=para$best.parameters$cost,gamma=para$best.parameters$gamma)
#prediction
class <- predict(model,newdata=test)
class
mean(class==test[,3])
table(class,test[,3])
###3-class
#svm uses the one-against-one approach
index <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE, prob = c(0.6, 0.4))
train <- iris[index,c("Petal.Length", "Petal.Width", "Species")]
test <- iris[!index,c("Petal.Length", "Petal.Width", "Species")]
model <- svm(Species~.,data=train,kernal="linear",cost=0.1,scale=TRUE)
plot(model,train)
model$index
model <- svm(Species~.,data=train,kernal="radial",scale=TRUE,cost=1,gamma=0.5)
summary(model)
class <- predict(model,newdata=test)
class
mean(class==test[,5])
table(class,test[,5])

#challenge:use svm classifier for sonar dataset
library(mlbench)
data(Sonar)
n <- dim(Sonar)[1]
index <- sample(1:n,0.6*n,replace=FALSE)
train <- Sonar[index,]
test <- Sonar[-index,]
model <- svm(Class~.,data=train,kernal="radial",scale=TRUE)
class <- predict(model,newdata=test)
class
mean(class==test[,61])
table(class,test[,61])