#Random Forest Classifier
library(randomForest) #load the library 
#split the dataset into a traing sample and a test sample
#60% data to be used as training set
index <- sample(c(TRUE,FALSE),nrow(iris),replace=TRUE,prob=c(0.6,0.4))
train <- iris[index,]  #training sample
test <- iris[!index,]  #test sample
#construct a random forest model
#set the number of trees is 20
# set the number of variables randomly sampled at each split is 3
model <- randomForest(Species~.,data=train,mtry=3,ntree=20)
# also can construct the model by R default
# by default, the number of trees is 500 and the number of variables
# randomly sampled is sqrt(p)
model <- randomForest(Species~.,data=train)
importance(model) ## Show "importance" of variables: higher value mean more important:
treesize(model) #size of trees(number of nodes) in and ensemble
hist(treesize(model)) #histogram of the tree size
getTree(model,1,labelVar=TRUE) #extract the structure of the first tree 
class <- predict(model,newdata=test,type="class")
mean(class==test[,5])
table(class,test[,5])
#Challenge:use Random Forest classifier for sonar dataset
library(mlbench)
data(Sonar)
n <- dim(Sonar)[1]
index <- sample(1:n,0.6*n,replace=FALSE)
train <- Sonar[index,]
test <- Sonar[-index,]
model <-randomForest(Class~.,data=train)
#prediction
class <- predict(model,newdata=test,type="class")
mean(class==test[,61])
table(class,test[,61])
#compare with decision tree
#model construced by decision tree
dt.model <- rpart(Class~.,data=train)
rf.model <- randomForest(Class~.,data=train)
dt.class <- predict(dt.model,newdata=test,type="class")
rf.class <- predict(rf.model,newdata=test,type="class")
mean(dt.class==test[,61])
mean(rf.class==test[,61]) 
