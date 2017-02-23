#logistic regression
#remove the observations corresponding to setosa
myiris <- iris[!iris$Species == "setosa", c("Petal.Length", "Petal.Width","Species")]
#Splitting the data set into training and testing data set
index <- sample(c(TRUE,FALSE),nrow(myiris),replace=TRUE,prob=c(0.6,0.4))
train <- myiris[index,]
test <- myiris[!index,]
# Model training
model <- glm(Species~Petal.Length,data=train,family=binomial(link="logit"))

#Making Prediction
prob <- predict(model,newdata=test,type="response")
#draw a curve based on prediction from logistic regression model
plot((as.numeric(test$Species)-2)~test$Petal.Length,xlab="Petal.Length",ylab="Species")
curve(predict(model,data.frame(Petal.Length=x),type="response"),add=TRUE)
prob
class <- ifelse(prob>0.5,"virginica","versicolor")
class
mean(class == test[,3])  # Accuracy
table(class, test[,3])         # Confusion Matrix
#Adding more predictor variable
glmModel <- glm(Species ~ Petal.Length + Petal.Width, data = train, family = binomial(link="logit"))
pred <- predict(glmModel, test, type = "response")
predClass <- ifelse(pred > 0.5, "virginica", "versicolor")
mean(predClass == test[,3]) 
table(predClass, test[,3])
#challenge
library(mlbench)
data(Sonar)
n <- dim(Sonar)[1]
index <- sample(1:n,0.6*n,replace=FALSE)
train <- Sonar[index,]
test <- Sonar[-index,]
model <- glm(Class~.,data=train,family=binomial(link="logit"))
prob <- predict(model,test,type="response")
prob
class <- ifelse(prob>0.5,"R","M")
class
mean(class == test[,61])  # Accuracy
table(class, test[,61])         # Confusion Matrix