#Regularization
library(glmnet) #load the library
#Creating training and the test data set
index <- sample(1:nrow(mtcars),0.6*nrow(mtcars),replace=FALSE)
train <- mtcars[index,]
test <- mtcars[-index,]
#Model training
model <- glmnet(data.matrix(train[,-1]),train[,1],alpha=1) #alpha=0 (ridge)
#Model Prediction
pred <- predict(model,data.matrix(test[,-1]),s=2) #s is the value of lambda
#Model visualization
plot(model, xvar = "lambda", label = TRUE, lwd =2, cex.axis = 1.5, cex.lab = 1.5)
coef(model, s = exp(-1))
coef(model, s = exp(-1))[5]
#Model Assesment
mean((pred - test[,1])^2)
#cv to determine the best penalty parameter using lasso
model.cv <- cv.glmnet(data.matrix(train[,-1]),train[,1],alpha=1,nfolds=5)
#best lambda
bestlam <- model.cv$lambda.min
model.cv
pred.cv <- predict(model.cv,data.matrix(test[,-1]),s=bestlam)
pred.cv
#Model Assesment
mean((pred.cv - test[,1])^2)
