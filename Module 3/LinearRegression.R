##Linear Regression
#plot the relationship between miles/gallon and weight of mtcars dataset
plot(mtcars[,c("wt","mpg")])
model <- lm(mpg~wt,data=mtcars)
abline(model) #the fitted straight line
abline(model,col="red")
coef(model) #coefficients of the linear model
sumModel <- summary(model)
sumModel
sumModel$r.squared 
r <- cor(mtcars$mpg,mtcars$wt)
r^2
value <- predict(model,data.frame(wt=mtcars$wt))
value

#Multivariate linear regression
plot(mtcars)
model <- lm(mpg~.,data=mtcars)
summary(model)
value <- predict(model,data.frame(mtcars))
value
#challenge
library(MASS)
Boston
names(Boston)
?Boston
model <- lm(medv~.,data=Boston)
summary(model)
pred <- predict(model,newdata=Boston)
#sqrt(sum((pred - Boston$medv)^2)/length(pred))
sqrt(mean((pred-Boston$medv)^2))