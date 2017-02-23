#Dimensionality Reduction
cor(iris[,1:4]) #correlation matrix
#perform a principal components analysis 
pcI <- prcomp(iris[,1:4],scale=TRUE)
summary(pcI)
pcI$rotation #loadings
eigen(cor(iris[,1:4]))$vector #same with loadings
# variance explained by  principle components
pcv <- pcI$sdev^2 
eigen(cor(iris[,1:4]))$values #eigenvaluess of the correlation matrix
pve <- pcv/sum(pcv) #percentage of variance explained by pc
sum(pve)
plot(pve,xlab="Principal Componnet",ylab="Proportion of variance explained",ylim=c(0,1),type="b")
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of 
      variance explained", ylim=c(0,1), type="b")
#prediction
predict(pcI)
predict(pcI)[,1:2] #first two PCs
#or
pcI$x[,1:2]
