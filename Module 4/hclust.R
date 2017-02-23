##Hierarchical clustering
m <- dist(iris[,-5])
hc <- hclust(m)
plot(hc)
plot(hc,labels=FALSE)
clusters <- cutree(hc, k = 3)
#challenge
data(mtcars)
mymtcars <- scale(mtcars[,-1])
m <- dist(mymtcars)
hc <- hclust(m)
plot(hc)
clusters <- cutree(hc,k=3)
