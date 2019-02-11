#-----------------Hierarchichal clustering----------#

d2<-iris[,-c(5)]
d2 <- scale(d2)

## Ward Hierarchical Clustering

dis <- c("euclidean","manhattan","minkowski")
met <- c("single","complete","average","mcquitty","median")
 
 d <- dist(d2, method = "euclidean")
 fit <- hclust(d, method="average")
 # # display dendogram
 
 plot(fit) 
 
## draw dendogram with green borders around the 2 clusters

  rect.hclust(fit, k=2, border="green")