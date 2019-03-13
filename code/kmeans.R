scaled_data <- as.data.frame(scale(data[, c(3:13)]))

scaled_data_k <-kmeans(scaled_data, centers = 4)

ggpairs(cbind(scaled_data, Cluster=as.factor(scaled_data_k$cluster)),
        columns=1:11, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both")


## list of cluster assignments
o=order(scaled_data_k$cluster)
lclust<-data.frame(data$Country[o],data$Year, scaled_data_k$cluster[o])


bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)
for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(scaled_data, centers=i)$betweenss
  wss[i] <- kmeans(scaled_data, centers=i)$tot.withinss
}

# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1))

grid.arrange(p3, p4, ncol=2)


fviz_cluster(scaled_data_k,data = scaled_data)
