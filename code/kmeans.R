##
library(GGally)
library(ggplot2)
library(factoextra)
library(pdp)
# Readind data and  use scale to normalize 

afdata = read.csv("../Clustering-Algorithms/data/afdata_imputed.csv", header = TRUE, dec = ".", sep = ",")
afdata$X = NULL

scaled_data <- as.data.frame(scale(afdata[, c(3:13)]))


# K means algorithm with k = 4

scaled_data_k <-kmeans(scaled_data, centers = 4)

plot_clus_coord(scaled_data_k, afdata[, c(3:13)])

# Scatter plots for all variables and higlighting cluster groups
ggpairs(cbind(scaled_data, Cluster=as.factor(scaled_data_k$cluster)),
        columns=1:11, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both")


## list of cluster assignments (pair country year changes over time)
o=order(scaled_data_k$cluster)
lclust<-data.frame(afdata$Country[o],afdata$Year, scaled_data_k$cluster[o])



# Run the algorithm for different values of k, and minimize the within and maximize between
# Elbow method

bss <- numeric()
wss <- numeric()
set.seed(1234)
for(i in 1:20){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(scaled_data, centers=i)$betweenss
  wss[i] <- kmeans(scaled_data, centers=i)$tot.withinss
}

# Between-cluster sum of squares vs Choice of k
p1 <- qplot(1:20, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 20, 1))

# Total within-cluster sum of squares vs Choice of k
p2 <- qplot(1:20, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 20, 1))

# Plot. It appears that the optimal number could be k = 4

grid.arrange(p1, p2, ncol=2)


# Cluster plot
fviz_cluster(scaled_data_k, data= scaled_data,geom = "point", frame.type = "norm")


