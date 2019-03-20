##
library(GGally)
library(ggplot2)
library(factoextra)
library(pdp)
library (mclust)
# Reading data and  use scale to normalize 

afdata = read.csv("../Clustering-Algorithms/data/afdata_imputed.csv", header = TRUE, dec = ".", sep = ",")

afdata$X = NULL

scaled_data <- as.data.frame(scale(afdata[, c(3:13)]))

# Mixture of models algorithm with G = 4
mclust<- Mclust(scaled_data, G=4)
summary(mclust$BIC)

summary(mclust)

# Scatter plots for all variables and higlighting cluster groups
ggpairs(cbind(scaled_data, Cluster=as.factor(mclust$classification)),
        columns=1:11, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both")

#try to interpret clusters
summary(afdata[mclust$classification==1,])
summary(afdata[mclust$classification==2,])
summary(afdata[mclust$classification==3,])
summary(afdata[mclust$classification==4,])


afdata[ , "ClusterDescription"] <- NA
n=length(afdata$Country)
i=1
while (i<=n)
{
  
  if (mclust$classification[i]==4) {
    afdata$ClusterDescription[i] <-"Advanced"
  }  
  else if (mclust$classification[i]==3) {
    afdata$ClusterDescription[i] <-"Followers"
  }  
  if (mclust$classification[i]==2) {
    afdata$ClusterDescription[i] <-"Moderated"
  }  
  if (mclust$classification[i]==1) {
    afdata$ClusterDescription[i] <-"Early KEs"
  }  
  
  i=i+1
}

## list of cluster assignments (pair country year changes over time)
o=order(mclust$classification)

lclust<-data.frame(afdata$Country[o],afdata$Year, afdata$ClusterDescription[o])

#we visualize countries per year to see how they are changing cluster
CountryYerarCluster<-data.frame(afdata$Country,afdata$Year, afdata$ClusterDescription)


# Cluster plot
fviz_cluster(mclust, data= scaled_data,geom = "point", frame.type = "norm")

#Save data with descriptions
write.csv(afdata,"../Clustering-Algorithms/data/afdata_imputed_ClusterDescription4.csv")