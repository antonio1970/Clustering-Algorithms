##
library(GGally)
library(corrplot)
library(ggplot2)
library(factoextra)
library(pdp)
library (mclust)
library(ggiraphExtra)

# Reading data and  use scale to normalize 

afdata = read.csv("../Clustering-Algorithms/data/afdata_imputed.csv", header = TRUE, dec = ".", sep = ",")

afdata$X = NULL


# Data description. 50 countries and 23 years

length(unique(afdata$Country))
length(unique(afdata$Year))

# Check missing values, Already imputed missing values
anyNA(afdata)


# using GGally to show a complete picture of the features to be used in the analysis

ggpairs(afdata[, c(3:13)], title = "Cross-variable Plot" , upper=list(continuous="blank"))

# Correlation matrix

m = cor(afdata[, c(3:13)], use ="pairwise")
m
corrplot(m, method = 'number', type = 'upper')

# Scaled data

scaled_data <- as.data.frame(scale(afdata[, c(3:13)]))

# Mixture of models algorithm trying from 2 to 10 models
mclust<- Mclust(scaled_data, G=2:10)

output <-clustCombi(mclust)

summary(output)
summary(mclust, parameters = TRUE)

#  models were selected
plot(mclust, what="BIC")
plot(mclust, what="classification")
plot(output, what = "entropy") 
plot(output, what = "tree")



x <- data.frame(scaled_data, K=mclust$classification)

ggRadar(data= x, aes(color= K), interactive = FALSE)

p <-ggRadar(data= x, aes(color= K), interactive = FALSE)
p + ggtitle("Mclust- k = 4:10")



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
summary(afdata[mclust$classification==5,])
summary(afdata[mclust$classification==6,])

#does this make sense?
#if not change it by some labels that make sense
#VeryGood = 6
#Good= 2
#Fine=1
#Bad=3
#VeryBad==4
#TheWorst=5

afdata[ , "ClusterDescription"] <- NA
n=length(afdata$Country)
i=1
while (i<=n)
{
  
  if (mclust$classification[i]==6) {
    afdata$ClusterDescription[i] <-"VeryGood"
  }  
  else if (mclust$classification[i]==2) {
    afdata$ClusterDescription[i] <-"Good"
  }  
  if (mclust$classification[i]==1) {
    afdata$ClusterDescription[i] <-"Fine"
  }  
  if (mclust$classification[i]==3) {
    afdata$ClusterDescription[i] <-"Bad"
  }  
  if (mclust$classification[i]==4) {
    afdata$ClusterDescription[i] <-"VeryBad"
  }  
  if (mclust$classification[i]==5) {
    afdata$ClusterDescription[i] <-"TheWorst"
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
write.csv(afdata,"../Clustering-Algorithms/data/afdata_imputed_ClsterDescription.csv")

