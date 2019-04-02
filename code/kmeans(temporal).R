# Kmeans por periodo ver evolución temporal

##
library(GGally)
library(ggplot2)
library(factoextra)
library(pdp)
# Readind data and  use scale to normalize 

afdata = read.csv("../Clustering-Algorithms/data/afdata_imputed.csv", header = TRUE, dec = ".", sep = ",")
afdata$X = NULL


# Graphical representation 3 periods
afdata2011 = subset(afdata, Year>=2011)  # 2011-2017 time period
summary(afdata2011)


afdata0610 = subset(afdata, Year>=2006 & Year<2011) 
summary(afdata0610)

afdata0105 = subset(afdata, Year>=2001 & Year<2006)
summary(afdata0105)


# First period

# K means algorithm with k = 4. Year = 2011-2017

scaled_data_2011 <-as.data.frame(scale(afdata2011[,c(3:13)]))
k2011 <-kmeans(scaled_data_2011, centers = 4)

# labels to clusters and bind to the original data

x <- data.frame(scaled_data_2011, K=k2011$cluster)

ggRadar(data= x, aes(color= K), interactive = FALSE)

p <-ggRadar(data= x, aes(color= K), interactive = FALSE)
p + ggtitle("K-means. Period: 2011-2017")



devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)
library(ggradar)

suppressPackageStartupMessages(library(dplyr))
library(scales)
library(tibble)


# Cluster plot
fviz_cluster(k2011, data= scaled_data_2011, main ="Cluster-2011-2017", geom = "point", frame.type = "norm")

# Second period. Year 2006-2010

scaled_data_0610 <-as.data.frame(scale(afdata0610[,c(3:13)]))
k0610 <-kmeans(scaled_data_0610, centers = 4)

# Cluster plot
fviz_cluster(k0610, data= scaled_data_0610, main = "Cluster-2006-2010", geom = "point", frame.type = "norm")


#Third period. Year 2001-2005

scaled_data_0105 <-as.data.frame(scale(afdata0105[,c(3:13)]))
k0105 <-kmeans(scaled_data_0105, centers = 4)

# Cluster plot
fviz_cluster(k0105, data= scaled_data_0105, main = "Cluster-2001-2005", geom = "point", frame.type = "norm")

