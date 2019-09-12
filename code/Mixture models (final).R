## date: 20-05-2019
library(GGally)
library(corrplot)
library(ggplot2)
library(factoextra)
library(pdp)
library (mclust)
library(ggiraphExtra)
library(xtable)
library(stargazer)


# Reading data and  use scale to normalize 

afdata = read.csv("../Clustering-Algorithms/data/afdata_imputed.csv", header = TRUE, dec = ".", sep = ",")

afdata$X = NULL


# Data description. 50 countries and 23 years

length(unique(afdata$Country))
length(unique(afdata$Year))

# Check missing values, Already imputed missing values
anyNA(afdata)

#Summary statistics

table_summary <-summary(afdata[, c(3:13)])
table_summary # alternatively witk skimr::skim
stargazer(afdata[, c(3:13)])

print(xtable(table_summary), rotate.rownames = TRUE, include.rownames = FALSE)

# using GGally to show a complete picture of the features to be used in the analysis

ggpairs(afdata[, c(3:13)], title = "Cross-variable Plot" , upper=list(continuous="blank"))

# Correlation matrix

m = cor(afdata[, c(3:13)], use ="pairwise")
m
png(file="corr.png", res=300, width=4500, height=4500)
corrplot(m, method = "number",type = "upper", cl.pos="n", col = "black")
dev.off()


windows() ## create window to plot your file
result <- mvn(data = afdata[, c(3:13)],mvnTest = "royston", univariatePlot = "histogram")
dev.off() 

# Scaled data

scaled_data <- as.data.frame(scale(afdata[, c(3:13)]))

# Mixture of models algorithm trying from 2 to 10 models
mclust<- Mclust(scaled_data, G=2:10)

sink('analysis-output.csv')
summary(mclust, parameters = TRUE)
sink()


#  models were selected
plot(mclust, what="BIC")
plot(mclust, what="classification")

#Combination of models 
output <-clustCombi(mclust)
summary(output)

#The combination of models seems to suggest that K = 4 is a more appropriate value
plot(output, what = "entropy") 
plot(output, what = "tree")
#Combinations of suggested models: 
#First merge groups 3 and 4, and then merge the group resulting from the merger with group 1
output$combiM

#original clusters according to Mclust
x <- data.frame(scaled_data, K=mclust$classification)

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


#Original clusters interpretation

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
    afdata$ClusterDescription[i] <-"Fine2"
  }  
  if (mclust$classification[i]==4) {
    afdata$ClusterDescription[i] <-"Bad"
  }  
  if (mclust$classification[i]==5) {
    afdata$ClusterDescription[i] <-"Bad2"
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

# Classification uncertainty
fviz_mclust(mclust, "uncertainty", palette = "jco")

#try to interpret clusters mergring
#first merger: 3 and 4
summary(afdata[mclust$classification==3,])
summary(afdata[mclust$classification==4,])
summary(afdata[mclust$classification==3 | mclust$classification==4,])

#Interpretación de la primera fusión: grupos 3 y 4. El grupo 4 está peor en la dimensión de educación, 
#especialmente secundaria y terciaria. Ambos están más o menos igual en infraestructuras de 
#telecomunicación (el grupo 3 parece tener más teléfonos pero el grupo 4 está 
#ligeramente mejor en penetración de Internet). Ambos tienen un comportamiento muy similar en cuanto a 
#producción científica. En cuanto a las variables regulatorias, el grupo 3 está un poco "menos mal" 
#que el grupo 4. En el caso de TNTBA el grupo 4 está mejor que el grupo 3. 
#En resumen, no hay diferencias muy marcadas entre ambos, 
#en algunas cosas están mejor uno y otras cosas están mejor el otro.

#second merger: 1 and 3 (originals 3 and 4)
summary(afdata[mclust$classification==1,])
summary(afdata[mclust$classification==3 | mclust$classification==4,])
summary(afdata[mclust$classification==3 | mclust$classification==4 | mclust$classification==1,])

#Interpretación de la segunda fusión: grupos 1 y 3 [originales 3 y 4]. 
#El grupo 1 está ligeramente mejor en educación y en penetración de Internet. 
#La dimensión de innovación, y en calidad regulatoria es sorprendentemente similar al grupo 3. 
#Está ligeramente mejor que el grupo 3 en TNTBA



#try to interpret clusters
summary(afdata[output$classification[[4]]==1,])
print(xtable(summary(afdata[output$classification[[4]]==1,])))
summary(afdata[output$classification[[4]]==2,])
print(xtable(summary(afdata[output$classification[[4]]==2,])))
summary(afdata[output$classification[[4]]==3,])
print(xtable(summary(afdata[output$classification[[4]]==3,])))
summary(afdata[output$classification[[4]]==4,])
print(xtable(summary(afdata[output$classification[[4]]==4,])))

xm <- data.frame(scaled_data, K=output$classification[[4]])

p <-ggRadar(data= xm, aes(color= K), interactive = FALSE)
p + ggtitle("After merging clusters")

# Scatter plots for all variables and higlighting cluster groups
ggpairs(cbind(scaled_data, Cluster=as.factor(output$classification[[4]])),
        columns=1:11, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both")


#Merged clusters naming
#does this make sense?

afdata[ , "MergedClusterDescription"] <- NA
n=length(afdata$Country)
i=1
while (i<=n)
{
  
  if (output$classification[[4]][i]==4) {
    afdata$MergedClusterDescription[i] <-"VeryGood"
  }  
  else if (output$classification[[4]][i]==2) {
    afdata$MergedClusterDescription[i] <-"Good"
  }  
  if (output$classification[[4]][i]==1) {
    afdata$MergedClusterDescription[i] <-"Bad"
  }  
  if (output$classification[[4]][i]==3) {
    afdata$MergedClusterDescription[i] <-"Worst"
  }
  i=i+1
}



## list of cluster assignments (pair country year changes over time)
o=order(mclust$classification)

lclust<-data.frame(afdata$Country[o],afdata$Year, afdata$ClusterDescription[o])

#we visualize countries per year to see how they are changing cluster
CountryYerarCluster<-data.frame(afdata$Country,afdata$Year, afdata$MergedClusterDescription)


#Save data with descriptions
write.csv(afdata,"../Clustering-Algorithms/data/afdata_imputed_ClsterDescription.csv")


#print all the other possible mergers
i=2
while (i<=6)
{
  
  x_tmp <- data.frame(scaled_data, K=output$classification[[i]])
  
  p <-ggRadar(data= x_tmp, aes(color= K), interactive = FALSE)
  p + ggtitle("After merging clusters")
  print(p)
  i=i+1
}
