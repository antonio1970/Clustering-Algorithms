library(tidyverse)
# Read the KE Index and create a dataframe

kei <- read.csv("../Clustering-Algorithms/data/keindex.csv")
kei$X= NULL
colnames(kei) <- c("Country", "Year", "KEI_Index")
kei<-kei[order(kei$Year),]


# Merge two files with another column with the KEI index. Initial check
afdata = read.csv("../Clustering-Algorithms/data/afdata.csv", header = TRUE, dec = ".", sep = ",")

class(afdata$Country)
levels(afdata$Country)

class(kei$Country)
levels(kei$Country)

# Both factors have differente levels


combined <- sort(union(levels(afdata$Country), levels(kei$Country)))
afdata_full <- left_join(mutate(afdata, Country=factor(Country, levels=combined)),
               mutate(kei, Country=factor(Country, levels=combined)))
write.csv(afdata_full, "afdata_full.csv")
