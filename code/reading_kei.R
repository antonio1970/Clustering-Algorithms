# Read the KE Index and create a dataframe

kei <- read.csv("../Clustering-Algorithms/keindex.csv")
kei$X= NULL
colnames(kei) <- c("Country", "Year", "KEI_Index")
kei<-kei[order(kei$Year),]
