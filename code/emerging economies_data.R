# Load libraries
library(tidyverse)
library(corrplot)
library(GGally)
library(naniar)
library(readxl)

# read csv file with full data for all countries and years

knowledge <- read.csv2("../Clustering-Algorithms/data/KE_data_1995-2017 final version_cluster.csv", header = TRUE, dec = ",", sep = ";")

# read csv file with KE index for all countries 


kei <- read.csv("../Clustering-Algorithms/data/keindex.csv")
kei$X= NULL
colnames(kei) <- c("Country", "Year", "KEI_Index")
kei<-kei[order(kei$Year),]

# Select subset of countries

emergingcountries <- c ("Argentina",	"Brazil", "Colombia", "Chile", "China", "Czech Republic","Egypt",
                  "Greece",	"Hungary", "India", "Indonesia", "Israel", "Malaysia", "Mexico", "Pakistan", "Peru", "Philippines", "Poland", "Qatar", "Russian Federation",
                  "Saudi Arabia", "Taiwan", "Thailand", "Turkey", "United Arab Emirates")

# KE index and other variables only for emerging economies

sub_kei <- kei %>% filter(kei$Country %in% emergingcountries) %>% droplevels
levels(sub_kei$Country)
sub_kei<-sub_kei[order(sub_kei$Year),]


# Merge both datasets
combine = merge(sub_kei, knowledge, by=c("Country", "Year"),all.x = TRUE, all.y = TRUE)

emerging_data <-combine %>% select(Country, Year, KEI_Index,REGQU, RULEL, PRIMARY, SECONDARY, TERTIARY,
                   TELEP3,FIXBI2,INTERN3, PATEN2, STJOU2, TNTBA)
write.csv(emerging_data, file="emerging_data.csv")
