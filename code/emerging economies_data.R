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

# Check datasets before merging, dimensions etc.

dim(kei)
dim(knowledge)

length (unique(knowledge$Country))
length(unique(kei$Country))

# Merge both datasets

combine <- full_join(kei, knowledge)
length(unique(combine$Country))
dim(combine)


head(combine)
tail(combine)

table(combine$Year)

#

combine<-combine[order(combine[, "Country"], combine[, "Year"]),]


emergingcountries <- c ("Argentina",	"Brazil", "Colombia", "China", "Czech Republic","Egypt, Arab Rep.",
                        "Greece",	"Hungary", "India", "Indonesia", "Israel", "Malaysia", "Mexico", "Pakistan", "Peru", "Philippines", "Poland", "Qatar", "Russian Federation",
                        "Saudi Arabia", "Thailand", "Turkey", "United Arab Emirates")

sub_combine <- combine %>% filter(combine$Country %in% emergingcountries) 
sub_combine<-sub_combine[order(sub_combine$Country),]

sub_combine <- sub_combine %>% 
  select(Country, Year, KEI_Index, REGQU, RULEL, PRIMARY, SECONDARY, TERTIARY,TELEP3,FIXBI2,INTERN3, PATEN2, STJOU2, TNTBA)



write.csv(sub_combine, file ="emerging_economies.csv")


