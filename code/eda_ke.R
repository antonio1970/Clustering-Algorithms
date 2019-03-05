# Load libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(naniar)

# read csv file

knowledge <- read.csv2("../data/KE_data_1995-2017 final version_cluster.csv", header = TRUE, dec = ",", sep = ";")

# Select subset of countries

afcountries <- c ("Algeria",	"Ethiopia", "Niger", "Angola", "Gabon", "Nigeria","Benin",
                    "Gambia",	"Rwanda", "Botswana", "Guinea Bissau", "South Africa", "Burkina Faso",
                  "Ghana", "Sao Tome and Principe", "Burundi", "Guinea", "Senegal", "Cameroon", "Kenya", "Seychelles",
                  "Cape Verde", "Lesotho", "Sierra Leone", "Central African Republic", "Liberia",	"Somalia", "Chad",	
                  "Libya", "Sudan", "Cote d'Ivoire", "Madagascar", "Swaziland", "Comoros", "Malawi", "Tanzania", "Congo Democratic",
                   "Mali", "Tunisia", "Congo Republic", "Mauritania", "Togo", "Djibouti", "Mauritius", "Uganda", "Egypt", "Morocco", 
                  "Zambia", "Equatorial Guinea", "Mozambique", "Zimbawe", "Eritrea", "Namibia	")
  
# Subset of countries
  
afdata <-subset(knowledge, knowledge$Country %in% afcountries)
head (afdata)
# write.xlsx(afdata, file= "afdata.xlsx", row.names = FALSE) #Without Java requirmentes (writexl, library)

# Summary
summary(afdata)

# Number of countries  

countrylist = unique(afdata$Country)
length(countrylist)

# Years   = number of years 24

yearlist = unique(afdata$Year)
length(yearlist)

# Number of missing values
n_miss(knowledge)
n_complete(knowledge)
prop_complete(knowledge)
pct_miss(knowledge)

#pattern of missing values in each variable
miss_var_summary(afdata[, c(3:14)])


# Missing values per variable per country

  
afdata %>% group_by(Country) %>% 
    miss_var_summary()

# using GGally

ggpairs(afdata[, c(3:14)])

# Correlation matrix

m = cor(knowledge[, c(3:14)], use ="pairwise")
m
corrplot(m)

# Generate regional dummiess
ca <- c("Angola", "Cameroon", "Cabo Verde", "Central African Republic", "Chad","Equatorial Guinea", "Eritrea", "Ethiopia",
        "Gabon", "Sao Tome and Principe")
knowledge$Country.centralafrica <- ifelse(knowledge$Country %in% ca,1,0)

ea <- c ("Burundi", "Comoros", "Congo Democratic Republic", "Congo Republic", "Djibouti", "Kenya",
         "Rwanda", "Seychelles", "Somalia", "Sudan", "Tanzania", "Uganda", "Zambia", "Zimbawe")

knowledge$Country.eastafrica <- ifelse(knowledge$Country %in% ea,1,0)

na <- c("Algeria", "Egypt", "Libya", "Mauritania", "Morocco", "Sudan", "Tunisia")
knowledge$Country.northafrica <- ifelse(knowledge$Country %in% na,1,0)

sa<-  c("Botswana", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "South  Africa", "Swaziland")
knowledge$Country.southafrica <- ifelse(knowledge$Country %in% sa,1,0)

wa <- c("Benin", "Burkina Faso", "Cote d'Ivoire", "Gambia", "Guinea Bissau", "Ghana", "Guinea", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
knowledge$Country.westafrica <- ifelse(knowledge$Country %in% wa,1,0)

# Split the data per year, generate 23 data frames

datayear <- split( afdata , f = afdata$Year)
datayear$`1995`
summary(datayear$`1995`)

year1995 <-as.data.frame(datayear$`1995`)
