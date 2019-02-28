# Load libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(naniar)

# read csv file

knowledge <- read.csv2("../Proyecto investigacion/Nilufar datos de KE/Cluster datos/KE_data_1995-2017 final version (corrected).csv", header = TRUE, dec = ",", sep = ";")

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
prop_miss(knowledge)
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


# Split the data per year, generate 23 data frames

datayear <- split( afdata , f = afdata$Year)
datayear$`1995`
summary(datayear$`1995`)

year1995 <-as.data.frame(datayear$`1995`)
