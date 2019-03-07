# Load libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(naniar)
library(readxl)

# read csv file

knowledge <- read.csv2("../Clustering-Algorithms/data/KE_data_1995-2017 final version_cluster.csv", header = TRUE, dec = ",", sep = ";")

# Select subset of countries

afcountries <- c ("Algeria",	"Ethiopia", "Niger", "Angola", "Gabon", "Nigeria","Benin",
                    "Gambia",	"Rwanda", "Botswana", "Guinea Bissau", "South Africa", "Burkina Faso",
                  "Ghana", "Sao Tome and Principe", "Burundi", "Guinea", "Senegal", "Cameroon", "Kenya", "Seychelles",
                  "Cabo Verde", "Lesotho", "Sierra Leone", "Central African Republic", "Liberia",	"Somalia", "Chad",	
                  "Libya", "Sudan", "Cote d'Ivoire", "Madagascar", "Swaziland", "Comoros", "Malawi", "Tanzania", "Congo Democratic Republic",
                   "Mali", "Tunisia", "Congo Republic", "Mauritania", "Togo", "Djibouti", "Mauritius", "Uganda", "Egypt", "Morocco", 
                  "Zambia", "Equatorial Guinea", "Mozambique", "Zimbabwe", "Eritrea", "Namibia")
  
# Subset of countries
  
afdata <-subset(knowledge, knowledge$Country %in% afcountries)


# Generate regional dummiess
ca <- c("Angola", "Cameroon", "Cabo Verde", "Central African Republic", "Chad","Equatorial Guinea", "Eritrea", "Ethiopia",
        "Gabon", "Sao Tome and Principe")
ea <- c ("Burundi", "Comoros", "Congo Democratic Republic", "Congo Republic", "Djibouti", "Kenya",
         "Rwanda", "Seychelles", "Somalia", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
na <- c("Algeria", "Egypt", "Libya", "Mauritania", "Morocco", "Sudan", "Tunisia")
sa<-  c("Botswana", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "South Africa", "Swaziland")
wa <- c("Benin", "Burkina Faso", "Cote d'Ivoire", "Gambia", "Guinea Bissau", "Ghana", "Guinea", "Liberia", "Mali", "Niger", 
        "Nigeria", "Senegal", "Sierra Leone", "Togo")


# write.csv(afdata, file= "afdata.csv", row.names = FALSE) #Without Java requirmentes (writexl, library)


# De manera más elegante

func1 <- function(x){
  ifelse(x %in% ca, "Central Africa", ifelse(x %in% ea,"East Africa", ifelse(x%in% sa, "South Africa", ifelse(x%in% wa, "West Africa", ifelse(x%in% na, "North Africa", "otros")))))
}

afdata$region <- func1(afdata$Country)

table(afdata$region)

afdata$region = as.factor(afdata$region)

# Write csv file
write.csv(afdata, file= "afdata.csv", row.names = FALSE)
write.xlsx(afdata, file ="afdata.xlsx", row.names = FALSE)
# Summary
summary(afdata)

# Number of countries included in the sample 

countrylist = unique(afdata$Country)
length(countrylist)

# Number of time periods 

yearlist = unique(afdata$Year)
length(yearlist)

# Number of missing values (% percentage)

prop_complete(afdata)
pct_miss(afdata)

#pattern of missing values in each variable
miss_var_summary(afdata[, c(3:15)])


# Missing values per country

  
afdata %>% group_by(Country) %>% 
    miss_var_summary()

# Missing values per year

afdata %>% group_by(Year) %>% 
  miss_var_summary()

# using GGally to show a complete picture of the features to be used in the analysis

ggpairs(afdata[, c(3:15)])

# Correlation matrix

m = cor(knowledge[, c(3:15)], use ="pairwise")
m
corrplot(m)


# cORRELATION per year

afdata %>%
  group_by(Year) %>%
  summarize(COR=cor(REGQU, RULEL, use="pairwise.complete.obs")) 
