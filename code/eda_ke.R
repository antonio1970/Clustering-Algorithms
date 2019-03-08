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
                    "Gambia, The",	"Rwanda", "Botswana", "South Africa", "Burkina Faso",
                  "Ghana", "Sao Tome and Principe", "Burundi", "Guinea", "Senegal", "Cameroon", "Kenya", "Seychelles",
                  "Cabo Verde", "Lesotho", "Sierra Leone", "Central African Republic", "Liberia",	"Chad",	
                  "Libya", "Sudan", "Cote d'Ivoire", "Madagascar", "Comoros", "Malawi", "Tanzania", "Congo, Dem. Rep.",
                   "Mali", "Tunisia", "Congo, Rep.", "Mauritania", "Togo", "Djibouti", "Mauritius", "Uganda", "Egypt, Arab Rep.", "Morocco", 
                  "Zambia", "Equatorial Guinea", "Mozambique", "Zimbabwe", "Eritrea", "Namibia")
  
# Subset of countries
  
afdata <-subset(knowledge, knowledge$Country %in% afcountries)


# Select a set of variables to be used in the empirical analysis

afdata1 <- afdata %>% 
  select(Country, Year, REGQU, RULEL, PRIMARY, SECONDARY, TERTIARY,TELEP3,FIXBI2,INTERN3, PATEN2, STJOU2, TNTBA)


# Generate regional dummiess
ca <- c("Angola", "Cameroon", "Cabo Verde", "Central African Republic", "Chad","Equatorial Guinea", "Eritrea", "Ethiopia",
        "Gabon", "Sao Tome and Principe")
ea <- c ("Burundi", "Comoros", "Congo, Dem. Rep.", "Congo, Rep.", "Djibouti", "Kenya",
         "Rwanda", "Seychelles", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
na <- c("Algeria", "Egypt, Arab Rep.", "Libya", "Mauritania", "Morocco", "Sudan", "Tunisia")
sa<-  c("Botswana", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "South Africa")
wa <- c("Benin", "Burkina Faso", "Cote d'Ivoire", "Gambia, The", "Ghana", "Guinea", "Liberia", "Mali", "Niger", 
        "Nigeria", "Senegal", "Sierra Leone", "Togo")


# De manera más elegante

func1 <- function(x){
  ifelse(x %in% ca, "Central Africa", ifelse(x %in% ea,"East Africa", ifelse(x%in% sa, "South Africa", ifelse(x%in% wa, "West Africa", ifelse(x%in% na, "North Africa", "otros")))))
}

afdata1$region <- func1(afdata1$Country)
afdata1$region = as.factor(afdata1$region)



# Number of countries included in the sample 

countrylist = unique(afdata1$Country)
length(countrylist)

# Number of time periods 

yearlist = unique(afdata1$Year)
length(yearlist)

# Number of missing values (% percentage)

prop_complete(afdata1)
pct_miss(afdata1)

#pattern of missing values in each variable
miss_var_summary(afdata1[, c(3:13)])



# using GGally to show a complete picture of the features to be used in the analysis

ggpairs(afdata1[, c(3:13)])

# Correlation matrix

m = cor(afdata1[, c(3:13)], use ="pairwise")
m
corrplot(m, method = 'number', type = 'upper')


# Summary of the variables for different time periods

afdata2011 = subset(afdata1, Year>=2011)  # 2011-2017 time period
summary(afdata2011)
plot(afdata2011, col= afdata2011$region)

afdata0610 = subset(afdata1, Year>=2006 & Year<2011) 
summary(afdata0610)
plot(afdata0610, col = afdata$region)

afdata0105 = subset(afdata1, Year>=2001 & Year<2006)
summary(afdata0105)
plot(afdata0105, col =afdata$region)

# Save file as csv and excel file

write.csv(afdata1, file= "afdata1.csv", row.names = FALSE)
write.xlsx(afdata1, file ="afdata1.xlsx", row.names = FALSE)

