# load libraries before loading the data
library(tidyverse)
library(writexl)

# Reading the dataset
total_pop <- read.csv("../Clustering-Algorithms/data/POPULATION_WB.csv",  sep = ";")


# Using tidyr to reshape the data accordingly, and using arrange to order alphabetically. Panel data format

gather(total_pop, key = Year, value = totpop, year60:year2017) %>% 
  arrange(Country)
#  Save the file as csv file

write_csv(total_pop, path = "C:/Users/anton/OneDrive/Escritorio/Clustering-Algorithms/totalpop.csv")

# Alternatively, save the file as an Excel file in our working directory


write_xlsx(total_pop, "C:/Users/anton/OneDrive/Escritorio/Clustering-Algorithms/data/totalpop.xlsx")
