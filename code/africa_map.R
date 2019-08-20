# read dbf file
library(foreign)
africa <-read.dbf("../Africa_SHP/Africa.dbf", as.is = F)
table(africa$COUNTRY)

# Check the names of the countries included in the shapefile

paises_shp <-list(unique(africa$COUNTRY))
table(paises_shp)

length(unique(africa$COUNTRY)) # 52 paises
# Check the names of the countries included in my imputed datafile

paises_database <- list(unique(afdata$Country))
paises_database
length(unique(afdata$Country)) # 50 paises

identical(paises_database, paises_shp) # they are not identical

# Compare both vectors
africa$COUNTRY<-as.character(africa$COUNTRY)
afdata$Country<-as.character(afdata$Country)
setdiff(africa$COUNTRY, afdata$Country)

# To correct the name of the shapefile
africa$COUNTRY[africa$COUNTRY =="Egypt"]<- "Egypt, Arab Rep."

# averages
primary_promedio <- as.data.table(aggregate(PRIMARY~Country, data = afdata, FUN = mean))

primary_promedio %>% 
  group_by(Country) %>% 
  arrange(desc(PRIMARY)) %>% 
  head()
