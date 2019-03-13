library("caret") # install.packages("RANN")

afdata = read.csv("afdata.csv", header = TRUE, dec = ".", sep = ",")

#impute FIXBI2 filling up with zeros NAs until find a non-NA value

n=length(afdata$FIXBI2)
i=1
while (i<=n)
{
  
  if (is.na(afdata$FIXBI2[i])) {
    afdata$FIXBI2[i] <-0
    i=i+1
    next
  }
  
  print(paste (afdata$Country[i], " : ",afdata$FIXBI2[i]))
  #move to the next country
  while (afdata$Year[i]!=1995 & i<=n){
    i=i+1
  }
}


# Select for imputation of KNNs

afdata <- afdata %>% 
  select(Country, Year, PRIMARY, SECONDARY, TERTIARY)

# inpute data

k_value=3



knn_imput = function(afdata) {
  numeric_columns = (sapply(afdata, class) %in% c("numeric", "integer"))
  means = apply(afdata[, numeric_columns], 2, mean, na.rm = TRUE)
  sds = apply(afdata[, numeric_columns], 2, sd, na.rm = TRUE)
  
  afdata[, numeric_columns] = sweep(afdata[, numeric_columns], 2, means)
  afdata[, numeric_columns] = sweep(afdata[, numeric_columns], 2, sds, "/")
  
  imputer = preProcess(afdata, method = "knnImpute", k=k_value)
  
  imputed_data =  predict(imputer, newdata = afdata)
  
  imputed_data[, numeric_columns] = sweep(
    imputed_data[, numeric_columns], 2, sds, "*"
  )
  imputed_data[, numeric_columns] = sweep(
    imputed_data[, numeric_columns], 2, means, "+"
  )
  
  imputed_data
}

imputed_data = knn_imput(afdata)

# "mark" imputed data

numeric_columns = which(sapply(afdata, class) %in% c("numeric", "integer"))
modified_afdata = afdata

for (col in numeric_columns) {
  indices = is.na(afdata[, col])
  modified_afdata[indices, col] = (
    format(round(imputed_data[indices, col], 3), nsmall = 3)
  )
}

