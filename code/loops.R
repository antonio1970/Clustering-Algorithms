
# Imputation of some of the variables before using Knns

# Generate a copy of the data

afdata1_imputed <- afdata1

save(afdata1_imputed, file = "afdata1_imputed.Rdata")


# Generate a loop for filling with zeros missing values for FIXBI2, and check the result

n=length(afdata1_imputed$FIXBI2)
i=1
while (i<=n)
{
  
  if (is.na(afdata1_imputed$FIXBI2[i])) {
    afdata1_imputed$FIXBI2[i] <-0
    i=i+1
    next
  }
  
  print(paste (afdata1_imputed$Country[i], " : ",afdata1_imputed$FIXBI2[i]))
  #move to the next country
  while (afdata1_imputed$Year[i]!=1995 & i<=n){
    i=i+1
  }
}






