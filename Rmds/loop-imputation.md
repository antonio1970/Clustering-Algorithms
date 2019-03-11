loop:imputation
================

``` r
library(naniar)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
load(file= "C:/Users/anton/OneDrive/Escritorio/Clustering-Algorithms/afdata1_imputed.Rdata")
```

``` r
# Check FIXBI2 missing values
table(is.na(afdata1_imputed$FIXBI2))
```

    ## 
    ## FALSE  TRUE 
    ##   623   527

``` r
prop.table(table(is.na(afdata1_imputed$FIXBI2)))*100
```

    ## 
    ##    FALSE     TRUE 
    ## 54.17391 45.82609

``` r
afdata1_imputed %>% 
  group_by(Country) %>% 
     summarise(sumNA = sum(is.na(FIXBI2)))
```

    ## # A tibble: 50 x 2
    ##    Country                  sumNA
    ##    <fct>                    <int>
    ##  1 Algeria                      8
    ##  2 Angola                      11
    ##  3 Benin                        7
    ##  4 Botswana                    10
    ##  5 Burkina Faso                 7
    ##  6 Burundi                     13
    ##  7 Cabo Verde                   9
    ##  8 Cameroon                    10
    ##  9 Central African Republic    20
    ## 10 Chad                        14
    ## # ... with 40 more rows

``` r
#generate loop for filling in missing values with zeros
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
```

    ## [1] "Algeria  :  0.555495308"
    ## [1] "Angola  :  0.368070928"
    ## [1] "Benin  :  0.002878529"
    ## [1] "Botswana  :  0.862137714"
    ## [1] "Burkina Faso  :  0.004067322"
    ## [1] "Burundi  :  0.019483056"
    ## [1] "Cabo Verde  :  0.605135311"
    ## [1] "Cameroon  :  0.011595338"
    ## [1] "Central African Republic  :  0.180958962"
    ## [1] "Chad  :  0.025819832"
    ## [1] "Comoros  :  0.001674402"
    ## [1] "Cote d'Ivoire  :  0.023360581"
    ## [1] "Congo, Dem. Rep.  :  0"
    ## [1] "Djibouti  :  0.00557426"
    ## [1] "Egypt, Arab Rep.  :  0.701707084"
    ## [1] "Equatorial Guinea  :  0.237681182"
    ## [1] "Eritrea  :  0.010395413"
    ## [1] "Ethiopia  :  0.000785718"
    ## [1] "Gabon  :  0.127997976"
    ## [1] "Ghana  :  0.042884638"
    ## [1] "Guinea  :  0.046321301"
    ## [1] "Kenya  :  0.149771329"
    ## [1] "Lesotho  :  0.011376988"
    ## [1] "Liberia  :  0.030030087"
    ## [1] "Libya  :  7.639419152"
    ## [1] "Madagascar  :  0.072085841"
    ## [1] "Malawi  :  0.005593074"
    ## [1] "Mali  :  0.233233921"
    ## [1] "Mauritania  :  0.052384116"
    ## [1] "Mauritius  :  0.236588935"
    ## [1] "Morocco  :  0.067768198"
    ## [1] "Mozambique  :  0.102146596"
    ## [1] "Namibia  :  0.065938522"
    ## [1] "Niger  :  0.005865767"
    ## [1] "Nigeria  :  0.003598689"
    ## [1] "Congo, Rep.  :  0.029386099"
    ## [1] "Rwanda  :  0.130181785"
    ## [1] "Sao Tome and Principe  :  0.784545089"
    ## [1] "Senegal  :  0.115419452"
    ## [1] "Seychelles  :  4.231585329"
    ## [1] "South Africa  :  0.056755628"
    ## [1] "Sudan  :  0.02627016"
    ## [1] "Tanzania  :  0.065693078"
    ## [1] "Gambia, The  :  0.025740523"
    ## [1] "Togo  :  0.200253944"
    ## [1] "Tunisia  :  0.00263576"
    ## [1] "Uganda  :  0.02977865"
    ## [1] "Zambia  :  0.001994071"
    ## [1] "Zimbabwe  :  0.062347543"

``` r
# Proportion of missing values after imputation

afdata1_imputed %>% 
  filter(is.na(FIXBI2)) %>% 
  count(Country)
```

    ## # A tibble: 17 x 2
    ##    Country                      n
    ##    <fct>                    <int>
    ##  1 Central African Republic     4
    ##  2 Comoros                      1
    ##  3 Congo, Dem. Rep.             2
    ##  4 Congo, Rep.                  3
    ##  5 Cote d'Ivoire                2
    ##  6 Djibouti                     2
    ##  7 Equatorial Guinea            1
    ##  8 Eritrea                      1
    ##  9 Gambia, The                  2
    ## 10 Guinea                       1
    ## 11 Lesotho                      1
    ## 12 Liberia                      1
    ## 13 Libya                        1
    ## 14 Malawi                       1
    ## 15 Mali                         2
    ## 16 Nigeria                      2
    ## 17 Sudan                        1
