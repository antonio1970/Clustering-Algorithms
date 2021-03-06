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
missing<- afdata1_imputed %>% 
  group_by(Country) %>% 
     summarise(sumNA = sum(is.na(FIXBI2))) %>% 
  print(missing, n=50)
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
    ## 11 Comoros                     10
    ## 12 Congo, Dem. Rep.            17
    ## 13 Congo, Rep.                 17
    ## 14 Cote d'Ivoire               10
    ## 15 Djibouti                     7
    ## 16 Egypt, Arab Rep.             7
    ## 17 Equatorial Guinea           11
    ## 18 Eritrea                     14
    ## 19 Ethiopia                     8
    ## 20 Gabon                        8
    ## 21 Gambia, The                 11
    ## 22 Ghana                        9
    ## 23 Guinea                      16
    ## 24 Kenya                       10
    ## 25 Lesotho                     10
    ## 26 Liberia                     14
    ## 27 Libya                       14
    ## 28 Madagascar                  11
    ## 29 Malawi                       9
    ## 30 Mali                        13
    ## 31 Mauritania                  10
    ## 32 Mauritius                    7
    ## 33 Morocco                      7
    ## 34 Mozambique                  11
    ## 35 Namibia                     10
    ## 36 Niger                        9
    ## 37 Nigeria                     12
    ## 38 Rwanda                       9
    ## 39 Sao Tome and Principe       11
    ## 40 Senegal                      7
    ## 41 Seychelles                   9
    ## 42 Sierra Leone                23
    ## 43 South Africa                 7
    ## 44 Sudan                       10
    ## 45 Tanzania                    10
    ## 46 Togo                        12
    ## 47 Tunisia                      7
    ## 48 Uganda                      10
    ## 49 Zambia                       5
    ## 50 Zimbabwe                     6

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
# Histogram after imputation
hist(afdata1_imputed$FIXBI2, col = 'blue')
```

![](loop-imputation_files/figure-markdown_github/unnamed-chunk-6-1.png)

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

``` r
afdata1_imputed[afdata1_imputed$Country=="Central African Republic", c("Country","Year", "FIXBI2")]
```

    ##                      Country Year    FIXBI2
    ## 645 Central African Republic 1995 0.0000000
    ## 646 Central African Republic 1996 0.0000000
    ## 647 Central African Republic 1997 0.0000000
    ## 648 Central African Republic 1998 0.0000000
    ## 649 Central African Republic 1999 0.0000000
    ## 650 Central African Republic 2000 0.0000000
    ## 651 Central African Republic 2001 0.0000000
    ## 652 Central African Republic 2002 0.0000000
    ## 653 Central African Republic 2003 0.0000000
    ## 654 Central African Republic 2004 0.0000000
    ## 655 Central African Republic 2005 0.0000000
    ## 656 Central African Republic 2006 0.0000000
    ## 657 Central African Republic 2007 0.0000000
    ## 658 Central African Republic 2008 0.0000000
    ## 659 Central African Republic 2009 0.0000000
    ## 660 Central African Republic 2010 0.0000000
    ## 661 Central African Republic 2011 0.1809590
    ## 662 Central African Republic 2012 0.1347314
    ## 663 Central African Republic 2013        NA
    ## 664 Central African Republic 2014        NA
    ## 665 Central African Republic 2015 0.1979719
    ## 666 Central African Republic 2016        NA
    ## 667 Central African Republic 2017        NA

``` r
#linear imputation for the rest
library(imputeTS)
afdata1_imputed$FIXBI2 <- na.interpolation(afdata1_imputed$FIXBI2, option ="linear")
```

``` r
afdata1_imputed[afdata1_imputed$Country=="Central African Republic", c("Country","Year", "FIXBI2")]
```

    ##                      Country Year     FIXBI2
    ## 645 Central African Republic 1995 0.00000000
    ## 646 Central African Republic 1996 0.00000000
    ## 647 Central African Republic 1997 0.00000000
    ## 648 Central African Republic 1998 0.00000000
    ## 649 Central African Republic 1999 0.00000000
    ## 650 Central African Republic 2000 0.00000000
    ## 651 Central African Republic 2001 0.00000000
    ## 652 Central African Republic 2002 0.00000000
    ## 653 Central African Republic 2003 0.00000000
    ## 654 Central African Republic 2004 0.00000000
    ## 655 Central African Republic 2005 0.00000000
    ## 656 Central African Republic 2006 0.00000000
    ## 657 Central African Republic 2007 0.00000000
    ## 658 Central African Republic 2008 0.00000000
    ## 659 Central African Republic 2009 0.00000000
    ## 660 Central African Republic 2010 0.00000000
    ## 661 Central African Republic 2011 0.18095896
    ## 662 Central African Republic 2012 0.13473139
    ## 663 Central African Republic 2013 0.15581156
    ## 664 Central African Republic 2014 0.17689172
    ## 665 Central African Republic 2015 0.19797189
    ## 666 Central African Republic 2016 0.13198126
    ## 667 Central African Republic 2017 0.06599063
