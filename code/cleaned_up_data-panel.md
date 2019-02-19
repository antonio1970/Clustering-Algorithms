cleaned up panel-data (Excel)
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.1.0     v purrr   0.2.5
    ## v tibble  2.0.1     v dplyr   0.7.8
    ## v tidyr   0.8.2     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'readr' was built under R version 3.5.2

    ## -- Conflicts --------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(writexl)
```

    ## Warning: package 'writexl' was built under R version 3.5.2

``` r
total_pop <- read.csv("../data/POPULATION_WB.csv",  sep = ";")
head(total_pop, 5)
```

    ##          Country   year60   year61   year62   year63   year64   year65
    ## 1    Afghanistan  8996351  9166764  9345868  9533954  9731361  9938414
    ## 2        Albania  1608800  1659800  1711319  1762621  1814135  1864791
    ## 3        Algeria 11124888 11404859 11690153 11985136 12295970 12626952
    ## 4 American Samoa    20013    20486    21117    21882    22698    23520
    ## 5        Andorra    13411    14375    15370    16412    17469    18549
    ##     year66   year67   year68   year69   year70   year71   year72   year73
    ## 1 10152331 10372630 10604346 10854428 11126123 11417825 11721940 12027822
    ## 2  1914573  1965598  2022272  2081695  2135479  2187853  2243126  2296752
    ## 3 12980267 13354197 13744387 14144438 14550034 14960109 15377093 15804428
    ## 4    24321    25116    25885    26614    27292    27916    28492    29014
    ## 5    19647    20758    21890    23058    24276    25559    26892    28232
    ##     year74   year75   year76   year77   year78   year79   year80   year81
    ## 1 12321541 12590286 12840299 13067538 13237734 13306695 13248370 13053954
    ## 2  2350124  2404831  2458526  2513546  2566266  2617832  2671997  2726056
    ## 3 16247113 16709099 17190239 17690184 18212326 18760761 19337715 19943664
    ## 4    29488    29932    30321    30689    31102    31673    32457    33493
    ## 5    29520    30705    31777    32771    33737    34818    36067    37500
    ##     year82   year83   year84   year85   year86   year87   year88   year89
    ## 1 12749645 12389269 12047115 11783050 11601041 11502761 11540888 11777609
    ## 2  2784278  2843960  2904429  2964762  3022635  3083605  3142336  3227943
    ## 3 20575701 21228289 21893853 22565905 23241272 23917897 24591492 25257672
    ## 4    34738    36160    37688    39241    40837    42450    44047    45593
    ## 5    39114    40867    42706    44600    46517    48455    50434    52448
    ##     year90   year91   year92   year93   year94   year95   year96   year97
    ## 1 12249114 12993657 13981231 15095099 16172719 17099541 17822884 18381605
    ## 2  3286542  3266790  3247039  3227287  3207536  3187784  3168033  3148281
    ## 3 25912367 26554329 27181094 27786259 28362253 28904298 29411415 29886839
    ## 4    47038    48375    49593    50720    51803    52868    53929    54941
    ## 5    54509    56671    58888    60971    62677    63850    64360    64327
    ##     year98   year99 year2000 year2001 year2002 year2003 year2004 year2005
    ## 1 18863999 19403676 20093756 20966463 21979923 23064851 24118979 25070798
    ## 2  3128530  3108778  3089027  3060173  3051010  3039616  3026939  3011487
    ## 3 30335732 30765613 31183660 31592153 31995046 32403514 32831096 33288437
    ## 4    55901    56770    57521    58175    58731    59117    59264    59118
    ## 5    64142    64370    65390    67341    70049    73182    76244    78867
    ##   year2006 year2007 year2008 year2009 year2010 year2011 year2012 year2013
    ## 1 25893450 26616792 27294031 28004331 28803167 29708599 30696958 31731688
    ## 2  2992547  2970017  2947314  2927519  2913021  2905195  2900401  2895092
    ## 3 33777915 34300076 34860715 35465760 36117637 36819558 37565847 38338562
    ## 4    58650    57903    57030    56227    55637    55320    55230    55307
    ## 5    80991    82683    83861    84462    84449    83751    82431    80788
    ##   year2014 year2015 year2016 year2017
    ## 1 32758020 33736494 34656032 35530081
    ## 2  2889104  2880703  2876101  2873457
    ## 3 39113313 39871528 40606052 41318142
    ## 4    55437    55537    55599    55641
    ## 5    79223    78014    77281    76965

``` r
gather(total_pop, key = Year, value = totpop, year60:year2017) %>% 
  arrange(Country)
```

    ##                              Country     Year     totpop
    ## 1                        Afghanistan   year60    8996351
    ## 2                        Afghanistan   year61    9166764
    ## 3                        Afghanistan   year62    9345868
    ## 4                        Afghanistan   year63    9533954
    ## 5                        Afghanistan   year64    9731361
    ## 6                        Afghanistan   year65    9938414
    ## 7                        Afghanistan   year66   10152331
    ## 8                        Afghanistan   year67   10372630
    ## 9                        Afghanistan   year68   10604346
    ## 10                       Afghanistan   year69   10854428
    ## 11                       Afghanistan   year70   11126123
    ## 12                       Afghanistan   year71   11417825
    ## 13                       Afghanistan   year72   11721940
    ## 14                       Afghanistan   year73   12027822
    ## 15                       Afghanistan   year74   12321541
    ## 16                       Afghanistan   year75   12590286
    ## 17                       Afghanistan   year76   12840299
    ## 18                       Afghanistan   year77   13067538
    ## 19                       Afghanistan   year78   13237734
    ## 20                       Afghanistan   year79   13306695
    ## 21                       Afghanistan   year80   13248370
    ## 22                       Afghanistan   year81   13053954
    ## 23                       Afghanistan   year82   12749645
    ## 24                       Afghanistan   year83   12389269
    ## 25                       Afghanistan   year84   12047115
    ## 26                       Afghanistan   year85   11783050
    ## 27                       Afghanistan   year86   11601041
    ## 28                       Afghanistan   year87   11502761
    ## 29                       Afghanistan   year88   11540888
    ## 30                       Afghanistan   year89   11777609
    ## 31                       Afghanistan   year90   12249114
    ## 32                       Afghanistan   year91   12993657
    ## 33                       Afghanistan   year92   13981231
    ## 34                       Afghanistan   year93   15095099
    ## 35                       Afghanistan   year94   16172719
    ## 36                       Afghanistan   year95   17099541
    ## 37                       Afghanistan   year96   17822884
    ## 38                       Afghanistan   year97   18381605
    ## 39                       Afghanistan   year98   18863999
    ## 40                       Afghanistan   year99   19403676
    ## 41                       Afghanistan year2000   20093756
    ## 42                       Afghanistan year2001   20966463
    ## 43                       Afghanistan year2002   21979923
    ## 44                       Afghanistan year2003   23064851
    ## 45                       Afghanistan year2004   24118979
    ## 46                       Afghanistan year2005   25070798
    ## 47                       Afghanistan year2006   25893450
    ## 48                       Afghanistan year2007   26616792
    ## 49                       Afghanistan year2008   27294031
    ## 50                       Afghanistan year2009   28004331
    ## 51                       Afghanistan year2010   28803167
    ## 52                       Afghanistan year2011   29708599
    ## 53                       Afghanistan year2012   30696958
    ## 54                       Afghanistan year2013   31731688
    ## 55                       Afghanistan year2014   32758020
    ## 56                       Afghanistan year2015   33736494
    ## 57                       Afghanistan year2016   34656032
    ## 58                       Afghanistan year2017   35530081
    ## 59                           Albania   year60    1608800
    ## 60                           Albania   year61    1659800
    ## 61                           Albania   year62    1711319
    ## 62                           Albania   year63    1762621
    ## 63                           Albania   year64    1814135
    ## 64                           Albania   year65    1864791
    ## 65                           Albania   year66    1914573
    ## 66                           Albania   year67    1965598
    ## 67                           Albania   year68    2022272
    ## 68                           Albania   year69    2081695
    ## 69                           Albania   year70    2135479
    ## 70                           Albania   year71    2187853
    ## 71                           Albania   year72    2243126
    ## 72                           Albania   year73    2296752
    ## 73                           Albania   year74    2350124
    ## 74                           Albania   year75    2404831
    ## 75                           Albania   year76    2458526
    ## 76                           Albania   year77    2513546
    ## 77                           Albania   year78    2566266
    ## 78                           Albania   year79    2617832
    ## 79                           Albania   year80    2671997
    ## 80                           Albania   year81    2726056
    ## 81                           Albania   year82    2784278
    ## 82                           Albania   year83    2843960
    ## 83                           Albania   year84    2904429
    ## 84                           Albania   year85    2964762
    ## 85                           Albania   year86    3022635
    ## 86                           Albania   year87    3083605
    ## 87                           Albania   year88    3142336
    ## 88                           Albania   year89    3227943
    ## 89                           Albania   year90    3286542
    ## 90                           Albania   year91    3266790
    ## 91                           Albania   year92    3247039
    ## 92                           Albania   year93    3227287
    ## 93                           Albania   year94    3207536
    ## 94                           Albania   year95    3187784
    ## 95                           Albania   year96    3168033
    ## 96                           Albania   year97    3148281
    ## 97                           Albania   year98    3128530
    ## 98                           Albania   year99    3108778
    ## 99                           Albania year2000    3089027
    ## 100                          Albania year2001    3060173
    ## 101                          Albania year2002    3051010
    ## 102                          Albania year2003    3039616
    ## 103                          Albania year2004    3026939
    ## 104                          Albania year2005    3011487
    ## 105                          Albania year2006    2992547
    ## 106                          Albania year2007    2970017
    ## 107                          Albania year2008    2947314
    ## 108                          Albania year2009    2927519
    ## 109                          Albania year2010    2913021
    ## 110                          Albania year2011    2905195
    ## 111                          Albania year2012    2900401
    ## 112                          Albania year2013    2895092
    ## 113                          Albania year2014    2889104
    ## 114                          Albania year2015    2880703
    ## 115                          Albania year2016    2876101
    ## 116                          Albania year2017    2873457
    ## 117                          Algeria   year60   11124888
    ## 118                          Algeria   year61   11404859
    ## 119                          Algeria   year62   11690153
    ## 120                          Algeria   year63   11985136
    ## 121                          Algeria   year64   12295970
    ## 122                          Algeria   year65   12626952
    ## 123                          Algeria   year66   12980267
    ## 124                          Algeria   year67   13354197
    ## 125                          Algeria   year68   13744387
    ## 126                          Algeria   year69   14144438
    ## 127                          Algeria   year70   14550034
    ## 128                          Algeria   year71   14960109
    ## 129                          Algeria   year72   15377093
    ## 130                          Algeria   year73   15804428
    ## 131                          Algeria   year74   16247113
    ## 132                          Algeria   year75   16709099
    ## 133                          Algeria   year76   17190239
    ## 134                          Algeria   year77   17690184
    ## 135                          Algeria   year78   18212326
    ## 136                          Algeria   year79   18760761
    ## 137                          Algeria   year80   19337715
    ## 138                          Algeria   year81   19943664
    ## 139                          Algeria   year82   20575701
    ## 140                          Algeria   year83   21228289
    ## 141                          Algeria   year84   21893853
    ## 142                          Algeria   year85   22565905
    ## 143                          Algeria   year86   23241272
    ## 144                          Algeria   year87   23917897
    ## 145                          Algeria   year88   24591492
    ## 146                          Algeria   year89   25257672
    ## 147                          Algeria   year90   25912367
    ## 148                          Algeria   year91   26554329
    ## 149                          Algeria   year92   27181094
    ## 150                          Algeria   year93   27786259
    ## 151                          Algeria   year94   28362253
    ## 152                          Algeria   year95   28904298
    ## 153                          Algeria   year96   29411415
    ## 154                          Algeria   year97   29886839
    ## 155                          Algeria   year98   30335732
    ## 156                          Algeria   year99   30765613
    ## 157                          Algeria year2000   31183660
    ## 158                          Algeria year2001   31592153
    ## 159                          Algeria year2002   31995046
    ## 160                          Algeria year2003   32403514
    ## 161                          Algeria year2004   32831096
    ## 162                          Algeria year2005   33288437
    ## 163                          Algeria year2006   33777915
    ## 164                          Algeria year2007   34300076
    ## 165                          Algeria year2008   34860715
    ## 166                          Algeria year2009   35465760
    ## 167                          Algeria year2010   36117637
    ## 168                          Algeria year2011   36819558
    ## 169                          Algeria year2012   37565847
    ## 170                          Algeria year2013   38338562
    ## 171                          Algeria year2014   39113313
    ## 172                          Algeria year2015   39871528
    ## 173                          Algeria year2016   40606052
    ## 174                          Algeria year2017   41318142
    ## 175                   American Samoa   year60      20013
    ## 176                   American Samoa   year61      20486
    ## 177                   American Samoa   year62      21117
    ## 178                   American Samoa   year63      21882
    ## 179                   American Samoa   year64      22698
    ## 180                   American Samoa   year65      23520
    ## 181                   American Samoa   year66      24321
    ## 182                   American Samoa   year67      25116
    ## 183                   American Samoa   year68      25885
    ## 184                   American Samoa   year69      26614
    ## 185                   American Samoa   year70      27292
    ## 186                   American Samoa   year71      27916
    ## 187                   American Samoa   year72      28492
    ## 188                   American Samoa   year73      29014
    ## 189                   American Samoa   year74      29488
    ## 190                   American Samoa   year75      29932
    ## 191                   American Samoa   year76      30321
    ## 192                   American Samoa   year77      30689
    ## 193                   American Samoa   year78      31102
    ## 194                   American Samoa   year79      31673
    ## 195                   American Samoa   year80      32457
    ## 196                   American Samoa   year81      33493
    ## 197                   American Samoa   year82      34738
    ## 198                   American Samoa   year83      36160
    ## 199                   American Samoa   year84      37688
    ## 200                   American Samoa   year85      39241
    ## 201                   American Samoa   year86      40837
    ## 202                   American Samoa   year87      42450
    ## 203                   American Samoa   year88      44047
    ## 204                   American Samoa   year89      45593
    ## 205                   American Samoa   year90      47038
    ## 206                   American Samoa   year91      48375
    ## 207                   American Samoa   year92      49593
    ## 208                   American Samoa   year93      50720
    ## 209                   American Samoa   year94      51803
    ## 210                   American Samoa   year95      52868
    ## 211                   American Samoa   year96      53929
    ## 212                   American Samoa   year97      54941
    ## 213                   American Samoa   year98      55901
    ## 214                   American Samoa   year99      56770
    ## 215                   American Samoa year2000      57521
    ## 216                   American Samoa year2001      58175
    ## 217                   American Samoa year2002      58731
    ## 218                   American Samoa year2003      59117
    ## 219                   American Samoa year2004      59264
    ## 220                   American Samoa year2005      59118
    ## 221                   American Samoa year2006      58650
    ## 222                   American Samoa year2007      57903
    ## 223                   American Samoa year2008      57030
    ## 224                   American Samoa year2009      56227
    ## 225                   American Samoa year2010      55637
    ## 226                   American Samoa year2011      55320
    ## 227                   American Samoa year2012      55230
    ## 228                   American Samoa year2013      55307
    ## 229                   American Samoa year2014      55437
    ## 230                   American Samoa year2015      55537
    ## 231                   American Samoa year2016      55599
    ## 232                   American Samoa year2017      55641
    ## 233                          Andorra   year60      13411
    ## 234                          Andorra   year61      14375
    ## 235                          Andorra   year62      15370
    ## 236                          Andorra   year63      16412
    ## 237                          Andorra   year64      17469
    ## 238                          Andorra   year65      18549
    ## 239                          Andorra   year66      19647
    ## 240                          Andorra   year67      20758
    ## 241                          Andorra   year68      21890
    ## 242                          Andorra   year69      23058
    ## 243                          Andorra   year70      24276
    ## 244                          Andorra   year71      25559
    ## 245                          Andorra   year72      26892
    ## 246                          Andorra   year73      28232
    ## 247                          Andorra   year74      29520
    ## 248                          Andorra   year75      30705
    ## 249                          Andorra   year76      31777
    ## 250                          Andorra   year77      32771
    ## 251                          Andorra   year78      33737
    ## 252                          Andorra   year79      34818
    ## 253                          Andorra   year80      36067
    ## 254                          Andorra   year81      37500
    ## 255                          Andorra   year82      39114
    ## 256                          Andorra   year83      40867
    ## 257                          Andorra   year84      42706
    ## 258                          Andorra   year85      44600
    ## 259                          Andorra   year86      46517
    ## 260                          Andorra   year87      48455
    ## 261                          Andorra   year88      50434
    ## 262                          Andorra   year89      52448
    ## 263                          Andorra   year90      54509
    ## 264                          Andorra   year91      56671
    ## 265                          Andorra   year92      58888
    ## 266                          Andorra   year93      60971
    ## 267                          Andorra   year94      62677
    ## 268                          Andorra   year95      63850
    ## 269                          Andorra   year96      64360
    ## 270                          Andorra   year97      64327
    ## 271                          Andorra   year98      64142
    ## 272                          Andorra   year99      64370
    ## 273                          Andorra year2000      65390
    ## 274                          Andorra year2001      67341
    ## 275                          Andorra year2002      70049
    ## 276                          Andorra year2003      73182
    ## 277                          Andorra year2004      76244
    ## 278                          Andorra year2005      78867
    ## 279                          Andorra year2006      80991
    ## 280                          Andorra year2007      82683
    ## 281                          Andorra year2008      83861
    ## 282                          Andorra year2009      84462
    ## 283                          Andorra year2010      84449
    ## 284                          Andorra year2011      83751
    ## 285                          Andorra year2012      82431
    ## 286                          Andorra year2013      80788
    ## 287                          Andorra year2014      79223
    ## 288                          Andorra year2015      78014
    ## 289                          Andorra year2016      77281
    ## 290                          Andorra year2017      76965
    ## 291                           Angola   year60    5643182
    ## 292                           Angola   year61    5753024
    ## 293                           Angola   year62    5866061
    ## 294                           Angola   year63    5980417
    ## 295                           Angola   year64    6093321
    ## 296                           Angola   year65    6203299
    ## 297                           Angola   year66    6309770
    ## 298                           Angola   year67    6414995
    ## 299                           Angola   year68    6523791
    ## 300                           Angola   year69    6642632
    ## 301                           Angola   year70    6776381
    ## 302                           Angola   year71    6927269
    ## 303                           Angola   year72    7094834
    ## 304                           Angola   year73    7277960
    ## 305                           Angola   year74    7474338
    ## 306                           Angola   year75    7682479
    ## 307                           Angola   year76    7900997
    ## 308                           Angola   year77    8130988
    ## 309                           Angola   year78    8376147
    ## 310                           Angola   year79    8641521
    ## 311                           Angola   year80    8929900
    ## 312                           Angola   year81    9244507
    ## 313                           Angola   year82    9582156
    ## 314                           Angola   year83    9931562
    ## 315                           Angola   year84   10277321
    ## 316                           Angola   year85   10609042
    ## 317                           Angola   year86   10921037
    ## 318                           Angola   year87   11218268
    ## 319                           Angola   year88   11513968
    ## 320                           Angola   year89   11827237
    ## 321                           Angola   year90   12171441
    ## 322                           Angola   year91   12553446
    ## 323                           Angola   year92   12968345
    ## 324                           Angola   year93   13403734
    ## 325                           Angola   year94   13841301
    ## 326                           Angola   year95   14268994
    ## 327                           Angola   year96   14682284
    ## 328                           Angola   year97   15088981
    ## 329                           Angola   year98   15504318
    ## 330                           Angola   year99   15949766
    ## 331                           Angola year2000   16440924
    ## 332                           Angola year2001   16983266
    ## 333                           Angola year2002   17572649
    ## 334                           Angola year2003   18203369
    ## 335                           Angola year2004   18865716
    ## 336                           Angola year2005   19552542
    ## 337                           Angola year2006   20262399
    ## 338                           Angola year2007   20997687
    ## 339                           Angola year2008   21759420
    ## 340                           Angola year2009   22549547
    ## 341                           Angola year2010   23369131
    ## 342                           Angola year2011   24218565
    ## 343                           Angola year2012   25096150
    ## 344                           Angola year2013   25998340
    ## 345                           Angola year2014   26920466
    ## 346                           Angola year2015   27859305
    ## 347                           Angola year2016   28813463
    ## 348                           Angola year2017   29784193
    ## 349              Antigua and Barbuda   year60      55339
    ## 350              Antigua and Barbuda   year61      56144
    ## 351              Antigua and Barbuda   year62      57144
    ## 352              Antigua and Barbuda   year63      58294
    ## 353              Antigua and Barbuda   year64      59524
    ## 354              Antigua and Barbuda   year65      60781
    ## 355              Antigua and Barbuda   year66      62059
    ## 356              Antigua and Barbuda   year67      63360
    ## 357              Antigua and Barbuda   year68      64655
    ## 358              Antigua and Barbuda   year69      65910
    ## 359              Antigua and Barbuda   year70      67098
    ## 360              Antigua and Barbuda   year71      68188
    ## 361              Antigua and Barbuda   year72      69176
    ## 362              Antigua and Barbuda   year73      70066
    ## 363              Antigua and Barbuda   year74      70878
    ## 364              Antigua and Barbuda   year75      71609
    ## 365              Antigua and Barbuda   year76      72285
    ## 366              Antigua and Barbuda   year77      72875
    ## 367              Antigua and Barbuda   year78      73324
    ## 368              Antigua and Barbuda   year79      73528
    ## 369              Antigua and Barbuda   year80      73442
    ## 370              Antigua and Barbuda   year81      73066
    ## 371              Antigua and Barbuda   year82      72448
    ## 372              Antigua and Barbuda   year83      71639
    ## 373              Antigua and Barbuda   year84      70725
    ## 374              Antigua and Barbuda   year85      69782
    ## 375              Antigua and Barbuda   year86      68809
    ## 376              Antigua and Barbuda   year87      67845
    ## 377              Antigua and Barbuda   year88      67058
    ## 378              Antigua and Barbuda   year89      66627
    ## 379              Antigua and Barbuda   year90      66696
    ## 380              Antigua and Barbuda   year91      67307
    ## 381              Antigua and Barbuda   year92      68427
    ## 382              Antigua and Barbuda   year93      69938
    ## 383              Antigua and Barbuda   year94      71719
    ## 384              Antigua and Barbuda   year95      73619
    ## 385              Antigua and Barbuda   year96      75628
    ## 386              Antigua and Barbuda   year97      77739
    ## 387              Antigua and Barbuda   year98      79851
    ## 388              Antigua and Barbuda   year99      81831
    ## 389              Antigua and Barbuda year2000      83584
    ## 390              Antigua and Barbuda year2001      85057
    ## 391              Antigua and Barbuda year2002      86266
    ## 392              Antigua and Barbuda year2003      87293
    ## 393              Antigua and Barbuda year2004      88257
    ## 394              Antigua and Barbuda year2005      89253
    ## 395              Antigua and Barbuda year2006      90301
    ## 396              Antigua and Barbuda year2007      91381
    ## 397              Antigua and Barbuda year2008      92478
    ## 398              Antigua and Barbuda year2009      93581
    ## 399              Antigua and Barbuda year2010      94661
    ## 400              Antigua and Barbuda year2011      95719
    ## 401              Antigua and Barbuda year2012      96777
    ## 402              Antigua and Barbuda year2013      97824
    ## 403              Antigua and Barbuda year2014      98875
    ## 404              Antigua and Barbuda year2015      99923
    ## 405              Antigua and Barbuda year2016     100963
    ## 406              Antigua and Barbuda year2017     102012
    ## 407                       Arab World   year60   92490932
    ## 408                       Arab World   year61   95044497
    ## 409                       Arab World   year62   97682294
    ## 410                       Arab World   year63  100411076
    ## 411                       Arab World   year64  103239902
    ## 412                       Arab World   year65  106174988
    ## 413                       Arab World   year66  109230593
    ## 414                       Arab World   year67  112406932
    ## 415                       Arab World   year68  115680165
    ## 416                       Arab World   year69  119016542
    ## 417                       Arab World   year70  122398374
    ## 418                       Arab World   year71  125807419
    ## 419                       Arab World   year72  129269375
    ## 420                       Arab World   year73  132863416
    ## 421                       Arab World   year74  136696761
    ## 422                       Arab World   year75  140843298
    ## 423                       Arab World   year76  145332378
    ## 424                       Arab World   year77  150133054
    ## 425                       Arab World   year78  155183724
    ## 426                       Arab World   year79  160392488
    ## 427                       Arab World   year80  165689490
    ## 428                       Arab World   year81  171051950
    ## 429                       Arab World   year82  176490084
    ## 430                       Arab World   year83  182005827
    ## 431                       Arab World   year84  187610756
    ## 432                       Arab World   year85  193310301
    ## 433                       Arab World   year86  199093767
    ## 434                       Arab World   year87  204942549
    ## 435                       Arab World   year88  210844771
    ## 436                       Arab World   year89  216787402
    ## 437                       Arab World   year90  224735446
    ## 438                       Arab World   year91  230829868
    ## 439                       Arab World   year92  235037179
    ## 440                       Arab World   year93  241286091
    ## 441                       Arab World   year94  247435930
    ## 442                       Arab World   year95  255029671
    ## 443                       Arab World   year96  260843462
    ## 444                       Arab World   year97  266575075
    ## 445                       Arab World   year98  272235146
    ## 446                       Arab World   year99  277962869
    ## 447                       Arab World year2000  283832016
    ## 448                       Arab World year2001  289850357
    ## 449                       Arab World year2002  296026575
    ## 450                       Arab World year2003  302434519
    ## 451                       Arab World year2004  309162029
    ## 452                       Arab World year2005  316264728
    ## 453                       Arab World year2006  323773264
    ## 454                       Arab World year2007  331653797
    ## 455                       Arab World year2008  339825483
    ## 456                       Arab World year2009  348145094
    ## 457                       Arab World year2010  356508908
    ## 458                       Arab World year2011  364895878
    ## 459                       Arab World year2012  373306993
    ## 460                       Arab World year2013  381702086
    ## 461                       Arab World year2014  390043028
    ## 462                       Arab World year2015  398304960
    ## 463                       Arab World year2016  406452690
    ## 464                       Arab World year2017  414491886
    ## 465                        Argentina   year60   20619075
    ## 466                        Argentina   year61   20953077
    ## 467                        Argentina   year62   21287682
    ## 468                        Argentina   year63   21621840
    ## 469                        Argentina   year64   21953929
    ## 470                        Argentina   year65   22283390
    ## 471                        Argentina   year66   22608748
    ## 472                        Argentina   year67   22932203
    ## 473                        Argentina   year68   23261278
    ## 474                        Argentina   year69   23605987
    ## 475                        Argentina   year70   23973058
    ## 476                        Argentina   year71   24366439
    ## 477                        Argentina   year72   24782949
    ## 478                        Argentina   year73   25213388
    ## 479                        Argentina   year74   25644506
    ## 480                        Argentina   year75   26066975
    ## 481                        Argentina   year76   26477152
    ## 482                        Argentina   year77   26878565
    ## 483                        Argentina   year78   27277741
    ## 484                        Argentina   year79   27684534
    ## 485                        Argentina   year80   28105888
    ## 486                        Argentina   year81   28543364
    ## 487                        Argentina   year82   28993987
    ## 488                        Argentina   year83   29454738
    ## 489                        Argentina   year84   29920904
    ## 490                        Argentina   year85   30388783
    ## 491                        Argentina   year86   30857244
    ## 492                        Argentina   year87   31326473
    ## 493                        Argentina   year88   31795517
    ## 494                        Argentina   year89   32263561
    ## 495                        Argentina   year90   32729739
    ## 496                        Argentina   year91   33193918
    ## 497                        Argentina   year92   33655151
    ## 498                        Argentina   year93   34110917
    ## 499                        Argentina   year94   34558115
    ## 500                        Argentina   year95   34994814
    ## 501                        Argentina   year96   35419682
    ## 502                        Argentina   year97   35833969
    ## 503                        Argentina   year98   36241590
    ## 504                        Argentina   year99   36648068
    ## 505                        Argentina year2000   37057452
    ## 506                        Argentina year2001   37471509
    ## 507                        Argentina year2002   37889370
    ## 508                        Argentina year2003   38309379
    ## 509                        Argentina year2004   38728696
    ## 510                        Argentina year2005   39145488
    ## 511                        Argentina year2006   39558890
    ## 512                        Argentina year2007   39970224
    ## 513                        Argentina year2008   40382389
    ## 514                        Argentina year2009   40799407
    ## 515                        Argentina year2010   41223889
    ## 516                        Argentina year2011   41656879
    ## 517                        Argentina year2012   42096739
    ## 518                        Argentina year2013   42539925
    ## 519                        Argentina year2014   42981515
    ## 520                        Argentina year2015   43417765
    ## 521                        Argentina year2016   43847430
    ## 522                        Argentina year2017   44271041
    ## 523                          Armenia   year60    1874120
    ## 524                          Armenia   year61    1941491
    ## 525                          Armenia   year62    2009526
    ## 526                          Armenia   year63    2077575
    ## 527                          Armenia   year64    2144998
    ## 528                          Armenia   year65    2211316
    ## 529                          Armenia   year66    2276031
    ## 530                          Armenia   year67    2339124
    ## 531                          Armenia   year68    2401140
    ## 532                          Armenia   year69    2462925
    ## 533                          Armenia   year70    2525065
    ## 534                          Armenia   year71    2587706
    ## 535                          Armenia   year72    2650484
    ## 536                          Armenia   year73    2712781
    ## 537                          Armenia   year74    2773747
    ## 538                          Armenia   year75    2832757
    ## 539                          Armenia   year76    2889579
    ## 540                          Armenia   year77    2944379
    ## 541                          Armenia   year78    2997411
    ## 542                          Armenia   year79    3049105
    ## 543                          Armenia   year80    3099751
    ## 544                          Armenia   year81    3148092
    ## 545                          Armenia   year82    3193686
    ## 546                          Armenia   year83    3238594
    ## 547                          Armenia   year84    3285595
    ## 548                          Armenia   year85    3335935
    ## 549                          Armenia   year86    3392256
    ## 550                          Armenia   year87    3451942
    ## 551                          Armenia   year88    3504651
    ## 552                          Armenia   year89    3536469
    ## 553                          Armenia   year90    3538165
    ## 554                          Armenia   year91    3505251
    ## 555                          Armenia   year92    3442810
    ## 556                          Armenia   year93    3363098
    ## 557                          Armenia   year94    3283660
    ## 558                          Armenia   year95    3217342
    ## 559                          Armenia   year96    3168215
    ## 560                          Armenia   year97    3133086
    ## 561                          Armenia   year98    3108684
    ## 562                          Armenia   year99    3089017
    ## 563                          Armenia year2000    3069588
    ## 564                          Armenia year2001    3050655
    ## 565                          Armenia year2002    3033897
    ## 566                          Armenia year2003    3017806
    ## 567                          Armenia year2004    3000612
    ## 568                          Armenia year2005    2981259
    ## 569                          Armenia year2006    2958500
    ## 570                          Armenia year2007    2933056
    ## 571                          Armenia year2008    2908220
    ## 572                          Armenia year2009    2888584
    ## 573                          Armenia year2010    2877311
    ## 574                          Armenia year2011    2875581
    ## 575                          Armenia year2012    2881922
    ## 576                          Armenia year2013    2893509
    ## 577                          Armenia year2014    2906220
    ## 578                          Armenia year2015    2916950
    ## 579                          Armenia year2016    2924816
    ## 580                          Armenia year2017    2930450
    ## 581                        Australia   year60   10276477
    ## 582                        Australia   year61   10483000
    ## 583                        Australia   year62   10742000
    ## 584                        Australia   year63   10950000
    ## 585                        Australia   year64   11167000
    ## 586                        Australia   year65   11388000
    ## 587                        Australia   year66   11651000
    ## 588                        Australia   year67   11799000
    ## 589                        Australia   year68   12009000
    ## 590                        Australia   year69   12263000
    ## 591                        Australia   year70   12507000
    ## 592                        Australia   year71   12937000
    ## 593                        Australia   year72   13177000
    ## 594                        Australia   year73   13380000
    ## 595                        Australia   year74   13723000
    ## 596                        Australia   year75   13893000
    ## 597                        Australia   year76   14033000
    ## 598                        Australia   year77   14192000
    ## 599                        Australia   year78   14358000
    ## 600                        Australia   year79   14514000
    ## 601                        Australia   year80   14692000
    ## 602                        Australia   year81   14927000
    ## 603                        Australia   year82   15178000
    ## 604                        Australia   year83   15369000
    ## 605                        Australia   year84   15544000
    ## 606                        Australia   year85   15758000
    ## 607                        Australia   year86   16018400
    ## 608                        Australia   year87   16263900
    ## 609                        Australia   year88   16532200
    ## 610                        Australia   year89   16814400
    ## 611                        Australia   year90   17065100
    ## 612                        Australia   year91   17284000
    ## 613                        Australia   year92   17495000
    ## 614                        Australia   year93   17667000
    ## 615                        Australia   year94   17855000
    ## 616                        Australia   year95   18072000
    ## 617                        Australia   year96   18311000
    ## 618                        Australia   year97   18517000
    ## 619                        Australia   year98   18711000
    ## 620                        Australia   year99   18926000
    ## 621                        Australia year2000   19153000
    ## 622                        Australia year2001   19413000
    ## 623                        Australia year2002   19651400
    ## 624                        Australia year2003   19895400
    ## 625                        Australia year2004   20127400
    ## 626                        Australia year2005   20394800
    ## 627                        Australia year2006   20697900
    ## 628                        Australia year2007   20827600
    ## 629                        Australia year2008   21249200
    ## 630                        Australia year2009   21691700
    ## 631                        Australia year2010   22031750
    ## 632                        Australia year2011   22340024
    ## 633                        Australia year2012   22742475
    ## 634                        Australia year2013   23145901
    ## 635                        Australia year2014   23504138
    ## 636                        Australia year2015   23850784
    ## 637                        Australia year2016   24210809
    ## 638                        Australia year2017   24598933
    ## 639                          Austria   year60    7047539
    ## 640                          Austria   year61    7086299
    ## 641                          Austria   year62    7129864
    ## 642                          Austria   year63    7175811
    ## 643                          Austria   year64    7223801
    ## 644                          Austria   year65    7270889
    ## 645                          Austria   year66    7322066
    ## 646                          Austria   year67    7376998
    ## 647                          Austria   year68    7415403
    ## 648                          Austria   year69    7441055
    ## 649                          Austria   year70    7467086
    ## 650                          Austria   year71    7500482
    ## 651                          Austria   year72    7544201
    ## 652                          Austria   year73    7586115
    ## 653                          Austria   year74    7599038
    ## 654                          Austria   year75    7578903
    ## 655                          Austria   year76    7565525
    ## 656                          Austria   year77    7568430
    ## 657                          Austria   year78    7562305
    ## 658                          Austria   year79    7549425
    ## 659                          Austria   year80    7549433
    ## 660                          Austria   year81    7568710
    ## 661                          Austria   year82    7574140
    ## 662                          Austria   year83    7561910
    ## 663                          Austria   year84    7561434
    ## 664                          Austria   year85    7564985
    ## 665                          Austria   year86    7569794
    ## 666                          Austria   year87    7574586
    ## 667                          Austria   year88    7585317
    ## 668                          Austria   year89    7619567
    ## 669                          Austria   year90    7677850
    ## 670                          Austria   year91    7754891
    ## 671                          Austria   year92    7840709
    ## 672                          Austria   year93    7905633
    ## 673                          Austria   year94    7936118
    ## 674                          Austria   year95    7948278
    ## 675                          Austria   year96    7959017
    ## 676                          Austria   year97    7968041
    ## 677                          Austria   year98    7976789
    ## 678                          Austria   year99    7992324
    ## 679                          Austria year2000    8011566
    ## 680                          Austria year2001    8042293
    ## 681                          Austria year2002    8081957
    ## 682                          Austria year2003    8121423
    ## 683                          Austria year2004    8171966
    ## 684                          Austria year2005    8227829
    ## 685                          Austria year2006    8268641
    ## 686                          Austria year2007    8295487
    ## 687                          Austria year2008    8321496
    ## 688                          Austria year2009    8343323
    ## 689                          Austria year2010    8363404
    ## 690                          Austria year2011    8391643
    ## 691                          Austria year2012    8429991
    ## 692                          Austria year2013    8479823
    ## 693                          Austria year2014    8546356
    ## 694                          Austria year2015    8642699
    ## 695                          Austria year2016    8736668
    ## 696                          Austria year2017    8809212
    ## 697                       Azerbaijan   year60    3895396
    ## 698                       Azerbaijan   year61    4030320
    ## 699                       Azerbaijan   year62    4171425
    ## 700                       Azerbaijan   year63    4315128
    ## 701                       Azerbaijan   year64    4456689
    ## 702                       Azerbaijan   year65    4592610
    ## 703                       Azerbaijan   year66    4721525
    ## 704                       Azerbaijan   year67    4843870
    ## 705                       Azerbaijan   year68    4960235
    ## 706                       Azerbaijan   year69    5071930
    ## 707                       Azerbaijan   year70    5180025
    ## 708                       Azerbaijan   year71    5284532
    ## 709                       Azerbaijan   year72    5385267
    ## 710                       Azerbaijan   year73    5483084
    ## 711                       Azerbaijan   year74    5579077
    ## 712                       Azerbaijan   year75    5674137
    ## 713                       Azerbaijan   year76    5768724
    ## 714                       Azerbaijan   year77    5863134
    ## 715                       Azerbaijan   year78    5957929
    ## 716                       Azerbaijan   year79    6053645
    ## 717                       Azerbaijan   year80    6150738
    ## 718                       Azerbaijan   year81    6249320
    ## 719                       Azerbaijan   year82    6349558
    ## 720                       Azerbaijan   year83    6452076
    ## 721                       Azerbaijan   year84    6557585
    ## 722                       Azerbaijan   year85    6666455
    ## 723                       Azerbaijan   year86    6778633
    ## 724                       Azerbaijan   year87    6893500
    ## 725                       Azerbaijan   year88    7010036
    ## 726                       Azerbaijan   year89    7126891
    ## 727                       Azerbaijan   year90    7159000
    ## 728                       Azerbaijan   year91    7271000
    ## 729                       Azerbaijan   year92    7382000
    ## 730                       Azerbaijan   year93    7495000
    ## 731                       Azerbaijan   year94    7597000
    ## 732                       Azerbaijan   year95    7685000
    ## 733                       Azerbaijan   year96    7763000
    ## 734                       Azerbaijan   year97    7838250
    ## 735                       Azerbaijan   year98    7913000
    ## 736                       Azerbaijan   year99    7982750
    ## 737                       Azerbaijan year2000    8048600
    ## 738                       Azerbaijan year2001    8111200
    ## 739                       Azerbaijan year2002    8171950
    ## 740                       Azerbaijan year2003    8234100
    ## 741                       Azerbaijan year2004    8306500
    ## 742                       Azerbaijan year2005    8391850
    ## 743                       Azerbaijan year2006    8484550
    ## 744                       Azerbaijan year2007    8581300
    ## 745                       Azerbaijan year2008    8763400
    ## 746                       Azerbaijan year2009    8947243
    ## 747                       Azerbaijan year2010    9054332
    ## 748                       Azerbaijan year2011    9173082
    ## 749                       Azerbaijan year2012    9295784
    ## 750                       Azerbaijan year2013    9416801
    ## 751                       Azerbaijan year2014    9535079
    ## 752                       Azerbaijan year2015    9649341
    ## 753                       Azerbaijan year2016    9757812
    ## 754                       Azerbaijan year2017    9862429
    ## 755                     Bahamas, The   year60     109528
    ## 756                     Bahamas, The   year61     115108
    ## 757                     Bahamas, The   year62     121083
    ## 758                     Bahamas, The   year63     127333
    ## 759                     Bahamas, The   year64     133698
    ## 760                     Bahamas, The   year65     140054
    ## 761                     Bahamas, The   year66     146366
    ## 762                     Bahamas, The   year67     152609
    ## 763                     Bahamas, The   year68     158627
    ## 764                     Bahamas, The   year69     164248
    ## 765                     Bahamas, The   year70     169354
    ## 766                     Bahamas, The   year71     173863
    ## 767                     Bahamas, The   year72     177839
    ## 768                     Bahamas, The   year73     181488
    ## 769                     Bahamas, The   year74     185099
    ## 770                     Bahamas, The   year75     188882
    ## 771                     Bahamas, The   year76     192902
    ## 772                     Bahamas, The   year77     197111
    ## 773                     Bahamas, The   year78     201513
    ## 774                     Bahamas, The   year79     206032
    ## 775                     Bahamas, The   year80     210661
    ## 776                     Bahamas, The   year81     215396
    ## 777                     Bahamas, The   year82     220275
    ## 778                     Bahamas, The   year83     225187
    ## 779                     Bahamas, The   year84     230015
    ## 780                     Bahamas, The   year85     234687
    ## 781                     Bahamas, The   year86     239131
    ## 782                     Bahamas, The   year87     243393
    ## 783                     Bahamas, The   year88     247579
    ## 784                     Bahamas, The   year89     251849
    ## 785                     Bahamas, The   year90     256336
    ## 786                     Bahamas, The   year91     261116
    ## 787                     Bahamas, The   year92     266134
    ## 788                     Bahamas, The   year93     271165
    ## 789                     Bahamas, The   year94     275895
    ## 790                     Bahamas, The   year95     280150
    ## 791                     Bahamas, The   year96     283790
    ## 792                     Bahamas, The   year97     286970
    ## 793                     Bahamas, The   year98     290060
    ## 794                     Bahamas, The   year99     293572
    ## 795                     Bahamas, The year2000     297890
    ## 796                     Bahamas, The year2001     303135
    ## 797                     Bahamas, The year2002     309157
    ## 798                     Bahamas, The year2003     315746
    ## 799                     Bahamas, The year2004     322526
    ## 800                     Bahamas, The year2005     329249
    ## 801                     Bahamas, The year2006     335830
    ## 802                     Bahamas, The year2007     342328
    ## 803                     Bahamas, The year2008     348676
    ## 804                     Bahamas, The year2009     354856
    ## 805                     Bahamas, The year2010     360832
    ## 806                     Bahamas, The year2011     366568
    ## 807                     Bahamas, The year2012     372039
    ## 808                     Bahamas, The year2013     377240
    ## 809                     Bahamas, The year2014     382169
    ## 810                     Bahamas, The year2015     386838
    ## 811                     Bahamas, The year2016     391232
    ## 812                     Bahamas, The year2017     395361
    ## 813                          Bahrain   year60     162427
    ## 814                          Bahrain   year61     167894
    ## 815                          Bahrain   year62     173144
    ## 816                          Bahrain   year63     178140
    ## 817                          Bahrain   year64     182887
    ## 818                          Bahrain   year65     187431
    ## 819                          Bahrain   year66     191780
    ## 820                          Bahrain   year67     196063
    ## 821                          Bahrain   year68     200653
    ## 822                          Bahrain   year69     206043
    ## 823                          Bahrain   year70     212605
    ## 824                          Bahrain   year71     220312
    ## 825                          Bahrain   year72     229155
    ## 826                          Bahrain   year73     239527
    ## 827                          Bahrain   year74     251911
    ## 828                          Bahrain   year75     266543
    ## 829                          Bahrain   year76     283752
    ## 830                          Bahrain   year77     303175
    ## 831                          Bahrain   year78     323473
    ## 832                          Bahrain   year79     342798
    ## 833                          Bahrain   year80     359888
    ## 834                          Bahrain   year81     374120
    ## 835                          Bahrain   year82     385950
    ## 836                          Bahrain   year83     396454
    ## 837                          Bahrain   year84     407227
    ## 838                          Bahrain   year85     419430
    ## 839                          Bahrain   year86     433482
    ## 840                          Bahrain   year87     448973
    ## 841                          Bahrain   year88     465202
    ## 842                          Bahrain   year89     481090
    ## 843                          Bahrain   year90     495931
    ## 844                          Bahrain   year91     509765
    ## 845                          Bahrain   year92     523087
    ## 846                          Bahrain   year93     536213
    ## 847                          Bahrain   year94     549588
    ## 848                          Bahrain   year95     563699
    ## 849                          Bahrain   year96     578668
    ## 850                          Bahrain   year97     594930
    ## 851                          Bahrain   year98     613702
    ## 852                          Bahrain   year99     636545
    ## 853                          Bahrain year2000     664614
    ## 854                          Bahrain year2001     697549
    ## 855                          Bahrain year2002     735148
    ## 856                          Bahrain year2003     778711
    ## 857                          Bahrain year2004     829848
    ## 858                          Bahrain year2005     889168
    ## 859                          Bahrain year2006     958414
    ## 860                          Bahrain year2007    1035891
    ## 861                          Bahrain year2008    1114590
    ## 862                          Bahrain year2009    1185029
    ## 863                          Bahrain year2010    1240862
    ## 864                          Bahrain year2011    1278269
    ## 865                          Bahrain year2012    1300217
    ## 866                          Bahrain year2013    1315411
    ## 867                          Bahrain year2014    1336397
    ## 868                          Bahrain year2015    1371855
    ## 869                          Bahrain year2016    1425171
    ## 870                          Bahrain year2017    1492584
    ## 871                       Bangladesh   year60   48199747
    ## 872                       Bangladesh   year61   49592802
    ## 873                       Bangladesh   year62   51030137
    ## 874                       Bangladesh   year63   52532417
    ## 875                       Bangladesh   year64   54129100
    ## 876                       Bangladesh   year65   55834038
    ## 877                       Bangladesh   year66   57672990
    ## 878                       Bangladesh   year67   59620669
    ## 879                       Bangladesh   year68   61579473
    ## 880                       Bangladesh   year69   63417394
    ## 881                       Bangladesh   year70   65047770
    ## 882                       Bangladesh   year71   66424744
    ## 883                       Bangladesh   year72   67597470
    ## 884                       Bangladesh   year73   68691185
    ## 885                       Bangladesh   year74   69884420
    ## 886                       Bangladesh   year75   71305923
    ## 887                       Bangladesh   year76   72999136
    ## 888                       Bangladesh   year77   74925896
    ## 889                       Bangladesh   year78   77033846
    ## 890                       Bangladesh   year79   79236776
    ## 891                       Bangladesh   year80   81470860
    ## 892                       Bangladesh   year81   83721268
    ## 893                       Bangladesh   year82   86007331
    ## 894                       Bangladesh   year83   88338242
    ## 895                       Bangladesh   year84   90732362
    ## 896                       Bangladesh   year85   93199865
    ## 897                       Bangladesh   year86   95742431
    ## 898                       Bangladesh   year87   98343809
    ## 899                       Bangladesh   year88  100975321
    ## 900                       Bangladesh   year89  103599232
    ## 901                       Bangladesh   year90  106188642
    ## 902                       Bangladesh   year91  108727432
    ## 903                       Bangladesh   year92  111221938
    ## 904                       Bangladesh   year93  113695139
    ## 905                       Bangladesh   year94  116182267
    ## 906                       Bangladesh   year95  118706871
    ## 907                       Bangladesh   year96  121269645
    ## 908                       Bangladesh   year97  123854640
    ## 909                       Bangladesh   year98  126447965
    ## 910                       Bangladesh   year99  129029691
    ## 911                       Bangladesh year2000  131581243
    ## 912                       Bangladesh year2001  134107160
    ## 913                       Bangladesh year2002  136600667
    ## 914                       Bangladesh year2003  139019001
    ## 915                       Bangladesh year2004  141307489
    ## 916                       Bangladesh year2005  143431101
    ## 917                       Bangladesh year2006  145368004
    ## 918                       Bangladesh year2007  147139191
    ## 919                       Bangladesh year2008  148805814
    ## 920                       Bangladesh year2009  150454708
    ## 921                       Bangladesh year2010  152149102
    ## 922                       Bangladesh year2011  153911916
    ## 923                       Bangladesh year2012  155727053
    ## 924                       Bangladesh year2013  157571292
    ## 925                       Bangladesh year2014  159405279
    ## 926                       Bangladesh year2015  161200886
    ## 927                       Bangladesh year2016  162951560
    ## 928                       Bangladesh year2017  164669751
    ## 929                         Barbados   year60     230939
    ## 930                         Barbados   year61     231678
    ## 931                         Barbados   year62     232586
    ## 932                         Barbados   year63     233587
    ## 933                         Barbados   year64     234547
    ## 934                         Barbados   year65     235374
    ## 935                         Barbados   year66     236044
    ## 936                         Barbados   year67     236621
    ## 937                         Barbados   year68     237199
    ## 938                         Barbados   year69     237913
    ## 939                         Barbados   year70     238848
    ## 940                         Barbados   year71     240035
    ## 941                         Barbados   year72     241441
    ## 942                         Barbados   year73     242976
    ## 943                         Barbados   year74     244539
    ## 944                         Barbados   year75     246034
    ## 945                         Barbados   year76     247444
    ## 946                         Barbados   year77     248784
    ## 947                         Barbados   year78     250032
    ## 948                         Barbados   year79     251177
    ## 949                         Barbados   year80     252194
    ## 950                         Barbados   year81     253080
    ## 951                         Barbados   year82     253841
    ## 952                         Barbados   year83     254518
    ## 953                         Barbados   year84     255193
    ## 954                         Barbados   year85     255924
    ## 955                         Barbados   year86     256736
    ## 956                         Barbados   year87     257611
    ## 957                         Barbados   year88     258527
    ## 958                         Barbados   year89     259458
    ## 959                         Barbados   year90     260374
    ## 960                         Barbados   year91     261275
    ## 961                         Barbados   year92     262184
    ## 962                         Barbados   year93     263089
    ## 963                         Barbados   year94     264015
    ## 964                         Barbados   year95     264959
    ## 965                         Barbados   year96     265942
    ## 966                         Barbados   year97     266945
    ## 967                         Barbados   year98     267950
    ## 968                         Barbados   year99     268922
    ## 969                         Barbados year2000     269847
    ## 970                         Barbados year2001     270685
    ## 971                         Barbados year2002     271478
    ## 972                         Barbados year2003     272258
    ## 973                         Barbados year2004     273091
    ## 974                         Barbados year2005     274009
    ## 975                         Barbados year2006     275039
    ## 976                         Barbados year2007     276150
    ## 977                         Barbados year2008     277319
    ## 978                         Barbados year2009     278470
    ## 979                         Barbados year2010     279569
    ## 980                         Barbados year2011     280601
    ## 981                         Barbados year2012     281585
    ## 982                         Barbados year2013     282509
    ## 983                         Barbados year2014     283385
    ## 984                         Barbados year2015     284217
    ## 985                         Barbados year2016     284996
    ## 986                         Barbados year2017     285719
    ## 987                          Belarus   year60    8198000
    ## 988                          Belarus   year61    8271216
    ## 989                          Belarus   year62    8351928
    ## 990                          Belarus   year63    8437232
    ## 991                          Belarus   year64    8524224
    ## 992                          Belarus   year65    8610000
    ## 993                          Belarus   year66    8696496
    ## 994                          Belarus   year67    8785648
    ## 995                          Belarus   year68    8874552
    ## 996                          Belarus   year69    8960304
    ## 997                          Belarus   year70    9040000
    ## 998                          Belarus   year71    9115576
    ## 999                          Belarus   year72    9188968
    ## 1000                         Belarus   year73    9257272
    ## 1001                         Belarus   year74    9317584
    ## 1002                         Belarus   year75    9367000
    ## 1003                         Belarus   year76    9411000
    ## 1004                         Belarus   year77    9463000
    ## 1005                         Belarus   year78    9525000
    ## 1006                         Belarus   year79    9584000
    ## 1007                         Belarus   year80    9643000
    ## 1008                         Belarus   year81    9710000
    ## 1009                         Belarus   year82    9776000
    ## 1010                         Belarus   year83    9843000
    ## 1011                         Belarus   year84    9910000
    ## 1012                         Belarus   year85    9975000
    ## 1013                         Belarus   year86   10043000
    ## 1014                         Belarus   year87   10111000
    ## 1015                         Belarus   year88   10140000
    ## 1016                         Belarus   year89   10170000
    ## 1017                         Belarus   year90   10189000
    ## 1018                         Belarus   year91   10194000
    ## 1019                         Belarus   year92   10216000
    ## 1020                         Belarus   year93   10239000
    ## 1021                         Belarus   year94   10227000
    ## 1022                         Belarus   year95   10194000
    ## 1023                         Belarus   year96   10160000
    ## 1024                         Belarus   year97   10117000
    ## 1025                         Belarus   year98   10069000
    ## 1026                         Belarus   year99   10026738
    ## 1027                         Belarus year2000    9979610
    ## 1028                         Belarus year2001    9928549
    ## 1029                         Belarus year2002    9865548
    ## 1030                         Belarus year2003    9796749
    ## 1031                         Belarus year2004    9730146
    ## 1032                         Belarus year2005    9663915
    ## 1033                         Belarus year2006    9604924
    ## 1034                         Belarus year2007    9560953
    ## 1035                         Belarus year2008    9527985
    ## 1036                         Belarus year2009    9506765
    ## 1037                         Belarus year2010    9490583
    ## 1038                         Belarus year2011    9473172
    ## 1039                         Belarus year2012    9464495
    ## 1040                         Belarus year2013    9465997
    ## 1041                         Belarus year2014    9474511
    ## 1042                         Belarus year2015    9489616
    ## 1043                         Belarus year2016    9501534
    ## 1044                         Belarus year2017    9507875
    ## 1045                         Belgium   year60    9153489
    ## 1046                         Belgium   year61    9183948
    ## 1047                         Belgium   year62    9220578
    ## 1048                         Belgium   year63    9289770
    ## 1049                         Belgium   year64    9378113
    ## 1050                         Belgium   year65    9463667
    ## 1051                         Belgium   year66    9527807
    ## 1052                         Belgium   year67    9580991
    ## 1053                         Belgium   year68    9618756
    ## 1054                         Belgium   year69    9646032
    ## 1055                         Belgium   year70    9655549
    ## 1056                         Belgium   year71    9673162
    ## 1057                         Belgium   year72    9711115
    ## 1058                         Belgium   year73    9741720
    ## 1059                         Belgium   year74    9772419
    ## 1060                         Belgium   year75    9800700
    ## 1061                         Belgium   year76    9818227
    ## 1062                         Belgium   year77    9830358
    ## 1063                         Belgium   year78    9839534
    ## 1064                         Belgium   year79    9848382
    ## 1065                         Belgium   year80    9859242
    ## 1066                         Belgium   year81    9858982
    ## 1067                         Belgium   year82    9856303
    ## 1068                         Belgium   year83    9855520
    ## 1069                         Belgium   year84    9855372
    ## 1070                         Belgium   year85    9858308
    ## 1071                         Belgium   year86    9861823
    ## 1072                         Belgium   year87    9870234
    ## 1073                         Belgium   year88    9901664
    ## 1074                         Belgium   year89    9937697
    ## 1075                         Belgium   year90    9967379
    ## 1076                         Belgium   year91   10004486
    ## 1077                         Belgium   year92   10045158
    ## 1078                         Belgium   year93   10084475
    ## 1079                         Belgium   year94   10115603
    ## 1080                         Belgium   year95   10136811
    ## 1081                         Belgium   year96   10156637
    ## 1082                         Belgium   year97   10181245
    ## 1083                         Belgium   year98   10203008
    ## 1084                         Belgium   year99   10226419
    ## 1085                         Belgium year2000   10251250
    ## 1086                         Belgium year2001   10286570
    ## 1087                         Belgium year2002   10332785
    ## 1088                         Belgium year2003   10376133
    ## 1089                         Belgium year2004   10421137
    ## 1090                         Belgium year2005   10478617
    ## 1091                         Belgium year2006   10547958
    ## 1092                         Belgium year2007   10625700
    ## 1093                         Belgium year2008   10709973
    ## 1094                         Belgium year2009   10796493
    ## 1095                         Belgium year2010   10895586
    ## 1096                         Belgium year2011   11047744
    ## 1097                         Belgium year2012   11128246
    ## 1098                         Belgium year2013   11182817
    ## 1099                         Belgium year2014   11209057
    ## 1100                         Belgium year2015   11274196
    ## 1101                         Belgium year2016   11331422
    ## 1102                         Belgium year2017   11372068
    ## 1103                          Belize   year60      92064
    ## 1104                          Belize   year61      94703
    ## 1105                          Belize   year62      97384
    ## 1106                          Belize   year63     100164
    ## 1107                          Belize   year64     103069
    ## 1108                          Belize   year65     106119
    ## 1109                          Belize   year66     109347
    ## 1110                          Belize   year67     112692
    ## 1111                          Belize   year68     116061
    ## 1112                          Belize   year69     119261
    ## 1113                          Belize   year70     122182
    ## 1114                          Belize   year71     124793
    ## 1115                          Belize   year72     127150
    ## 1116                          Belize   year73     129294
    ## 1117                          Belize   year74     131307
    ## 1118                          Belize   year75     133260
    ## 1119                          Belize   year76     135147
    ## 1120                          Belize   year77     136989
    ## 1121                          Belize   year78     138965
    ## 1122                          Belize   year79     141305
    ## 1123                          Belize   year80     144155
    ## 1124                          Belize   year81     147566
    ## 1125                          Belize   year82     151500
    ## 1126                          Belize   year83     155822
    ## 1127                          Belize   year84     160347
    ## 1128                          Belize   year85     164921
    ## 1129                          Belize   year86     169568
    ## 1130                          Belize   year87     174320
    ## 1131                          Belize   year88     179028
    ## 1132                          Belize   year89     183469
    ## 1133                          Belize   year90     187552
    ## 1134                          Belize   year91     191126
    ## 1135                          Belize   year92     194317
    ## 1136                          Belize   year93     197616
    ## 1137                          Belize   year94     201674
    ## 1138                          Belize   year95     206963
    ## 1139                          Belize   year96     213676
    ## 1140                          Belize   year97     221606
    ## 1141                          Belize   year98     230284
    ## 1142                          Belize   year99     239026
    ## 1143                          Belize year2000     247315
    ## 1144                          Belize year2001     254984
    ## 1145                          Belize year2002     262206
    ## 1146                          Belize year2003     269130
    ## 1147                          Belize year2004     276089
    ## 1148                          Belize year2005     283277
    ## 1149                          Belize year2006     290747
    ## 1150                          Belize year2007     298407
    ## 1151                          Belize year2008     306165
    ## 1152                          Belize year2009     313929
    ## 1153                          Belize year2010     321608
    ## 1154                          Belize year2011     329192
    ## 1155                          Belize year2012     336701
    ## 1156                          Belize year2013     344181
    ## 1157                          Belize year2014     351694
    ## 1158                          Belize year2015     359288
    ## 1159                          Belize year2016     366954
    ## 1160                          Belize year2017     374681
    ## 1161                           Benin   year60    2431622
    ## 1162                           Benin   year61    2465867
    ## 1163                           Benin   year62    2502896
    ## 1164                           Benin   year63    2542859
    ## 1165                           Benin   year64    2585965
    ## 1166                           Benin   year65    2632356
    ## 1167                           Benin   year66    2682159
    ## 1168                           Benin   year67    2735307
    ## 1169                           Benin   year68    2791590
    ## 1170                           Benin   year69    2850661
    ## 1171                           Benin   year70    2912340
    ## 1172                           Benin   year71    2976572
    ## 1173                           Benin   year72    3043567
    ## 1174                           Benin   year73    3113675
    ## 1175                           Benin   year74    3187412
    ## 1176                           Benin   year75    3265165
    ## 1177                           Benin   year76    3347173
    ## 1178                           Benin   year77    3433439
    ## 1179                           Benin   year78    3523938
    ## 1180                           Benin   year79    3618526
    ## 1181                           Benin   year80    3717165
    ## 1182                           Benin   year81    3820128
    ## 1183                           Benin   year82    3927714
    ## 1184                           Benin   year83    4039949
    ## 1185                           Benin   year84    4156819
    ## 1186                           Benin   year85    4278501
    ## 1187                           Benin   year86    4404506
    ## 1188                           Benin   year87    4535263
    ## 1189                           Benin   year88    4672852
    ## 1190                           Benin   year89    4820016
    ## 1191                           Benin   year90    4978496
    ## 1192                           Benin   year91    5149499
    ## 1193                           Benin   year92    5331803
    ## 1194                           Benin   year93    5521763
    ## 1195                           Benin   year94    5714220
    ## 1196                           Benin   year95    5905558
    ## 1197                           Benin   year96    6094259
    ## 1198                           Benin   year97    6281639
    ## 1199                           Benin   year98    6470265
    ## 1200                           Benin   year99    6664098
    ## 1201                           Benin year2000    6865951
    ## 1202                           Benin year2001    7076733
    ## 1203                           Benin year2002    7295394
    ## 1204                           Benin year2003    7520555
    ## 1205                           Benin year2004    7750004
    ## 1206                           Benin year2005    7982225
    ## 1207                           Benin year2006    8216896
    ## 1208                           Benin year2007    8454791
    ## 1209                           Benin year2008    8696916
    ## 1210                           Benin year2009    8944706
    ## 1211                           Benin year2010    9199259
    ## 1212                           Benin year2011    9460802
    ## 1213                           Benin year2012    9729160
    ## 1214                           Benin year2013   10004451
    ## 1215                           Benin year2014   10286712
    ## 1216                           Benin year2015   10575952
    ## 1217                           Benin year2016   10872298
    ## 1218                           Benin year2017   11175692
    ## 1219                         Bermuda   year60      44400
    ## 1220                         Bermuda   year61      45500
    ## 1221                         Bermuda   year62      46600
    ## 1222                         Bermuda   year63      47700
    ## 1223                         Bermuda   year64      48900
    ## 1224                         Bermuda   year65      50100
    ## 1225                         Bermuda   year66      51000
    ## 1226                         Bermuda   year67      52000
    ## 1227                         Bermuda   year68      53000
    ## 1228                         Bermuda   year69      54000
    ## 1229                         Bermuda   year70      55000
    ## 1230                         Bermuda   year71      54600
    ## 1231                         Bermuda   year72      54200
    ## 1232                         Bermuda   year73      53800
    ## 1233                         Bermuda   year74      53400
    ## 1234                         Bermuda   year75      53000
    ## 1235                         Bermuda   year76      53200
    ## 1236                         Bermuda   year77      53400
    ## 1237                         Bermuda   year78      53600
    ## 1238                         Bermuda   year79      53800
    ## 1239                         Bermuda   year80      54670
    ## 1240                         Bermuda   year81      55050
    ## 1241                         Bermuda   year82      55449
    ## 1242                         Bermuda   year83      55930
    ## 1243                         Bermuda   year84      56423
    ## 1244                         Bermuda   year85      56898
    ## 1245                         Bermuda   year86      57382
    ## 1246                         Bermuda   year87      57849
    ## 1247                         Bermuda   year88      58347
    ## 1248                         Bermuda   year89      58841
    ## 1249                         Bermuda   year90      59326
    ## 1250                         Bermuda   year91      59021
    ## 1251                         Bermuda   year92      58595
    ## 1252                         Bermuda   year93      58910
    ## 1253                         Bermuda   year94      59320
    ## 1254                         Bermuda   year95      59746
    ## 1255                         Bermuda   year96      60129
    ## 1256                         Bermuda   year97      60497
    ## 1257                         Bermuda   year98      60943
    ## 1258                         Bermuda   year99      61285
    ## 1259                         Bermuda year2000      61833
    ## 1260                         Bermuda year2001      62504
    ## 1261                         Bermuda year2002      62912
    ## 1262                         Bermuda year2003      63325
    ## 1263                         Bermuda year2004      63740
    ## 1264                         Bermuda year2005      64154
    ## 1265                         Bermuda year2006      64523
    ## 1266                         Bermuda year2007      64888
    ## 1267                         Bermuda year2008      65273
    ## 1268                         Bermuda year2009      65636
    ## 1269                         Bermuda year2010      65124
    ## 1270                         Bermuda year2011      64564
    ## 1271                         Bermuda year2012      64798
    ## 1272                         Bermuda year2013      65001
    ## 1273                         Bermuda year2014      65139
    ## 1274                         Bermuda year2015      65239
    ## 1275                         Bermuda year2016      65341
    ## 1276                         Bermuda year2017      65441
    ## 1277                          Bhutan   year60     223288
    ## 1278                          Bhutan   year61     228918
    ## 1279                          Bhutan   year62     234706
    ## 1280                          Bhutan   year63     240778
    ## 1281                          Bhutan   year64     247325
    ## 1282                          Bhutan   year65     254464
    ## 1283                          Bhutan   year66     262244
    ## 1284                          Bhutan   year67     270622
    ## 1285                          Bhutan   year68     279515
    ## 1286                          Bhutan   year69     288774
    ## 1287                          Bhutan   year70     298301
    ## 1288                          Bhutan   year71     308053
    ## 1289                          Bhutan   year72     318045
    ## 1290                          Bhutan   year73     328312
    ## 1291                          Bhutan   year74     338943
    ## 1292                          Bhutan   year75     349982
    ## 1293                          Bhutan   year76     361455
    ## 1294                          Bhutan   year77     373324
    ## 1295                          Bhutan   year78     385384
    ## 1296                          Bhutan   year79     397390
    ## 1297                          Bhutan   year80     409172
    ## 1298                          Bhutan   year81     420380
    ## 1299                          Bhutan   year82     431050
    ## 1300                          Bhutan   year83     441847
    ## 1301                          Bhutan   year84     453720
    ## 1302                          Bhutan   year85     467178
    ## 1303                          Bhutan   year86     482952
    ## 1304                          Bhutan   year87     500437
    ## 1305                          Bhutan   year88     517273
    ## 1306                          Bhutan   year89     530257
    ## 1307                          Bhutan   year90     537280
    ## 1308                          Bhutan   year91     537284
    ## 1309                          Bhutan   year92     531525
    ## 1310                          Bhutan   year93     523117
    ## 1311                          Bhutan   year94     516503
    ## 1312                          Bhutan   year95     514877
    ## 1313                          Bhutan   year96     519282
    ## 1314                          Bhutan   year97     528754
    ## 1315                          Bhutan   year98     542155
    ## 1316                          Bhutan   year99     557543
    ## 1317                          Bhutan year2000     573416
    ## 1318                          Bhutan year2001     589600
    ## 1319                          Bhutan year2002     606399
    ## 1320                          Bhutan year2003     623434
    ## 1321                          Bhutan year2004     640282
    ## 1322                          Bhutan year2005     656639
    ## 1323                          Bhutan year2006     672228
    ## 1324                          Bhutan year2007     686958
    ## 1325                          Bhutan year2008     700950
    ## 1326                          Bhutan year2009     714458
    ## 1327                          Bhutan year2010     727641
    ## 1328                          Bhutan year2011     740510
    ## 1329                          Bhutan year2012     752967
    ## 1330                          Bhutan year2013     764961
    ## 1331                          Bhutan year2014     776448
    ## 1332                          Bhutan year2015     787386
    ## 1333                          Bhutan year2016     797765
    ## 1334                          Bhutan year2017     807610
    ## 1335                         Bolivia   year60    3693449
    ## 1336                         Bolivia   year61    3764813
    ## 1337                         Bolivia   year62    3838097
    ## 1338                         Bolivia   year63    3913395
    ## 1339                         Bolivia   year64    3990857
    ## 1340                         Bolivia   year65    4070590
    ## 1341                         Bolivia   year66    4152668
    ## 1342                         Bolivia   year67    4237125
    ## 1343                         Bolivia   year68    4324064
    ## 1344                         Bolivia   year69    4413590
    ## 1345                         Bolivia   year70    4505778
    ## 1346                         Bolivia   year71    4600591
    ## 1347                         Bolivia   year72    4698083
    ## 1348                         Bolivia   year73    4798509
    ## 1349                         Bolivia   year74    4902168
    ## 1350                         Bolivia   year75    5009257
    ## 1351                         Bolivia   year76    5119833
    ## 1352                         Bolivia   year77    5233677
    ## 1353                         Bolivia   year78    5350322
    ## 1354                         Bolivia   year79    5469123
    ## 1355                         Bolivia   year80    5589575
    ## 1356                         Bolivia   year81    5711599
    ## 1357                         Bolivia   year82    5835182
    ## 1358                         Bolivia   year83    5959960
    ## 1359                         Bolivia   year84    6085496
    ## 1360                         Bolivia   year85    6211550
    ## 1361                         Bolivia   year86    6337893
    ## 1362                         Bolivia   year87    6464732
    ## 1363                         Bolivia   year88    6592787
    ## 1364                         Bolivia   year89    6723046
    ## 1365                         Bolivia   year90    6856244
    ## 1366                         Bolivia   year91    6992521
    ## 1367                         Bolivia   year92    7131707
    ## 1368                         Bolivia   year93    7273825
    ## 1369                         Bolivia   year94    7418861
    ## 1370                         Bolivia   year95    7566714
    ## 1371                         Bolivia   year96    7717443
    ## 1372                         Bolivia   year97    7870855
    ## 1373                         Bolivia   year98    8026254
    ## 1374                         Bolivia   year99    8182712
    ## 1375                         Bolivia year2000    8339512
    ## 1376                         Bolivia year2001    8496375
    ## 1377                         Bolivia year2002    8653345
    ## 1378                         Bolivia year2003    8810420
    ## 1379                         Bolivia year2004    8967741
    ## 1380                         Bolivia year2005    9125409
    ## 1381                         Bolivia year2006    9283334
    ## 1382                         Bolivia year2007    9441444
    ## 1383                         Bolivia year2008    9599855
    ## 1384                         Bolivia year2009    9758748
    ## 1385                         Bolivia year2010    9918242
    ## 1386                         Bolivia year2011   10078343
    ## 1387                         Bolivia year2012   10239004
    ## 1388                         Bolivia year2013   10400264
    ## 1389                         Bolivia year2014   10562159
    ## 1390                         Bolivia year2015   10724705
    ## 1391                         Bolivia year2016   10887882
    ## 1392                         Bolivia year2017   11051600
    ## 1393          Bosnia and Herzegovina   year60    3225668
    ## 1394          Bosnia and Herzegovina   year61    3288602
    ## 1395          Bosnia and Herzegovina   year62    3353226
    ## 1396          Bosnia and Herzegovina   year63    3417574
    ## 1397          Bosnia and Herzegovina   year64    3478995
    ## 1398          Bosnia and Herzegovina   year65    3535640
    ## 1399          Bosnia and Herzegovina   year66    3586634
    ## 1400          Bosnia and Herzegovina   year67    3632669
    ## 1401          Bosnia and Herzegovina   year68    3675452
    ## 1402          Bosnia and Herzegovina   year69    3717466
    ## 1403          Bosnia and Herzegovina   year70    3760527
    ## 1404          Bosnia and Herzegovina   year71    3805285
    ## 1405          Bosnia and Herzegovina   year72    3851151
    ## 1406          Bosnia and Herzegovina   year73    3897255
    ## 1407          Bosnia and Herzegovina   year74    3942223
    ## 1408          Bosnia and Herzegovina   year75    3985103
    ## 1409          Bosnia and Herzegovina   year76    4025265
    ## 1410          Bosnia and Herzegovina   year77    4063191
    ## 1411          Bosnia and Herzegovina   year78    4100350
    ## 1412          Bosnia and Herzegovina   year79    4138819
    ## 1413          Bosnia and Herzegovina   year80    4179855
    ## 1414          Bosnia and Herzegovina   year81    4222511
    ## 1415          Bosnia and Herzegovina   year82    4265310
    ## 1416          Bosnia and Herzegovina   year83    4308106
    ## 1417          Bosnia and Herzegovina   year84    4350746
    ## 1418          Bosnia and Herzegovina   year85    4392130
    ## 1419          Bosnia and Herzegovina   year86    4435504
    ## 1420          Bosnia and Herzegovina   year87    4478519
    ## 1421          Bosnia and Herzegovina   year88    4508056
    ## 1422          Bosnia and Herzegovina   year89    4506653
    ## 1423          Bosnia and Herzegovina   year90    4463422
    ## 1424          Bosnia and Herzegovina   year91    4371603
    ## 1425          Bosnia and Herzegovina   year92    4239154
    ## 1426          Bosnia and Herzegovina   year93    4087999
    ## 1427          Bosnia and Herzegovina   year94    3948816
    ## 1428          Bosnia and Herzegovina   year95    3843712
    ## 1429          Bosnia and Herzegovina   year96    3780378
    ## 1430          Bosnia and Herzegovina   year97    3752431
    ## 1431          Bosnia and Herzegovina   year98    3750485
    ## 1432          Bosnia and Herzegovina   year99    3759118
    ## 1433          Bosnia and Herzegovina year2000    3766706
    ## 1434          Bosnia and Herzegovina year2001    3771284
    ## 1435          Bosnia and Herzegovina year2002    3775807
    ## 1436          Bosnia and Herzegovina year2003    3779247
    ## 1437          Bosnia and Herzegovina year2004    3781287
    ## 1438          Bosnia and Herzegovina year2005    3781530
    ## 1439          Bosnia and Herzegovina year2006    3779468
    ## 1440          Bosnia and Herzegovina year2007    3774000
    ## 1441          Bosnia and Herzegovina year2008    3763599
    ## 1442          Bosnia and Herzegovina year2009    3746561
    ## 1443          Bosnia and Herzegovina year2010    3722084
    ## 1444          Bosnia and Herzegovina year2011    3688865
    ## 1445          Bosnia and Herzegovina year2012    3648200
    ## 1446          Bosnia and Herzegovina year2013    3604999
    ## 1447          Bosnia and Herzegovina year2014    3566002
    ## 1448          Bosnia and Herzegovina year2015    3535961
    ## 1449          Bosnia and Herzegovina year2016    3516816
    ## 1450          Bosnia and Herzegovina year2017    3507017
    ## 1451                        Botswana   year60     524552
    ## 1452                        Botswana   year61     537249
    ## 1453                        Botswana   year62     550840
    ## 1454                        Botswana   year63     565353
    ## 1455                        Botswana   year64     580799
    ## 1456                        Botswana   year65     597190
    ## 1457                        Botswana   year66     614613
    ## 1458                        Botswana   year67     633154
    ## 1459                        Botswana   year68     652843
    ## 1460                        Botswana   year69     673640
    ## 1461                        Botswana   year70     695597
    ## 1462                        Botswana   year71     718639
    ## 1463                        Botswana   year72     742835
    ## 1464                        Botswana   year73     768512
    ## 1465                        Botswana   year74     796095
    ## 1466                        Botswana   year75     825840
    ## 1467                        Botswana   year76     857855
    ## 1468                        Botswana   year77     891926
    ## 1469                        Botswana   year78     927585
    ## 1470                        Botswana   year79     964166
    ## 1471                        Botswana   year80    1001158
    ## 1472                        Botswana   year81    1038397
    ## 1473                        Botswana   year82    1075889
    ## 1474                        Botswana   year83    1113539
    ## 1475                        Botswana   year84    1151292
    ## 1476                        Botswana   year85    1189114
    ## 1477                        Botswana   year86    1226810
    ## 1478                        Botswana   year87    1264314
    ## 1479                        Botswana   year88    1301818
    ## 1480                        Botswana   year89    1339624
    ## 1481                        Botswana   year90    1377912
    ## 1482                        Botswana   year91    1416731
    ## 1483                        Botswana   year92    1455833
    ## 1484                        Botswana   year93    1494693
    ## 1485                        Botswana   year94    1532622
    ## 1486                        Botswana   year95    1569094
    ## 1487                        Botswana   year96    1604060
    ## 1488                        Botswana   year97    1637635
    ## 1489                        Botswana   year98    1669625
    ## 1490                        Botswana   year99    1699862
    ## 1491                        Botswana year2000    1728340
    ## 1492                        Botswana year2001    1754935
    ## 1493                        Botswana year2002    1779953
    ## 1494                        Botswana year2003    1804339
    ## 1495                        Botswana year2004    1829330
    ## 1496                        Botswana year2005    1855852
    ## 1497                        Botswana year2006    1884238
    ## 1498                        Botswana year2007    1914414
    ## 1499                        Botswana year2008    1946351
    ## 1500                        Botswana year2009    1979882
    ## 1501                        Botswana year2010    2014866
    ## 1502                        Botswana year2011    2051339
    ## 1503                        Botswana year2012    2089315
    ## 1504                        Botswana year2013    2128507
    ## 1505                        Botswana year2014    2168573
    ## 1506                        Botswana year2015    2209197
    ## 1507                        Botswana year2016    2250260
    ## 1508                        Botswana year2017    2291661
    ## 1509                          Brazil   year60   72207554
    ## 1510                          Brazil   year61   74351763
    ## 1511                          Brazil   year62   76573248
    ## 1512                          Brazil   year63   78854019
    ## 1513                          Brazil   year64   81168654
    ## 1514                          Brazil   year65   83498020
    ## 1515                          Brazil   year66   85837799
    ## 1516                          Brazil   year67   88191378
    ## 1517                          Brazil   year68   90557064
    ## 1518                          Brazil   year69   92935072
    ## 1519                          Brazil   year70   95326793
    ## 1520                          Brazil   year71   97728961
    ## 1521                          Brazil   year72  100143598
    ## 1522                          Brazil   year73  102584278
    ## 1523                          Brazil   year74  105069367
    ## 1524                          Brazil   year75  107612100
    ## 1525                          Brazil   year76  110213082
    ## 1526                          Brazil   year77  112867867
    ## 1527                          Brazil   year78  115577669
    ## 1528                          Brazil   year79  118342626
    ## 1529                          Brazil   year80  121159761
    ## 1530                          Brazil   year81  124030908
    ## 1531                          Brazil   year82  126947365
    ## 1532                          Brazil   year83  129882321
    ## 1533                          Brazil   year84  132800684
    ## 1534                          Brazil   year85  135676281
    ## 1535                          Brazil   year86  138499464
    ## 1536                          Brazil   year87  141273488
    ## 1537                          Brazil   year88  144001542
    ## 1538                          Brazil   year89  146691981
    ## 1539                          Brazil   year90  149352145
    ## 1540                          Brazil   year91  151976577
    ## 1541                          Brazil   year92  154564278
    ## 1542                          Brazil   year93  157132682
    ## 1543                          Brazil   year94  159705123
    ## 1544                          Brazil   year95  162296612
    ## 1545                          Brazil   year96  164913306
    ## 1546                          Brazil   year97  167545164
    ## 1547                          Brazil   year98  170170640
    ## 1548                          Brazil   year99  172759243
    ## 1549                          Brazil year2000  175287587
    ## 1550                          Brazil year2001  177750670
    ## 1551                          Brazil year2002  180151021
    ## 1552                          Brazil year2003  182482149
    ## 1553                          Brazil year2004  184738458
    ## 1554                          Brazil year2005  186917361
    ## 1555                          Brazil year2006  189012412
    ## 1556                          Brazil year2007  191026637
    ## 1557                          Brazil year2008  192979029
    ## 1558                          Brazil year2009  194895996
    ## 1559                          Brazil year2010  196796269
    ## 1560                          Brazil year2011  198686688
    ## 1561                          Brazil year2012  200560983
    ## 1562                          Brazil year2013  202408632
    ## 1563                          Brazil year2014  204213133
    ## 1564                          Brazil year2015  205962108
    ## 1565                          Brazil year2016  207652865
    ## 1566                          Brazil year2017  209288278
    ## 1567          British Virgin Islands   year60       8033
    ## 1568          British Virgin Islands   year61       8155
    ## 1569          British Virgin Islands   year62       8298
    ## 1570          British Virgin Islands   year63       8452
    ## 1571          British Virgin Islands   year64       8627
    ## 1572          British Virgin Islands   year65       8814
    ## 1573          British Virgin Islands   year66       9007
    ## 1574          British Virgin Islands   year67       9218
    ## 1575          British Virgin Islands   year68       9424
    ## 1576          British Virgin Islands   year69       9621
    ## 1577          British Virgin Islands   year70       9803
    ## 1578          British Virgin Islands   year71       9970
    ## 1579          British Virgin Islands   year72      10125
    ## 1580          British Virgin Islands   year73      10264
    ## 1581          British Virgin Islands   year74      10379
    ## 1582          British Virgin Islands   year75      10476
    ## 1583          British Virgin Islands   year76      10543
    ## 1584          British Virgin Islands   year77      10591
    ## 1585          British Virgin Islands   year78      10662
    ## 1586          British Virgin Islands   year79      10792
    ## 1587          British Virgin Islands   year80      11002
    ## 1588          British Virgin Islands   year81      11315
    ## 1589          British Virgin Islands   year82      11712
    ## 1590          British Virgin Islands   year83      12188
    ## 1591          British Virgin Islands   year84      12731
    ## 1592          British Virgin Islands   year85      13304
    ## 1593          British Virgin Islands   year86      13938
    ## 1594          British Virgin Islands   year87      14589
    ## 1595          British Virgin Islands   year88      15266
    ## 1596          British Virgin Islands   year89      15900
    ## 1597          British Virgin Islands   year90      16461
    ## 1598          British Virgin Islands   year91      16934
    ## 1599          British Virgin Islands   year92      17344
    ## 1600          British Virgin Islands   year93      17703
    ## 1601          British Virgin Islands   year94      18052
    ## 1602          British Virgin Islands   year95      18427
    ## 1603          British Virgin Islands   year96      18833
    ## 1604          British Virgin Islands   year97      19270
    ## 1605          British Virgin Islands   year98      19722
    ## 1606          British Virgin Islands   year99      20188
    ## 1607          British Virgin Islands year2000      20645
    ## 1608          British Virgin Islands year2001      21085
    ## 1609          British Virgin Islands year2002      21529
    ## 1610          British Virgin Islands year2003      22000
    ## 1611          British Virgin Islands year2004      22541
    ## 1612          British Virgin Islands year2005      23168
    ## 1613          British Virgin Islands year2006      23905
    ## 1614          British Virgin Islands year2007      24731
    ## 1615          British Virgin Islands year2008      25604
    ## 1616          British Virgin Islands year2009      26447
    ## 1617          British Virgin Islands year2010      27224
    ## 1618          British Virgin Islands year2011      27901
    ## 1619          British Virgin Islands year2012      28509
    ## 1620          British Virgin Islands year2013      29056
    ## 1621          British Virgin Islands year2014      29588
    ## 1622          British Virgin Islands year2015      30113
    ## 1623          British Virgin Islands year2016      30661
    ## 1624          British Virgin Islands year2017      31196
    ## 1625               Brunei Darussalam   year60      81745
    ## 1626               Brunei Darussalam   year61      85596
    ## 1627               Brunei Darussalam   year62      89516
    ## 1628               Brunei Darussalam   year63      93576
    ## 1629               Brunei Darussalam   year64      97848
    ## 1630               Brunei Darussalam   year65     102425
    ## 1631               Brunei Darussalam   year66     107316
    ## 1632               Brunei Darussalam   year67     112494
    ## 1633               Brunei Darussalam   year68     117950
    ## 1634               Brunei Darussalam   year69     123653
    ## 1635               Brunei Darussalam   year70     129583
    ## 1636               Brunei Darussalam   year71     135726
    ## 1637               Brunei Darussalam   year72     142073
    ## 1638               Brunei Darussalam   year73     148560
    ## 1639               Brunei Darussalam   year74     155109
    ## 1640               Brunei Darussalam   year75     161671
    ## 1641               Brunei Darussalam   year76     168224
    ## 1642               Brunei Darussalam   year77     174773
    ## 1643               Brunei Darussalam   year78     181257
    ## 1644               Brunei Darussalam   year79     187656
    ## 1645               Brunei Darussalam   year80     193949
    ## 1646               Brunei Darussalam   year81     200085
    ## 1647               Brunei Darussalam   year82     206128
    ## 1648               Brunei Darussalam   year83     212136
    ## 1649               Brunei Darussalam   year84     218227
    ## 1650               Brunei Darussalam   year85     224512
    ## 1651               Brunei Darussalam   year86     230972
    ## 1652               Brunei Darussalam   year87     237622
    ## 1653               Brunei Darussalam   year88     244458
    ## 1654               Brunei Darussalam   year89     251514
    ## 1655               Brunei Darussalam   year90     258785
    ## 1656               Brunei Darussalam   year91     266274
    ## 1657               Brunei Darussalam   year92     273963
    ## 1658               Brunei Darussalam   year93     281751
    ## 1659               Brunei Darussalam   year94     289525
    ## 1660               Brunei Darussalam   year95     297192
    ## 1661               Brunei Darussalam   year96     304699
    ## 1662               Brunei Darussalam   year97     312038
    ## 1663               Brunei Darussalam   year98     319222
    ## 1664               Brunei Darussalam   year99     326289
    ## 1665               Brunei Darussalam year2000     333241
    ## 1666               Brunei Darussalam year2001     340117
    ## 1667               Brunei Darussalam year2002     346867
    ## 1668               Brunei Darussalam year2003     353389
    ## 1669               Brunei Darussalam year2004     359523
    ## 1670               Brunei Darussalam year2005     365158
    ## 1671               Brunei Darussalam year2006     370250
    ## 1672               Brunei Darussalam year2007     374864
    ## 1673               Brunei Darussalam year2008     379252
    ## 1674               Brunei Darussalam year2009     383772
    ## 1675               Brunei Darussalam year2010     388662
    ## 1676               Brunei Darussalam year2011     394013
    ## 1677               Brunei Darussalam year2012     399748
    ## 1678               Brunei Darussalam year2013     405716
    ## 1679               Brunei Darussalam year2014     411704
    ## 1680               Brunei Darussalam year2015     417542
    ## 1681               Brunei Darussalam year2016     423196
    ## 1682               Brunei Darussalam year2017     428697
    ## 1683                        Bulgaria   year60    7867374
    ## 1684                        Bulgaria   year61    7943118
    ## 1685                        Bulgaria   year62    8012946
    ## 1686                        Bulgaria   year63    8078145
    ## 1687                        Bulgaria   year64    8144340
    ## 1688                        Bulgaria   year65    8204168
    ## 1689                        Bulgaria   year66    8258057
    ## 1690                        Bulgaria   year67    8310226
    ## 1691                        Bulgaria   year68    8369603
    ## 1692                        Bulgaria   year69    8434172
    ## 1693                        Bulgaria   year70    8489574
    ## 1694                        Bulgaria   year71    8536395
    ## 1695                        Bulgaria   year72    8576200
    ## 1696                        Bulgaria   year73    8620967
    ## 1697                        Bulgaria   year74    8678745
    ## 1698                        Bulgaria   year75    8720742
    ## 1699                        Bulgaria   year76    8758599
    ## 1700                        Bulgaria   year77    8804183
    ## 1701                        Bulgaria   year78    8814032
    ## 1702                        Bulgaria   year79    8825940
    ## 1703                        Bulgaria   year80    8861535
    ## 1704                        Bulgaria   year81    8891117
    ## 1705                        Bulgaria   year82    8917457
    ## 1706                        Bulgaria   year83    8939738
    ## 1707                        Bulgaria   year84    8960679
    ## 1708                        Bulgaria   year85    8960547
    ## 1709                        Bulgaria   year86    8958171
    ## 1710                        Bulgaria   year87    8971359
    ## 1711                        Bulgaria   year88    8981446
    ## 1712                        Bulgaria   year89    8876972
    ## 1713                        Bulgaria   year90    8718289
    ## 1714                        Bulgaria   year91    8632367
    ## 1715                        Bulgaria   year92    8540164
    ## 1716                        Bulgaria   year93    8472313
    ## 1717                        Bulgaria   year94    8443591
    ## 1718                        Bulgaria   year95    8406067
    ## 1719                        Bulgaria   year96    8362826
    ## 1720                        Bulgaria   year97    8312068
    ## 1721                        Bulgaria   year98    8256786
    ## 1722                        Bulgaria   year99    8210624
    ## 1723                        Bulgaria year2000    8170172
    ## 1724                        Bulgaria year2001    8009142
    ## 1725                        Bulgaria year2002    7837161
    ## 1726                        Bulgaria year2003    7775327
    ## 1727                        Bulgaria year2004    7716860
    ## 1728                        Bulgaria year2005    7658972
    ## 1729                        Bulgaria year2006    7601022
    ## 1730                        Bulgaria year2007    7545338
    ## 1731                        Bulgaria year2008    7492561
    ## 1732                        Bulgaria year2009    7444443
    ## 1733                        Bulgaria year2010    7395599
    ## 1734                        Bulgaria year2011    7348328
    ## 1735                        Bulgaria year2012    7305888
    ## 1736                        Bulgaria year2013    7265115
    ## 1737                        Bulgaria year2014    7223938
    ## 1738                        Bulgaria year2015    7177991
    ## 1739                        Bulgaria year2016    7127822
    ## 1740                        Bulgaria year2017    7075991
    ## 1741                    Burkina Faso   year60    4829288
    ## 1742                    Burkina Faso   year61    4894580
    ## 1743                    Burkina Faso   year62    4960326
    ## 1744                    Burkina Faso   year63    5027821
    ## 1745                    Burkina Faso   year64    5098890
    ## 1746                    Burkina Faso   year65    5174870
    ## 1747                    Burkina Faso   year66    5256363
    ## 1748                    Burkina Faso   year67    5343019
    ## 1749                    Burkina Faso   year68    5434041
    ## 1750                    Burkina Faso   year69    5528174
    ## 1751                    Burkina Faso   year70    5624600
    ## 1752                    Burkina Faso   year71    5723381
    ## 1753                    Burkina Faso   year72    5825173
    ## 1754                    Burkina Faso   year73    5930483
    ## 1755                    Burkina Faso   year74    6040041
    ## 1756                    Burkina Faso   year75    6154545
    ## 1757                    Burkina Faso   year76    6274037
    ## 1758                    Burkina Faso   year77    6398935
    ## 1759                    Burkina Faso   year78    6530819
    ## 1760                    Burkina Faso   year79    6671656
    ## 1761                    Burkina Faso   year80    6822843
    ## 1762                    Burkina Faso   year81    6985160
    ## 1763                    Burkina Faso   year82    7158255
    ## 1764                    Burkina Faso   year83    7340905
    ## 1765                    Burkina Faso   year84    7531242
    ## 1766                    Burkina Faso   year85    7727907
    ## 1767                    Burkina Faso   year86    7930694
    ## 1768                    Burkina Faso   year87    8140073
    ## 1769                    Burkina Faso   year88    8356305
    ## 1770                    Burkina Faso   year89    8579823
    ## 1771                    Burkina Faso   year90    8811034
    ## 1772                    Burkina Faso   year91    9050084
    ## 1773                    Burkina Faso   year92    9297113
    ## 1774                    Burkina Faso   year93    9552476
    ## 1775                    Burkina Faso   year94    9816588
    ## 1776                    Burkina Faso   year95   10089878
    ## 1777                    Burkina Faso   year96   10372745
    ## 1778                    Burkina Faso   year97   10665546
    ## 1779                    Burkina Faso   year98   10968724
    ## 1780                    Burkina Faso   year99   11282701
    ## 1781                    Burkina Faso year2000   11607942
    ## 1782                    Burkina Faso year2001   11944587
    ## 1783                    Burkina Faso year2002   12293100
    ## 1784                    Burkina Faso year2003   12654621
    ## 1785                    Burkina Faso year2004   13030569
    ## 1786                    Burkina Faso year2005   13421930
    ## 1787                    Burkina Faso year2006   13829177
    ## 1788                    Burkina Faso year2007   14252021
    ## 1789                    Burkina Faso year2008   14689726
    ## 1790                    Burkina Faso year2009   15141099
    ## 1791                    Burkina Faso year2010   15605217
    ## 1792                    Burkina Faso year2011   16081904
    ## 1793                    Burkina Faso year2012   16571216
    ## 1794                    Burkina Faso year2013   17072723
    ## 1795                    Burkina Faso year2014   17585977
    ## 1796                    Burkina Faso year2015   18110624
    ## 1797                    Burkina Faso year2016   18646433
    ## 1798                    Burkina Faso year2017   19193382
    ## 1799                         Burundi   year60    2786106
    ## 1800                         Burundi   year61    2839666
    ## 1801                         Burundi   year62    2893669
    ## 1802                         Burundi   year63    2949926
    ## 1803                         Burundi   year64    3010859
    ## 1804                         Burundi   year65    3077876
    ## 1805                         Burundi   year66    3152723
    ## 1806                         Burundi   year67    3234023
    ## 1807                         Burundi   year68    3316233
    ## 1808                         Burundi   year69    3391753
    ## 1809                         Burundi   year70    3455606
    ## 1810                         Burundi   year71    3505391
    ## 1811                         Burundi   year72    3544047
    ## 1812                         Burundi   year73    3578490
    ## 1813                         Burundi   year74    3618585
    ## 1814                         Burundi   year75    3671494
    ## 1815                         Burundi   year76    3739659
    ## 1816                         Burundi   year77    3821194
    ## 1817                         Burundi   year78    3913768
    ## 1818                         Burundi   year79    4013310
    ## 1819                         Burundi   year80    4116817
    ## 1820                         Burundi   year81    4223195
    ## 1821                         Burundi   year82    4333386
    ## 1822                         Burundi   year83    4448728
    ## 1823                         Burundi   year84    4571292
    ## 1824                         Burundi   year85    4702066
    ## 1825                         Burundi   year86    4841565
    ## 1826                         Burundi   year87    4987736
    ## 1827                         Burundi   year88    5135956
    ## 1828                         Burundi   year89    5280024
    ## 1829                         Burundi   year90    5415415
    ## 1830                         Burundi   year91    5542048
    ## 1831                         Burundi   year92    5661139
    ## 1832                         Burundi   year93    5771398
    ## 1833                         Burundi   year94    5871607
    ## 1834                         Burundi   year95    5962058
    ## 1835                         Burundi   year96    6041112
    ## 1836                         Burundi   year97    6112097
    ## 1837                         Burundi   year98    6186352
    ## 1838                         Burundi   year99    6278940
    ## 1839                         Burundi year2000    6400706
    ## 1840                         Burundi year2001    6555829
    ## 1841                         Burundi year2002    6741569
    ## 1842                         Burundi year2003    6953113
    ## 1843                         Burundi year2004    7182451
    ## 1844                         Burundi year2005    7423289
    ## 1845                         Burundi year2006    7675338
    ## 1846                         Burundi year2007    7939573
    ## 1847                         Burundi year2008    8212264
    ## 1848                         Burundi year2009    8489031
    ## 1849                         Burundi year2010    8766930
    ## 1850                         Burundi year2011    9043508
    ## 1851                         Burundi year2012    9319710
    ## 1852                         Burundi year2013    9600186
    ## 1853                         Burundi year2014    9891790
    ## 1854                         Burundi year2015   10199270
    ## 1855                         Burundi year2016   10524117
    ## 1856                         Burundi year2017   10864245
    ## 1857                      Cabo Verde   year60     202310
    ## 1858                      Cabo Verde   year61     205956
    ## 1859                      Cabo Verde   year62     210867
    ## 1860                      Cabo Verde   year63     216908
    ## 1861                      Cabo Verde   year64     223846
    ## 1862                      Cabo Verde   year65     231428
    ## 1863                      Cabo Verde   year66     239770
    ## 1864                      Cabo Verde   year67     248747
    ## 1865                      Cabo Verde   year68     257509
    ## 1866                      Cabo Verde   year69     264909
    ## 1867                      Cabo Verde   year70     270198
    ## 1868                      Cabo Verde   year71     272992
    ## 1869                      Cabo Verde   year72     273651
    ## 1870                      Cabo Verde   year73     273005
    ## 1871                      Cabo Verde   year74     272292
    ## 1872                      Cabo Verde   year75     272423
    ## 1873                      Cabo Verde   year76     273652
    ## 1874                      Cabo Verde   year77     275767
    ## 1875                      Cabo Verde   year78     278739
    ## 1876                      Cabo Verde   year79     282415
    ## 1877                      Cabo Verde   year80     286657
    ## 1878                      Cabo Verde   year81     291602
    ## 1879                      Cabo Verde   year82     297285
    ## 1880                      Cabo Verde   year83     303368
    ## 1881                      Cabo Verde   year84     309397
    ## 1882                      Cabo Verde   year85     315069
    ## 1883                      Cabo Verde   year86     320183
    ## 1884                      Cabo Verde   year87     324893
    ## 1885                      Cabo Verde   year88     329671
    ## 1886                      Cabo Verde   year89     335184
    ## 1887                      Cabo Verde   year90     341883
    ## 1888                      Cabo Verde   year91     349934
    ## 1889                      Cabo Verde   year92     359090
    ## 1890                      Cabo Verde   year93     369014
    ## 1891                      Cabo Verde   year94     379156
    ## 1892                      Cabo Verde   year95     389127
    ## 1893                      Cabo Verde   year96     398773
    ## 1894                      Cabo Verde   year97     408175
    ## 1895                      Cabo Verde   year98     417323
    ## 1896                      Cabo Verde   year99     426285
    ## 1897                      Cabo Verde year2000     435079
    ## 1898                      Cabo Verde year2001     443716
    ## 1899                      Cabo Verde year2002     452106
    ## 1900                      Cabo Verde year2003     460147
    ## 1901                      Cabo Verde year2004     467664
    ## 1902                      Cabo Verde year2005     474567
    ## 1903                      Cabo Verde year2006     480795
    ## 1904                      Cabo Verde year2007     486438
    ## 1905                      Cabo Verde year2008     491723
    ## 1906                      Cabo Verde year2009     496963
    ## 1907                      Cabo Verde year2010     502384
    ## 1908                      Cabo Verde year2011     508067
    ## 1909                      Cabo Verde year2012     513979
    ## 1910                      Cabo Verde year2013     520106
    ## 1911                      Cabo Verde year2014     526437
    ## 1912                      Cabo Verde year2015     532913
    ## 1913                      Cabo Verde year2016     539560
    ## 1914                      Cabo Verde year2017     546388
    ## 1915                        Cambodia   year60    5722370
    ## 1916                        Cambodia   year61    5873015
    ## 1917                        Cambodia   year62    6028551
    ## 1918                        Cambodia   year63    6183747
    ## 1919                        Cambodia   year64    6331583
    ## 1920                        Cambodia   year65    6467197
    ## 1921                        Cambodia   year66    6584766
    ## 1922                        Cambodia   year67    6685321
    ## 1923                        Cambodia   year68    6778723
    ## 1924                        Cambodia   year69    6879184
    ## 1925                        Cambodia   year70    6994848
    ## 1926                        Cambodia   year71    7137749
    ## 1927                        Cambodia   year72    7300152
    ## 1928                        Cambodia   year73    7447285
    ## 1929                        Cambodia   year74    7531424
    ## 1930                        Cambodia   year75    7522593
    ## 1931                        Cambodia   year76    7402873
    ## 1932                        Cambodia   year77    7194279
    ## 1933                        Cambodia   year78    6955566
    ## 1934                        Cambodia   year79    6768724
    ## 1935                        Cambodia   year80    6692107
    ## 1936                        Cambodia   year81    6748193
    ## 1937                        Cambodia   year82    6918101
    ## 1938                        Cambodia   year83    7168236
    ## 1939                        Cambodia   year84    7446019
    ## 1940                        Cambodia   year85    7712978
    ## 1941                        Cambodia   year86    7958976
    ## 1942                        Cambodia   year87    8196037
    ## 1943                        Cambodia   year88    8433798
    ## 1944                        Cambodia   year89    8689152
    ## 1945                        Cambodia   year90    8973342
    ## 1946                        Cambodia   year91    9286976
    ## 1947                        Cambodia   year92    9621504
    ## 1948                        Cambodia   year93    9968275
    ## 1949                        Cambodia   year94   10315376
    ## 1950                        Cambodia   year95   10653558
    ## 1951                        Cambodia   year96   10980273
    ## 1952                        Cambodia   year97   11295880
    ## 1953                        Cambodia   year98   11597739
    ## 1954                        Cambodia   year99   11883636
    ## 1955                        Cambodia year2000   12152354
    ## 1956                        Cambodia year2001   12402473
    ## 1957                        Cambodia year2002   12634729
    ## 1958                        Cambodia year2003   12853124
    ## 1959                        Cambodia year2004   13063377
    ## 1960                        Cambodia year2005   13270201
    ## 1961                        Cambodia year2006   13474489
    ## 1962                        Cambodia year2007   13676693
    ## 1963                        Cambodia year2008   13880509
    ## 1964                        Cambodia year2009   14090208
    ## 1965                        Cambodia year2010   14308740
    ## 1966                        Cambodia year2011   14537886
    ## 1967                        Cambodia year2012   14776866
    ## 1968                        Cambodia year2013   15022692
    ## 1969                        Cambodia year2014   15270790
    ## 1970                        Cambodia year2015   15517635
    ## 1971                        Cambodia year2016   15762370
    ## 1972                        Cambodia year2017   16005373
    ## 1973                        Cameroon   year60    5176268
    ## 1974                        Cameroon   year61    5285231
    ## 1975                        Cameroon   year62    5399922
    ## 1976                        Cameroon   year63    5520332
    ## 1977                        Cameroon   year64    5646316
    ## 1978                        Cameroon   year65    5777834
    ## 1979                        Cameroon   year66    5915123
    ## 1980                        Cameroon   year67    6058539
    ## 1981                        Cameroon   year68    6208282
    ## 1982                        Cameroon   year69    6364569
    ## 1983                        Cameroon   year70    6527635
    ## 1984                        Cameroon   year71    6697745
    ## 1985                        Cameroon   year72    6875228
    ## 1986                        Cameroon   year73    7060603
    ## 1987                        Cameroon   year74    7254468
    ## 1988                        Cameroon   year75    7457362
    ## 1989                        Cameroon   year76    7669445
    ## 1990                        Cameroon   year77    7890969
    ## 1991                        Cameroon   year78    8122529
    ## 1992                        Cameroon   year79    8364835
    ## 1993                        Cameroon   year80    8618354
    ## 1994                        Cameroon   year81    8883016
    ## 1995                        Cameroon   year82    9158566
    ## 1996                        Cameroon   year83    9445003
    ## 1997                        Cameroon   year84    9742263
    ## 1998                        Cameroon   year85   10050023
    ## 1999                        Cameroon   year86   10368300
    ## 2000                        Cameroon   year87   10696274
    ## 2001                        Cameroon   year88   11031817
    ## 2002                        Cameroon   year89   11372160
    ## 2003                        Cameroon   year90   11715218
    ## 2004                        Cameroon   year91   12060729
    ## 2005                        Cameroon   year92   12408931
    ## 2006                        Cameroon   year93   12758881
    ## 2007                        Cameroon   year94   13109660
    ## 2008                        Cameroon   year95   13460994
    ## 2009                        Cameroon   year96   13812472
    ## 2010                        Cameroon   year97   14165423
    ## 2011                        Cameroon   year98   14523570
    ## 2012                        Cameroon   year99   14891891
    ## 2013                        Cameroon year2000   15274234
    ## 2014                        Cameroon year2001   15671927
    ## 2015                        Cameroon year2002   16084886
    ## 2016                        Cameroon year2003   16513822
    ## 2017                        Cameroon year2004   16959081
    ## 2018                        Cameroon year2005   17420795
    ## 2019                        Cameroon year2006   17899562
    ## 2020                        Cameroon year2007   18395389
    ## 2021                        Cameroon year2008   18907008
    ## 2022                        Cameroon year2009   19432541
    ## 2023                        Cameroon year2010   19970495
    ## 2024                        Cameroon year2011   20520447
    ## 2025                        Cameroon year2012   21082383
    ## 2026                        Cameroon year2013   21655715
    ## 2027                        Cameroon year2014   22239904
    ## 2028                        Cameroon year2015   22834522
    ## 2029                        Cameroon year2016   23439189
    ## 2030                        Cameroon year2017   24053727
    ## 2031                          Canada   year60   17909009
    ## 2032                          Canada   year61   18271000
    ## 2033                          Canada   year62   18614000
    ## 2034                          Canada   year63   18964000
    ## 2035                          Canada   year64   19325000
    ## 2036                          Canada   year65   19678000
    ## 2037                          Canada   year66   20048000
    ## 2038                          Canada   year67   20412000
    ## 2039                          Canada   year68   20744000
    ## 2040                          Canada   year69   21028000
    ## 2041                          Canada   year70   21324000
    ## 2042                          Canada   year71   21645535
    ## 2043                          Canada   year72   21993631
    ## 2044                          Canada   year73   22369408
    ## 2045                          Canada   year74   22774087
    ## 2046                          Canada   year75   23209000
    ## 2047                          Canada   year76   23518000
    ## 2048                          Canada   year77   23796000
    ## 2049                          Canada   year78   24036000
    ## 2050                          Canada   year79   24277000
    ## 2051                          Canada   year80   24593000
    ## 2052                          Canada   year81   24900000
    ## 2053                          Canada   year82   25202000
    ## 2054                          Canada   year83   25456000
    ## 2055                          Canada   year84   25702000
    ## 2056                          Canada   year85   25942000
    ## 2057                          Canada   year86   26204000
    ## 2058                          Canada   year87   26550000
    ## 2059                          Canada   year88   26895000
    ## 2060                          Canada   year89   27379000
    ## 2061                          Canada   year90   27791000
    ## 2062                          Canada   year91   28171682
    ## 2063                          Canada   year92   28519597
    ## 2064                          Canada   year93   28833410
    ## 2065                          Canada   year94   29111906
    ## 2066                          Canada   year95   29354000
    ## 2067                          Canada   year96   29671900
    ## 2068                          Canada   year97   29987200
    ## 2069                          Canada   year98   30247900
    ## 2070                          Canada   year99   30499200
    ## 2071                          Canada year2000   30769700
    ## 2072                          Canada year2001   31081900
    ## 2073                          Canada year2002   31362000
    ## 2074                          Canada year2003   31676000
    ## 2075                          Canada year2004   31995000
    ## 2076                          Canada year2005   32312000
    ## 2077                          Canada year2006   32570505
    ## 2078                          Canada year2007   32887928
    ## 2079                          Canada year2008   33245773
    ## 2080                          Canada year2009   33628571
    ## 2081                          Canada year2010   34005274
    ## 2082                          Canada year2011   34342780
    ## 2083                          Canada year2012   34750545
    ## 2084                          Canada year2013   35152370
    ## 2085                          Canada year2014   35535348
    ## 2086                          Canada year2015   35832513
    ## 2087                          Canada year2016   36264604
    ## 2088                          Canada year2017   36708083
    ## 2089          Caribbean small states   year60    4198307
    ## 2090          Caribbean small states   year61    4277802
    ## 2091          Caribbean small states   year62    4357746
    ## 2092          Caribbean small states   year63    4436804
    ## 2093          Caribbean small states   year64    4513246
    ## 2094          Caribbean small states   year65    4585777
    ## 2095          Caribbean small states   year66    4653919
    ## 2096          Caribbean small states   year67    4718167
    ## 2097          Caribbean small states   year68    4779624
    ## 2098          Caribbean small states   year69    4839881
    ## 2099          Caribbean small states   year70    4900059
    ## 2100          Caribbean small states   year71    4960647
    ## 2101          Caribbean small states   year72    5021359
    ## 2102          Caribbean small states   year73    5082049
    ## 2103          Caribbean small states   year74    5142246
    ## 2104          Caribbean small states   year75    5201705
    ## 2105          Caribbean small states   year76    5260062
    ## 2106          Caribbean small states   year77    5317542
    ## 2107          Caribbean small states   year78    5375393
    ## 2108          Caribbean small states   year79    5435143
    ## 2109          Caribbean small states   year80    5497756
    ## 2110          Caribbean small states   year81    5564200
    ## 2111          Caribbean small states   year82    5633661
    ## 2112          Caribbean small states   year83    5702754
    ## 2113          Caribbean small states   year84    5766957
    ## 2114          Caribbean small states   year85    5823242
    ## 2115          Caribbean small states   year86    5870023
    ## 2116          Caribbean small states   year87    5908886
    ## 2117          Caribbean small states   year88    5943661
    ## 2118          Caribbean small states   year89    5979907
    ## 2119          Caribbean small states   year90    6021614
    ## 2120          Caribbean small states   year91    6070204
    ## 2121          Caribbean small states   year92    6124265
    ## 2122          Caribbean small states   year93    6181538
    ## 2123          Caribbean small states   year94    6238576
    ## 2124          Caribbean small states   year95    6292827
    ## 2125          Caribbean small states   year96    6343683
    ## 2126          Caribbean small states   year97    6392040
    ## 2127          Caribbean small states   year98    6438587
    ## 2128          Caribbean small states   year99    6484510
    ## 2129          Caribbean small states year2000    6530691
    ## 2130          Caribbean small states year2001    6577216
    ## 2131          Caribbean small states year2002    6623792
    ## 2132          Caribbean small states year2003    6670276
    ## 2133          Caribbean small states year2004    6716373
    ## 2134          Caribbean small states year2005    6761932
    ## 2135          Caribbean small states year2006    6806838
    ## 2136          Caribbean small states year2007    6851221
    ## 2137          Caribbean small states year2008    6895315
    ## 2138          Caribbean small states year2009    6939534
    ## 2139          Caribbean small states year2010    6984096
    ## 2140          Caribbean small states year2011    7029022
    ## 2141          Caribbean small states year2012    7074129
    ## 2142          Caribbean small states year2013    7118888
    ## 2143          Caribbean small states year2014    7162679
    ## 2144          Caribbean small states year2015    7204948
    ## 2145          Caribbean small states year2016    7245472
    ## 2146          Caribbean small states year2017    7284294
    ## 2147                  Cayman Islands   year60       7865
    ## 2148                  Cayman Islands   year61       8026
    ## 2149                  Cayman Islands   year62       8146
    ## 2150                  Cayman Islands   year63       8227
    ## 2151                  Cayman Islands   year64       8298
    ## 2152                  Cayman Islands   year65       8369
    ## 2153                  Cayman Islands   year66       8441
    ## 2154                  Cayman Islands   year67       8521
    ## 2155                  Cayman Islands   year68       8631
    ## 2156                  Cayman Islands   year69       8827
    ## 2157                  Cayman Islands   year70       9144
    ## 2158                  Cayman Islands   year71       9581
    ## 2159                  Cayman Islands   year72      10136
    ## 2160                  Cayman Islands   year73      10784
    ## 2161                  Cayman Islands   year74      11498
    ## 2162                  Cayman Islands   year75      12244
    ## 2163                  Cayman Islands   year76      13022
    ## 2164                  Cayman Islands   year77      13841
    ## 2165                  Cayman Islands   year78      14661
    ## 2166                  Cayman Islands   year79      15444
    ## 2167                  Cayman Islands   year80      16162
    ## 2168                  Cayman Islands   year81      16789
    ## 2169                  Cayman Islands   year82      17356
    ## 2170                  Cayman Islands   year83      17906
    ## 2171                  Cayman Islands   year84      18543
    ## 2172                  Cayman Islands   year85      19313
    ## 2173                  Cayman Islands   year86      20251
    ## 2174                  Cayman Islands   year87      21339
    ## 2175                  Cayman Islands   year88      22538
    ## 2176                  Cayman Islands   year89      23776
    ## 2177                  Cayman Islands   year90      25010
    ## 2178                  Cayman Islands   year91      26213
    ## 2179                  Cayman Islands   year92      27404
    ## 2180                  Cayman Islands   year93      28646
    ## 2181                  Cayman Islands   year94      30055
    ## 2182                  Cayman Islands   year95      31672
    ## 2183                  Cayman Islands   year96      33536
    ## 2184                  Cayman Islands   year97      35597
    ## 2185                  Cayman Islands   year98      37740
    ## 2186                  Cayman Islands   year99      39808
    ## 2187                  Cayman Islands year2000      41687
    ## 2188                  Cayman Islands year2001      43316
    ## 2189                  Cayman Islands year2002      44738
    ## 2190                  Cayman Islands year2003      46028
    ## 2191                  Cayman Islands year2004      47299
    ## 2192                  Cayman Islands year2005      48622
    ## 2193                  Cayman Islands year2006      50031
    ## 2194                  Cayman Islands year2007      51483
    ## 2195                  Cayman Islands year2008      52926
    ## 2196                  Cayman Islands year2009      54279
    ## 2197                  Cayman Islands year2010      55507
    ## 2198                  Cayman Islands year2011      56579
    ## 2199                  Cayman Islands year2012      57523
    ## 2200                  Cayman Islands year2013      58371
    ## 2201                  Cayman Islands year2014      59172
    ## 2202                  Cayman Islands year2015      59963
    ## 2203                  Cayman Islands year2016      60765
    ## 2204                  Cayman Islands year2017      61559
    ## 2205                            Chad   year60    3001593
    ## 2206                            Chad   year61    3060355
    ## 2207                            Chad   year62    3121216
    ## 2208                            Chad   year63    3183551
    ## 2209                            Chad   year64    3246505
    ## 2210                            Chad   year65    3309573
    ## 2211                            Chad   year66    3372170
    ## 2212                            Chad   year67    3434811
    ## 2213                            Chad   year68    3499352
    ## 2214                            Chad   year69    3568376
    ## 2215                            Chad   year70    3643549
    ## 2216                            Chad   year71    3726091
    ## 2217                            Chad   year72    3815103
    ## 2218                            Chad   year73    3907632
    ## 2219                            Chad   year74    3999512
    ## 2220                            Chad   year75    4087948
    ## 2221                            Chad   year76    4172230
    ## 2222                            Chad   year77    4253989
    ## 2223                            Chad   year78    4335645
    ## 2224                            Chad   year79    4420716
    ## 2225                            Chad   year80    4512042
    ## 2226                            Chad   year81    4610167
    ## 2227                            Chad   year82    4715197
    ## 2228                            Chad   year83    4829094
    ## 2229                            Chad   year84    4954046
    ## 2230                            Chad   year85    5091535
    ## 2231                            Chad   year86    5243006
    ## 2232                            Chad   year87    5408087
    ## 2233                            Chad   year88    5584339
    ## 2234                            Chad   year89    5768086
    ## 2235                            Chad   year90    5956859
    ## 2236                            Chad   year91    6150081
    ## 2237                            Chad   year92    6349089
    ## 2238                            Chad   year93    6555603
    ## 2239                            Chad   year94    6772133
    ## 2240                            Chad   year95    7000722
    ## 2241                            Chad   year96    7241134
    ## 2242                            Chad   year97    7493251
    ## 2243                            Chad   year98    7759258
    ## 2244                            Chad   year99    8041846
    ## 2245                            Chad year2000    8342559
    ## 2246                            Chad year2001    8663012
    ## 2247                            Chad year2002    9001689
    ## 2248                            Chad year2003    9353201
    ## 2249                            Chad year2004    9710043
    ## 2250                            Chad year2005   10067009
    ## 2251                            Chad year2006   10421597
    ## 2252                            Chad year2007   10775708
    ## 2253                            Chad year2008   11133861
    ## 2254                            Chad year2009   11502786
    ## 2255                            Chad year2010   11887202
    ## 2256                            Chad year2011   12288651
    ## 2257                            Chad year2012   12705135
    ## 2258                            Chad year2013   13133589
    ## 2259                            Chad year2014   13569438
    ## 2260                            Chad year2015   14009413
    ## 2261                            Chad year2016   14452543
    ## 2262                            Chad year2017   14899994
    ## 2263                 Channel Islands   year60     109420
    ## 2264                 Channel Islands   year61     110399
    ## 2265                 Channel Islands   year62     111457
    ## 2266                 Channel Islands   year63     112595
    ## 2267                 Channel Islands   year64     113773
    ## 2268                 Channel Islands   year65     114995
    ## 2269                 Channel Islands   year66     116227
    ## 2270                 Channel Islands   year67     117474
    ## 2271                 Channel Islands   year68     118726
    ## 2272                 Channel Islands   year69     119972
    ## 2273                 Channel Islands   year70     121197
    ## 2274                 Channel Islands   year71     122413
    ## 2275                 Channel Islands   year72     123614
    ## 2276                 Channel Islands   year73     124725
    ## 2277                 Channel Islands   year74     125682
    ## 2278                 Channel Islands   year75     126415
    ## 2279                 Channel Islands   year76     126902
    ## 2280                 Channel Islands   year77     127183
    ## 2281                 Channel Islands   year78     127390
    ## 2282                 Channel Islands   year79     127692
    ## 2283                 Channel Islands   year80     128212
    ## 2284                 Channel Islands   year81     128981
    ## 2285                 Channel Islands   year82     129979
    ## 2286                 Channel Islands   year83     131156
    ## 2287                 Channel Islands   year84     132453
    ## 2288                 Channel Islands   year85     133808
    ## 2289                 Channel Islands   year86     135230
    ## 2290                 Channel Islands   year87     136716
    ## 2291                 Channel Islands   year88     138187
    ## 2292                 Channel Islands   year89     139530
    ## 2293                 Channel Islands   year90     140671
    ## 2294                 Channel Islands   year91     141568
    ## 2295                 Channel Islands   year92     142258
    ## 2296                 Channel Islands   year93     142819
    ## 2297                 Channel Islands   year94     143384
    ## 2298                 Channel Islands   year95     144046
    ## 2299                 Channel Islands   year96     144829
    ## 2300                 Channel Islands   year97     145715
    ## 2301                 Channel Islands   year98     146671
    ## 2302                 Channel Islands   year99     147687
    ## 2303                 Channel Islands year2000     148725
    ## 2304                 Channel Islands year2001     149793
    ## 2305                 Channel Islands year2002     150901
    ## 2306                 Channel Islands year2003     152038
    ## 2307                 Channel Islands year2004     153170
    ## 2308                 Channel Islands year2005     154294
    ## 2309                 Channel Islands year2006     155411
    ## 2310                 Channel Islands year2007     156513
    ## 2311                 Channel Islands year2008     157581
    ## 2312                 Channel Islands year2009     158603
    ## 2313                 Channel Islands year2010     159581
    ## 2314                 Channel Islands year2011     160497
    ## 2315                 Channel Islands year2012     161358
    ## 2316                 Channel Islands year2013     162180
    ## 2317                 Channel Islands year2014     162969
    ## 2318                 Channel Islands year2015     163758
    ## 2319                 Channel Islands year2016     164541
    ## 2320                 Channel Islands year2017     165314
    ## 2321                           Chile   year60    7716625
    ## 2322                           Chile   year61    7890156
    ## 2323                           Chile   year62    8067136
    ## 2324                           Chile   year63    8247415
    ## 2325                           Chile   year64    8430838
    ## 2326                           Chile   year65    8617077
    ## 2327                           Chile   year66    8806137
    ## 2328                           Chile   year67    8997325
    ## 2329                           Chile   year68    9188822
    ## 2330                           Chile   year69    9378243
    ## 2331                           Chile   year70    9563865
    ## 2332                           Chile   year71    9745189
    ## 2333                           Chile   year72    9922558
    ## 2334                           Chile   year73   10096295
    ## 2335                           Chile   year74   10267056
    ## 2336                           Chile   year75   10435534
    ## 2337                           Chile   year76   10601836
    ## 2338                           Chile   year77   10766419
    ## 2339                           Chile   year78   10930783
    ## 2340                           Chile   year79   11096868
    ## 2341                           Chile   year80   11266226
    ## 2342                           Chile   year81   11439144
    ## 2343                           Chile   year82   11615836
    ## 2344                           Chile   year83   11797534
    ## 2345                           Chile   year84   11985658
    ## 2346                           Chile   year85   12181028
    ## 2347                           Chile   year86   12384108
    ## 2348                           Chile   year87   12594145
    ## 2349                           Chile   year88   12809025
    ## 2350                           Chile   year89   13025797
    ## 2351                           Chile   year90   13242132
    ## 2352                           Chile   year91   13457244
    ## 2353                           Chile   year92   13671033
    ## 2354                           Chile   year93   13882668
    ## 2355                           Chile   year94   14091389
    ## 2356                           Chile   year95   14296613
    ## 2357                           Chile   year96   14497826
    ## 2358                           Chile   year97   14694835
    ## 2359                           Chile   year98   14887756
    ## 2360                           Chile   year99   15076952
    ## 2361                           Chile year2000   15262754
    ## 2362                           Chile year2001   15444969
    ## 2363                           Chile year2002   15623635
    ## 2364                           Chile year2003   15799542
    ## 2365                           Chile year2004   15973778
    ## 2366                           Chile year2005   16147064
    ## 2367                           Chile year2006   16319792
    ## 2368                           Chile year2007   16491687
    ## 2369                           Chile year2008   16661942
    ## 2370                           Chile year2009   16829442
    ## 2371                           Chile year2010   16993354
    ## 2372                           Chile year2011   17153357
    ## 2373                           Chile year2012   17309746
    ## 2374                           Chile year2013   17462982
    ## 2375                           Chile year2014   17613798
    ## 2376                           Chile year2015   17762681
    ## 2377                           Chile year2016   17909754
    ## 2378                           Chile year2017   18054726
    ## 2379                           China   year60  667070000
    ## 2380                           China   year61  660330000
    ## 2381                           China   year62  665770000
    ## 2382                           China   year63  682335000
    ## 2383                           China   year64  698355000
    ## 2384                           China   year65  715185000
    ## 2385                           China   year66  735400000
    ## 2386                           China   year67  754550000
    ## 2387                           China   year68  774510000
    ## 2388                           China   year69  796025000
    ## 2389                           China   year70  818315000
    ## 2390                           China   year71  841105000
    ## 2391                           China   year72  862030000
    ## 2392                           China   year73  881940000
    ## 2393                           China   year74  900350000
    ## 2394                           China   year75  916395000
    ## 2395                           China   year76  930685000
    ## 2396                           China   year77  943455000
    ## 2397                           China   year78  956165000
    ## 2398                           China   year79  969005000
    ## 2399                           China   year80  981235000
    ## 2400                           China   year81  993885000
    ## 2401                           China   year82 1008630000
    ## 2402                           China   year83 1023310000
    ## 2403                           China   year84 1036825000
    ## 2404                           China   year85 1051040000
    ## 2405                           China   year86 1066790000
    ## 2406                           China   year87 1084035000
    ## 2407                           China   year88 1101630000
    ## 2408                           China   year89 1118650000
    ## 2409                           China   year90 1135185000
    ## 2410                           China   year91 1150780000
    ## 2411                           China   year92 1164970000
    ## 2412                           China   year93 1178440000
    ## 2413                           China   year94 1191835000
    ## 2414                           China   year95 1204855000
    ## 2415                           China   year96 1217550000
    ## 2416                           China   year97 1230075000
    ## 2417                           China   year98 1241935000
    ## 2418                           China   year99 1252735000
    ## 2419                           China year2000 1262645000
    ## 2420                           China year2001 1271850000
    ## 2421                           China year2002 1280400000
    ## 2422                           China year2003 1288400000
    ## 2423                           China year2004 1296075000
    ## 2424                           China year2005 1303720000
    ## 2425                           China year2006 1311020000
    ## 2426                           China year2007 1317885000
    ## 2427                           China year2008 1324655000
    ## 2428                           China year2009 1331260000
    ## 2429                           China year2010 1337705000
    ## 2430                           China year2011 1344130000
    ## 2431                           China year2012 1350695000
    ## 2432                           China year2013 1357380000
    ## 2433                           China year2014 1364270000
    ## 2434                           China year2015 1371220000
    ## 2435                           China year2016 1378665000
    ## 2436                           China year2017 1386395000
    ## 2437                        Colombia   year60   16480383
    ## 2438                        Colombia   year61   16982315
    ## 2439                        Colombia   year62   17500171
    ## 2440                        Colombia   year63   18033550
    ## 2441                        Colombia   year64   18581974
    ## 2442                        Colombia   year65   19144223
    ## 2443                        Colombia   year66   19721462
    ## 2444                        Colombia   year67   20311371
    ## 2445                        Colombia   year68   20905059
    ## 2446                        Colombia   year69   21490945
    ## 2447                        Colombia   year70   22061215
    ## 2448                        Colombia   year71   22611986
    ## 2449                        Colombia   year72   23146803
    ## 2450                        Colombia   year73   23674504
    ## 2451                        Colombia   year74   24208021
    ## 2452                        Colombia   year75   24756973
    ## 2453                        Colombia   year76   25323406
    ## 2454                        Colombia   year77   25905127
    ## 2455                        Colombia   year78   26502166
    ## 2456                        Colombia   year79   27113512
    ## 2457                        Colombia   year80   27737900
    ## 2458                        Colombia   year81   28375991
    ## 2459                        Colombia   year82   29027162
    ## 2460                        Colombia   year83   29687094
    ## 2461                        Colombia   year84   30350086
    ## 2462                        Colombia   year85   31011688
    ## 2463                        Colombia   year86   31669776
    ## 2464                        Colombia   year87   32324325
    ## 2465                        Colombia   year88   32975535
    ## 2466                        Colombia   year89   33624444
    ## 2467                        Colombia   year90   34271565
    ## 2468                        Colombia   year91   34916766
    ## 2469                        Colombia   year92   35558682
    ## 2470                        Colombia   year93   36195168
    ## 2471                        Colombia   year94   36823537
    ## 2472                        Colombia   year95   37441977
    ## 2473                        Colombia   year96   38049038
    ## 2474                        Colombia   year97   38645411
    ## 2475                        Colombia   year98   39234062
    ## 2476                        Colombia   year99   39819279
    ## 2477                        Colombia year2000   40403958
    ## 2478                        Colombia year2001   40988909
    ## 2479                        Colombia year2002   41572491
    ## 2480                        Colombia year2003   42152151
    ## 2481                        Colombia year2004   42724163
    ## 2482                        Colombia year2005   43285634
    ## 2483                        Colombia year2006   43835722
    ## 2484                        Colombia year2007   44374572
    ## 2485                        Colombia year2008   44901544
    ## 2486                        Colombia year2009   45416181
    ## 2487                        Colombia year2010   45918097
    ## 2488                        Colombia year2011   46406646
    ## 2489                        Colombia year2012   46881475
    ## 2490                        Colombia year2013   47342981
    ## 2491                        Colombia year2014   47791911
    ## 2492                        Colombia year2015   48228697
    ## 2493                        Colombia year2016   48653419
    ## 2494                        Colombia year2017   49065615
    ## 2495                         Comoros   year60     191121
    ## 2496                         Comoros   year61     194139
    ## 2497                         Comoros   year62     197198
    ## 2498                         Comoros   year63     200372
    ## 2499                         Comoros   year64     203753
    ## 2500                         Comoros   year65     207424
    ## 2501                         Comoros   year66     211478
    ## 2502                         Comoros   year67     215897
    ## 2503                         Comoros   year68     220575
    ## 2504                         Comoros   year69     225325
    ## 2505                         Comoros   year70     230054
    ## 2506                         Comoros   year71     234644
    ## 2507                         Comoros   year72     239235
    ## 2508                         Comoros   year73     244208
    ## 2509                         Comoros   year74     250104
    ## 2510                         Comoros   year75     257290
    ## 2511                         Comoros   year76     265953
    ## 2512                         Comoros   year77     275900
    ## 2513                         Comoros   year78     286634
    ## 2514                         Comoros   year79     297447
    ## 2515                         Comoros   year80     307829
    ## 2516                         Comoros   year81     317606
    ## 2517                         Comoros   year82     326946
    ## 2518                         Comoros   year83     336096
    ## 2519                         Comoros   year84     345466
    ## 2520                         Comoros   year85     355337
    ## 2521                         Comoros   year86     365760
    ## 2522                         Comoros   year87     376654
    ## 2523                         Comoros   year88     387963
    ## 2524                         Comoros   year89     399632
    ## 2525                         Comoros   year90     411594
    ## 2526                         Comoros   year91     423872
    ## 2527                         Comoros   year92     436448
    ## 2528                         Comoros   year93     449274
    ## 2529                         Comoros   year94     462277
    ## 2530                         Comoros   year95     475394
    ## 2531                         Comoros   year96     488627
    ## 2532                         Comoros   year97     501953
    ## 2533                         Comoros   year98     515385
    ## 2534                         Comoros   year99     528848
    ## 2535                         Comoros year2000     542357
    ## 2536                         Comoros year2001     555888
    ## 2537                         Comoros year2002     569479
    ## 2538                         Comoros year2003     583211
    ## 2539                         Comoros year2004     597228
    ## 2540                         Comoros year2005     611627
    ## 2541                         Comoros year2006     626425
    ## 2542                         Comoros year2007     641620
    ## 2543                         Comoros year2008     657229
    ## 2544                         Comoros year2009     673252
    ## 2545                         Comoros year2010     689692
    ## 2546                         Comoros year2011     706569
    ## 2547                         Comoros year2012     723868
    ## 2548                         Comoros year2013     741500
    ## 2549                         Comoros year2014     759385
    ## 2550                         Comoros year2015     777424
    ## 2551                         Comoros year2016     795601
    ## 2552                         Comoros year2017     813912
    ## 2553                Congo, Dem. Rep.   year60   15248251
    ## 2554                Congo, Dem. Rep.   year61   15637733
    ## 2555                Congo, Dem. Rep.   year62   16041263
    ## 2556                Congo, Dem. Rep.   year63   16461930
    ## 2557                Congo, Dem. Rep.   year64   16903923
    ## 2558                Congo, Dem. Rep.   year65   17369883
    ## 2559                Congo, Dem. Rep.   year66   17861881
    ## 2560                Congo, Dem. Rep.   year67   18378214
    ## 2561                Congo, Dem. Rep.   year68   18913203
    ## 2562                Congo, Dem. Rep.   year69   19458904
    ## 2563                Congo, Dem. Rep.   year70   20009935
    ## 2564                Congo, Dem. Rep.   year71   20562865
    ## 2565                Congo, Dem. Rep.   year72   21120140
    ## 2566                Congo, Dem. Rep.   year73   21689239
    ## 2567                Congo, Dem. Rep.   year74   22280923
    ## 2568                Congo, Dem. Rep.   year75   22902319
    ## 2569                Congo, Dem. Rep.   year76   23559071
    ## 2570                Congo, Dem. Rep.   year77   24247550
    ## 2571                Congo, Dem. Rep.   year78   24954655
    ## 2572                Congo, Dem. Rep.   year79   25661884
    ## 2573                Congo, Dem. Rep.   year80   26357462
    ## 2574                Congo, Dem. Rep.   year81   27039468
    ## 2575                Congo, Dem. Rep.   year82   27717337
    ## 2576                Congo, Dem. Rep.   year83   28404876
    ## 2577                Congo, Dem. Rep.   year84   29121474
    ## 2578                Congo, Dem. Rep.   year85   29883446
    ## 2579                Congo, Dem. Rep.   year86   30685824
    ## 2580                Congo, Dem. Rep.   year87   31529823
    ## 2581                Congo, Dem. Rep.   year88   32444156
    ## 2582                Congo, Dem. Rep.   year89   33465441
    ## 2583                Congo, Dem. Rep.   year90   34614581
    ## 2584                Congo, Dem. Rep.   year91   35914825
    ## 2585                Congo, Dem. Rep.   year92   37346147
    ## 2586                Congo, Dem. Rep.   year93   38833595
    ## 2587                Congo, Dem. Rep.   year94   40273701
    ## 2588                Congo, Dem. Rep.   year95   41595744
    ## 2589                Congo, Dem. Rep.   year96   42770544
    ## 2590                Congo, Dem. Rep.   year97   43830146
    ## 2591                Congo, Dem. Rep.   year98   44840529
    ## 2592                Congo, Dem. Rep.   year99   45898667
    ## 2593                Congo, Dem. Rep. year2000   47076387
    ## 2594                Congo, Dem. Rep. year2001   48394338
    ## 2595                Congo, Dem. Rep. year2002   49835756
    ## 2596                Congo, Dem. Rep. year2003   51390033
    ## 2597                Congo, Dem. Rep. year2004   53034217
    ## 2598                Congo, Dem. Rep. year2005   54751476
    ## 2599                Congo, Dem. Rep. year2006   56543011
    ## 2600                Congo, Dem. Rep. year2007   58417562
    ## 2601                Congo, Dem. Rep. year2008   60373608
    ## 2602                Congo, Dem. Rep. year2009   62409435
    ## 2603                Congo, Dem. Rep. year2010   64523263
    ## 2604                Congo, Dem. Rep. year2011   66713597
    ## 2605                Congo, Dem. Rep. year2012   68978682
    ## 2606                Congo, Dem. Rep. year2013   71316033
    ## 2607                Congo, Dem. Rep. year2014   73722860
    ## 2608                Congo, Dem. Rep. year2015   76196619
    ## 2609                Congo, Dem. Rep. year2016   78736153
    ## 2610                Congo, Dem. Rep. year2017   81339988
    ## 2611                     Congo, Rep.   year60    1037220
    ## 2612                     Congo, Rep.   year61    1064111
    ## 2613                     Congo, Rep.   year62    1092292
    ## 2614                     Congo, Rep.   year63    1121735
    ## 2615                     Congo, Rep.   year64    1152412
    ## 2616                     Congo, Rep.   year65    1184316
    ## 2617                     Congo, Rep.   year66    1217391
    ## 2618                     Congo, Rep.   year67    1251703
    ## 2619                     Congo, Rep.   year68    1287516
    ## 2620                     Congo, Rep.   year69    1325147
    ## 2621                     Congo, Rep.   year70    1364812
    ## 2622                     Congo, Rep.   year71    1406643
    ## 2623                     Congo, Rep.   year72    1450518
    ## 2624                     Congo, Rep.   year73    1496047
    ## 2625                     Congo, Rep.   year74    1542690
    ## 2626                     Congo, Rep.   year75    1590039
    ## 2627                     Congo, Rep.   year76    1637941
    ## 2628                     Congo, Rep.   year77    1686524
    ## 2629                     Congo, Rep.   year78    1736099
    ## 2630                     Congo, Rep.   year79    1787129
    ## 2631                     Congo, Rep.   year80    1839935
    ## 2632                     Congo, Rep.   year81    1894676
    ## 2633                     Congo, Rep.   year82    1951195
    ## 2634                     Congo, Rep.   year83    2009165
    ## 2635                     Congo, Rep.   year84    2068132
    ## 2636                     Congo, Rep.   year85    2127770
    ## 2637                     Congo, Rep.   year86    2188046
    ## 2638                     Congo, Rep.   year87    2249146
    ## 2639                     Congo, Rep.   year88    2311348
    ## 2640                     Congo, Rep.   year89    2375008
    ## 2641                     Congo, Rep.   year90    2440457
    ## 2642                     Congo, Rep.   year91    2507772
    ## 2643                     Congo, Rep.   year92    2577035
    ## 2644                     Congo, Rep.   year93    2648507
    ## 2645                     Congo, Rep.   year94    2722497
    ## 2646                     Congo, Rep.   year95    2799255
    ## 2647                     Congo, Rep.   year96    2879222
    ## 2648                     Congo, Rep.   year97    2962470
    ## 2649                     Congo, Rep.   year98    3048453
    ## 2650                     Congo, Rep.   year99    3136344
    ## 2651                     Congo, Rep. year2000    3225727
    ## 2652                     Congo, Rep. year2001    3315806
    ## 2653                     Congo, Rep. year2002    3407180
    ## 2654                     Congo, Rep. year2003    3502519
    ## 2655                     Congo, Rep. year2004    3605439
    ## 2656                     Congo, Rep. year2005    3718243
    ## 2657                     Congo, Rep. year2006    3842365
    ## 2658                     Congo, Rep. year2007    3976246
    ## 2659                     Congo, Rep. year2008    4115435
    ## 2660                     Congo, Rep. year2009    4253712
    ## 2661                     Congo, Rep. year2010    4386693
    ## 2662                     Congo, Rep. year2011    4512730
    ## 2663                     Congo, Rep. year2012    4633363
    ## 2664                     Congo, Rep. year2013    4751393
    ## 2665                     Congo, Rep. year2014    4871101
    ## 2666                     Congo, Rep. year2015    4995648
    ## 2667                     Congo, Rep. year2016    5125821
    ## 2668                     Congo, Rep. year2017    5260750
    ## 2669                      Costa Rica   year60    1333040
    ## 2670                      Costa Rica   year61    1381917
    ## 2671                      Costa Rica   year62    1432585
    ## 2672                      Costa Rica   year63    1484510
    ## 2673                      Costa Rica   year64    1537041
    ## 2674                      Costa Rica   year65    1589621
    ## 2675                      Costa Rica   year66    1642186
    ## 2676                      Costa Rica   year67    1694710
    ## 2677                      Costa Rica   year68    1746869
    ## 2678                      Costa Rica   year69    1798311
    ## 2679                      Costa Rica   year70    1848866
    ## 2680                      Costa Rica   year71    1898360
    ## 2681                      Costa Rica   year72    1947048
    ## 2682                      Costa Rica   year73    1995743
    ## 2683                      Costa Rica   year74    2045580
    ## 2684                      Costa Rica   year75    2097407
    ## 2685                      Costa Rica   year76    2151497
    ## 2686                      Costa Rica   year77    2207725
    ## 2687                      Costa Rica   year78    2266154
    ## 2688                      Costa Rica   year79    2326704
    ## 2689                      Costa Rica   year80    2389310
    ## 2690                      Costa Rica   year81    2454129
    ## 2691                      Costa Rica   year82    2521168
    ## 2692                      Costa Rica   year83    2589930
    ## 2693                      Costa Rica   year84    2659781
    ## 2694                      Costa Rica   year85    2730233
    ## 2695                      Costa Rica   year86    2800986
    ## 2696                      Costa Rica   year87    2872211
    ## 2697                      Costa Rica   year88    2944557
    ## 2698                      Costa Rica   year89    3018955
    ## 2699                      Costa Rica   year90    3095995
    ## 2700                      Costa Rica   year91    3175649
    ## 2701                      Costa Rica   year92    3257466
    ## 2702                      Costa Rica   year93    3341004
    ## 2703                      Costa Rica   year94    3425690
    ## 2704                      Costa Rica   year95    3510926
    ## 2705                      Costa Rica   year96    3596732
    ## 2706                      Costa Rica   year97    3682725
    ## 2707                      Costa Rica   year98    3767373
    ## 2708                      Costa Rica   year99    3848723
    ## 2709                      Costa Rica year2000    3925443
    ## 2710                      Costa Rica year2001    3996798
    ## 2711                      Costa Rica year2002    4063204
    ## 2712                      Costa Rica year2003    4125971
    ## 2713                      Costa Rica year2004    4187038
    ## 2714                      Costa Rica year2005    4247841
    ## 2715                      Costa Rica year2006    4308794
    ## 2716                      Costa Rica year2007    4369469
    ## 2717                      Costa Rica year2008    4429508
    ## 2718                      Costa Rica year2009    4488263
    ## 2719                      Costa Rica year2010    4545280
    ## 2720                      Costa Rica year2011    4600474
    ## 2721                      Costa Rica year2012    4654122
    ## 2722                      Costa Rica year2013    4706401
    ## 2723                      Costa Rica year2014    4757575
    ## 2724                      Costa Rica year2015    4807852
    ## 2725                      Costa Rica year2016    4857274
    ## 2726                      Costa Rica year2017    4905769
    ## 2727                   Cote d'Ivoire   year60    3558988
    ## 2728                   Cote d'Ivoire   year61    3694205
    ## 2729                   Cote d'Ivoire   year62    3841071
    ## 2730                   Cote d'Ivoire   year63    3996941
    ## 2731                   Cote d'Ivoire   year64    4157965
    ## 2732                   Cote d'Ivoire   year65    4321791
    ## 2733                   Cote d'Ivoire   year66    4487204
    ## 2734                   Cote d'Ivoire   year67    4656353
    ## 2735                   Cote d'Ivoire   year68    4834279
    ## 2736                   Cote d'Ivoire   year69    5027971
    ## 2737                   Cote d'Ivoire   year70    5242395
    ## 2738                   Cote d'Ivoire   year71    5479338
    ## 2739                   Cote d'Ivoire   year72    5737281
    ## 2740                   Cote d'Ivoire   year73    6013862
    ## 2741                   Cote d'Ivoire   year74    6305287
    ## 2742                   Cote d'Ivoire   year75    6608609
    ## 2743                   Cote d'Ivoire   year76    6922982
    ## 2744                   Cote d'Ivoire   year77    7248828
    ## 2745                   Cote d'Ivoire   year78    7585914
    ## 2746                   Cote d'Ivoire   year79    7934279
    ## 2747                   Cote d'Ivoire   year80    8293675
    ## 2748                   Cote d'Ivoire   year81    8664057
    ## 2749                   Cote d'Ivoire   year82    9044473
    ## 2750                   Cote d'Ivoire   year83    9432731
    ## 2751                   Cote d'Ivoire   year84    9826055
    ## 2752                   Cote d'Ivoire   year85   10222558
    ## 2753                   Cote d'Ivoire   year86   10620267
    ## 2754                   Cote d'Ivoire   year87   11019651
    ## 2755                   Cote d'Ivoire   year88   11424260
    ## 2756                   Cote d'Ivoire   year89   11839243
    ## 2757                   Cote d'Ivoire   year90   12267754
    ## 2758                   Cote d'Ivoire   year91   12710008
    ## 2759                   Cote d'Ivoire   year92   13163019
    ## 2760                   Cote d'Ivoire   year93   13622731
    ## 2761                   Cote d'Ivoire   year94   14083611
    ## 2762                   Cote d'Ivoire   year95   14540820
    ## 2763                   Cote d'Ivoire   year96   14995249
    ## 2764                   Cote d'Ivoire   year97   15445986
    ## 2765                   Cote d'Ivoire   year98   15884552
    ## 2766                   Cote d'Ivoire   year99   16300233
    ## 2767                   Cote d'Ivoire year2000   16686561
    ## 2768                   Cote d'Ivoire year2001   17040152
    ## 2769                   Cote d'Ivoire year2002   17366517
    ## 2770                   Cote d'Ivoire year2003   17679355
    ## 2771                   Cote d'Ivoire year2004   17997738
    ## 2772                   Cote d'Ivoire year2005   18336303
    ## 2773                   Cote d'Ivoire year2006   18699435
    ## 2774                   Cote d'Ivoire year2007   19085941
    ## 2775                   Cote d'Ivoire year2008   19497986
    ## 2776                   Cote d'Ivoire year2009   19936366
    ## 2777                   Cote d'Ivoire year2010   20401331
    ## 2778                   Cote d'Ivoire year2011   20895311
    ## 2779                   Cote d'Ivoire year2012   21418603
    ## 2780                   Cote d'Ivoire year2013   21966312
    ## 2781                   Cote d'Ivoire year2014   22531350
    ## 2782                   Cote d'Ivoire year2015   23108472
    ## 2783                   Cote d'Ivoire year2016   23695919
    ## 2784                   Cote d'Ivoire year2017   24294750
    ## 2785                         Croatia   year60    4140000
    ## 2786                         Croatia   year61    4171672
    ## 2787                         Croatia   year62    4202104
    ## 2788                         Croatia   year63    4231408
    ## 2789                         Croatia   year64    4259680
    ## 2790                         Croatia   year65    4287000
    ## 2791                         Croatia   year66    4313000
    ## 2792                         Croatia   year67    4339000
    ## 2793                         Croatia   year68    4364000
    ## 2794                         Croatia   year69    4387000
    ## 2795                         Croatia   year70    4411000
    ## 2796                         Croatia   year71    4435000
    ## 2797                         Croatia   year72    4457000
    ## 2798                         Croatia   year73    4478000
    ## 2799                         Croatia   year74    4497000
    ## 2800                         Croatia   year75    4514000
    ## 2801                         Croatia   year76    4530000
    ## 2802                         Croatia   year77    4532000
    ## 2803                         Croatia   year78    4556000
    ## 2804                         Croatia   year79    4571000
    ## 2805                         Croatia   year80    4588000
    ## 2806                         Croatia   year81    4608000
    ## 2807                         Croatia   year82    4635000
    ## 2808                         Croatia   year83    4659000
    ## 2809                         Croatia   year84    4680000
    ## 2810                         Croatia   year85    4701000
    ## 2811                         Croatia   year86    4722000
    ## 2812                         Croatia   year87    4740000
    ## 2813                         Croatia   year88    4757000
    ## 2814                         Croatia   year89    4767000
    ## 2815                         Croatia   year90    4780000
    ## 2816                         Croatia   year91    4510000
    ## 2817                         Croatia   year92    4470000
    ## 2818                         Croatia   year93    4640000
    ## 2819                         Croatia   year94    4650000
    ## 2820                         Croatia   year95    4669000
    ## 2821                         Croatia   year96    4494000
    ## 2822                         Croatia   year97    4572000
    ## 2823                         Croatia   year98    4501000
    ## 2824                         Croatia   year99    4554000
    ## 2825                         Croatia year2000    4426000
    ## 2826                         Croatia year2001    4440000
    ## 2827                         Croatia year2002    4440000
    ## 2828                         Croatia year2003    4440000
    ## 2829                         Croatia year2004    4439000
    ## 2830                         Croatia year2005    4442000
    ## 2831                         Croatia year2006    4440000
    ## 2832                         Croatia year2007    4436000
    ## 2833                         Croatia year2008    4434508
    ## 2834                         Croatia year2009    4429078
    ## 2835                         Croatia year2010    4417781
    ## 2836                         Croatia year2011    4280622
    ## 2837                         Croatia year2012    4267558
    ## 2838                         Croatia year2013    4255689
    ## 2839                         Croatia year2014    4238389
    ## 2840                         Croatia year2015    4203604
    ## 2841                         Croatia year2016    4174349
    ## 2842                         Croatia year2017    4125700
    ## 2843                            Cuba   year60    7141135
    ## 2844                            Cuba   year61    7289826
    ## 2845                            Cuba   year62    7450402
    ## 2846                            Cuba   year63    7618354
    ## 2847                            Cuba   year64    7787146
    ## 2848                            Cuba   year65    7951933
    ## 2849                            Cuba   year66    8110430
    ## 2850                            Cuba   year67    8263546
    ## 2851                            Cuba   year68    8413327
    ## 2852                            Cuba   year69    8563193
    ## 2853                            Cuba   year70    8715123
    ## 2854                            Cuba   year71    8869961
    ## 2855                            Cuba   year72    9025300
    ## 2856                            Cuba   year73    9176052
    ## 2857                            Cuba   year74    9315373
    ## 2858                            Cuba   year75    9438442
    ## 2859                            Cuba   year76    9544271
    ## 2860                            Cuba   year77    9634680
    ## 2861                            Cuba   year78    9711392
    ## 2862                            Cuba   year79    9777290
    ## 2863                            Cuba   year80    9835177
    ## 2864                            Cuba   year81    9884213
    ## 2865                            Cuba   year82    9925623
    ## 2866                            Cuba   year83    9966733
    ## 2867                            Cuba   year84   10017059
    ## 2868                            Cuba   year85   10082989
    ## 2869                            Cuba   year86   10168087
    ## 2870                            Cuba   year87   10269567
    ## 2871                            Cuba   year88   10379548
    ## 2872                            Cuba   year89   10486509
    ## 2873                            Cuba   year90   10582081
    ## 2874                            Cuba   year91   10663585
    ## 2875                            Cuba   year92   10733363
    ## 2876                            Cuba   year93   10794135
    ## 2877                            Cuba   year94   10850585
    ## 2878                            Cuba   year95   10906043
    ## 2879                            Cuba   year96   10961012
    ## 2880                            Cuba   year97   11013983
    ## 2881                            Cuba   year98   11064097
    ## 2882                            Cuba   year99   11110004
    ## 2883                            Cuba year2000   11150736
    ## 2884                            Cuba year2001   11186542
    ## 2885                            Cuba year2002   11217998
    ## 2886                            Cuba year2003   11244885
    ## 2887                            Cuba year2004   11266941
    ## 2888                            Cuba year2005   11284253
    ## 2889                            Cuba year2006   11296233
    ## 2890                            Cuba year2007   11303687
    ## 2891                            Cuba year2008   11309754
    ## 2892                            Cuba year2009   11318602
    ## 2893                            Cuba year2010   11333051
    ## 2894                            Cuba year2011   11354651
    ## 2895                            Cuba year2012   11382146
    ## 2896                            Cuba year2013   11412167
    ## 2897                            Cuba year2014   11439767
    ## 2898                            Cuba year2015   11461432
    ## 2899                            Cuba year2016   11475982
    ## 2900                            Cuba year2017   11484636
    ## 2901                         Curacao   year60     124826
    ## 2902                         Curacao   year61     126125
    ## 2903                         Curacao   year62     128414
    ## 2904                         Curacao   year63     130860
    ## 2905                         Curacao   year64     133148
    ## 2906                         Curacao   year65     135266
    ## 2907                         Curacao   year66     136682
    ## 2908                         Curacao   year67     138140
    ## 2909                         Curacao   year68     140298
    ## 2910                         Curacao   year69     142581
    ## 2911                         Curacao   year70     144739
    ## 2912                         Curacao   year71     147389
    ## 2913                         Curacao   year72     147710
    ## 2914                         Curacao   year73     146912
    ## 2915                         Curacao   year74     148351
    ## 2916                         Curacao   year75     149129
    ## 2917                         Curacao   year76     149399
    ## 2918                         Curacao   year77     149459
    ## 2919                         Curacao   year78     148341
    ## 2920                         Curacao   year79     147851
    ## 2921                         Curacao   year80     148041
    ## 2922                         Curacao   year81     148629
    ## 2923                         Curacao   year82     150101
    ## 2924                         Curacao   year83     151159
    ## 2925                         Curacao   year84     151940
    ## 2926                         Curacao   year85     152711
    ## 2927                         Curacao   year86     152662
    ## 2928                         Curacao   year87     151456
    ## 2929                         Curacao   year88     149254
    ## 2930                         Curacao   year89     146937
    ## 2931                         Curacao   year90     145400
    ## 2932                         Curacao   year91     144403
    ## 2933                         Curacao   year92     143912
    ## 2934                         Curacao   year93     144299
    ## 2935                         Curacao   year94     144630
    ## 2936                         Curacao   year95     145139
    ## 2937                         Curacao   year96     146306
    ## 2938                         Curacao   year97     146956
    ## 2939                         Curacao   year98     144472
    ## 2940                         Curacao   year99     139428
    ## 2941                         Curacao year2000     133860
    ## 2942                         Curacao year2001     129047
    ## 2943                         Curacao year2002     129205
    ## 2944                         Curacao year2003     131897
    ## 2945                         Curacao year2004     134192
    ## 2946                         Curacao year2005     137658
    ## 2947                         Curacao year2006     141239
    ## 2948                         Curacao year2007     144056
    ## 2949                         Curacao year2008     145880
    ## 2950                         Curacao year2009     146833
    ## 2951                         Curacao year2010     148703
    ## 2952                         Curacao year2011     150831
    ## 2953                         Curacao year2012     152088
    ## 2954                         Curacao year2013     153822
    ## 2955                         Curacao year2014     155909
    ## 2956                         Curacao year2015     157980
    ## 2957                         Curacao year2016     159663
    ## 2958                         Curacao year2017     161014
    ## 2959                          Cyprus   year60     572930
    ## 2960                          Cyprus   year61     576395
    ## 2961                          Cyprus   year62     577691
    ## 2962                          Cyprus   year63     577913
    ## 2963                          Cyprus   year64     578627
    ## 2964                          Cyprus   year65     580966
    ## 2965                          Cyprus   year66     585308
    ## 2966                          Cyprus   year67     591308
    ## 2967                          Cyprus   year68     598493
    ## 2968                          Cyprus   year69     606113
    ## 2969                          Cyprus   year70     613621
    ## 2970                          Cyprus   year71     620859
    ## 2971                          Cyprus   year72     628002
    ## 2972                          Cyprus   year73     635111
    ## 2973                          Cyprus   year74     642339
    ## 2974                          Cyprus   year75     649755
    ## 2975                          Cyprus   year76     657534
    ## 2976                          Cyprus   year77     665528
    ## 2977                          Cyprus   year78     673251
    ## 2978                          Cyprus   year79     680011
    ## 2979                          Cyprus   year80     685406
    ## 2980                          Cyprus   year81     689173
    ## 2981                          Cyprus   year82     691702
    ## 2982                          Cyprus   year83     694077
    ## 2983                          Cyprus   year84     697717
    ## 2984                          Cyprus   year85     703687
    ## 2985                          Cyprus   year86     712341
    ## 2986                          Cyprus   year87     723380
    ## 2987                          Cyprus   year88     736479
    ## 2988                          Cyprus   year89     751044
    ## 2989                          Cyprus   year90     766614
    ## 2990                          Cyprus   year91     783129
    ## 2991                          Cyprus   year92     800609
    ## 2992                          Cyprus   year93     818751
    ## 2993                          Cyprus   year94     837110
    ## 2994                          Cyprus   year95     855384
    ## 2995                          Cyprus   year96     873423
    ## 2996                          Cyprus   year97     891192
    ## 2997                          Cyprus   year98     908704
    ## 2998                          Cyprus   year99     926050
    ## 2999                          Cyprus year2000     943286
    ## 3000                          Cyprus year2001     960282
    ## 3001                          Cyprus year2002     976966
    ## 3002                          Cyprus year2003     993563
    ## 3003                          Cyprus year2004    1010410
    ## 3004                          Cyprus year2005    1027658
    ## 3005                          Cyprus year2006    1045509
    ## 3006                          Cyprus year2007    1063712
    ## 3007                          Cyprus year2008    1081563
    ## 3008                          Cyprus year2009    1098076
    ## 3009                          Cyprus year2010    1112607
    ## 3010                          Cyprus year2011    1124835
    ## 3011                          Cyprus year2012    1135062
    ## 3012                          Cyprus year2013    1143896
    ## 3013                          Cyprus year2014    1152309
    ## 3014                          Cyprus year2015    1160985
    ## 3015                          Cyprus year2016    1170125
    ## 3016                          Cyprus year2017    1179551
    ## 3017                  Czech Republic   year60    9602006
    ## 3018                  Czech Republic   year61    9586651
    ## 3019                  Czech Republic   year62    9624660
    ## 3020                  Czech Republic   year63    9670685
    ## 3021                  Czech Republic   year64    9727804
    ## 3022                  Czech Republic   year65    9779358
    ## 3023                  Czech Republic   year66    9821040
    ## 3024                  Czech Republic   year67    9852899
    ## 3025                  Czech Republic   year68    9876346
    ## 3026                  Czech Republic   year69    9896580
    ## 3027                  Czech Republic   year70    9858071
    ## 3028                  Czech Republic   year71    9826815
    ## 3029                  Czech Republic   year72    9867632
    ## 3030                  Czech Republic   year73    9922266
    ## 3031                  Czech Republic   year74    9988459
    ## 3032                  Czech Republic   year75   10058620
    ## 3033                  Czech Republic   year76   10125939
    ## 3034                  Czech Republic   year77   10186755
    ## 3035                  Czech Republic   year78   10242098
    ## 3036                  Czech Republic   year79   10292341
    ## 3037                  Czech Republic   year80   10304193
    ## 3038                  Czech Republic   year81   10300591
    ## 3039                  Czech Republic   year82   10314826
    ## 3040                  Czech Republic   year83   10323856
    ## 3041                  Czech Republic   year84   10330213
    ## 3042                  Czech Republic   year85   10337118
    ## 3043                  Czech Republic   year86   10342227
    ## 3044                  Czech Republic   year87   10347318
    ## 3045                  Czech Republic   year88   10355276
    ## 3046                  Czech Republic   year89   10361068
    ## 3047                  Czech Republic   year90   10333355
    ## 3048                  Czech Republic   year91   10308578
    ## 3049                  Czech Republic   year92   10319123
    ## 3050                  Czech Republic   year93   10329855
    ## 3051                  Czech Republic   year94   10333587
    ## 3052                  Czech Republic   year95   10327253
    ## 3053                  Czech Republic   year96   10315241
    ## 3054                  Czech Republic   year97   10304131
    ## 3055                  Czech Republic   year98   10294373
    ## 3056                  Czech Republic   year99   10283860
    ## 3057                  Czech Republic year2000   10255063
    ## 3058                  Czech Republic year2001   10216605
    ## 3059                  Czech Republic year2002   10196916
    ## 3060                  Czech Republic year2003   10193998
    ## 3061                  Czech Republic year2004   10197101
    ## 3062                  Czech Republic year2005   10211216
    ## 3063                  Czech Republic year2006   10238905
    ## 3064                  Czech Republic year2007   10298828
    ## 3065                  Czech Republic year2008   10384603
    ## 3066                  Czech Republic year2009   10443936
    ## 3067                  Czech Republic year2010   10474410
    ## 3068                  Czech Republic year2011   10496088
    ## 3069                  Czech Republic year2012   10510785
    ## 3070                  Czech Republic year2013   10514272
    ## 3071                  Czech Republic year2014   10525347
    ## 3072                  Czech Republic year2015   10546059
    ## 3073                  Czech Republic year2016   10566332
    ## 3074                  Czech Republic year2017   10591323
    ## 3075                         Denmark   year60    4579603
    ## 3076                         Denmark   year61    4611687
    ## 3077                         Denmark   year62    4647727
    ## 3078                         Denmark   year63    4684483
    ## 3079                         Denmark   year64    4722072
    ## 3080                         Denmark   year65    4759012
    ## 3081                         Denmark   year66    4797381
    ## 3082                         Denmark   year67    4835354
    ## 3083                         Denmark   year68    4864883
    ## 3084                         Denmark   year69    4891860
    ## 3085                         Denmark   year70    4928757
    ## 3086                         Denmark   year71    4963126
    ## 3087                         Denmark   year72    4991596
    ## 3088                         Denmark   year73    5021861
    ## 3089                         Denmark   year74    5045297
    ## 3090                         Denmark   year75    5059862
    ## 3091                         Denmark   year76    5072596
    ## 3092                         Denmark   year77    5088419
    ## 3093                         Denmark   year78    5104248
    ## 3094                         Denmark   year79    5116801
    ## 3095                         Denmark   year80    5123027
    ## 3096                         Denmark   year81    5121572
    ## 3097                         Denmark   year82    5117810
    ## 3098                         Denmark   year83    5114297
    ## 3099                         Denmark   year84    5111619
    ## 3100                         Denmark   year85    5113691
    ## 3101                         Denmark   year86    5120534
    ## 3102                         Denmark   year87    5127024
    ## 3103                         Denmark   year88    5129516
    ## 3104                         Denmark   year89    5132594
    ## 3105                         Denmark   year90    5140939
    ## 3106                         Denmark   year91    5154298
    ## 3107                         Denmark   year92    5171370
    ## 3108                         Denmark   year93    5188628
    ## 3109                         Denmark   year94    5206180
    ## 3110                         Denmark   year95    5233373
    ## 3111                         Denmark   year96    5263074
    ## 3112                         Denmark   year97    5284991
    ## 3113                         Denmark   year98    5304219
    ## 3114                         Denmark   year99    5321799
    ## 3115                         Denmark year2000    5339616
    ## 3116                         Denmark year2001    5358783
    ## 3117                         Denmark year2002    5375931
    ## 3118                         Denmark year2003    5390574
    ## 3119                         Denmark year2004    5404523
    ## 3120                         Denmark year2005    5419432
    ## 3121                         Denmark year2006    5437272
    ## 3122                         Denmark year2007    5461438
    ## 3123                         Denmark year2008    5493621
    ## 3124                         Denmark year2009    5523095
    ## 3125                         Denmark year2010    5547683
    ## 3126                         Denmark year2011    5570572
    ## 3127                         Denmark year2012    5591572
    ## 3128                         Denmark year2013    5614932
    ## 3129                         Denmark year2014    5643475
    ## 3130                         Denmark year2015    5683483
    ## 3131                         Denmark year2016    5728010
    ## 3132                         Denmark year2017    5769603
    ## 3133                        Djibouti   year60      83636
    ## 3134                        Djibouti   year61      88498
    ## 3135                        Djibouti   year62      94204
    ## 3136                        Djibouti   year63     100628
    ## 3137                        Djibouti   year64     107583
    ## 3138                        Djibouti   year65     114963
    ## 3139                        Djibouti   year66     122866
    ## 3140                        Djibouti   year67     131397
    ## 3141                        Djibouti   year68     140462
    ## 3142                        Djibouti   year69     149887
    ## 3143                        Djibouti   year70     159659
    ## 3144                        Djibouti   year71     169372
    ## 3145                        Djibouti   year72     179224
    ## 3146                        Djibouti   year73     190568
    ## 3147                        Djibouti   year74     205181
    ## 3148                        Djibouti   year75     224183
    ## 3149                        Djibouti   year76     248556
    ## 3150                        Djibouti   year77     277479
    ## 3151                        Djibouti   year78     308008
    ## 3152                        Djibouti   year79     336085
    ## 3153                        Djibouti   year80     358960
    ## 3154                        Djibouti   year81     374937
    ## 3155                        Djibouti   year82     385271
    ## 3156                        Djibouti   year83     393802
    ## 3157                        Djibouti   year84     406017
    ## 3158                        Djibouti   year85     425613
    ## 3159                        Djibouti   year86     454361
    ## 3160                        Djibouti   year87     490330
    ## 3161                        Djibouti   year88     528999
    ## 3162                        Djibouti   year89     563864
    ## 3163                        Djibouti   year90     590398
    ## 3164                        Djibouti   year91     606844
    ## 3165                        Djibouti   year92     615054
    ## 3166                        Djibouti   year93     618495
    ## 3167                        Djibouti   year94     622366
    ## 3168                        Djibouti   year95     630388
    ## 3169                        Djibouti   year96     643682
    ## 3170                        Djibouti   year97     660953
    ## 3171                        Djibouti   year98     680612
    ## 3172                        Djibouti   year99     700099
    ## 3173                        Djibouti year2000     717584
    ## 3174                        Djibouti year2001     732711
    ## 3175                        Djibouti year2002     746221
    ## 3176                        Djibouti year2003     758615
    ## 3177                        Djibouti year2004     770752
    ## 3178                        Djibouti year2005     783254
    ## 3179                        Djibouti year2006     796208
    ## 3180                        Djibouti year2007     809402
    ## 3181                        Djibouti year2008     822934
    ## 3182                        Djibouti year2009     836840
    ## 3183                        Djibouti year2010     851146
    ## 3184                        Djibouti year2011     865937
    ## 3185                        Djibouti year2012     881185
    ## 3186                        Djibouti year2013     896688
    ## 3187                        Djibouti year2014     912164
    ## 3188                        Djibouti year2015     927414
    ## 3189                        Djibouti year2016     942333
    ## 3190                        Djibouti year2017     956985
    ## 3191                        Dominica   year60      60011
    ## 3192                        Dominica   year61      61032
    ## 3193                        Dominica   year62      61982
    ## 3194                        Dominica   year63      62918
    ## 3195                        Dominica   year64      63926
    ## 3196                        Dominica   year65      65038
    ## 3197                        Dominica   year66      66311
    ## 3198                        Dominica   year67      67686
    ## 3199                        Dominica   year68      69040
    ## 3200                        Dominica   year69      70213
    ## 3201                        Dominica   year70      71073
    ## 3202                        Dominica   year71      71569
    ## 3203                        Dominica   year72      71734
    ## 3204                        Dominica   year73      71744
    ## 3205                        Dominica   year74      71807
    ## 3206                        Dominica   year75      72094
    ## 3207                        Dominica   year76      72642
    ## 3208                        Dominica   year77      73411
    ## 3209                        Dominica   year78      74242
    ## 3210                        Dominica   year79      74925
    ## 3211                        Dominica   year80      75314
    ## 3212                        Dominica   year81      75375
    ## 3213                        Dominica   year82      75170
    ## 3214                        Dominica   year83      74747
    ## 3215                        Dominica   year84      74213
    ## 3216                        Dominica   year85      73643
    ## 3217                        Dominica   year86      73025
    ## 3218                        Dominica   year87      72370
    ## 3219                        Dominica   year88      71742
    ## 3220                        Dominica   year89      71242
    ## 3221                        Dominica   year90      70926
    ## 3222                        Dominica   year91      70842
    ## 3223                        Dominica   year92      70970
    ## 3224                        Dominica   year93      71210
    ## 3225                        Dominica   year94      71373
    ## 3226                        Dominica   year95      71368
    ## 3227                        Dominica   year96      71145
    ## 3228                        Dominica   year97      70753
    ## 3229                        Dominica   year98      70290
    ## 3230                        Dominica   year99      69903
    ## 3231                        Dominica year2000      69676
    ## 3232                        Dominica year2001      69670
    ## 3233                        Dominica year2002      69824
    ## 3234                        Dominica year2003      70093
    ## 3235                        Dominica year2004      70379
    ## 3236                        Dominica year2005      70627
    ## 3237                        Dominica year2006      70807
    ## 3238                        Dominica year2007      70950
    ## 3239                        Dominica year2008      71074
    ## 3240                        Dominica year2009      71229
    ## 3241                        Dominica year2010      71440
    ## 3242                        Dominica year2011      71718
    ## 3243                        Dominica year2012      72044
    ## 3244                        Dominica year2013      72400
    ## 3245                        Dominica year2014      72778
    ## 3246                        Dominica year2015      73162
    ## 3247                        Dominica year2016      73543
    ## 3248                        Dominica year2017      73925
    ## 3249              Dominican Republic   year60    3294042
    ## 3250              Dominican Republic   year61    3406299
    ## 3251              Dominican Republic   year62    3521278
    ## 3252              Dominican Republic   year63    3638628
    ## 3253              Dominican Republic   year64    3757956
    ## 3254              Dominican Republic   year65    3878948
    ## 3255              Dominican Republic   year66    4001375
    ## 3256              Dominican Republic   year67    4125109
    ## 3257              Dominican Republic   year68    4250025
    ## 3258              Dominican Republic   year69    4376054
    ## 3259              Dominican Republic   year70    4503114
    ## 3260              Dominican Republic   year71    4631114
    ## 3261              Dominican Republic   year72    4759934
    ## 3262              Dominican Republic   year73    4889436
    ## 3263              Dominican Republic   year74    5019473
    ## 3264              Dominican Republic   year75    5149935
    ## 3265              Dominican Republic   year76    5280723
    ## 3266              Dominican Republic   year77    5411865
    ## 3267              Dominican Republic   year78    5543517
    ## 3268              Dominican Republic   year79    5675931
    ## 3269              Dominican Republic   year80    5809269
    ## 3270              Dominican Republic   year81    5943591
    ## 3271              Dominican Republic   year82    6078820
    ## 3272              Dominican Republic   year83    6214857
    ## 3273              Dominican Republic   year84    6351572
    ## 3274              Dominican Republic   year85    6488856
    ## 3275              Dominican Republic   year86    6626542
    ## 3276              Dominican Republic   year87    6764624
    ## 3277              Dominican Republic   year88    6903316
    ## 3278              Dominican Republic   year89    7042937
    ## 3279              Dominican Republic   year90    7183647
    ## 3280              Dominican Republic   year91    7325622
    ## 3281              Dominican Republic   year92    7468551
    ## 3282              Dominican Republic   year93    7611465
    ## 3283              Dominican Republic   year94    7753052
    ## 3284              Dominican Republic   year95    7892423
    ## 3285              Dominican Republic   year96    8029113
    ## 3286              Dominican Republic   year97    8163472
    ## 3287              Dominican Republic   year98    8296375
    ## 3288              Dominican Republic   year99    8429112
    ## 3289              Dominican Republic year2000    8562622
    ## 3290              Dominican Republic year2001    8697126
    ## 3291              Dominican Republic year2002    8832285
    ## 3292              Dominican Republic year2003    8967760
    ## 3293              Dominican Republic year2004    9102998
    ## 3294              Dominican Republic year2005    9237566
    ## 3295              Dominican Republic year2006    9371338
    ## 3296              Dominican Republic year2007    9504353
    ## 3297              Dominican Republic year2008    9636520
    ## 3298              Dominican Republic year2009    9767758
    ## 3299              Dominican Republic year2010    9897985
    ## 3300              Dominican Republic year2011   10027095
    ## 3301              Dominican Republic year2012   10154950
    ## 3302              Dominican Republic year2013   10281296
    ## 3303              Dominican Republic year2014   10405844
    ## 3304              Dominican Republic year2015   10528394
    ## 3305              Dominican Republic year2016   10648791
    ## 3306              Dominican Republic year2017   10766998
    ## 3307                         Ecuador   year60    4545550
    ## 3308                         Ecuador   year61    4676859
    ## 3309                         Ecuador   year62    4812890
    ## 3310                         Ecuador   year63    4953733
    ## 3311                         Ecuador   year64    5099468
    ## 3312                         Ecuador   year65    5250119
    ## 3313                         Ecuador   year66    5405685
    ## 3314                         Ecuador   year67    5566057
    ## 3315                         Ecuador   year68    5730906
    ## 3316                         Ecuador   year69    5899845
    ## 3317                         Ecuador   year70    6072527
    ## 3318                         Ecuador   year71    6248835
    ## 3319                         Ecuador   year72    6428711
    ## 3320                         Ecuador   year73    6611916
    ## 3321                         Ecuador   year74    6798206
    ## 3322                         Ecuador   year75    6987391
    ## 3323                         Ecuador   year76    7179399
    ## 3324                         Ecuador   year77    7374234
    ## 3325                         Ecuador   year78    7571959
    ## 3326                         Ecuador   year79    7772653
    ## 3327                         Ecuador   year80    7976445
    ## 3328                         Ecuador   year81    8183194
    ## 3329                         Ecuador   year82    8392940
    ## 3330                         Ecuador   year83    8606213
    ## 3331                         Ecuador   year84    8823751
    ## 3332                         Ecuador   year85    9045979
    ## 3333                         Ecuador   year86    9272906
    ## 3334                         Ecuador   year87    9504129
    ## 3335                         Ecuador   year88    9739176
    ## 3336                         Ecuador   year89    9977377
    ## 3337                         Ecuador   year90   10218091
    ## 3338                         Ecuador   year91   10460990
    ## 3339                         Ecuador   year92   10705667
    ## 3340                         Ecuador   year93   10951202
    ## 3341                         Ecuador   year94   11196479
    ## 3342                         Ecuador   year95   11440583
    ## 3343                         Ecuador   year96   11683479
    ## 3344                         Ecuador   year97   11924993
    ## 3345                         Ecuador   year98   12163885
    ## 3346                         Ecuador   year99   12398691
    ## 3347                         Ecuador year2000   12628596
    ## 3348                         Ecuador year2001   12852755
    ## 3349                         Ecuador year2002   13072060
    ## 3350                         Ecuador year2003   13289601
    ## 3351                         Ecuador year2004   13509647
    ## 3352                         Ecuador year2005   13735233
    ## 3353                         Ecuador year2006   13967480
    ## 3354                         Ecuador year2007   14205453
    ## 3355                         Ecuador year2008   14447562
    ## 3356                         Ecuador year2009   14691275
    ## 3357                         Ecuador year2010   14934690
    ## 3358                         Ecuador year2011   15177355
    ## 3359                         Ecuador year2012   15419666
    ## 3360                         Ecuador year2013   15661547
    ## 3361                         Ecuador year2014   15903112
    ## 3362                         Ecuador year2015   16144368
    ## 3363                         Ecuador year2016   16385068
    ## 3364                         Ecuador year2017   16624858
    ## 3365                Egypt, Arab Rep.   year60   26996533
    ## 3366                Egypt, Arab Rep.   year61   27744712
    ## 3367                Egypt, Arab Rep.   year62   28506176
    ## 3368                Egypt, Arab Rep.   year63   29281250
    ## 3369                Egypt, Arab Rep.   year64   30071102
    ## 3370                Egypt, Arab Rep.   year65   30875964
    ## 3371                Egypt, Arab Rep.   year66   31697616
    ## 3372                Egypt, Arab Rep.   year67   32534021
    ## 3373                Egypt, Arab Rep.   year68   33377259
    ## 3374                Egypt, Arab Rep.   year69   34216826
    ## 3375                Egypt, Arab Rep.   year70   35046273
    ## 3376                Egypt, Arab Rep.   year71   35863382
    ## 3377                Egypt, Arab Rep.   year72   36673642
    ## 3378                Egypt, Arab Rep.   year73   37488067
    ## 3379                Egypt, Arab Rep.   year74   38322022
    ## 3380                Egypt, Arab Rep.   year75   39187702
    ## 3381                Egypt, Arab Rep.   year76   40089032
    ## 3382                Egypt, Arab Rep.   year77   41026477
    ## 3383                Egypt, Arab Rep.   year78   42004655
    ## 3384                Egypt, Arab Rep.   year79   43027816
    ## 3385                Egypt, Arab Rep.   year80   44099142
    ## 3386                Egypt, Arab Rep.   year81   45216506
    ## 3387                Egypt, Arab Rep.   year82   46379620
    ## 3388                Egypt, Arab Rep.   year83   47594556
    ## 3389                Egypt, Arab Rep.   year84   48868951
    ## 3390                Egypt, Arab Rep.   year85   50204985
    ## 3391                Egypt, Arab Rep.   year86   51607703
    ## 3392                Egypt, Arab Rep.   year87   53066229
    ## 3393                Egypt, Arab Rep.   year88   54547296
    ## 3394                Egypt, Arab Rep.   year89   56006573
    ## 3395                Egypt, Arab Rep.   year90   57412215
    ## 3396                Egypt, Arab Rep.   year91   58752390
    ## 3397                Egypt, Arab Rep.   year92   60035536
    ## 3398                Egypt, Arab Rep.   year93   61275601
    ## 3399                Egypt, Arab Rep.   year94   62495745
    ## 3400                Egypt, Arab Rep.   year95   63714386
    ## 3401                Egypt, Arab Rep.   year96   64933456
    ## 3402                Egypt, Arab Rep.   year97   66151117
    ## 3403                Egypt, Arab Rep.   year98   67378056
    ## 3404                Egypt, Arab Rep.   year99   68626664
    ## 3405                Egypt, Arab Rep. year2000   69905988
    ## 3406                Egypt, Arab Rep. year2001   71226940
    ## 3407                Egypt, Arab Rep. year2002   72590118
    ## 3408                Egypt, Arab Rep. year2003   73981942
    ## 3409                Egypt, Arab Rep. year2004   75381899
    ## 3410                Egypt, Arab Rep. year2005   76778149
    ## 3411                Egypt, Arab Rep. year2006   78159048
    ## 3412                Egypt, Arab Rep. year2007   79537253
    ## 3413                Egypt, Arab Rep. year2008   80953881
    ## 3414                Egypt, Arab Rep. year2009   82465022
    ## 3415                Egypt, Arab Rep. year2010   84107606
    ## 3416                Egypt, Arab Rep. year2011   85897561
    ## 3417                Egypt, Arab Rep. year2012   87813257
    ## 3418                Egypt, Arab Rep. year2013   89807433
    ## 3419                Egypt, Arab Rep. year2014   91812566
    ## 3420                Egypt, Arab Rep. year2015   93778172
    ## 3421                Egypt, Arab Rep. year2016   95688681
    ## 3422                Egypt, Arab Rep. year2017   97553151
    ## 3423                     El Salvador   year60    2762899
    ## 3424                     El Salvador   year61    2843240
    ## 3425                     El Salvador   year62    2927857
    ## 3426                     El Salvador   year63    3015887
    ## 3427                     El Salvador   year64    3106186
    ## 3428                     El Salvador   year65    3197863
    ## 3429                     El Salvador   year66    3290411
    ## 3430                     El Salvador   year67    3383701
    ## 3431                     El Salvador   year68    3477742
    ## 3432                     El Salvador   year69    3572707
    ## 3433                     El Salvador   year70    3668595
    ## 3434                     El Salvador   year71    3765166
    ## 3435                     El Salvador   year72    3861931
    ## 3436                     El Salvador   year73    3958323
    ## 3437                     El Salvador   year74    4053713
    ## 3438                     El Salvador   year75    4147525
    ## 3439                     El Salvador   year76    4239675
    ## 3440                     El Salvador   year77    4329964
    ## 3441                     El Salvador   year78    4417516
    ## 3442                     El Salvador   year79    4501316
    ## 3443                     El Salvador   year80    4580704
    ## 3444                     El Salvador   year81    4655364
    ## 3445                     El Salvador   year82    4725720
    ## 3446                     El Salvador   year83    4792903
    ## 3447                     El Salvador   year84    4858532
    ## 3448                     El Salvador   year85    4923860
    ## 3449                     El Salvador   year86    4988943
    ## 3450                     El Salvador   year87    5053714
    ## 3451                     El Salvador   year88    5119035
    ## 3452                     El Salvador   year89    5185943
    ## 3453                     El Salvador   year90    5254984
    ## 3454                     El Salvador   year91    5326657
    ## 3455                     El Salvador   year92    5400331
    ## 3456                     El Salvador   year93    5474000
    ## 3457                     El Salvador   year94    5544945
    ## 3458                     El Salvador   year95    5611115
    ## 3459                     El Salvador   year96    5671925
    ## 3460                     El Salvador   year97    5727755
    ## 3461                     El Salvador   year98    5778706
    ## 3462                     El Salvador   year99    5825187
    ## 3463                     El Salvador year2000    5867626
    ## 3464                     El Salvador year2001    5905962
    ## 3465                     El Salvador year2002    5940303
    ## 3466                     El Salvador year2003    5971535
    ## 3467                     El Salvador year2004    6000775
    ## 3468                     El Salvador year2005    6028961
    ## 3469                     El Salvador year2006    6056478
    ## 3470                     El Salvador year2007    6083475
    ## 3471                     El Salvador year2008    6110301
    ## 3472                     El Salvador year2009    6137276
    ## 3473                     El Salvador year2010    6164626
    ## 3474                     El Salvador year2011    6192560
    ## 3475                     El Salvador year2012    6221246
    ## 3476                     El Salvador year2013    6250777
    ## 3477                     El Salvador year2014    6281189
    ## 3478                     El Salvador year2015    6312478
    ## 3479                     El Salvador year2016    6344722
    ## 3480                     El Salvador year2017    6377853
    ## 3481               Equatorial Guinea   year60     255323
    ## 3482               Equatorial Guinea   year61     258947
    ## 3483               Equatorial Guinea   year62     262590
    ## 3484               Equatorial Guinea   year63     266598
    ## 3485               Equatorial Guinea   year64     271457
    ## 3486               Equatorial Guinea   year65     277396
    ## 3487               Equatorial Guinea   year66     284868
    ## 3488               Equatorial Guinea   year67     293440
    ## 3489               Equatorial Guinea   year68     301353
    ## 3490               Equatorial Guinea   year69     306233
    ## 3491               Equatorial Guinea   year70     306515
    ## 3492               Equatorial Guinea   year71     301666
    ## 3493               Equatorial Guinea   year72     292585
    ## 3494               Equatorial Guinea   year73     281021
    ## 3495               Equatorial Guinea   year74     269426
    ## 3496               Equatorial Guinea   year75     259747
    ## 3497               Equatorial Guinea   year76     252194
    ## 3498               Equatorial Guinea   year77     246677
    ## 3499               Equatorial Guinea   year78     244485
    ## 3500               Equatorial Guinea   year79     247078
    ## 3501               Equatorial Guinea   year80     255325
    ## 3502               Equatorial Guinea   year81     270063
    ## 3503               Equatorial Guinea   year82     290617
    ## 3504               Equatorial Guinea   year83     314475
    ## 3505               Equatorial Guinea   year84     338086
    ## 3506               Equatorial Guinea   year85     358896
    ## 3507               Equatorial Guinea   year86     376024
    ## 3508               Equatorial Guinea   year87     390173
    ## 3509               Equatorial Guinea   year88     402326
    ## 3510               Equatorial Guinea   year89     414138
    ## 3511               Equatorial Guinea   year90     426846
    ## 3512               Equatorial Guinea   year91     440624
    ## 3513               Equatorial Guinea   year92     455148
    ## 3514               Equatorial Guinea   year93     470610
    ## 3515               Equatorial Guinea   year94     487140
    ## 3516               Equatorial Guinea   year95     504871
    ## 3517               Equatorial Guinea   year96     523999
    ## 3518               Equatorial Guinea   year97     544636
    ## 3519               Equatorial Guinea   year98     566673
    ## 3520               Equatorial Guinea   year99     589938
    ## 3521               Equatorial Guinea year2000     614323
    ## 3522               Equatorial Guinea year2001     639762
    ## 3523               Equatorial Guinea year2002     666407
    ## 3524               Equatorial Guinea year2003     694611
    ## 3525               Equatorial Guinea year2004     724817
    ## 3526               Equatorial Guinea year2005     757317
    ## 3527               Equatorial Guinea year2006     792217
    ## 3528               Equatorial Guinea year2007     829327
    ## 3529               Equatorial Guinea year2008     868418
    ## 3530               Equatorial Guinea year2009     909111
    ## 3531               Equatorial Guinea year2010     951104
    ## 3532               Equatorial Guinea year2011     994290
    ## 3533               Equatorial Guinea year2012    1038593
    ## 3534               Equatorial Guinea year2013    1083746
    ## 3535               Equatorial Guinea year2014    1129424
    ## 3536               Equatorial Guinea year2015    1175389
    ## 3537               Equatorial Guinea year2016    1221490
    ## 3538               Equatorial Guinea year2017    1267689
    ## 3539                         Eritrea   year60    1397491
    ## 3540                         Eritrea   year61    1432640
    ## 3541                         Eritrea   year62    1469645
    ## 3542                         Eritrea   year63    1508273
    ## 3543                         Eritrea   year64    1548187
    ## 3544                         Eritrea   year65    1589179
    ## 3545                         Eritrea   year66    1631147
    ## 3546                         Eritrea   year67    1674204
    ## 3547                         Eritrea   year68    1718525
    ## 3548                         Eritrea   year69    1764343
    ## 3549                         Eritrea   year70    1811878
    ## 3550                         Eritrea   year71    1861199
    ## 3551                         Eritrea   year72    1912302
    ## 3552                         Eritrea   year73    1965160
    ## 3553                         Eritrea   year74    2019717
    ## 3554                         Eritrea   year75    2075965
    ## 3555                         Eritrea   year76    2133723
    ## 3556                         Eritrea   year77    2193068
    ## 3557                         Eritrea   year78    2254450
    ## 3558                         Eritrea   year79    2318495
    ## 3559                         Eritrea   year80    2385540
    ## 3560                         Eritrea   year81    2454766
    ## 3561                         Eritrea   year82    2525521
    ## 3562                         Eritrea   year83    2598410
    ## 3563                         Eritrea   year84    2674289
    ## 3564                         Eritrea   year85    2753151
    ## 3565                         Eritrea   year86    2837111
    ## 3566                         Eritrea   year87    2924349
    ## 3567                         Eritrea   year88    3006361
    ## 3568                         Eritrea   year89    3071771
    ## 3569                         Eritrea   year90    3113311
    ## 3570                         Eritrea   year91    3127297
    ## 3571                         Eritrea   year92    3118582
    ## 3572                         Eritrea   year93    3099047
    ## 3573                         Eritrea   year94    3085443
    ## 3574                         Eritrea   year95    3090159
    ## 3575                         Eritrea   year96    3116379
    ## 3576                         Eritrea   year97    3161350
    ## 3577                         Eritrea   year98    3224223
    ## 3578                         Eritrea   year99    3302263
    ## 3579                         Eritrea year2000    3392801
    ## 3580                         Eritrea year2001    3497124
    ## 3581                         Eritrea year2002    3614639
    ## 3582                         Eritrea year2003    3738265
    ## 3583                         Eritrea year2004    3858623
    ## 3584                         Eritrea year2005    3969007
    ## 3585                         Eritrea year2006    4066648
    ## 3586                         Eritrea year2007    4153332
    ## 3587                         Eritrea year2008    4232636
    ## 3588                         Eritrea year2009    4310334
    ## 3589                         Eritrea year2010    4390840
    ## 3590                         Eritrea year2011    4474690
    ## 3591                         Eritrea year2012         NA
    ## 3592                         Eritrea year2013         NA
    ## 3593                         Eritrea year2014         NA
    ## 3594                         Eritrea year2015         NA
    ## 3595                         Eritrea year2016         NA
    ## 3596                         Eritrea year2017         NA
    ## 3597                         Estonia   year60    1211537
    ## 3598                         Estonia   year61    1225077
    ## 3599                         Estonia   year62    1241623
    ## 3600                         Estonia   year63    1258857
    ## 3601                         Estonia   year64    1277086
    ## 3602                         Estonia   year65    1294566
    ## 3603                         Estonia   year66    1308597
    ## 3604                         Estonia   year67    1318946
    ## 3605                         Estonia   year68    1331214
    ## 3606                         Estonia   year69    1345249
    ## 3607                         Estonia   year70    1360076
    ## 3608                         Estonia   year71    1376955
    ## 3609                         Estonia   year72    1392518
    ## 3610                         Estonia   year73    1405951
    ## 3611                         Estonia   year74    1418169
    ## 3612                         Estonia   year75    1429352
    ## 3613                         Estonia   year76    1439576
    ## 3614                         Estonia   year77    1450211
    ## 3615                         Estonia   year78    1460188
    ## 3616                         Estonia   year79    1468333
    ## 3617                         Estonia   year80    1477219
    ## 3618                         Estonia   year81    1487666
    ## 3619                         Estonia   year82    1498414
    ## 3620                         Estonia   year83    1508745
    ## 3621                         Estonia   year84    1518617
    ## 3622                         Estonia   year85    1528781
    ## 3623                         Estonia   year86    1540190
    ## 3624                         Estonia   year87    1552221
    ## 3625                         Estonia   year88    1561900
    ## 3626                         Estonia   year89    1568131
    ## 3627                         Estonia   year90    1569174
    ## 3628                         Estonia   year91    1561314
    ## 3629                         Estonia   year92    1533091
    ## 3630                         Estonia   year93    1494128
    ## 3631                         Estonia   year94    1462514
    ## 3632                         Estonia   year95    1436634
    ## 3633                         Estonia   year96    1415594
    ## 3634                         Estonia   year97    1399535
    ## 3635                         Estonia   year98    1386156
    ## 3636                         Estonia   year99    1390244
    ## 3637                         Estonia year2000    1396985
    ## 3638                         Estonia year2001    1388115
    ## 3639                         Estonia year2002    1379350
    ## 3640                         Estonia year2003    1370720
    ## 3641                         Estonia year2004    1362550
    ## 3642                         Estonia year2005    1354775
    ## 3643                         Estonia year2006    1346810
    ## 3644                         Estonia year2007    1340680
    ## 3645                         Estonia year2008    1337090
    ## 3646                         Estonia year2009    1334515
    ## 3647                         Estonia year2010    1331475
    ## 3648                         Estonia year2011    1327439
    ## 3649                         Estonia year2012    1322696
    ## 3650                         Estonia year2013    1317997
    ## 3651                         Estonia year2014    1314545
    ## 3652                         Estonia year2015    1315407
    ## 3653                         Estonia year2016    1315790
    ## 3654                         Estonia year2017    1315480
    ## 3655                        Eswatini   year60     349174
    ## 3656                        Eswatini   year61     357453
    ## 3657                        Eswatini   year62     365636
    ## 3658                        Eswatini   year63     373897
    ## 3659                        Eswatini   year64     382469
    ## 3660                        Eswatini   year65     391546
    ## 3661                        Eswatini   year66     401183
    ## 3662                        Eswatini   year67     411352
    ## 3663                        Eswatini   year68     422140
    ## 3664                        Eswatini   year69     433588
    ## 3665                        Eswatini   year70     445729
    ## 3666                        Eswatini   year71     458605
    ## 3667                        Eswatini   year72     472230
    ## 3668                        Eswatini   year73     486561
    ## 3669                        Eswatini   year74     501512
    ## 3670                        Eswatini   year75     517024
    ## 3671                        Eswatini   year76     533214
    ## 3672                        Eswatini   year77     550118
    ## 3673                        Eswatini   year78     567559
    ## 3674                        Eswatini   year79     585344
    ## 3675                        Eswatini   year80     603372
    ## 3676                        Eswatini   year81     621276
    ## 3677                        Eswatini   year82     639237
    ## 3678                        Eswatini   year83     658320
    ## 3679                        Eswatini   year84     679976
    ## 3680                        Eswatini   year85     705085
    ## 3681                        Eswatini   year86     734243
    ## 3682                        Eswatini   year87     766707
    ## 3683                        Eswatini   year88     800456
    ## 3684                        Eswatini   year89     832682
    ## 3685                        Eswatini   year90     861373
    ## 3686                        Eswatini   year91     885623
    ## 3687                        Eswatini   year92     906034
    ## 3688                        Eswatini   year93     924025
    ## 3689                        Eswatini   year94     941774
    ## 3690                        Eswatini   year95     960792
    ## 3691                        Eswatini   year96     981764
    ## 3692                        Eswatini   year97    1003995
    ## 3693                        Eswatini   year98    1026009
    ## 3694                        Eswatini   year99    1045629
    ## 3695                        Eswatini year2000    1061468
    ## 3696                        Eswatini year2001    1072927
    ## 3697                        Eswatini year2002    1080930
    ## 3698                        Eswatini year2003    1087392
    ## 3699                        Eswatini year2004    1095053
    ## 3700                        Eswatini year2005    1105873
    ## 3701                        Eswatini year2006    1120514
    ## 3702                        Eswatini year2007    1138434
    ## 3703                        Eswatini year2008    1158897
    ## 3704                        Eswatini year2009    1180675
    ## 3705                        Eswatini year2010    1202843
    ## 3706                        Eswatini year2011    1225258
    ## 3707                        Eswatini year2012    1248158
    ## 3708                        Eswatini year2013    1271456
    ## 3709                        Eswatini year2014    1295097
    ## 3710                        Eswatini year2015    1319011
    ## 3711                        Eswatini year2016    1343098
    ## 3712                        Eswatini year2017    1367254
    ## 3713                        Ethiopia   year60   22151278
    ## 3714                        Ethiopia   year61   22671190
    ## 3715                        Ethiopia   year62   23221389
    ## 3716                        Ethiopia   year63   23798429
    ## 3717                        Ethiopia   year64   24397024
    ## 3718                        Ethiopia   year65   25013626
    ## 3719                        Ethiopia   year66   25641376
    ## 3720                        Ethiopia   year67   26281208
    ## 3721                        Ethiopia   year68   26946079
    ## 3722                        Ethiopia   year69   27654161
    ## 3723                        Ethiopia   year70   28415077
    ## 3724                        Ethiopia   year71   29245207
    ## 3725                        Ethiopia   year72   30132580
    ## 3726                        Ethiopia   year73   31025115
    ## 3727                        Ethiopia   year74   31851708
    ## 3728                        Ethiopia   year75   32566821
    ## 3729                        Ethiopia   year76   33146891
    ## 3730                        Ethiopia   year77   33622390
    ## 3731                        Ethiopia   year78   34068316
    ## 3732                        Ethiopia   year79   34590226
    ## 3733                        Ethiopia   year80   35264898
    ## 3734                        Ethiopia   year81   36120288
    ## 3735                        Ethiopia   year82   37136848
    ## 3736                        Ethiopia   year83   38285883
    ## 3737                        Ethiopia   year84   39518801
    ## 3738                        Ethiopia   year85   40800343
    ## 3739                        Ethiopia   year86   42120730
    ## 3740                        Ethiopia   year87   43493283
    ## 3741                        Ethiopia   year88   44932064
    ## 3742                        Ethiopia   year89   46458913
    ## 3743                        Ethiopia   year90   48086516
    ## 3744                        Ethiopia   year91   49821083
    ## 3745                        Ethiopia   year92   51647768
    ## 3746                        Ethiopia   year93   53532956
    ## 3747                        Ethiopia   year94   55431123
    ## 3748                        Ethiopia   year95   57309880
    ## 3749                        Ethiopia   year96   59155148
    ## 3750                        Ethiopia   year97   60976450
    ## 3751                        Ethiopia   year98   62794151
    ## 3752                        Ethiopia   year99   64640054
    ## 3753                        Ethiopia year2000   66537331
    ## 3754                        Ethiopia year2001   68492257
    ## 3755                        Ethiopia year2002   70497192
    ## 3756                        Ethiopia year2003   72545144
    ## 3757                        Ethiopia year2004   74624405
    ## 3758                        Ethiopia year2005   76727083
    ## 3759                        Ethiopia year2006   78850689
    ## 3760                        Ethiopia year2007   81000409
    ## 3761                        Ethiopia year2008   83184892
    ## 3762                        Ethiopia year2009   85416253
    ## 3763                        Ethiopia year2010   87702670
    ## 3764                        Ethiopia year2011   90046756
    ## 3765                        Ethiopia year2012   92444183
    ## 3766                        Ethiopia year2013   94887724
    ## 3767                        Ethiopia year2014   97366774
    ## 3768                        Ethiopia year2015   99873033
    ## 3769                        Ethiopia year2016  102403196
    ## 3770                        Ethiopia year2017  104957438
    ## 3771                   Faroe Islands   year60      34661
    ## 3772                   Faroe Islands   year61      35115
    ## 3773                   Faroe Islands   year62      35570
    ## 3774                   Faroe Islands   year63      36014
    ## 3775                   Faroe Islands   year64      36454
    ## 3776                   Faroe Islands   year65      36900
    ## 3777                   Faroe Islands   year66      37334
    ## 3778                   Faroe Islands   year67      37768
    ## 3779                   Faroe Islands   year68      38200
    ## 3780                   Faroe Islands   year69      38646
    ## 3781                   Faroe Islands   year70      39083
    ## 3782                   Faroe Islands   year71      39537
    ## 3783                   Faroe Islands   year72      40009
    ## 3784                   Faroe Islands   year73      40486
    ## 3785                   Faroe Islands   year74      40955
    ## 3786                   Faroe Islands   year75      41407
    ## 3787                   Faroe Islands   year76      41848
    ## 3788                   Faroe Islands   year77      42275
    ## 3789                   Faroe Islands   year78      42693
    ## 3790                   Faroe Islands   year79      43101
    ## 3791                   Faroe Islands   year80      43514
    ## 3792                   Faroe Islands   year81      43917
    ## 3793                   Faroe Islands   year82      44307
    ## 3794                   Faroe Islands   year83      44700
    ## 3795                   Faroe Islands   year84      45122
    ## 3796                   Faroe Islands   year85      45573
    ## 3797                   Faroe Islands   year86      46077
    ## 3798                   Faroe Islands   year87      46621
    ## 3799                   Faroe Islands   year88      47117
    ## 3800                   Faroe Islands   year89      47466
    ## 3801                   Faroe Islands   year90      47594
    ## 3802                   Faroe Islands   year91      47457
    ## 3803                   Faroe Islands   year92      47101
    ## 3804                   Faroe Islands   year93      46640
    ## 3805                   Faroe Islands   year94      46250
    ## 3806                   Faroe Islands   year95      46040
    ## 3807                   Faroe Islands   year96      46058
    ## 3808                   Faroe Islands   year97      46251
    ## 3809                   Faroe Islands   year98      46580
    ## 3810                   Faroe Islands   year99      46937
    ## 3811                   Faroe Islands year2000      47258
    ## 3812                   Faroe Islands year2001      47526
    ## 3813                   Faroe Islands year2002      47769
    ## 3814                   Faroe Islands year2003      47974
    ## 3815                   Faroe Islands year2004      48143
    ## 3816                   Faroe Islands year2005      48285
    ## 3817                   Faroe Islands year2006      48383
    ## 3818                   Faroe Islands year2007      48448
    ## 3819                   Faroe Islands year2008      48485
    ## 3820                   Faroe Islands year2009      48517
    ## 3821                   Faroe Islands year2010      48550
    ## 3822                   Faroe Islands year2011      48608
    ## 3823                   Faroe Islands year2012      48666
    ## 3824                   Faroe Islands year2013      48747
    ## 3825                   Faroe Islands year2014      48842
    ## 3826                   Faroe Islands year2015      48965
    ## 3827                   Faroe Islands year2016      49117
    ## 3828                   Faroe Islands year2017      49290
    ## 3829                            Fiji   year60     393386
    ## 3830                            Fiji   year61     407156
    ## 3831                            Fiji   year62     421577
    ## 3832                            Fiji   year63     436208
    ## 3833                            Fiji   year64     450450
    ## 3834                            Fiji   year65     463883
    ## 3835                            Fiji   year66     476324
    ## 3836                            Fiji   year67     487913
    ## 3837                            Fiji   year68     498892
    ## 3838                            Fiji   year69     509658
    ## 3839                            Fiji   year70     520529
    ## 3840                            Fiji   year71     531601
    ## 3841                            Fiji   year72     542814
    ## 3842                            Fiji   year73     554107
    ## 3843                            Fiji   year74     565388
    ## 3844                            Fiji   year75     576595
    ## 3845                            Fiji   year76     587520
    ## 3846                            Fiji   year77     598259
    ## 3847                            Fiji   year78     609345
    ## 3848                            Fiji   year79     621538
    ## 3849                            Fiji   year80     635255
    ## 3850                            Fiji   year81     650955
    ## 3851                            Fiji   year82     668198
    ## 3852                            Fiji   year83     685391
    ## 3853                            Fiji   year84     700366
    ## 3854                            Fiji   year85     711661
    ## 3855                            Fiji   year86     718548
    ## 3856                            Fiji   year87     721725
    ## 3857                            Fiji   year88     722917
    ## 3858                            Fiji   year89     724624
    ## 3859                            Fiji   year90     728628
    ## 3860                            Fiji   year91     735473
    ## 3861                            Fiji   year92     744531
    ## 3862                            Fiji   year93     755026
    ## 3863                            Fiji   year94     765667
    ## 3864                            Fiji   year95     775498
    ## 3865                            Fiji   year96     784476
    ## 3866                            Fiji   year97     792860
    ## 3867                            Fiji   year98     800315
    ## 3868                            Fiji   year99     806494
    ## 3869                            Fiji year2000     811223
    ## 3870                            Fiji year2001     814218
    ## 3871                            Fiji year2002     815691
    ## 3872                            Fiji year2003     816628
    ## 3873                            Fiji year2004     818354
    ## 3874                            Fiji year2005     821817
    ## 3875                            Fiji year2006     827411
    ## 3876                            Fiji year2007     834812
    ## 3877                            Fiji year2008     843340
    ## 3878                            Fiji year2009     851967
    ## 3879                            Fiji year2010     859950
    ## 3880                            Fiji year2011     867086
    ## 3881                            Fiji year2012     873596
    ## 3882                            Fiji year2013     879715
    ## 3883                            Fiji year2014     885806
    ## 3884                            Fiji year2015     892149
    ## 3885                            Fiji year2016     898760
    ## 3886                            Fiji year2017     905502
    ## 3887                         Finland   year60    4429634
    ## 3888                         Finland   year61    4461005
    ## 3889                         Finland   year62    4491443
    ## 3890                         Finland   year63    4523309
    ## 3891                         Finland   year64    4548543
    ## 3892                         Finland   year65    4563732
    ## 3893                         Finland   year66    4580869
    ## 3894                         Finland   year67    4605744
    ## 3895                         Finland   year68    4626469
    ## 3896                         Finland   year69    4623785
    ## 3897                         Finland   year70    4606307
    ## 3898                         Finland   year71    4612124
    ## 3899                         Finland   year72    4639657
    ## 3900                         Finland   year73    4666081
    ## 3901                         Finland   year74    4690574
    ## 3902                         Finland   year75    4711440
    ## 3903                         Finland   year76    4725664
    ## 3904                         Finland   year77    4738902
    ## 3905                         Finland   year78    4752528
    ## 3906                         Finland   year79    4764690
    ## 3907                         Finland   year80    4779535
    ## 3908                         Finland   year81    4799964
    ## 3909                         Finland   year82    4826933
    ## 3910                         Finland   year83    4855787
    ## 3911                         Finland   year84    4881803
    ## 3912                         Finland   year85    4902206
    ## 3913                         Finland   year86    4918154
    ## 3914                         Finland   year87    4932123
    ## 3915                         Finland   year88    4946481
    ## 3916                         Finland   year89    4964371
    ## 3917                         Finland   year90    4986431
    ## 3918                         Finland   year91    5013740
    ## 3919                         Finland   year92    5041992
    ## 3920                         Finland   year93    5066447
    ## 3921                         Finland   year94    5088333
    ## 3922                         Finland   year95    5107790
    ## 3923                         Finland   year96    5124573
    ## 3924                         Finland   year97    5139835
    ## 3925                         Finland   year98    5153498
    ## 3926                         Finland   year99    5165474
    ## 3927                         Finland year2000    5176209
    ## 3928                         Finland year2001    5188008
    ## 3929                         Finland year2002    5200598
    ## 3930                         Finland year2003    5213014
    ## 3931                         Finland year2004    5228172
    ## 3932                         Finland year2005    5246096
    ## 3933                         Finland year2006    5266268
    ## 3934                         Finland year2007    5288720
    ## 3935                         Finland year2008    5313399
    ## 3936                         Finland year2009    5338871
    ## 3937                         Finland year2010    5363352
    ## 3938                         Finland year2011    5388272
    ## 3939                         Finland year2012    5413971
    ## 3940                         Finland year2013    5438972
    ## 3941                         Finland year2014    5461512
    ## 3942                         Finland year2015    5479531
    ## 3943                         Finland year2016    5495303
    ## 3944                         Finland year2017    5511303
    ## 3945                          France   year60   46814237
    ## 3946                          France   year61   47444751
    ## 3947                          France   year62   48119649
    ## 3948                          France   year63   48803680
    ## 3949                          France   year64   49449403
    ## 3950                          France   year65   50023774
    ## 3951                          France   year66   50508717
    ## 3952                          France   year67   50915456
    ## 3953                          France   year68   51276054
    ## 3954                          France   year69   51638260
    ## 3955                          France   year70   52035095
    ## 3956                          France   year71   52480421
    ## 3957                          France   year72   52959228
    ## 3958                          France   year73   53441264
    ## 3959                          France   year74   53882416
    ## 3960                          France   year75   54252574
    ## 3961                          France   year76   54541493
    ## 3962                          France   year77   54764462
    ## 3963                          France   year78   54947975
    ## 3964                          France   year79   55130594
    ## 3965                          France   year80   55340782
    ## 3966                          France   year81   55585824
    ## 3967                          France   year82   55858727
    ## 3968                          France   year83   56156284
    ## 3969                          France   year84   56470769
    ## 3970                          France   year85   56795686
    ## 3971                          France   year86   57132691
    ## 3972                          France   year87   57482591
    ## 3973                          France   year88   57836486
    ## 3974                          France   year89   58182702
    ## 3975                          France   year90   58512808
    ## 3976                          France   year91   58559311
    ## 3977                          France   year92   58851217
    ## 3978                          France   year93   59106768
    ## 3979                          France   year94   59327192
    ## 3980                          France   year95   59541899
    ## 3981                          France   year96   59753100
    ## 3982                          France   year97   59964851
    ## 3983                          France   year98   60186288
    ## 3984                          France   year99   60496718
    ## 3985                          France year2000   60912500
    ## 3986                          France year2001   61357430
    ## 3987                          France year2002   61805267
    ## 3988                          France year2003   62244886
    ## 3989                          France year2004   62704895
    ## 3990                          France year2005   63179351
    ## 3991                          France year2006   63621381
    ## 3992                          France year2007   64016227
    ## 3993                          France year2008   64374989
    ## 3994                          France year2009   64707044
    ## 3995                          France year2010   65027507
    ## 3996                          France year2011   65342775
    ## 3997                          France year2012   65659789
    ## 3998                          France year2013   65998660
    ## 3999                          France year2014   66316092
    ## 4000                          France year2015   66593366
    ## 4001                          France year2016   66859768
    ## 4002                          France year2017   67118648
    ## 4003                French Polynesia   year60      78076
    ## 4004                French Polynesia   year61      80703
    ## 4005                French Polynesia   year62      83651
    ## 4006                French Polynesia   year63      86837
    ## 4007                French Polynesia   year64      90132
    ## 4008                French Polynesia   year65      93438
    ## 4009                French Polynesia   year66      96732
    ## 4010                French Polynesia   year67     100029
    ## 4011                French Polynesia   year68     103386
    ## 4012                French Polynesia   year69     106857
    ## 4013                French Polynesia   year70     110495
    ## 4014                French Polynesia   year71     114313
    ## 4015                French Polynesia   year72     118279
    ## 4016                French Polynesia   year73     122356
    ## 4017                French Polynesia   year74     126486
    ## 4018                French Polynesia   year75     130619
    ## 4019                French Polynesia   year76     134748
    ## 4020                French Polynesia   year77     138864
    ## 4021                French Polynesia   year78     143032
    ## 4022                French Polynesia   year79     147296
    ## 4023                French Polynesia   year80     151708
    ## 4024                French Polynesia   year81     156243
    ## 4025                French Polynesia   year82     160888
    ## 4026                French Polynesia   year83     165613
    ## 4027                French Polynesia   year84     170396
    ## 4028                French Polynesia   year85     175204
    ## 4029                French Polynesia   year86     180075
    ## 4030                French Polynesia   year87     184950
    ## 4031                French Polynesia   year88     189738
    ## 4032                French Polynesia   year89     194252
    ## 4033                French Polynesia   year90     198375
    ## 4034                French Polynesia   year91     202016
    ## 4035                French Polynesia   year92     205266
    ## 4036                French Polynesia   year93     208345
    ## 4037                French Polynesia   year94     211579
    ## 4038                French Polynesia   year95     215196
    ## 4039                French Polynesia   year96     219283
    ## 4040                French Polynesia   year97     223731
    ## 4041                French Polynesia   year98     228376
    ## 4042                French Polynesia   year99     232952
    ## 4043                French Polynesia year2000     237258
    ## 4044                French Polynesia year2001     241273
    ## 4045                French Polynesia year2002     245006
    ## 4046                French Polynesia year2003     248499
    ## 4047                French Polynesia year2004     251775
    ## 4048                French Polynesia year2005     254886
    ## 4049                French Polynesia year2006     257832
    ## 4050                French Polynesia year2007     260594
    ## 4051                French Polynesia year2008     263179
    ## 4052                French Polynesia year2009     265581
    ## 4053                French Polynesia year2010     267820
    ## 4054                French Polynesia year2011     269843
    ## 4055                French Polynesia year2012     271703
    ## 4056                French Polynesia year2013     273528
    ## 4057                French Polynesia year2014     275484
    ## 4058                French Polynesia year2015     277690
    ## 4059                French Polynesia year2016     280208
    ## 4060                French Polynesia year2017     283007
    ## 4061                           Gabon   year60     499184
    ## 4062                           Gabon   year61     504167
    ## 4063                           Gabon   year62     509806
    ## 4064                           Gabon   year63     516265
    ## 4065                           Gabon   year64     523789
    ## 4066                           Gabon   year65     532511
    ## 4067                           Gabon   year66     542557
    ## 4068                           Gabon   year67     553823
    ## 4069                           Gabon   year68     565873
    ## 4070                           Gabon   year69     578108
    ## 4071                           Gabon   year70     590118
    ## 4072                           Gabon   year71     601731
    ## 4073                           Gabon   year72     613123
    ## 4074                           Gabon   year73     624621
    ## 4075                           Gabon   year74     636696
    ## 4076                           Gabon   year75     649716
    ## 4077                           Gabon   year76     663770
    ## 4078                           Gabon   year77     678774
    ## 4079                           Gabon   year78     694732
    ## 4080                           Gabon   year79     711533
    ## 4081                           Gabon   year80     729159
    ## 4082                           Gabon   year81     747587
    ## 4083                           Gabon   year82     766855
    ## 4084                           Gabon   year83     787013
    ## 4085                           Gabon   year84     808083
    ## 4086                           Gabon   year85     830085
    ## 4087                           Gabon   year86     853027
    ## 4088                           Gabon   year87     876863
    ## 4089                           Gabon   year88     901458
    ## 4090                           Gabon   year89     926622
    ## 4091                           Gabon   year90     952212
    ## 4092                           Gabon   year91     978223
    ## 4093                           Gabon   year92    1004676
    ## 4094                           Gabon   year93    1031504
    ## 4095                           Gabon   year94    1058663
    ## 4096                           Gabon   year95    1086137
    ## 4097                           Gabon   year96    1113994
    ## 4098                           Gabon   year97    1142324
    ## 4099                           Gabon   year98    1171224
    ## 4100                           Gabon   year99    1200773
    ## 4101                           Gabon year2000    1231122
    ## 4102                           Gabon year2001    1262259
    ## 4103                           Gabon year2002    1294409
    ## 4104                           Gabon year2003    1328146
    ## 4105                           Gabon year2004    1364205
    ## 4106                           Gabon year2005    1403126
    ## 4107                           Gabon year2006    1444844
    ## 4108                           Gabon year2007    1489193
    ## 4109                           Gabon year2008    1536411
    ## 4110                           Gabon year2009    1586754
    ## 4111                           Gabon year2010    1640210
    ## 4112                           Gabon year2011    1697101
    ## 4113                           Gabon year2012    1756817
    ## 4114                           Gabon year2013    1817271
    ## 4115                           Gabon year2014    1875713
    ## 4116                           Gabon year2015    1930175
    ## 4117                           Gabon year2016    1979786
    ## 4118                           Gabon year2017    2025137
    ## 4119                     Gambia, The   year60     367928
    ## 4120                     Gambia, The   year61     376737
    ## 4121                     Gambia, The   year62     383523
    ## 4122                     Gambia, The   year63     389072
    ## 4123                     Gambia, The   year64     394553
    ## 4124                     Gambia, The   year65     400861
    ## 4125                     Gambia, The   year66     408180
    ## 4126                     Gambia, The   year67     416339
    ## 4127                     Gambia, The   year68     425510
    ## 4128                     Gambia, The   year69     435798
    ## 4129                     Gambia, The   year70     447285
    ## 4130                     Gambia, The   year71     460194
    ## 4131                     Gambia, The   year72     474539
    ## 4132                     Gambia, The   year73     489861
    ## 4133                     Gambia, The   year74     505512
    ## 4134                     Gambia, The   year75     521070
    ## 4135                     Gambia, The   year76     536409
    ## 4136                     Gambia, The   year77     551817
    ## 4137                     Gambia, The   year78     567831
    ## 4138                     Gambia, The   year79     585157
    ## 4139                     Gambia, The   year80     604369
    ## 4140                     Gambia, The   year81     625411
    ## 4141                     Gambia, The   year82     648210
    ## 4142                     Gambia, The   year83     673238
    ## 4143                     Gambia, The   year84     701104
    ## 4144                     Gambia, The   year85     732096
    ## 4145                     Gambia, The   year86     766589
    ## 4146                     Gambia, The   year87     804125
    ## 4147                     Gambia, The   year88     843050
    ## 4148                     Gambia, The   year89     881138
    ## 4149                     Gambia, The   year90     916808
    ## 4150                     Gambia, The   year91     949493
    ## 4151                     Gambia, The   year92     979718
    ## 4152                     Gambia, The   year93    1008358
    ## 4153                     Gambia, The   year94    1036829
    ## 4154                     Gambia, The   year95    1066223
    ## 4155                     Gambia, The   year96    1096708
    ## 4156                     Gambia, The   year97    1128169
    ## 4157                     Gambia, The   year98    1160944
    ## 4158                     Gambia, The   year99    1195420
    ## 4159                     Gambia, The year2000    1231844
    ## 4160                     Gambia, The year2001    1270495
    ## 4161                     Gambia, The year2002    1311349
    ## 4162                     Gambia, The year2003    1354194
    ## 4163                     Gambia, The year2004    1398573
    ## 4164                     Gambia, The year2005    1444204
    ## 4165                     Gambia, The year2006    1491021
    ## 4166                     Gambia, The year2007    1539116
    ## 4167                     Gambia, The year2008    1588572
    ## 4168                     Gambia, The year2009    1639560
    ## 4169                     Gambia, The year2010    1692149
    ## 4170                     Gambia, The year2011    1746363
    ## 4171                     Gambia, The year2012    1802125
    ## 4172                     Gambia, The year2013    1859324
    ## 4173                     Gambia, The year2014    1917852
    ## 4174                     Gambia, The year2015    1977590
    ## 4175                     Gambia, The year2016    2038501
    ## 4176                     Gambia, The year2017    2100568
    ## 4177                         Georgia   year60    3645600
    ## 4178                         Georgia   year61    3703600
    ## 4179                         Georgia   year62    3760300
    ## 4180                         Georgia   year63    3816100
    ## 4181                         Georgia   year64    3870300
    ## 4182                         Georgia   year65    3921600
    ## 4183                         Georgia   year66    3966700
    ## 4184                         Georgia   year67    4005800
    ## 4185                         Georgia   year68    4042300
    ## 4186                         Georgia   year69    4080300
    ## 4187                         Georgia   year70    4119900
    ## 4188                         Georgia   year71    4163000
    ## 4189                         Georgia   year72    4205300
    ## 4190                         Georgia   year73    4242500
    ## 4191                         Georgia   year74    4279500
    ## 4192                         Georgia   year75    4311200
    ## 4193                         Georgia   year76    4342400
    ## 4194                         Georgia   year77    4372100
    ## 4195                         Georgia   year78    4397700
    ## 4196                         Georgia   year79    4430200
    ## 4197                         Georgia   year80    4467700
    ## 4198                         Georgia   year81    4504500
    ## 4199                         Georgia   year82    4542800
    ## 4200                         Georgia   year83    4582900
    ## 4201                         Georgia   year84    4622200
    ## 4202                         Georgia   year85    4662900
    ## 4203                         Georgia   year86    4704500
    ## 4204                         Georgia   year87    4743500
    ## 4205                         Georgia   year88    4790700
    ## 4206                         Georgia   year89    4803300
    ## 4207                         Georgia   year90    4802000
    ## 4208                         Georgia   year91    4835900
    ## 4209                         Georgia   year92    4873500
    ## 4210                         Georgia   year93    4911100
    ## 4211                         Georgia   year94    4861600
    ## 4212                         Georgia   year95    4734000
    ## 4213                         Georgia   year96    4616100
    ## 4214                         Georgia   year97    4531600
    ## 4215                         Georgia   year98    4487300
    ## 4216                         Georgia   year99    4452500
    ## 4217                         Georgia year2000    4418300
    ## 4218                         Georgia year2001    4386400
    ## 4219                         Georgia year2002    4357000
    ## 4220                         Georgia year2003    4301000
    ## 4221                         Georgia year2004    4245000
    ## 4222                         Georgia year2005    4190000
    ## 4223                         Georgia year2006    4136000
    ## 4224                         Georgia year2007    4082000
    ## 4225                         Georgia year2008    4030000
    ## 4226                         Georgia year2009    3978000
    ## 4227                         Georgia year2010    3926000
    ## 4228                         Georgia year2011    3875000
    ## 4229                         Georgia year2012    3825000
    ## 4230                         Georgia year2013    3776000
    ## 4231                         Georgia year2014    3727000
    ## 4232                         Georgia year2015    3717100
    ## 4233                         Georgia year2016    3719300
    ## 4234                         Georgia year2017    3717100
    ## 4235                         Germany   year60   72814900
    ## 4236                         Germany   year61   73377632
    ## 4237                         Germany   year62   74025784
    ## 4238                         Germany   year63   74714353
    ## 4239                         Germany   year64   75318337
    ## 4240                         Germany   year65   75963695
    ## 4241                         Germany   year66   76600311
    ## 4242                         Germany   year67   76951336
    ## 4243                         Germany   year68   77294314
    ## 4244                         Germany   year69   77909682
    ## 4245                         Germany   year70   78169289
    ## 4246                         Germany   year71   78312842
    ## 4247                         Germany   year72   78688452
    ## 4248                         Germany   year73   78936666
    ## 4249                         Germany   year74   78967433
    ## 4250                         Germany   year75   78673554
    ## 4251                         Germany   year76   78336950
    ## 4252                         Germany   year77   78159814
    ## 4253                         Germany   year78   78091820
    ## 4254                         Germany   year79   78126350
    ## 4255                         Germany   year80   78288576
    ## 4256                         Germany   year81   78407907
    ## 4257                         Germany   year82   78333366
    ## 4258                         Germany   year83   78128282
    ## 4259                         Germany   year84   77858685
    ## 4260                         Germany   year85   77684873
    ## 4261                         Germany   year86   77720436
    ## 4262                         Germany   year87   77839920
    ## 4263                         Germany   year88   78144619
    ## 4264                         Germany   year89   78751283
    ## 4265                         Germany   year90   79433029
    ## 4266                         Germany   year91   80013896
    ## 4267                         Germany   year92   80624598
    ## 4268                         Germany   year93   81156363
    ## 4269                         Germany   year94   81438348
    ## 4270                         Germany   year95   81678051
    ## 4271                         Germany   year96   81914831
    ## 4272                         Germany   year97   82034771
    ## 4273                         Germany   year98   82047195
    ## 4274                         Germany   year99   82100243
    ## 4275                         Germany year2000   82211508
    ## 4276                         Germany year2001   82349925
    ## 4277                         Germany year2002   82488495
    ## 4278                         Germany year2003   82534176
    ## 4279                         Germany year2004   82516260
    ## 4280                         Germany year2005   82469422
    ## 4281                         Germany year2006   82376451
    ## 4282                         Germany year2007   82266372
    ## 4283                         Germany year2008   82110097
    ## 4284                         Germany year2009   81902307
    ## 4285                         Germany year2010   81776930
    ## 4286                         Germany year2011   80274983
    ## 4287                         Germany year2012   80425823
    ## 4288                         Germany year2013   80645605
    ## 4289                         Germany year2014   80982500
    ## 4290                         Germany year2015   81686611
    ## 4291                         Germany year2016   82348669
    ## 4292                         Germany year2017   82695000
    ## 4293                           Ghana   year60    6652287
    ## 4294                           Ghana   year61    6866539
    ## 4295                           Ghana   year62    7085464
    ## 4296                           Ghana   year63    7303432
    ## 4297                           Ghana   year64    7513289
    ## 4298                           Ghana   year65    7710549
    ## 4299                           Ghana   year66    7890992
    ## 4300                           Ghana   year67    8057444
    ## 4301                           Ghana   year68    8221020
    ## 4302                           Ghana   year69    8397347
    ## 4303                           Ghana   year70    8596983
    ## 4304                           Ghana   year71    8827273
    ## 4305                           Ghana   year72    9083573
    ## 4306                           Ghana   year73    9350111
    ## 4307                           Ghana   year74    9604276
    ## 4308                           Ghana   year75    9831407
    ## 4309                           Ghana   year76   10023472
    ## 4310                           Ghana   year77   10189890
    ## 4311                           Ghana   year78   10354499
    ## 4312                           Ghana   year79   10550777
    ## 4313                           Ghana   year80   10802028
    ## 4314                           Ghana   year81   11117605
    ## 4315                           Ghana   year82   11488106
    ## 4316                           Ghana   year83   11895125
    ## 4317                           Ghana   year84   12311158
    ## 4318                           Ghana   year85   12716228
    ## 4319                           Ghana   year86   13104296
    ## 4320                           Ghana   year87   13481406
    ## 4321                           Ghana   year88   13854214
    ## 4322                           Ghana   year89   14233874
    ## 4323                           Ghana   year90   14628260
    ## 4324                           Ghana   year91   15039514
    ## 4325                           Ghana   year92   15463854
    ## 4326                           Ghana   year93   15896432
    ## 4327                           Ghana   year94   16330174
    ## 4328                           Ghana   year95   16760467
    ## 4329                           Ghana   year96   17185608
    ## 4330                           Ghana   year97   17608812
    ## 4331                           Ghana   year98   18036494
    ## 4332                           Ghana   year99   18477612
    ## 4333                           Ghana year2000   18938762
    ## 4334                           Ghana year2001   19421605
    ## 4335                           Ghana year2002   19924522
    ## 4336                           Ghana year2003   20446782
    ## 4337                           Ghana year2004   20986536
    ## 4338                           Ghana year2005   21542009
    ## 4339                           Ghana year2006   22113425
    ## 4340                           Ghana year2007   22700212
    ## 4341                           Ghana year2008   23298640
    ## 4342                           Ghana year2009   23903831
    ## 4343                           Ghana year2010   24512104
    ## 4344                           Ghana year2011   25121796
    ## 4345                           Ghana year2012   25733049
    ## 4346                           Ghana year2013   26346251
    ## 4347                           Ghana year2014   26962563
    ## 4348                           Ghana year2015   27582821
    ## 4349                           Ghana year2016   28206728
    ## 4350                           Ghana year2017   28833629
    ## 4351                       Gibraltar   year60      23394
    ## 4352                       Gibraltar   year61      23786
    ## 4353                       Gibraltar   year62      24284
    ## 4354                       Gibraltar   year63      24848
    ## 4355                       Gibraltar   year64      25454
    ## 4356                       Gibraltar   year65      26041
    ## 4357                       Gibraltar   year66      26612
    ## 4358                       Gibraltar   year67      27174
    ## 4359                       Gibraltar   year68      27694
    ## 4360                       Gibraltar   year69      28159
    ## 4361                       Gibraltar   year70      28560
    ## 4362                       Gibraltar   year71      28869
    ## 4363                       Gibraltar   year72      29104
    ## 4364                       Gibraltar   year73      29278
    ## 4365                       Gibraltar   year74      29427
    ## 4366                       Gibraltar   year75      29578
    ## 4367                       Gibraltar   year76      29742
    ## 4368                       Gibraltar   year77      29902
    ## 4369                       Gibraltar   year78      30049
    ## 4370                       Gibraltar   year79      30177
    ## 4371                       Gibraltar   year80      30272
    ## 4372                       Gibraltar   year81      30334
    ## 4373                       Gibraltar   year82      30381
    ## 4374                       Gibraltar   year83      30383
    ## 4375                       Gibraltar   year84      30325
    ## 4376                       Gibraltar   year85      30207
    ## 4377                       Gibraltar   year86      30004
    ## 4378                       Gibraltar   year87      29744
    ## 4379                       Gibraltar   year88      29469
    ## 4380                       Gibraltar   year89      29262
    ## 4381                       Gibraltar   year90      29164
    ## 4382                       Gibraltar   year91      29212
    ## 4383                       Gibraltar   year92      29379
    ## 4384                       Gibraltar   year93      29623
    ## 4385                       Gibraltar   year94      29895
    ## 4386                       Gibraltar   year95      30147
    ## 4387                       Gibraltar   year96      30382
    ## 4388                       Gibraltar   year97      30594
    ## 4389                       Gibraltar   year98      30801
    ## 4390                       Gibraltar   year99      30991
    ## 4391                       Gibraltar year2000      31180
    ## 4392                       Gibraltar year2001      31374
    ## 4393                       Gibraltar year2002      31544
    ## 4394                       Gibraltar year2003      31720
    ## 4395                       Gibraltar year2004      31896
    ## 4396                       Gibraltar year2005      32085
    ## 4397                       Gibraltar year2006      32296
    ## 4398                       Gibraltar year2007      32510
    ## 4399                       Gibraltar year2008      32732
    ## 4400                       Gibraltar year2009      32956
    ## 4401                       Gibraltar year2010      33189
    ## 4402                       Gibraltar year2011      33405
    ## 4403                       Gibraltar year2012      33623
    ## 4404                       Gibraltar year2013      33831
    ## 4405                       Gibraltar year2014      34038
    ## 4406                       Gibraltar year2015      34228
    ## 4407                       Gibraltar year2016      34408
    ## 4408                       Gibraltar year2017      34571
    ## 4409                          Greece   year60    8331725
    ## 4410                          Greece   year61    8398050
    ## 4411                          Greece   year62    8448233
    ## 4412                          Greece   year63    8479625
    ## 4413                          Greece   year64    8510429
    ## 4414                          Greece   year65    8550333
    ## 4415                          Greece   year66    8613651
    ## 4416                          Greece   year67    8684088
    ## 4417                          Greece   year68    8740765
    ## 4418                          Greece   year69    8772764
    ## 4419                          Greece   year70    8792806
    ## 4420                          Greece   year71    8831036
    ## 4421                          Greece   year72    8888628
    ## 4422                          Greece   year73    8929086
    ## 4423                          Greece   year74    8962022
    ## 4424                          Greece   year75    9046541
    ## 4425                          Greece   year76    9188150
    ## 4426                          Greece   year77    9308479
    ## 4427                          Greece   year78    9429959
    ## 4428                          Greece   year79    9548258
    ## 4429                          Greece   year80    9642505
    ## 4430                          Greece   year81    9729350
    ## 4431                          Greece   year82    9789513
    ## 4432                          Greece   year83    9846627
    ## 4433                          Greece   year84    9895801
    ## 4434                          Greece   year85    9934300
    ## 4435                          Greece   year86    9967213
    ## 4436                          Greece   year87   10000595
    ## 4437                          Greece   year88   10036983
    ## 4438                          Greece   year89   10089498
    ## 4439                          Greece   year90   10196792
    ## 4440                          Greece   year91   10319927
    ## 4441                          Greece   year92   10399061
    ## 4442                          Greece   year93   10460415
    ## 4443                          Greece   year94   10512922
    ## 4444                          Greece   year95   10562153
    ## 4445                          Greece   year96   10608800
    ## 4446                          Greece   year97   10661259
    ## 4447                          Greece   year98   10720509
    ## 4448                          Greece   year99   10761698
    ## 4449                          Greece year2000   10805808
    ## 4450                          Greece year2001   10862132
    ## 4451                          Greece year2002   10902022
    ## 4452                          Greece year2003   10928070
    ## 4453                          Greece year2004   10955141
    ## 4454                          Greece year2005   10987314
    ## 4455                          Greece year2006   11020362
    ## 4456                          Greece year2007   11048473
    ## 4457                          Greece year2008   11077841
    ## 4458                          Greece year2009   11107017
    ## 4459                          Greece year2010   11121341
    ## 4460                          Greece year2011   11104899
    ## 4461                          Greece year2012   11045011
    ## 4462                          Greece year2013   10965211
    ## 4463                          Greece year2014   10892413
    ## 4464                          Greece year2015   10820883
    ## 4465                          Greece year2016   10775971
    ## 4466                          Greece year2017   10760421
    ## 4467                       Greenland   year60      32500
    ## 4468                       Greenland   year61      33700
    ## 4469                       Greenland   year62      35000
    ## 4470                       Greenland   year63      36400
    ## 4471                       Greenland   year64      37600
    ## 4472                       Greenland   year65      39200
    ## 4473                       Greenland   year66      40500
    ## 4474                       Greenland   year67      41900
    ## 4475                       Greenland   year68      43400
    ## 4476                       Greenland   year69      44900
    ## 4477                       Greenland   year70      46400
    ## 4478                       Greenland   year71      47200
    ## 4479                       Greenland   year72      48300
    ## 4480                       Greenland   year73      49000
    ## 4481                       Greenland   year74      49500
    ## 4482                       Greenland   year75      49600
    ## 4483                       Greenland   year76      49700
    ## 4484                       Greenland   year77      49400
    ## 4485                       Greenland   year78      49200
    ## 4486                       Greenland   year79      49600
    ## 4487                       Greenland   year80      50200
    ## 4488                       Greenland   year81      51000
    ## 4489                       Greenland   year82      51500
    ## 4490                       Greenland   year83      52100
    ## 4491                       Greenland   year84      52700
    ## 4492                       Greenland   year85      53200
    ## 4493                       Greenland   year86      53500
    ## 4494                       Greenland   year87      54100
    ## 4495                       Greenland   year88      54800
    ## 4496                       Greenland   year89      55300
    ## 4497                       Greenland   year90      55600
    ## 4498                       Greenland   year91      55500
    ## 4499                       Greenland   year92      55300
    ## 4500                       Greenland   year93      55200
    ## 4501                       Greenland   year94      55500
    ## 4502                       Greenland   year95      55800
    ## 4503                       Greenland   year96      55900
    ## 4504                       Greenland   year97      56000
    ## 4505                       Greenland   year98      56100
    ## 4506                       Greenland   year99      56100
    ## 4507                       Greenland year2000      56200
    ## 4508                       Greenland year2001      56350
    ## 4509                       Greenland year2002      56609
    ## 4510                       Greenland year2003      56765
    ## 4511                       Greenland year2004      56911
    ## 4512                       Greenland year2005      56935
    ## 4513                       Greenland year2006      56774
    ## 4514                       Greenland year2007      56555
    ## 4515                       Greenland year2008      56328
    ## 4516                       Greenland year2009      56323
    ## 4517                       Greenland year2010      56905
    ## 4518                       Greenland year2011      56890
    ## 4519                       Greenland year2012      56810
    ## 4520                       Greenland year2013      56483
    ## 4521                       Greenland year2014      56295
    ## 4522                       Greenland year2015      56114
    ## 4523                       Greenland year2016      56186
    ## 4524                       Greenland year2017      56171
    ## 4525                         Grenada   year60      89869
    ## 4526                         Grenada   year61      91260
    ## 4527                         Grenada   year62      92425
    ## 4528                         Grenada   year63      93350
    ## 4529                         Grenada   year64      94066
    ## 4530                         Grenada   year65      94581
    ## 4531                         Grenada   year66      94875
    ## 4532                         Grenada   year67      94961
    ## 4533                         Grenada   year68      94868
    ## 4534                         Grenada   year69      94682
    ## 4535                         Grenada   year70      94426
    ## 4536                         Grenada   year71      94185
    ## 4537                         Grenada   year72      93934
    ## 4538                         Grenada   year73      93630
    ## 4539                         Grenada   year74      93152
    ## 4540                         Grenada   year75      92448
    ## 4541                         Grenada   year76      91437
    ## 4542                         Grenada   year77      90184
    ## 4543                         Grenada   year78      89073
    ## 4544                         Grenada   year79      88568
    ## 4545                         Grenada   year80      89005
    ## 4546                         Grenada   year81      90572
    ## 4547                         Grenada   year82      93091
    ## 4548                         Grenada   year83      95985
    ## 4549                         Grenada   year84      98439
    ## 4550                         Grenada   year85      99906
    ## 4551                         Grenada   year86     100143
    ## 4552                         Grenada   year87      99380
    ## 4553                         Grenada   year88      98062
    ## 4554                         Grenada   year89      96869
    ## 4555                         Grenada   year90      96283
    ## 4556                         Grenada   year91      96454
    ## 4557                         Grenada   year92      97198
    ## 4558                         Grenada   year93      98305
    ## 4559                         Grenada   year94      99405
    ## 4560                         Grenada   year95     100255
    ## 4561                         Grenada   year96     100796
    ## 4562                         Grenada   year97     101122
    ## 4563                         Grenada   year98     101309
    ## 4564                         Grenada   year99     101442
    ## 4565                         Grenada year2000     101619
    ## 4566                         Grenada year2001     101849
    ## 4567                         Grenada year2002     102100
    ## 4568                         Grenada year2003     102375
    ## 4569                         Grenada year2004     102656
    ## 4570                         Grenada year2005     102949
    ## 4571                         Grenada year2006     103259
    ## 4572                         Grenada year2007     103586
    ## 4573                         Grenada year2008     103930
    ## 4574                         Grenada year2009     104296
    ## 4575                         Grenada year2010     104677
    ## 4576                         Grenada year2011     105075
    ## 4577                         Grenada year2012     105481
    ## 4578                         Grenada year2013     105909
    ## 4579                         Grenada year2014     106360
    ## 4580                         Grenada year2015     106823
    ## 4581                         Grenada year2016     107317
    ## 4582                         Grenada year2017     107825
    ## 4583                            Guam   year60      66742
    ## 4584                            Guam   year61      68072
    ## 4585                            Guam   year62      69604
    ## 4586                            Guam   year63      71286
    ## 4587                            Guam   year64      73051
    ## 4588                            Guam   year65      74830
    ## 4589                            Guam   year66      76607
    ## 4590                            Guam   year67      78404
    ## 4591                            Guam   year68      80217
    ## 4592                            Guam   year69      82040
    ## 4593                            Guam   year70      83877
    ## 4594                            Guam   year71      85726
    ## 4595                            Guam   year72      87587
    ## 4596                            Guam   year73      89464
    ## 4597                            Guam   year74      91377
    ## 4598                            Guam   year75      93352
    ## 4599                            Guam   year76      95385
    ## 4600                            Guam   year77      97477
    ## 4601                            Guam   year78      99630
    ## 4602                            Guam   year79     101844
    ## 4603                            Guam   year80     104133
    ## 4604                            Guam   year81     106485
    ## 4605                            Guam   year82     108906
    ## 4606                            Guam   year83     111402
    ## 4607                            Guam   year84     113961
    ## 4608                            Guam   year85     116572
    ## 4609                            Guam   year86     119232
    ## 4610                            Guam   year87     121919
    ## 4611                            Guam   year88     124673
    ## 4612                            Guam   year89     127522
    ## 4613                            Guam   year90     130482
    ## 4614                            Guam   year91     133558
    ## 4615                            Guam   year92     136692
    ## 4616                            Guam   year93     139818
    ## 4617                            Guam   year94     142802
    ## 4618                            Guam   year95     145561
    ## 4619                            Guam   year96     148060
    ## 4620                            Guam   year97     150303
    ## 4621                            Guam   year98     152277
    ## 4622                            Guam   year99     153953
    ## 4623                            Guam year2000     155329
    ## 4624                            Guam year2001     156401
    ## 4625                            Guam year2002     157175
    ## 4626                            Guam year2003     157714
    ## 4627                            Guam year2004     158099
    ## 4628                            Guam year2005     158402
    ## 4629                            Guam year2006     158648
    ## 4630                            Guam year2007     158855
    ## 4631                            Guam year2008     159035
    ## 4632                            Guam year2009     159231
    ## 4633                            Guam year2010     159444
    ## 4634                            Guam year2011     159678
    ## 4635                            Guam year2012     159973
    ## 4636                            Guam year2013     160375
    ## 4637                            Guam year2014     160967
    ## 4638                            Guam year2015     161797
    ## 4639                            Guam year2016     162896
    ## 4640                            Guam year2017     164229
    ## 4641                       Guatemala   year60    4210747
    ## 4642                       Guatemala   year61    4336143
    ## 4643                       Guatemala   year62    4464249
    ## 4644                       Guatemala   year63    4595510
    ## 4645                       Guatemala   year64    4730540
    ## 4646                       Guatemala   year65    4869716
    ## 4647                       Guatemala   year66    5013153
    ## 4648                       Guatemala   year67    5160609
    ## 4649                       Guatemala   year68    5311615
    ## 4650                       Guatemala   year69    5465512
    ## 4651                       Guatemala   year70    5621792
    ## 4652                       Guatemala   year71    5780480
    ## 4653                       Guatemala   year72    5941567
    ## 4654                       Guatemala   year73    6104530
    ## 4655                       Guatemala   year74    6268707
    ## 4656                       Guatemala   year75    6433728
    ## 4657                       Guatemala   year76    6599214
    ## 4658                       Guatemala   year77    6765516
    ## 4659                       Guatemala   year78    6933906
    ## 4660                       Guatemala   year79    7106145
    ## 4661                       Guatemala   year80    7283459
    ## 4662                       Guatemala   year81    7466488
    ## 4663                       Guatemala   year82    7654819
    ## 4664                       Guatemala   year83    7847472
    ## 4665                       Guatemala   year84    8042897
    ## 4666                       Guatemala   year85    8240060
    ## 4667                       Guatemala   year86    8438604
    ## 4668                       Guatemala   year87    8639108
    ## 4669                       Guatemala   year88    8842575
    ## 4670                       Guatemala   year89    9050465
    ## 4671                       Guatemala   year90    9263813
    ## 4672                       Guatemala   year91    9483270
    ## 4673                       Guatemala   year92    9708544
    ## 4674                       Guatemala   year93    9938692
    ## 4675                       Guatemala   year94   10172297
    ## 4676                       Guatemala   year95   10408489
    ## 4677                       Guatemala   year96   10646674
    ## 4678                       Guatemala   year97   10887634
    ## 4679                       Guatemala   year98   11133501
    ## 4680                       Guatemala   year99   11387203
    ## 4681                       Guatemala year2000   11650743
    ## 4682                       Guatemala year2001   11924946
    ## 4683                       Guatemala year2002   12208848
    ## 4684                       Guatemala year2003   12500478
    ## 4685                       Guatemala year2004   12796925
    ## 4686                       Guatemala year2005   13096028
    ## 4687                       Guatemala year2006   13397008
    ## 4688                       Guatemala year2007   13700286
    ## 4689                       Guatemala year2008   14006366
    ## 4690                       Guatemala year2009   14316208
    ## 4691                       Guatemala year2010   14630417
    ## 4692                       Guatemala year2011   14948919
    ## 4693                       Guatemala year2012   15271056
    ## 4694                       Guatemala year2013   15596214
    ## 4695                       Guatemala year2014   15923559
    ## 4696                       Guatemala year2015   16252429
    ## 4697                       Guatemala year2016   16582469
    ## 4698                       Guatemala year2017   16913503
    ## 4699                          Guinea   year60    3577409
    ## 4700                          Guinea   year61    3633652
    ## 4701                          Guinea   year62    3690664
    ## 4702                          Guinea   year63    3749505
    ## 4703                          Guinea   year64    3811659
    ## 4704                          Guinea   year65    3877806
    ## 4705                          Guinea   year66    3948869
    ## 4706                          Guinea   year67    4023486
    ## 4707                          Guinea   year68    4097191
    ## 4708                          Guinea   year69    4164003
    ## 4709                          Guinea   year70    4219770
    ## 4710                          Guinea   year71    4263840
    ## 4711                          Guinea   year72    4298091
    ## 4712                          Guinea   year73    4324360
    ## 4713                          Guinea   year74    4345545
    ## 4714                          Guinea   year75    4364514
    ## 4715                          Guinea   year76    4381601
    ## 4716                          Guinea   year77    4398484
    ## 4717                          Guinea   year78    4421134
    ## 4718                          Guinea   year79    4457078
    ## 4719                          Guinea   year80    4511902
    ## 4720                          Guinea   year81    4589784
    ## 4721                          Guinea   year82    4690605
    ## 4722                          Guinea   year83    4810496
    ## 4723                          Guinea   year84    4943144
    ## 4724                          Guinea   year85    5084767
    ## 4725                          Guinea   year86    5229797
    ## 4726                          Guinea   year87    5381483
    ## 4727                          Guinea   year88    5554882
    ## 4728                          Guinea   year89    5770652
    ## 4729                          Guinea   year90    6041094
    ## 4730                          Guinea   year91    6374329
    ## 4731                          Guinea   year92    6758838
    ## 4732                          Guinea   year93    7163236
    ## 4733                          Guinea   year94    7544291
    ## 4734                          Guinea   year95    7871173
    ## 4735                          Guinea   year96    8132552
    ## 4736                          Guinea   year97    8337988
    ## 4737                          Guinea   year98    8503297
    ## 4738                          Guinea   year99    8653769
    ## 4739                          Guinea year2000    8808546
    ## 4740                          Guinea year2001    8971139
    ## 4741                          Guinea year2002    9137345
    ## 4742                          Guinea year2003    9309848
    ## 4743                          Guinea year2004    9490229
    ## 4744                          Guinea year2005    9679745
    ## 4745                          Guinea year2006    9881428
    ## 4746                          Guinea year2007   10096727
    ## 4747                          Guinea year2008   10323142
    ## 4748                          Guinea year2009   10556524
    ## 4749                          Guinea year2010   10794170
    ## 4750                          Guinea year2011   11035170
    ## 4751                          Guinea year2012   11281469
    ## 4752                          Guinea year2013   11536615
    ## 4753                          Guinea year2014   11805509
    ## 4754                          Guinea year2015   12091533
    ## 4755                          Guinea year2016   12395924
    ## 4756                          Guinea year2017   12717176
    ## 4757                   Guinea-Bissau   year60     616409
    ## 4758                   Guinea-Bissau   year61     623415
    ## 4759                   Guinea-Bissau   year62     629969
    ## 4760                   Guinea-Bissau   year63     636586
    ## 4761                   Guinea-Bissau   year64     643961
    ## 4762                   Guinea-Bissau   year65     652562
    ## 4763                   Guinea-Bissau   year66     662463
    ## 4764                   Guinea-Bissau   year67     673462
    ## 4765                   Guinea-Bissau   year68     685476
    ## 4766                   Guinea-Bissau   year69     698338
    ## 4767                   Guinea-Bissau   year70     711827
    ## 4768                   Guinea-Bissau   year71     726256
    ## 4769                   Guinea-Bissau   year72     741490
    ## 4770                   Guinea-Bissau   year73     756280
    ## 4771                   Guinea-Bissau   year74     768945
    ## 4772                   Guinea-Bissau   year75     778470
    ## 4773                   Guinea-Bissau   year76     784156
    ## 4774                   Guinea-Bissau   year77     786754
    ## 4775                   Guinea-Bissau   year78     788495
    ## 4776                   Guinea-Bissau   year79     792462
    ## 4777                   Guinea-Bissau   year80     800854
    ## 4778                   Guinea-Bissau   year81     814507
    ## 4779                   Guinea-Bissau   year82     832668
    ## 4780                   Guinea-Bissau   year83     854113
    ## 4781                   Guinea-Bissau   year84     876873
    ## 4782                   Guinea-Bissau   year85     899509
    ## 4783                   Guinea-Bissau   year86     921626
    ## 4784                   Guinea-Bissau   year87     943617
    ## 4785                   Guinea-Bissau   year88     965742
    ## 4786                   Guinea-Bissau   year89     988520
    ## 4787                   Guinea-Bissau   year90    1012280
    ## 4788                   Guinea-Bissau   year91    1037155
    ## 4789                   Guinea-Bissau   year92    1062800
    ## 4790                   Guinea-Bissau   year93    1088569
    ## 4791                   Guinea-Bissau   year94    1113541
    ## 4792                   Guinea-Bissau   year95    1137122
    ## 4793                   Guinea-Bissau   year96    1159060
    ## 4794                   Guinea-Bissau   year97    1179727
    ## 4795                   Guinea-Bissau   year98    1199915
    ## 4796                   Guinea-Bissau   year99    1220794
    ## 4797                   Guinea-Bissau year2000    1243229
    ## 4798                   Guinea-Bissau year2001    1267512
    ## 4799                   Guinea-Bissau year2002    1293523
    ## 4800                   Guinea-Bissau year2003    1321202
    ## 4801                   Guinea-Bissau year2004    1350345
    ## 4802                   Guinea-Bissau year2005    1380838
    ## 4803                   Guinea-Bissau year2006    1412669
    ## 4804                   Guinea-Bissau year2007    1445958
    ## 4805                   Guinea-Bissau year2008    1480841
    ## 4806                   Guinea-Bissau year2009    1517448
    ## 4807                   Guinea-Bissau year2010    1555880
    ## 4808                   Guinea-Bissau year2011    1596154
    ## 4809                   Guinea-Bissau year2012    1638139
    ## 4810                   Guinea-Bissau year2013    1681495
    ## 4811                   Guinea-Bissau year2014    1725744
    ## 4812                   Guinea-Bissau year2015    1770526
    ## 4813                   Guinea-Bissau year2016    1815698
    ## 4814                   Guinea-Bissau year2017    1861283
    ## 4815                          Guyana   year60     571819
    ## 4816                          Guyana   year61     589274
    ## 4817                          Guyana   year62     606285
    ## 4818                          Guyana   year63     622575
    ## 4819                          Guyana   year64     637845
    ## 4820                          Guyana   year65     651868
    ## 4821                          Guyana   year66     664521
    ## 4822                          Guyana   year67     675871
    ## 4823                          Guyana   year68     686146
    ## 4824                          Guyana   year69     695745
    ## 4825                          Guyana   year70     704934
    ## 4826                          Guyana   year71     713684
    ## 4827                          Guyana   year72     721948
    ## 4828                          Guyana   year73     729916
    ## 4829                          Guyana   year74     737847
    ## 4830                          Guyana   year75     745841
    ## 4831                          Guyana   year76     754101
    ## 4832                          Guyana   year77     762424
    ## 4833                          Guyana   year78     770125
    ## 4834                          Guyana   year79     776254
    ## 4835                          Guyana   year80     780153
    ## 4836                          Guyana   year81     781732
    ## 4837                          Guyana   year82     781246
    ## 4838                          Guyana   year83     778948
    ## 4839                          Guyana   year84     775219
    ## 4840                          Guyana   year85     770435
    ## 4841                          Guyana   year86     764459
    ## 4842                          Guyana   year87     757506
    ## 4843                          Guyana   year88     750731
    ## 4844                          Guyana   year89     745665
    ## 4845                          Guyana   year90     743309
    ## 4846                          Guyana   year91     744289
    ## 4847                          Guyana   year92     748134
    ## 4848                          Guyana   year93     753484
    ## 4849                          Guyana   year94     758342
    ## 4850                          Guyana   year95     761291
    ## 4851                          Guyana   year96     761861
    ## 4852                          Guyana   year97     760510
    ## 4853                          Guyana   year98     757952
    ## 4854                          Guyana   year99     755278
    ## 4855                          Guyana year2000     753301
    ## 4856                          Guyana year2001     752263
    ## 4857                          Guyana year2002     751884
    ## 4858                          Guyana year2003     751857
    ## 4859                          Guyana year2004     751652
    ## 4860                          Guyana year2005     750946
    ## 4861                          Guyana year2006     749601
    ## 4862                          Guyana year2007     747869
    ## 4863                          Guyana year2008     746314
    ## 4864                          Guyana year2009     745693
    ## 4865                          Guyana year2010     746556
    ## 4866                          Guyana year2011     749100
    ## 4867                          Guyana year2012     753091
    ## 4868                          Guyana year2013     758081
    ## 4869                          Guyana year2014     763393
    ## 4870                          Guyana year2015     768514
    ## 4871                          Guyana year2016     773303
    ## 4872                          Guyana year2017     777859
    ## 4873                           Haiti   year60    3866159
    ## 4874                           Haiti   year61    3943364
    ## 4875                           Haiti   year62    4022593
    ## 4876                           Haiti   year63    4103730
    ## 4877                           Haiti   year64    4186640
    ## 4878                           Haiti   year65    4271133
    ## 4879                           Haiti   year66    4357484
    ## 4880                           Haiti   year67    4445530
    ## 4881                           Haiti   year68    4534234
    ## 4882                           Haiti   year69    4622208
    ## 4883                           Haiti   year70    4708642
    ## 4884                           Haiti   year71    4793155
    ## 4885                           Haiti   year72    4876560
    ## 4886                           Haiti   year73    4960657
    ## 4887                           Haiti   year74    5047944
    ## 4888                           Haiti   year75    5140357
    ## 4889                           Haiti   year76    5238245
    ## 4890                           Haiti   year77    5341419
    ## 4891                           Haiti   year78    5450549
    ## 4892                           Haiti   year79    5566266
    ## 4893                           Haiti   year80    5688836
    ## 4894                           Haiti   year81    5818671
    ## 4895                           Haiti   year82    5955267
    ## 4896                           Haiti   year83    6096692
    ## 4897                           Haiti   year84    6240329
    ## 4898                           Haiti   year85    6384195
    ## 4899                           Haiti   year86    6527543
    ## 4900                           Haiti   year87    6670568
    ## 4901                           Haiti   year88    6813348
    ## 4902                           Haiti   year89    6956300
    ## 4903                           Haiti   year90    7099732
    ## 4904                           Haiti   year91    7243391
    ## 4905                           Haiti   year92    7386975
    ## 4906                           Haiti   year93    7530705
    ## 4907                           Haiti   year94    7674911
    ## 4908                           Haiti   year95    7819806
    ## 4909                           Haiti   year96    7965553
    ## 4910                           Haiti   year97    8111951
    ## 4911                           Haiti   year98    8258483
    ## 4912                           Haiti   year99    8404398
    ## 4913                           Haiti year2000    8549200
    ## 4914                           Haiti year2001    8692567
    ## 4915                           Haiti year2002    8834733
    ## 4916                           Haiti year2003    8976552
    ## 4917                           Haiti year2004    9119178
    ## 4918                           Haiti year2005    9263404
    ## 4919                           Haiti year2006    9409457
    ## 4920                           Haiti year2007    9556889
    ## 4921                           Haiti year2008    9705029
    ## 4922                           Haiti year2009    9852870
    ## 4923                           Haiti year2010    9999617
    ## 4924                           Haiti year2011   10145054
    ## 4925                           Haiti year2012   10289210
    ## 4926                           Haiti year2013   10431776
    ## 4927                           Haiti year2014   10572466
    ## 4928                           Haiti year2015   10711061
    ## 4929                           Haiti year2016   10847334
    ## 4930                           Haiti year2017   10981229
    ## 4931                     High income   year60  780501923
    ## 4932                     High income   year61  792246929
    ## 4933                     High income   year62  802642237
    ## 4934                     High income   year63  812955277
    ## 4935                     High income   year64  823154587
    ## 4936                     High income   year65  832959686
    ## 4937                     High income   year66  842127682
    ## 4938                     High income   year67  850904554
    ## 4939                     High income   year68  858706694
    ## 4940                     High income   year69  868224174
    ## 4941                     High income   year70  876786721
    ## 4942                     High income   year71  886003787
    ## 4943                     High income   year72  895387452
    ## 4944                     High income   year73  903912425
    ## 4945                     High income   year74  913510969
    ## 4946                     High income   year75  922573384
    ## 4947                     High income   year76  930171314
    ## 4948                     High income   year77  937990304
    ## 4949                     High income   year78  945879993
    ## 4950                     High income   year79  954142946
    ## 4951                     High income   year80  962228266
    ## 4952                     High income   year81  970338016
    ## 4953                     High income   year82  978056271
    ## 4954                     High income   year83  985349949
    ## 4955                     High income   year84  992295742
    ## 4956                     High income   year85  999278230
    ## 4957                     High income   year86 1006551028
    ## 4958                     High income   year87 1013806386
    ## 4959                     High income   year88 1021209656
    ## 4960                     High income   year89 1029042492
    ## 4961                     High income   year90 1037334467
    ## 4962                     High income   year91 1045799408
    ## 4963                     High income   year92 1052656811
    ## 4964                     High income   year93 1061248480
    ## 4965                     High income   year94 1069147445
    ## 4966                     High income   year95 1078558586
    ## 4967                     High income   year96 1086071661
    ## 4968                     High income   year97 1093603568
    ## 4969                     High income   year98 1100763765
    ## 4970                     High income   year99 1108002668
    ## 4971                     High income year2000 1115010208
    ## 4972                     High income year2001 1122635089
    ## 4973                     High income year2002 1130299527
    ## 4974                     High income year2003 1137953182
    ## 4975                     High income year2004 1145971873
    ## 4976                     High income year2005 1154156034
    ## 4977                     High income year2006 1162908282
    ## 4978                     High income year2007 1172156434
    ## 4979                     High income year2008 1181962982
    ## 4980                     High income year2009 1190790909
    ## 4981                     High income year2010 1198787232
    ## 4982                     High income year2011 1204631343
    ## 4983                     High income year2012 1212058100
    ## 4984                     High income year2013 1219556921
    ## 4985                     High income year2014 1227211897
    ## 4986                     High income year2015 1234714041
    ## 4987                     High income year2016 1242137612
    ## 4988                     High income year2017 1249066228
    ## 4989                        Honduras   year60    2038637
    ## 4990                        Honduras   year61    2096407
    ## 4991                        Honduras   year62    2155652
    ## 4992                        Honduras   year63    2216707
    ## 4993                        Honduras   year64    2280045
    ## 4994                        Honduras   year65    2346010
    ## 4995                        Honduras   year66    2414807
    ## 4996                        Honduras   year67    2486414
    ## 4997                        Honduras   year68    2560727
    ## 4998                        Honduras   year69    2637517
    ## 4999                        Honduras   year70    2716659
    ## 5000                        Honduras   year71    2798125
    ## 5001                        Honduras   year72    2882113
    ## 5002                        Honduras   year73    2968994
    ## 5003                        Honduras   year74    3059254
    ## 5004                        Honduras   year75    3153261
    ## 5005                        Honduras   year76    3251158
    ## 5006                        Honduras   year77    3352835
    ## 5007                        Honduras   year78    3458104
    ## 5008                        Honduras   year79    3566665
    ## 5009                        Honduras   year80    3678286
    ## 5010                        Honduras   year81    3792938
    ## 5011                        Honduras   year82    3910657
    ## 5012                        Honduras   year83    4031349
    ## 5013                        Honduras   year84    4154887
    ## 5014                        Honduras   year85    4281189
    ## 5015                        Honduras   year86    4410158
    ## 5016                        Honduras   year87    4541804
    ## 5017                        Honduras   year88    4676361
    ## 5018                        Honduras   year89    4814137
    ## 5019                        Honduras   year90    4955328
    ## 5020                        Honduras   year91    5099951
    ## 5021                        Honduras   year92    5247836
    ## 5022                        Honduras   year93    5398805
    ## 5023                        Honduras   year94    5552625
    ## 5024                        Honduras   year95    5709051
    ## 5025                        Honduras   year96    5867849
    ## 5026                        Honduras   year97    6028882
    ## 5027                        Honduras   year98    6192026
    ## 5028                        Honduras   year99    6357221
    ## 5029                        Honduras year2000    6524283
    ## 5030                        Honduras year2001    6693061
    ## 5031                        Honduras year2002    6863157
    ## 5032                        Honduras year2003    7033821
    ## 5033                        Honduras year2004    7204153
    ## 5034                        Honduras year2005    7373430
    ## 5035                        Honduras year2006    7541406
    ## 5036                        Honduras year2007    7707972
    ## 5037                        Honduras year2008    7872658
    ## 5038                        Honduras year2009    8035021
    ## 5039                        Honduras year2010    8194778
    ## 5040                        Honduras year2011    8351600
    ## 5041                        Honduras year2012    8505646
    ## 5042                        Honduras year2013    8657785
    ## 5043                        Honduras year2014    8809216
    ## 5044                        Honduras year2015    8960829
    ## 5045                        Honduras year2016    9112867
    ## 5046                        Honduras year2017    9265067
    ## 5047            Hong Kong SAR, China   year60    3075605
    ## 5048            Hong Kong SAR, China   year61    3168100
    ## 5049            Hong Kong SAR, China   year62    3305200
    ## 5050            Hong Kong SAR, China   year63    3420900
    ## 5051            Hong Kong SAR, China   year64    3504600
    ## 5052            Hong Kong SAR, China   year65    3597900
    ## 5053            Hong Kong SAR, China   year66    3629900
    ## 5054            Hong Kong SAR, China   year67    3722800
    ## 5055            Hong Kong SAR, China   year68    3802700
    ## 5056            Hong Kong SAR, China   year69    3863900
    ## 5057            Hong Kong SAR, China   year70    3959000
    ## 5058            Hong Kong SAR, China   year71    4045300
    ## 5059            Hong Kong SAR, China   year72    4123600
    ## 5060            Hong Kong SAR, China   year73    4241600
    ## 5061            Hong Kong SAR, China   year74    4377800
    ## 5062            Hong Kong SAR, China   year75    4461600
    ## 5063            Hong Kong SAR, China   year76    4518000
    ## 5064            Hong Kong SAR, China   year77    4583700
    ## 5065            Hong Kong SAR, China   year78    4667500
    ## 5066            Hong Kong SAR, China   year79    4929700
    ## 5067            Hong Kong SAR, China   year80    5063100
    ## 5068            Hong Kong SAR, China   year81    5183400
    ## 5069            Hong Kong SAR, China   year82    5264500
    ## 5070            Hong Kong SAR, China   year83    5345100
    ## 5071            Hong Kong SAR, China   year84    5397900
    ## 5072            Hong Kong SAR, China   year85    5456200
    ## 5073            Hong Kong SAR, China   year86    5524600
    ## 5074            Hong Kong SAR, China   year87    5580500
    ## 5075            Hong Kong SAR, China   year88    5627600
    ## 5076            Hong Kong SAR, China   year89    5686200
    ## 5077            Hong Kong SAR, China   year90    5704500
    ## 5078            Hong Kong SAR, China   year91    5752000
    ## 5079            Hong Kong SAR, China   year92    5800500
    ## 5080            Hong Kong SAR, China   year93    5901000
    ## 5081            Hong Kong SAR, China   year94    6035400
    ## 5082            Hong Kong SAR, China   year95    6156100
    ## 5083            Hong Kong SAR, China   year96    6435500
    ## 5084            Hong Kong SAR, China   year97    6489300
    ## 5085            Hong Kong SAR, China   year98    6543700
    ## 5086            Hong Kong SAR, China   year99    6606500
    ## 5087            Hong Kong SAR, China year2000    6665000
    ## 5088            Hong Kong SAR, China year2001    6714300
    ## 5089            Hong Kong SAR, China year2002    6744100
    ## 5090            Hong Kong SAR, China year2003    6730800
    ## 5091            Hong Kong SAR, China year2004    6783500
    ## 5092            Hong Kong SAR, China year2005    6813200
    ## 5093            Hong Kong SAR, China year2006    6857100
    ## 5094            Hong Kong SAR, China year2007    6916300
    ## 5095            Hong Kong SAR, China year2008    6957800
    ## 5096            Hong Kong SAR, China year2009    6972800
    ## 5097            Hong Kong SAR, China year2010    7024200
    ## 5098            Hong Kong SAR, China year2011    7071600
    ## 5099            Hong Kong SAR, China year2012    7150100
    ## 5100            Hong Kong SAR, China year2013    7178900
    ## 5101            Hong Kong SAR, China year2014    7229500
    ## 5102            Hong Kong SAR, China year2015    7291300
    ## 5103            Hong Kong SAR, China year2016    7336600
    ## 5104            Hong Kong SAR, China year2017    7391700
    ## 5105                         Hungary   year60    9983967
    ## 5106                         Hungary   year61   10029321
    ## 5107                         Hungary   year62   10061734
    ## 5108                         Hungary   year63   10087947
    ## 5109                         Hungary   year64   10119835
    ## 5110                         Hungary   year65   10147935
    ## 5111                         Hungary   year66   10178653
    ## 5112                         Hungary   year67   10216604
    ## 5113                         Hungary   year68   10255815
    ## 5114                         Hungary   year69   10298723
    ## 5115                         Hungary   year70   10337910
    ## 5116                         Hungary   year71   10367537
    ## 5117                         Hungary   year72   10398489
    ## 5118                         Hungary   year73   10432055
    ## 5119                         Hungary   year74   10478720
    ## 5120                         Hungary   year75   10540525
    ## 5121                         Hungary   year76   10598677
    ## 5122                         Hungary   year77   10648031
    ## 5123                         Hungary   year78   10684822
    ## 5124                         Hungary   year79   10704152
    ## 5125                         Hungary   year80   10711122
    ## 5126                         Hungary   year81   10711848
    ## 5127                         Hungary   year82   10705535
    ## 5128                         Hungary   year83   10689463
    ## 5129                         Hungary   year84   10668095
    ## 5130                         Hungary   year85   10648713
    ## 5131                         Hungary   year86   10630564
    ## 5132                         Hungary   year87   10612741
    ## 5133                         Hungary   year88   10596487
    ## 5134                         Hungary   year89   10481719
    ## 5135                         Hungary   year90   10373988
    ## 5136                         Hungary   year91   10373400
    ## 5137                         Hungary   year92   10369341
    ## 5138                         Hungary   year93   10357523
    ## 5139                         Hungary   year94   10343355
    ## 5140                         Hungary   year95   10328965
    ## 5141                         Hungary   year96   10311238
    ## 5142                         Hungary   year97   10290486
    ## 5143                         Hungary   year98   10266570
    ## 5144                         Hungary   year99   10237530
    ## 5145                         Hungary year2000   10210971
    ## 5146                         Hungary year2001   10187576
    ## 5147                         Hungary year2002   10158608
    ## 5148                         Hungary year2003   10129552
    ## 5149                         Hungary year2004   10107146
    ## 5150                         Hungary year2005   10087065
    ## 5151                         Hungary year2006   10071370
    ## 5152                         Hungary year2007   10055780
    ## 5153                         Hungary year2008   10038188
    ## 5154                         Hungary year2009   10022650
    ## 5155                         Hungary year2010   10000023
    ## 5156                         Hungary year2011    9971727
    ## 5157                         Hungary year2012    9920362
    ## 5158                         Hungary year2013    9893082
    ## 5159                         Hungary year2014    9866468
    ## 5160                         Hungary year2015    9843028
    ## 5161                         Hungary year2016    9814023
    ## 5162                         Hungary year2017    9781127
    ## 5163                         Iceland   year60     175574
    ## 5164                         Iceland   year61     179029
    ## 5165                         Iceland   year62     182378
    ## 5166                         Iceland   year63     185653
    ## 5167                         Iceland   year64     188983
    ## 5168                         Iceland   year65     192286
    ## 5169                         Iceland   year66     195570
    ## 5170                         Iceland   year67     198751
    ## 5171                         Iceland   year68     201488
    ## 5172                         Iceland   year69     203369
    ## 5173                         Iceland   year70     204438
    ## 5174                         Iceland   year71     206098
    ## 5175                         Iceland   year72     209137
    ## 5176                         Iceland   year73     212317
    ## 5177                         Iceland   year74     215209
    ## 5178                         Iceland   year75     217979
    ## 5179                         Iceland   year76     220154
    ## 5180                         Iceland   year77     221799
    ## 5181                         Iceland   year78     223537
    ## 5182                         Iceland   year79     225735
    ## 5183                         Iceland   year80     228138
    ## 5184                         Iceland   year81     230755
    ## 5185                         Iceland   year82     233860
    ## 5186                         Iceland   year83     236977
    ## 5187                         Iceland   year84     239511
    ## 5188                         Iceland   year85     241405
    ## 5189                         Iceland   year86     243180
    ## 5190                         Iceland   year87     245859
    ## 5191                         Iceland   year88     249740
    ## 5192                         Iceland   year89     252852
    ## 5193                         Iceland   year90     254826
    ## 5194                         Iceland   year91     257797
    ## 5195                         Iceland   year92     261057
    ## 5196                         Iceland   year93     263725
    ## 5197                         Iceland   year94     266021
    ## 5198                         Iceland   year95     267468
    ## 5199                         Iceland   year96     268916
    ## 5200                         Iceland   year97     271128
    ## 5201                         Iceland   year98     274047
    ## 5202                         Iceland   year99     277381
    ## 5203                         Iceland year2000     281205
    ## 5204                         Iceland year2001     284968
    ## 5205                         Iceland year2002     287523
    ## 5206                         Iceland year2003     289521
    ## 5207                         Iceland year2004     292074
    ## 5208                         Iceland year2005     296734
    ## 5209                         Iceland year2006     303782
    ## 5210                         Iceland year2007     311566
    ## 5211                         Iceland year2008     317414
    ## 5212                         Iceland year2009     318499
    ## 5213                         Iceland year2010     318041
    ## 5214                         Iceland year2011     319014
    ## 5215                         Iceland year2012     320716
    ## 5216                         Iceland year2013     323764
    ## 5217                         Iceland year2014     327386
    ## 5218                         Iceland year2015     330815
    ## 5219                         Iceland year2016     335439
    ## 5220                         Iceland year2017     341284
    ## 5221                           India   year60  449480608
    ## 5222                           India   year61  458494963
    ## 5223                           India   year62  467852537
    ## 5224                           India   year63  477527970
    ## 5225                           India   year64  487484535
    ## 5226                           India   year65  497702365
    ## 5227                           India   year66  508161935
    ## 5228                           India   year67  518889779
    ## 5229                           India   year68  529967317
    ## 5230                           India   year69  541505076
    ## 5231                           India   year70  553578513
    ## 5232                           India   year71  566224812
    ## 5233                           India   year72  579411513
    ## 5234                           India   year73  593058926
    ## 5235                           India   year74  607050255
    ## 5236                           India   year75  621301720
    ## 5237                           India   year76  635771734
    ## 5238                           India   year77  650485030
    ## 5239                           India   year78  665502284
    ## 5240                           India   year79  680915804
    ## 5241                           India   year80  696783517
    ## 5242                           India   year81  713118032
    ## 5243                           India   year82  729868013
    ## 5244                           India   year83  746949067
    ## 5245                           India   year84  764245202
    ## 5246                           India   year85  781666671
    ## 5247                           India   year86  799181436
    ## 5248                           India   year87  816792741
    ## 5249                           India   year88  834489322
    ## 5250                           India   year89  852270034
    ## 5251                           India   year90  870133480
    ## 5252                           India   year91  888054875
    ## 5253                           India   year92  906021106
    ## 5254                           India   year93  924057817
    ## 5255                           India   year94  942204249
    ## 5256                           India   year95  960482795
    ## 5257                           India   year96  978893217
    ## 5258                           India   year97  997405318
    ## 5259                           India   year98 1015974042
    ## 5260                           India   year99 1034539214
    ## 5261                           India year2000 1053050912
    ## 5262                           India year2001 1071477855
    ## 5263                           India year2002 1089807112
    ## 5264                           India year2003 1108027848
    ## 5265                           India year2004 1126135777
    ## 5266                           India year2005 1144118674
    ## 5267                           India year2006 1161977719
    ## 5268                           India year2007 1179681239
    ## 5269                           India year2008 1197146906
    ## 5270                           India year2009 1214270132
    ## 5271                           India year2010 1230980691
    ## 5272                           India year2011 1247236029
    ## 5273                           India year2012 1263065852
    ## 5274                           India year2013 1278562207
    ## 5275                           India year2014 1293859294
    ## 5276                           India year2015 1309053980
    ## 5277                           India year2016 1324171354
    ## 5278                           India year2017 1339180127
    ## 5279                       Indonesia   year60   87792515
    ## 5280                       Indonesia   year61   90138235
    ## 5281                       Indonesia   year62   92558005
    ## 5282                       Indonesia   year63   95055665
    ## 5283                       Indonesia   year64   97638029
    ## 5284                       Indonesia   year65  100308894
    ## 5285                       Indonesia   year66  103067354
    ## 5286                       Indonesia   year67  105907403
    ## 5287                       Indonesia   year68  108821564
    ## 5288                       Indonesia   year69  111800091
    ## 5289                       Indonesia   year70  114834780
    ## 5290                       Indonesia   year71  117921998
    ## 5291                       Indonesia   year72  121059513
    ## 5292                       Indonesia   year73  124242298
    ## 5293                       Indonesia   year74  127465231
    ## 5294                       Indonesia   year75  130724115
    ## 5295                       Indonesia   year76  134010690
    ## 5296                       Indonesia   year77  137322118
    ## 5297                       Indonesia   year78  140665856
    ## 5298                       Indonesia   year79  144053518
    ## 5299                       Indonesia   year80  147490365
    ## 5300                       Indonesia   year81  150978840
    ## 5301                       Indonesia   year82  154506265
    ## 5302                       Indonesia   year83  158044343
    ## 5303                       Indonesia   year84  161555583
    ## 5304                       Indonesia   year85  165012196
    ## 5305                       Indonesia   year86  168402025
    ## 5306                       Indonesia   year87  171728917
    ## 5307                       Indonesia   year88  175000916
    ## 5308                       Indonesia   year89  178233223
    ## 5309                       Indonesia   year90  181436821
    ## 5310                       Indonesia   year91  184615979
    ## 5311                       Indonesia   year92  187766086
    ## 5312                       Indonesia   year93  190879523
    ## 5313                       Indonesia   year94  193945272
    ## 5314                       Indonesia   year95  196957849
    ## 5315                       Indonesia   year96  199914831
    ## 5316                       Indonesia   year97  202826465
    ## 5317                       Indonesia   year98  205715544
    ## 5318                       Indonesia   year99  208612556
    ## 5319                       Indonesia year2000  211540429
    ## 5320                       Indonesia year2001  214506502
    ## 5321                       Indonesia year2002  217508059
    ## 5322                       Indonesia year2003  220545214
    ## 5323                       Indonesia year2004  223614649
    ## 5324                       Indonesia year2005  226712730
    ## 5325                       Indonesia year2006  229838202
    ## 5326                       Indonesia year2007  232989141
    ## 5327                       Indonesia year2008  236159276
    ## 5328                       Indonesia year2009  239340478
    ## 5329                       Indonesia year2010  242524123
    ## 5330                       Indonesia year2011  245707511
    ## 5331                       Indonesia year2012  248883232
    ## 5332                       Indonesia year2013  252032263
    ## 5333                       Indonesia year2014  255131116
    ## 5334                       Indonesia year2015  258162113
    ## 5335                       Indonesia year2016  261115456
    ## 5336                       Indonesia year2017  263991379
    ## 5337              Iran, Islamic Rep.   year60   21906903
    ## 5338              Iran, Islamic Rep.   year61   22480418
    ## 5339              Iran, Islamic Rep.   year62   23071429
    ## 5340              Iran, Islamic Rep.   year63   23680432
    ## 5341              Iran, Islamic Rep.   year64   24308085
    ## 5342              Iran, Islamic Rep.   year65   24955115
    ## 5343              Iran, Islamic Rep.   year66   25624656
    ## 5344              Iran, Islamic Rep.   year67   26318119
    ## 5345              Iran, Islamic Rep.   year68   27032943
    ## 5346              Iran, Islamic Rep.   year69   27765243
    ## 5347              Iran, Islamic Rep.   year70   28514010
    ## 5348              Iran, Islamic Rep.   year71   29281268
    ## 5349              Iran, Islamic Rep.   year72   30074298
    ## 5350              Iran, Islamic Rep.   year73   30904271
    ## 5351              Iran, Islamic Rep.   year74   31785500
    ## 5352              Iran, Islamic Rep.   year75   32730554
    ## 5353              Iran, Islamic Rep.   year76   33737768
    ## 5354              Iran, Islamic Rep.   year77   34810723
    ## 5355              Iran, Islamic Rep.   year78   35972652
    ## 5356              Iran, Islamic Rep.   year79   37252659
    ## 5357              Iran, Islamic Rep.   year80   38668220
    ## 5358              Iran, Islamic Rep.   year81   40217629
    ## 5359              Iran, Islamic Rep.   year82   41883332
    ## 5360              Iran, Islamic Rep.   year83   43645092
    ## 5361              Iran, Islamic Rep.   year84   45474708
    ## 5362              Iran, Islamic Rep.   year85   47342702
    ## 5363              Iran, Islamic Rep.   year86   49256842
    ## 5364              Iran, Islamic Rep.   year87   51197482
    ## 5365              Iran, Islamic Rep.   year88   53075618
    ## 5366              Iran, Islamic Rep.   year89   54777114
    ## 5367              Iran, Islamic Rep.   year90   56226185
    ## 5368              Iran, Islamic Rep.   year91   57375584
    ## 5369              Iran, Islamic Rep.   year92   58260738
    ## 5370              Iran, Islamic Rep.   year93   58991218
    ## 5371              Iran, Islamic Rep.   year94   59725125
    ## 5372              Iran, Islamic Rep.   year95   60575644
    ## 5373              Iran, Islamic Rep.   year96   61583089
    ## 5374              Iran, Islamic Rep.   year97   62710557
    ## 5375              Iran, Islamic Rep.   year98   63900630
    ## 5376              Iran, Islamic Rep.   year99   65062660
    ## 5377              Iran, Islamic Rep. year2000   66131854
    ## 5378              Iran, Islamic Rep. year2001   67096414
    ## 5379              Iran, Islamic Rep. year2002   67983330
    ## 5380              Iran, Islamic Rep. year2003   68812713
    ## 5381              Iran, Islamic Rep. year2004   69617100
    ## 5382              Iran, Islamic Rep. year2005   70421811
    ## 5383              Iran, Islamic Rep. year2006   71227880
    ## 5384              Iran, Islamic Rep. year2007   72031103
    ## 5385              Iran, Islamic Rep. year2008   72845542
    ## 5386              Iran, Islamic Rep. year2009   73687565
    ## 5387              Iran, Islamic Rep. year2010   74567511
    ## 5388              Iran, Islamic Rep. year2011   75491582
    ## 5389              Iran, Islamic Rep. year2012   76453574
    ## 5390              Iran, Islamic Rep. year2013   77435384
    ## 5391              Iran, Islamic Rep. year2014   78411092
    ## 5392              Iran, Islamic Rep. year2015   79360487
    ## 5393              Iran, Islamic Rep. year2016   80277428
    ## 5394              Iran, Islamic Rep. year2017   81162788
    ## 5395                            Iraq   year60    7289761
    ## 5396                            Iraq   year61    7475352
    ## 5397                            Iraq   year62    7674223
    ## 5398                            Iraq   year63    7888914
    ## 5399                            Iraq   year64    8122199
    ## 5400                            Iraq   year65    8375793
    ## 5401                            Iraq   year66    8651164
    ## 5402                            Iraq   year67    8947404
    ## 5403                            Iraq   year68    9260682
    ## 5404                            Iraq   year69    9585576
    ## 5405                            Iraq   year70    9917983
    ## 5406                            Iraq   year71   10255903
    ## 5407                            Iraq   year72   10599845
    ## 5408                            Iraq   year73   10951166
    ## 5409                            Iraq   year74   11312305
    ## 5410                            Iraq   year75   11684589
    ## 5411                            Iraq   year76   12068168
    ## 5412                            Iraq   year77   12460914
    ## 5413                            Iraq   year78   12859094
    ## 5414                            Iraq   year79   13257799
    ## 5415                            Iraq   year80   13653356
    ## 5416                            Iraq   year81   14046540
    ## 5417                            Iraq   year82   14438309
    ## 5418                            Iraq   year83   14825789
    ## 5419                            Iraq   year84   15205501
    ## 5420                            Iraq   year85   15576395
    ## 5421                            Iraq   year86   15936375
    ## 5422                            Iraq   year87   16290149
    ## 5423                            Iraq   year88   16651807
    ## 5424                            Iraq   year89   17040190
    ## 5425                            Iraq   year90   17469005
    ## 5426                            Iraq   year91   17942715
    ## 5427                            Iraq   year92   18458187
    ## 5428                            Iraq   year93   19011917
    ## 5429                            Iraq   year94   19597239
    ## 5430                            Iraq   year95   20208387
    ## 5431                            Iraq   year96   20845893
    ## 5432                            Iraq   year97   21509291
    ## 5433                            Iraq   year98   22190250
    ## 5434                            Iraq   year99   22878156
    ## 5435                            Iraq year2000   23565413
    ## 5436                            Iraq year2001   24251649
    ## 5437                            Iraq year2002   24939299
    ## 5438                            Iraq year2003   25627626
    ## 5439                            Iraq year2004   26316609
    ## 5440                            Iraq year2005   27008426
    ## 5441                            Iraq year2006   27697912
    ## 5442                            Iraq year2007   28390433
    ## 5443                            Iraq year2008   29111417
    ## 5444                            Iraq year2009   29894652
    ## 5445                            Iraq year2010   30762701
    ## 5446                            Iraq year2011   31727053
    ## 5447                            Iraq year2012   32776571
    ## 5448                            Iraq year2013   33883145
    ## 5449                            Iraq year2014   35006080
    ## 5450                            Iraq year2015   36115649
    ## 5451                            Iraq year2016   37202572
    ## 5452                            Iraq year2017   38274618
    ## 5453                         Ireland   year60    2828600
    ## 5454                         Ireland   year61    2824400
    ## 5455                         Ireland   year62    2836050
    ## 5456                         Ireland   year63    2852650
    ## 5457                         Ireland   year64    2866550
    ## 5458                         Ireland   year65    2877300
    ## 5459                         Ireland   year66    2888800
    ## 5460                         Ireland   year67    2902450
    ## 5461                         Ireland   year68    2915550
    ## 5462                         Ireland   year69    2932650
    ## 5463                         Ireland   year70    2957250
    ## 5464                         Ireland   year71    2992050
    ## 5465                         Ireland   year72    3036850
    ## 5466                         Ireland   year73    3085950
    ## 5467                         Ireland   year74    3137500
    ## 5468                         Ireland   year75    3189550
    ## 5469                         Ireland   year76    3238050
    ## 5470                         Ireland   year77    3282200
    ## 5471                         Ireland   year78    3329100
    ## 5472                         Ireland   year79    3373750
    ## 5473                         Ireland   year80    3412800
    ## 5474                         Ireland   year81    3453000
    ## 5475                         Ireland   year82    3485800
    ## 5476                         Ireland   year83    3510600
    ## 5477                         Ireland   year84    3532423
    ## 5478                         Ireland   year85    3538082
    ## 5479                         Ireland   year86    3539690
    ## 5480                         Ireland   year87    3540057
    ## 5481                         Ireland   year88    3524949
    ## 5482                         Ireland   year89    3511009
    ## 5483                         Ireland   year90    3513974
    ## 5484                         Ireland   year91    3534235
    ## 5485                         Ireland   year92    3558430
    ## 5486                         Ireland   year93    3576261
    ## 5487                         Ireland   year94    3590386
    ## 5488                         Ireland   year95    3608841
    ## 5489                         Ireland   year96    3637510
    ## 5490                         Ireland   year97    3674171
    ## 5491                         Ireland   year98    3712696
    ## 5492                         Ireland   year99    3754786
    ## 5493                         Ireland year2000    3805174
    ## 5494                         Ireland year2001    3866243
    ## 5495                         Ireland year2002    3931947
    ## 5496                         Ireland year2003    3996521
    ## 5497                         Ireland year2004    4070262
    ## 5498                         Ireland year2005    4159914
    ## 5499                         Ireland year2006    4273591
    ## 5500                         Ireland year2007    4398942
    ## 5501                         Ireland year2008    4489544
    ## 5502                         Ireland year2009    4535375
    ## 5503                         Ireland year2010    4560155
    ## 5504                         Ireland year2011    4580084
    ## 5505                         Ireland year2012    4599533
    ## 5506                         Ireland year2013    4623816
    ## 5507                         Ireland year2014    4657740
    ## 5508                         Ireland year2015    4701957
    ## 5509                         Ireland year2016    4755335
    ## 5510                         Ireland year2017    4813608
    ## 5511                     Isle of Man   year60      48442
    ## 5512                     Isle of Man   year61      48281
    ## 5513                     Isle of Man   year62      48418
    ## 5514                     Isle of Man   year63      48800
    ## 5515                     Isle of Man   year64      49391
    ## 5516                     Isle of Man   year65      50141
    ## 5517                     Isle of Man   year66      51049
    ## 5518                     Isle of Man   year67      52118
    ## 5519                     Isle of Man   year68      53254
    ## 5520                     Isle of Man   year69      54376
    ## 5521                     Isle of Man   year70      55425
    ## 5522                     Isle of Man   year71      56352
    ## 5523                     Isle of Man   year72      57154
    ## 5524                     Isle of Man   year73      57900
    ## 5525                     Isle of Man   year74      58655
    ## 5526                     Isle of Man   year75      59478
    ## 5527                     Isle of Man   year76      60428
    ## 5528                     Isle of Man   year77      61443
    ## 5529                     Isle of Man   year78      62406
    ## 5530                     Isle of Man   year79      63151
    ## 5531                     Isle of Man   year80      63551
    ## 5532                     Isle of Man   year81      63540
    ## 5533                     Isle of Man   year82      63191
    ## 5534                     Isle of Man   year83      62730
    ## 5535                     Isle of Man   year84      62487
    ## 5536                     Isle of Man   year85      62696
    ## 5537                     Isle of Man   year86      63441
    ## 5538                     Isle of Man   year87      64630
    ## 5539                     Isle of Man   year88      66047
    ## 5540                     Isle of Man   year89      67388
    ## 5541                     Isle of Man   year90      68429
    ## 5542                     Isle of Man   year91      69096
    ## 5543                     Isle of Man   year92      69475
    ## 5544                     Isle of Man   year93      69656
    ## 5545                     Isle of Man   year94      69818
    ## 5546                     Isle of Man   year95      70070
    ## 5547                     Isle of Man   year96      70431
    ## 5548                     Isle of Man   year97      70869
    ## 5549                     Isle of Man   year98      71390
    ## 5550                     Isle of Man   year99      71952
    ## 5551                     Isle of Man year2000      72554
    ## 5552                     Isle of Man year2001      73192
    ## 5553                     Isle of Man year2002      73870
    ## 5554                     Isle of Man year2003      74587
    ## 5555                     Isle of Man year2004      75341
    ## 5556                     Isle of Man year2005      76118
    ## 5557                     Isle of Man year2006      76914
    ## 5558                     Isle of Man year2007      77727
    ## 5559                     Isle of Man year2008      78534
    ## 5560                     Isle of Man year2009      79325
    ## 5561                     Isle of Man year2010      80072
    ## 5562                     Isle of Man year2011      80759
    ## 5563                     Isle of Man year2012      81406
    ## 5564                     Isle of Man year2013      82013
    ## 5565                     Isle of Man year2014      82590
    ## 5566                     Isle of Man year2015      83167
    ## 5567                     Isle of Man year2016      83737
    ## 5568                     Isle of Man year2017      84287
    ## 5569                          Israel   year60    2114020
    ## 5570                          Israel   year61    2185000
    ## 5571                          Israel   year62    2293000
    ## 5572                          Israel   year63    2379000
    ## 5573                          Israel   year64    2475000
    ## 5574                          Israel   year65    2563000
    ## 5575                          Israel   year66    2629000
    ## 5576                          Israel   year67    2745000
    ## 5577                          Israel   year68    2803000
    ## 5578                          Israel   year69    2877000
    ## 5579                          Israel   year70    2974000
    ## 5580                          Israel   year71    3069000
    ## 5581                          Israel   year72    3148000
    ## 5582                          Israel   year73    3278000
    ## 5583                          Israel   year74    3377000
    ## 5584                          Israel   year75    3455000
    ## 5585                          Israel   year76    3533000
    ## 5586                          Israel   year77    3613000
    ## 5587                          Israel   year78    3690000
    ## 5588                          Israel   year79    3786000
    ## 5589                          Israel   year80    3878000
    ## 5590                          Israel   year81    3956000
    ## 5591                          Israel   year82    4031000
    ## 5592                          Israel   year83    4105000
    ## 5593                          Israel   year84    4159000
    ## 5594                          Israel   year85    4233000
    ## 5595                          Israel   year86    4299000
    ## 5596                          Israel   year87    4369000
    ## 5597                          Israel   year88    4442000
    ## 5598                          Israel   year89    4518000
    ## 5599                          Israel   year90    4660000
    ## 5600                          Israel   year91    4949000
    ## 5601                          Israel   year92    5123000
    ## 5602                          Israel   year93    5261000
    ## 5603                          Israel   year94    5399000
    ## 5604                          Israel   year95    5545000
    ## 5605                          Israel   year96    5692000
    ## 5606                          Israel   year97    5836000
    ## 5607                          Israel   year98    5971000
    ## 5608                          Israel   year99    6125000
    ## 5609                          Israel year2000    6289000
    ## 5610                          Israel year2001    6439000
    ## 5611                          Israel year2002    6570000
    ## 5612                          Israel year2003    6689700
    ## 5613                          Israel year2004    6809000
    ## 5614                          Israel year2005    6930100
    ## 5615                          Israel year2006    7053700
    ## 5616                          Israel year2007    7180100
    ## 5617                          Israel year2008    7308800
    ## 5618                          Israel year2009    7485600
    ## 5619                          Israel year2010    7623600
    ## 5620                          Israel year2011    7765800
    ## 5621                          Israel year2012    7910500
    ## 5622                          Israel year2013    8059500
    ## 5623                          Israel year2014    8215700
    ## 5624                          Israel year2015    8380100
    ## 5625                          Israel year2016    8546000
    ## 5626                          Israel year2017    8712400
    ## 5627                           Italy   year60   50199700
    ## 5628                           Italy   year61   50536350
    ## 5629                           Italy   year62   50879450
    ## 5630                           Italy   year63   51252000
    ## 5631                           Italy   year64   51675350
    ## 5632                           Italy   year65   52112350
    ## 5633                           Italy   year66   52519000
    ## 5634                           Italy   year67   52900500
    ## 5635                           Italy   year68   53235750
    ## 5636                           Italy   year69   53537950
    ## 5637                           Italy   year70   53821850
    ## 5638                           Italy   year71   54073490
    ## 5639                           Italy   year72   54381345
    ## 5640                           Italy   year73   54751406
    ## 5641                           Italy   year74   55110868
    ## 5642                           Italy   year75   55441001
    ## 5643                           Italy   year76   55718260
    ## 5644                           Italy   year77   55955411
    ## 5645                           Italy   year78   56155143
    ## 5646                           Italy   year79   56317749
    ## 5647                           Italy   year80   56433883
    ## 5648                           Italy   year81   56501675
    ## 5649                           Italy   year82   56543548
    ## 5650                           Italy   year83   56564074
    ## 5651                           Italy   year84   56576718
    ## 5652                           Italy   year85   56593071
    ## 5653                           Italy   year86   56596155
    ## 5654                           Italy   year87   56601931
    ## 5655                           Italy   year88   56629288
    ## 5656                           Italy   year89   56671781
    ## 5657                           Italy   year90   56719240
    ## 5658                           Italy   year91   56758521
    ## 5659                           Italy   year92   56797087
    ## 5660                           Italy   year93   56831821
    ## 5661                           Italy   year94   56843400
    ## 5662                           Italy   year95   56844303
    ## 5663                           Italy   year96   56860281
    ## 5664                           Italy   year97   56890372
    ## 5665                           Italy   year98   56906744
    ## 5666                           Italy   year99   56916317
    ## 5667                           Italy year2000   56942108
    ## 5668                           Italy year2001   56974100
    ## 5669                           Italy year2002   57059007
    ## 5670                           Italy year2003   57313203
    ## 5671                           Italy year2004   57685327
    ## 5672                           Italy year2005   57969484
    ## 5673                           Italy year2006   58143979
    ## 5674                           Italy year2007   58438310
    ## 5675                           Italy year2008   58826731
    ## 5676                           Italy year2009   59095365
    ## 5677                           Italy year2010   59277417
    ## 5678                           Italy year2011   59379449
    ## 5679                           Italy year2012   59539717
    ## 5680                           Italy year2013   60233948
    ## 5681                           Italy year2014   60789140
    ## 5682                           Italy year2015   60730582
    ## 5683                           Italy year2016   60627498
    ## 5684                           Italy year2017   60551416
    ## 5685                         Jamaica   year60    1628252
    ## 5686                         Jamaica   year61    1650806
    ## 5687                         Jamaica   year62    1676250
    ## 5688                         Jamaica   year63    1703395
    ## 5689                         Jamaica   year64    1730486
    ## 5690                         Jamaica   year65    1756266
    ## 5691                         Jamaica   year66    1780264
    ## 5692                         Jamaica   year67    1803064
    ## 5693                         Jamaica   year68    1825633
    ## 5694                         Jamaica   year69    1849414
    ## 5695                         Jamaica   year70    1875381
    ## 5696                         Jamaica   year71    1904016
    ## 5697                         Jamaica   year72    1934828
    ## 5698                         Jamaica   year73    1966700
    ## 5699                         Jamaica   year74    1998034
    ## 5700                         Jamaica   year75    2027737
    ## 5701                         Jamaica   year76    2055085
    ## 5702                         Jamaica   year77    2080538
    ## 5703                         Jamaica   year78    2105664
    ## 5704                         Jamaica   year79    2132690
    ## 5705                         Jamaica   year80    2163045
    ## 5706                         Jamaica   year81    2197583
    ## 5707                         Jamaica   year82    2235327
    ## 5708                         Jamaica   year83    2273666
    ## 5709                         Jamaica   year84    2308947
    ## 5710                         Jamaica   year85    2338638
    ## 5711                         Jamaica   year86    2361720
    ## 5712                         Jamaica   year87    2379279
    ## 5713                         Jamaica   year88    2393534
    ## 5714                         Jamaica   year89    2407720
    ## 5715                         Jamaica   year90    2424242
    ## 5716                         Jamaica   year91    2443689
    ## 5717                         Jamaica   year92    2465362
    ## 5718                         Jamaica   year93    2488782
    ## 5719                         Jamaica   year94    2513049
    ## 5720                         Jamaica   year95    2537440
    ## 5721                         Jamaica   year96    2561993
    ## 5722                         Jamaica   year97    2586827
    ## 5723                         Jamaica   year98    2611367
    ## 5724                         Jamaica   year99    2634882
    ## 5725                         Jamaica year2000    2656864
    ## 5726                         Jamaica year2001    2677011
    ## 5727                         Jamaica year2002    2695446
    ## 5728                         Jamaica year2003    2712511
    ## 5729                         Jamaica year2004    2728777
    ## 5730                         Jamaica year2005    2744673
    ## 5731                         Jamaica year2006    2760279
    ## 5732                         Jamaica year2007    2775467
    ## 5733                         Jamaica year2008    2790122
    ## 5734                         Jamaica year2009    2804082
    ## 5735                         Jamaica year2010    2817210
    ## 5736                         Jamaica year2011    2829493
    ## 5737                         Jamaica year2012    2840992
    ## 5738                         Jamaica year2013    2851807
    ## 5739                         Jamaica year2014    2862087
    ## 5740                         Jamaica year2015    2871934
    ## 5741                         Jamaica year2016    2881355
    ## 5742                         Jamaica year2017    2890299
    ## 5743                           Japan   year60   92500572
    ## 5744                           Japan   year61   94943000
    ## 5745                           Japan   year62   95832000
    ## 5746                           Japan   year63   96812000
    ## 5747                           Japan   year64   97826000
    ## 5748                           Japan   year65   98883000
    ## 5749                           Japan   year66   99790000
    ## 5750                           Japan   year67  100725000
    ## 5751                           Japan   year68  101061000
    ## 5752                           Japan   year69  103172000
    ## 5753                           Japan   year70  104345000
    ## 5754                           Japan   year71  105697000
    ## 5755                           Japan   year72  107188000
    ## 5756                           Japan   year73  108079000
    ## 5757                           Japan   year74  110162000
    ## 5758                           Japan   year75  111940000
    ## 5759                           Japan   year76  112771000
    ## 5760                           Japan   year77  113863000
    ## 5761                           Japan   year78  114898000
    ## 5762                           Japan   year79  115870000
    ## 5763                           Japan   year80  116782000
    ## 5764                           Japan   year81  117648000
    ## 5765                           Japan   year82  118449000
    ## 5766                           Japan   year83  119259000
    ## 5767                           Japan   year84  120018000
    ## 5768                           Japan   year85  120754000
    ## 5769                           Japan   year86  121492000
    ## 5770                           Japan   year87  122091000
    ## 5771                           Japan   year88  122613000
    ## 5772                           Japan   year89  123116000
    ## 5773                           Japan   year90  123537000
    ## 5774                           Japan   year91  123921000
    ## 5775                           Japan   year92  124229000
    ## 5776                           Japan   year93  124536000
    ## 5777                           Japan   year94  124961000
    ## 5778                           Japan   year95  125439000
    ## 5779                           Japan   year96  125757000
    ## 5780                           Japan   year97  126057000
    ## 5781                           Japan   year98  126400000
    ## 5782                           Japan   year99  126631000
    ## 5783                           Japan year2000  126843000
    ## 5784                           Japan year2001  127149000
    ## 5785                           Japan year2002  127445000
    ## 5786                           Japan year2003  127718000
    ## 5787                           Japan year2004  127761000
    ## 5788                           Japan year2005  127773000
    ## 5789                           Japan year2006  127854000
    ## 5790                           Japan year2007  128001000
    ## 5791                           Japan year2008  128063000
    ## 5792                           Japan year2009  128047000
    ## 5793                           Japan year2010  128070000
    ## 5794                           Japan year2011  127833000
    ## 5795                           Japan year2012  127629000
    ## 5796                           Japan year2013  127445000
    ## 5797                           Japan year2014  127276000
    ## 5798                           Japan year2015  127141000
    ## 5799                           Japan year2016  126994511
    ## 5800                           Japan year2017  126785797
    ## 5801                          Jordan   year60     932257
    ## 5802                          Jordan   year61     973083
    ## 5803                          Jordan   year62    1009733
    ## 5804                          Jordan   year63    1049302
    ## 5805                          Jordan   year64    1101459
    ## 5806                          Jordan   year65    1172550
    ## 5807                          Jordan   year66    1265806
    ## 5808                          Jordan   year67    1377465
    ## 5809                          Jordan   year68    1498309
    ## 5810                          Jordan   year69    1615277
    ## 5811                          Jordan   year70    1718913
    ## 5812                          Jordan   year71    1806605
    ## 5813                          Jordan   year72    1881214
    ## 5814                          Jordan   year73    1945626
    ## 5815                          Jordan   year74    2004833
    ## 5816                          Jordan   year75    2062918
    ## 5817                          Jordan   year76    2120069
    ## 5818                          Jordan   year77    2176135
    ## 5819                          Jordan   year78    2234594
    ## 5820                          Jordan   year79    2299655
    ## 5821                          Jordan   year80    2374422
    ## 5822                          Jordan   year81    2461193
    ## 5823                          Jordan   year82    2559718
    ## 5824                          Jordan   year83    2667470
    ## 5825                          Jordan   year84    2780428
    ## 5826                          Jordan   year85    2895985
    ## 5827                          Jordan   year86    3011300
    ## 5828                          Jordan   year87    3127917
    ## 5829                          Jordan   year88    3252672
    ## 5830                          Jordan   year89    3395023
    ## 5831                          Jordan   year90    3560582
    ## 5832                          Jordan   year91    3753433
    ## 5833                          Jordan   year92    3968198
    ## 5834                          Jordan   year93    4189431
    ## 5835                          Jordan   year94    4395953
    ## 5836                          Jordan   year95    4572904
    ## 5837                          Jordan   year96    4716373
    ## 5838                          Jordan   year97    4832267
    ## 5839                          Jordan   year98    4927912
    ## 5840                          Jordan   year99    5014899
    ## 5841                          Jordan year2000    5103130
    ## 5842                          Jordan year2001    5193482
    ## 5843                          Jordan year2002    5287488
    ## 5844                          Jordan year2003    5396774
    ## 5845                          Jordan year2004    5535595
    ## 5846                          Jordan year2005    5714111
    ## 5847                          Jordan year2006    5934232
    ## 5848                          Jordan year2007    6193191
    ## 5849                          Jordan year2008    6489822
    ## 5850                          Jordan year2009    6821116
    ## 5851                          Jordan year2010    7182390
    ## 5852                          Jordan year2011    7574943
    ## 5853                          Jordan year2012    7992573
    ## 5854                          Jordan year2013    8413464
    ## 5855                          Jordan year2014    8809306
    ## 5856                          Jordan year2015    9159302
    ## 5857                          Jordan year2016    9455802
    ## 5858                          Jordan year2017    9702353
    ## 5859                      Kazakhstan   year60    9714260
    ## 5860                      Kazakhstan   year61   10129861
    ## 5861                      Kazakhstan   year62   10532062
    ## 5862                      Kazakhstan   year63   10913552
    ## 5863                      Kazakhstan   year64   11267329
    ## 5864                      Kazakhstan   year65   11588870
    ## 5865                      Kazakhstan   year66   11872939
    ## 5866                      Kazakhstan   year67   12120504
    ## 5867                      Kazakhstan   year68   12341412
    ## 5868                      Kazakhstan   year69   12550121
    ## 5869                      Kazakhstan   year70   12757245
    ## 5870                      Kazakhstan   year71   12966920
    ## 5871                      Kazakhstan   year72   13176584
    ## 5872                      Kazakhstan   year73   13382211
    ## 5873                      Kazakhstan   year74   13577049
    ## 5874                      Kazakhstan   year75   13756789
    ## 5875                      Kazakhstan   year76   13920105
    ## 5876                      Kazakhstan   year77   14070681
    ## 5877                      Kazakhstan   year78   14215111
    ## 5878                      Kazakhstan   year79   14362417
    ## 5879                      Kazakhstan   year80   14518924
    ## 5880                      Kazakhstan   year81   14683789
    ## 5881                      Kazakhstan   year82   14853993
    ## 5882                      Kazakhstan   year83   15030495
    ## 5883                      Kazakhstan   year84   15214051
    ## 5884                      Kazakhstan   year85   15403006
    ## 5885                      Kazakhstan   year86   15600928
    ## 5886                      Kazakhstan   year87   15801753
    ## 5887                      Kazakhstan   year88   15982510
    ## 5888                      Kazakhstan   year89   16249500
    ## 5889                      Kazakhstan   year90   16348000
    ## 5890                      Kazakhstan   year91   16450500
    ## 5891                      Kazakhstan   year92   16439095
    ## 5892                      Kazakhstan   year93   16330419
    ## 5893                      Kazakhstan   year94   16095199
    ## 5894                      Kazakhstan   year95   15815626
    ## 5895                      Kazakhstan   year96   15577894
    ## 5896                      Kazakhstan   year97   15333703
    ## 5897                      Kazakhstan   year98   15071300
    ## 5898                      Kazakhstan   year99   14928426
    ## 5899                      Kazakhstan year2000   14883626
    ## 5900                      Kazakhstan year2001   14858335
    ## 5901                      Kazakhstan year2002   14858948
    ## 5902                      Kazakhstan year2003   14909018
    ## 5903                      Kazakhstan year2004   15012985
    ## 5904                      Kazakhstan year2005   15147029
    ## 5905                      Kazakhstan year2006   15308084
    ## 5906                      Kazakhstan year2007   15484192
    ## 5907                      Kazakhstan year2008   15674000
    ## 5908                      Kazakhstan year2009   16092822
    ## 5909                      Kazakhstan year2010   16321872
    ## 5910                      Kazakhstan year2011   16557201
    ## 5911                      Kazakhstan year2012   16792089
    ## 5912                      Kazakhstan year2013   17035550
    ## 5913                      Kazakhstan year2014   17288285
    ## 5914                      Kazakhstan year2015   17542806
    ## 5915                      Kazakhstan year2016   17794055
    ## 5916                      Kazakhstan year2017   18037646
    ## 5917                           Kenya   year60    8105440
    ## 5918                           Kenya   year61    8361441
    ## 5919                           Kenya   year62    8628972
    ## 5920                           Kenya   year63    8908422
    ## 5921                           Kenya   year64    9200157
    ## 5922                           Kenya   year65    9504703
    ## 5923                           Kenya   year66    9822499
    ## 5924                           Kenya   year67   10154484
    ## 5925                           Kenya   year68   10502245
    ## 5926                           Kenya   year69   10867716
    ## 5927                           Kenya   year70   11252492
    ## 5928                           Kenya   year71   11657514
    ## 5929                           Kenya   year72   12083188
    ## 5930                           Kenya   year73   12529852
    ## 5931                           Kenya   year74   12997595
    ## 5932                           Kenya   year75   13486629
    ## 5933                           Kenya   year76   13996704
    ## 5934                           Kenya   year77   14528293
    ## 5935                           Kenya   year78   15082994
    ## 5936                           Kenya   year79   15662852
    ## 5937                           Kenya   year80   16268990
    ## 5938                           Kenya   year81   16901677
    ## 5939                           Kenya   year82   17559430
    ## 5940                           Kenya   year83   18239404
    ## 5941                           Kenya   year84   18937738
    ## 5942                           Kenya   year85   19651225
    ## 5943                           Kenya   year86   20378626
    ## 5944                           Kenya   year87   21119318
    ## 5945                           Kenya   year88   21871442
    ## 5946                           Kenya   year89   22633022
    ## 5947                           Kenya   year90   23402507
    ## 5948                           Kenya   year91   24179598
    ## 5949                           Kenya   year92   24963953
    ## 5950                           Kenya   year93   25754114
    ## 5951                           Kenya   year94   26548486
    ## 5952                           Kenya   year95   27346456
    ## 5953                           Kenya   year96   28147734
    ## 5954                           Kenya   year97   28954114
    ## 5955                           Kenya   year98   29769803
    ## 5956                           Kenya   year99   30600397
    ## 5957                           Kenya year2000   31450483
    ## 5958                           Kenya year2001   32321482
    ## 5959                           Kenya year2002   33214009
    ## 5960                           Kenya year2003   34130852
    ## 5961                           Kenya year2004   35074931
    ## 5962                           Kenya year2005   36048288
    ## 5963                           Kenya year2006   37052050
    ## 5964                           Kenya year2007   38085909
    ## 5965                           Kenya year2008   39148416
    ## 5966                           Kenya year2009   40237204
    ## 5967                           Kenya year2010   41350152
    ## 5968                           Kenya year2011   42486839
    ## 5969                           Kenya year2012   43646629
    ## 5970                           Kenya year2013   44826849
    ## 5971                           Kenya year2014   46024250
    ## 5972                           Kenya year2015   47236259
    ## 5973                           Kenya year2016   48461567
    ## 5974                           Kenya year2017   49699862
    ## 5975                        Kiribati   year60      41233
    ## 5976                        Kiribati   year61      42257
    ## 5977                        Kiribati   year62      43302
    ## 5978                        Kiribati   year63      44363
    ## 5979                        Kiribati   year64      45425
    ## 5980                        Kiribati   year65      46453
    ## 5981                        Kiribati   year66      47459
    ## 5982                        Kiribati   year67      48437
    ## 5983                        Kiribati   year68      49388
    ## 5984                        Kiribati   year69      50294
    ## 5985                        Kiribati   year70      51178
    ## 5986                        Kiribati   year71      52025
    ## 5987                        Kiribati   year72      52824
    ## 5988                        Kiribati   year73      53604
    ## 5989                        Kiribati   year74      54380
    ## 5990                        Kiribati   year75      55169
    ## 5991                        Kiribati   year76      55977
    ## 5992                        Kiribati   year77      56810
    ## 5993                        Kiribati   year78      57662
    ## 5994                        Kiribati   year79      58506
    ## 5995                        Kiribati   year80      59339
    ## 5996                        Kiribati   year81      60133
    ## 5997                        Kiribati   year82      60920
    ## 5998                        Kiribati   year83      61768
    ## 5999                        Kiribati   year84      62765
    ## 6000                        Kiribati   year85      64003
    ## 6001                        Kiribati   year86      65518
    ## 6002                        Kiribati   year87      67261
    ## 6003                        Kiribati   year88      69098
    ## 6004                        Kiribati   year89      70860
    ## 6005                        Kiribati   year90      72412
    ## 6006                        Kiribati   year91      73700
    ## 6007                        Kiribati   year92      74769
    ## 6008                        Kiribati   year93      75719
    ## 6009                        Kiribati   year94      76671
    ## 6010                        Kiribati   year95      77730
    ## 6011                        Kiribati   year96      78907
    ## 6012                        Kiribati   year97      80184
    ## 6013                        Kiribati   year98      81550
    ## 6014                        Kiribati   year99      82966
    ## 6015                        Kiribati year2000      84406
    ## 6016                        Kiribati year2001      85858
    ## 6017                        Kiribati year2002      87343
    ## 6018                        Kiribati year2003      88895
    ## 6019                        Kiribati year2004      90542
    ## 6020                        Kiribati year2005      92325
    ## 6021                        Kiribati year2006      94260
    ## 6022                        Kiribati year2007      96311
    ## 6023                        Kiribati year2008      98440
    ## 6024                        Kiribati year2009     100568
    ## 6025                        Kiribati year2010     102652
    ## 6026                        Kiribati year2011     104656
    ## 6027                        Kiribati year2012     106613
    ## 6028                        Kiribati year2013     108535
    ## 6029                        Kiribati year2014     110458
    ## 6030                        Kiribati year2015     112407
    ## 6031                        Kiribati year2016     114395
    ## 6032                        Kiribati year2017     116398
    ## 6033       Korea, Dem. People?s Rep.   year60   11424176
    ## 6034       Korea, Dem. People?s Rep.   year61   11665595
    ## 6035       Korea, Dem. People?s Rep.   year62   11871712
    ## 6036       Korea, Dem. People?s Rep.   year63   12065468
    ## 6037       Korea, Dem. People?s Rep.   year64   12282419
    ## 6038       Korea, Dem. People?s Rep.   year65   12547525
    ## 6039       Korea, Dem. People?s Rep.   year66   12864954
    ## 6040       Korea, Dem. People?s Rep.   year67   13222694
    ## 6041       Korea, Dem. People?s Rep.   year68   13609982
    ## 6042       Korea, Dem. People?s Rep.   year69   14010339
    ## 6043       Korea, Dem. People?s Rep.   year70   14410400
    ## 6044       Korea, Dem. People?s Rep.   year71   14809521
    ## 6045       Korea, Dem. People?s Rep.   year72   15207771
    ## 6046       Korea, Dem. People?s Rep.   year73   15593351
    ## 6047       Korea, Dem. People?s Rep.   year74   15952078
    ## 6048       Korea, Dem. People?s Rep.   year75   16274740
    ## 6049       Korea, Dem. People?s Rep.   year76   16554746
    ## 6050       Korea, Dem. People?s Rep.   year77   16796578
    ## 6051       Korea, Dem. People?s Rep.   year78   17015983
    ## 6052       Korea, Dem. People?s Rep.   year79   17235666
    ## 6053       Korea, Dem. People?s Rep.   year80   17472140
    ## 6054       Korea, Dem. People?s Rep.   year81   17731230
    ## 6055       Korea, Dem. People?s Rep.   year82   18008564
    ## 6056       Korea, Dem. People?s Rep.   year83   18298214
    ## 6057       Korea, Dem. People?s Rep.   year84   18590138
    ## 6058       Korea, Dem. People?s Rep.   year85   18877238
    ## 6059       Korea, Dem. People?s Rep.   year86   19156795
    ## 6060       Korea, Dem. People?s Rep.   year87   19431986
    ## 6061       Korea, Dem. People?s Rep.   year88   19708323
    ## 6062       Korea, Dem. People?s Rep.   year89   19993755
    ## 6063       Korea, Dem. People?s Rep.   year90   20293054
    ## 6064       Korea, Dem. People?s Rep.   year91   20609150
    ## 6065       Korea, Dem. People?s Rep.   year92   20937404
    ## 6066       Korea, Dem. People?s Rep.   year93   21265834
    ## 6067       Korea, Dem. People?s Rep.   year94   21577982
    ## 6068       Korea, Dem. People?s Rep.   year95   21862299
    ## 6069       Korea, Dem. People?s Rep.   year96   22113548
    ## 6070       Korea, Dem. People?s Rep.   year97   22335638
    ## 6071       Korea, Dem. People?s Rep.   year98   22537336
    ## 6072       Korea, Dem. People?s Rep.   year99   22731985
    ## 6073       Korea, Dem. People?s Rep. year2000   22929075
    ## 6074       Korea, Dem. People?s Rep. year2001   23131810
    ## 6075       Korea, Dem. People?s Rep. year2002   23336681
    ## 6076       Korea, Dem. People?s Rep. year2003   23538540
    ## 6077       Korea, Dem. People?s Rep. year2004   23729498
    ## 6078       Korea, Dem. People?s Rep. year2005   23904167
    ## 6079       Korea, Dem. People?s Rep. year2006   24061097
    ## 6080       Korea, Dem. People?s Rep. year2007   24203289
    ## 6081       Korea, Dem. People?s Rep. year2008   24335146
    ## 6082       Korea, Dem. People?s Rep. year2009   24463021
    ## 6083       Korea, Dem. People?s Rep. year2010   24591599
    ## 6084       Korea, Dem. People?s Rep. year2011   24722298
    ## 6085       Korea, Dem. People?s Rep. year2012   24854034
    ## 6086       Korea, Dem. People?s Rep. year2013   24985976
    ## 6087       Korea, Dem. People?s Rep. year2014   25116363
    ## 6088       Korea, Dem. People?s Rep. year2015   25243917
    ## 6089       Korea, Dem. People?s Rep. year2016   25368620
    ## 6090       Korea, Dem. People?s Rep. year2017   25490965
    ## 6091                     Korea, Rep.   year60   25012374
    ## 6092                     Korea, Rep.   year61   25765673
    ## 6093                     Korea, Rep.   year62   26513030
    ## 6094                     Korea, Rep.   year63   27261747
    ## 6095                     Korea, Rep.   year64   27984155
    ## 6096                     Korea, Rep.   year65   28704674
    ## 6097                     Korea, Rep.   year66   29435571
    ## 6098                     Korea, Rep.   year67   30130983
    ## 6099                     Korea, Rep.   year68   30838302
    ## 6100                     Korea, Rep.   year69   31544266
    ## 6101                     Korea, Rep.   year70   32240827
    ## 6102                     Korea, Rep.   year71   32882704
    ## 6103                     Korea, Rep.   year72   33505406
    ## 6104                     Korea, Rep.   year73   34103149
    ## 6105                     Korea, Rep.   year74   34692266
    ## 6106                     Korea, Rep.   year75   35280725
    ## 6107                     Korea, Rep.   year76   35848523
    ## 6108                     Korea, Rep.   year77   36411795
    ## 6109                     Korea, Rep.   year78   36969185
    ## 6110                     Korea, Rep.   year79   37534236
    ## 6111                     Korea, Rep.   year80   38123775
    ## 6112                     Korea, Rep.   year81   38723248
    ## 6113                     Korea, Rep.   year82   39326352
    ## 6114                     Korea, Rep.   year83   39910403
    ## 6115                     Korea, Rep.   year84   40405956
    ## 6116                     Korea, Rep.   year85   40805744
    ## 6117                     Korea, Rep.   year86   41213674
    ## 6118                     Korea, Rep.   year87   41621690
    ## 6119                     Korea, Rep.   year88   42031247
    ## 6120                     Korea, Rep.   year89   42449038
    ## 6121                     Korea, Rep.   year90   42869283
    ## 6122                     Korea, Rep.   year91   43295704
    ## 6123                     Korea, Rep.   year92   43747962
    ## 6124                     Korea, Rep.   year93   44194628
    ## 6125                     Korea, Rep.   year94   44641540
    ## 6126                     Korea, Rep.   year95   45092991
    ## 6127                     Korea, Rep.   year96   45524681
    ## 6128                     Korea, Rep.   year97   45953580
    ## 6129                     Korea, Rep.   year98   46286503
    ## 6130                     Korea, Rep.   year99   46616677
    ## 6131                     Korea, Rep. year2000   47008111
    ## 6132                     Korea, Rep. year2001   47370164
    ## 6133                     Korea, Rep. year2002   47644736
    ## 6134                     Korea, Rep. year2003   47892330
    ## 6135                     Korea, Rep. year2004   48082519
    ## 6136                     Korea, Rep. year2005   48184561
    ## 6137                     Korea, Rep. year2006   48438292
    ## 6138                     Korea, Rep. year2007   48683638
    ## 6139                     Korea, Rep. year2008   49054708
    ## 6140                     Korea, Rep. year2009   49307835
    ## 6141                     Korea, Rep. year2010   49554112
    ## 6142                     Korea, Rep. year2011   49936638
    ## 6143                     Korea, Rep. year2012   50199853
    ## 6144                     Korea, Rep. year2013   50428893
    ## 6145                     Korea, Rep. year2014   50746659
    ## 6146                     Korea, Rep. year2015   51014947
    ## 6147                     Korea, Rep. year2016   51245707
    ## 6148                     Korea, Rep. year2017   51466201
    ## 6149                          Kosovo   year60     947000
    ## 6150                          Kosovo   year61     966000
    ## 6151                          Kosovo   year62     994000
    ## 6152                          Kosovo   year63    1022000
    ## 6153                          Kosovo   year64    1050000
    ## 6154                          Kosovo   year65    1078000
    ## 6155                          Kosovo   year66    1106000
    ## 6156                          Kosovo   year67    1135000
    ## 6157                          Kosovo   year68    1163000
    ## 6158                          Kosovo   year69    1191000
    ## 6159                          Kosovo   year70    1219000
    ## 6160                          Kosovo   year71    1247000
    ## 6161                          Kosovo   year72    1278000
    ## 6162                          Kosovo   year73    1308000
    ## 6163                          Kosovo   year74    1339000
    ## 6164                          Kosovo   year75    1369000
    ## 6165                          Kosovo   year76    1400000
    ## 6166                          Kosovo   year77    1430000
    ## 6167                          Kosovo   year78    1460000
    ## 6168                          Kosovo   year79    1491000
    ## 6169                          Kosovo   year80    1521000
    ## 6170                          Kosovo   year81    1552000
    ## 6171                          Kosovo   year82    1582000
    ## 6172                          Kosovo   year83    1614000
    ## 6173                          Kosovo   year84    1647000
    ## 6174                          Kosovo   year85    1682000
    ## 6175                          Kosovo   year86    1717000
    ## 6176                          Kosovo   year87    1753000
    ## 6177                          Kosovo   year88    1791000
    ## 6178                          Kosovo   year89    1827000
    ## 6179                          Kosovo   year90    1862000
    ## 6180                          Kosovo   year91    1898000
    ## 6181                          Kosovo   year92    1932000
    ## 6182                          Kosovo   year93    1965000
    ## 6183                          Kosovo   year94    1997000
    ## 6184                          Kosovo   year95    2029000
    ## 6185                          Kosovo   year96    2059000
    ## 6186                          Kosovo   year97    2086000
    ## 6187                          Kosovo   year98    1966000
    ## 6188                          Kosovo   year99    1762000
    ## 6189                          Kosovo year2000    1700000
    ## 6190                          Kosovo year2001    1701154
    ## 6191                          Kosovo year2002    1702310
    ## 6192                          Kosovo year2003    1703466
    ## 6193                          Kosovo year2004    1704622
    ## 6194                          Kosovo year2005    1705780
    ## 6195                          Kosovo year2006    1719536
    ## 6196                          Kosovo year2007    1733404
    ## 6197                          Kosovo year2008    1747383
    ## 6198                          Kosovo year2009    1761474
    ## 6199                          Kosovo year2010    1775680
    ## 6200                          Kosovo year2011    1791000
    ## 6201                          Kosovo year2012    1805200
    ## 6202                          Kosovo year2013    1824100
    ## 6203                          Kosovo year2014    1821800
    ## 6204                          Kosovo year2015    1801800
    ## 6205                          Kosovo year2016    1816200
    ## 6206                          Kosovo year2017    1830700
    ## 6207                          Kuwait   year60     269618
    ## 6208                          Kuwait   year61     301336
    ## 6209                          Kuwait   year62     338296
    ## 6210                          Kuwait   year63     379891
    ## 6211                          Kuwait   year64     425235
    ## 6212                          Kuwait   year65     473554
    ## 6213                          Kuwait   year66     524856
    ## 6214                          Kuwait   year67     579007
    ## 6215                          Kuwait   year68     634897
    ## 6216                          Kuwait   year69     691129
    ## 6217                          Kuwait   year70     746767
    ## 6218                          Kuwait   year71     801142
    ## 6219                          Kuwait   year72     854604
    ## 6220                          Kuwait   year73     908520
    ## 6221                          Kuwait   year74     964834
    ## 6222                          Kuwait   year75    1024940
    ## 6223                          Kuwait   year76    1089209
    ## 6224                          Kuwait   year77    1157033
    ## 6225                          Kuwait   year78    1227601
    ## 6226                          Kuwait   year79    1299683
    ## 6227                          Kuwait   year80    1372318
    ## 6228                          Kuwait   year81    1442991
    ## 6229                          Kuwait   year82    1511314
    ## 6230                          Kuwait   year83    1580638
    ## 6231                          Kuwait   year84    1655833
    ## 6232                          Kuwait   year85    1738994
    ## 6233                          Kuwait   year86    1836105
    ## 6234                          Kuwait   year87    1942810
    ## 6235                          Kuwait   year88    2038885
    ## 6236                          Kuwait   year89    2096932
    ## 6237                          Kuwait   year90    2099615
    ## 6238                          Kuwait   year91    2035661
    ## 6239                          Kuwait   year92         NA
    ## 6240                          Kuwait   year93         NA
    ## 6241                          Kuwait   year94         NA
    ## 6242                          Kuwait   year95    1610651
    ## 6243                          Kuwait   year96    1631740
    ## 6244                          Kuwait   year97    1715314
    ## 6245                          Kuwait   year98    1836353
    ## 6246                          Kuwait   year99    1957066
    ## 6247                          Kuwait year2000    2050741
    ## 6248                          Kuwait year2001    2109355
    ## 6249                          Kuwait year2002    2143833
    ## 6250                          Kuwait year2003    2169118
    ## 6251                          Kuwait year2004    2207939
    ## 6252                          Kuwait year2005    2276623
    ## 6253                          Kuwait year2006    2377258
    ## 6254                          Kuwait year2007    2503410
    ## 6255                          Kuwait year2008    2652340
    ## 6256                          Kuwait year2009    2818939
    ## 6257                          Kuwait year2010    2998083
    ## 6258                          Kuwait year2011    3191051
    ## 6259                          Kuwait year2012    3395556
    ## 6260                          Kuwait year2013    3598385
    ## 6261                          Kuwait year2014    3782450
    ## 6262                          Kuwait year2015    3935794
    ## 6263                          Kuwait year2016    4052584
    ## 6264                          Kuwait year2017    4136528
    ## 6265                 Kyrgyz Republic   year60    2172300
    ## 6266                 Kyrgyz Republic   year61    2255900
    ## 6267                 Kyrgyz Republic   year62    2333400
    ## 6268                 Kyrgyz Republic   year63    2413700
    ## 6269                 Kyrgyz Republic   year64    2495300
    ## 6270                 Kyrgyz Republic   year65    2573300
    ## 6271                 Kyrgyz Republic   year66    2655300
    ## 6272                 Kyrgyz Republic   year67    2736500
    ## 6273                 Kyrgyz Republic   year68    2818300
    ## 6274                 Kyrgyz Republic   year69    2894800
    ## 6275                 Kyrgyz Republic   year70    2959900
    ## 6276                 Kyrgyz Republic   year71    3022300
    ## 6277                 Kyrgyz Republic   year72    3088200
    ## 6278                 Kyrgyz Republic   year73    3153800
    ## 6279                 Kyrgyz Republic   year74    3223900
    ## 6280                 Kyrgyz Republic   year75    3292400
    ## 6281                 Kyrgyz Republic   year76    3358700
    ## 6282                 Kyrgyz Republic   year77    3423900
    ## 6283                 Kyrgyz Republic   year78    3487100
    ## 6284                 Kyrgyz Republic   year79    3552000
    ## 6285                 Kyrgyz Republic   year80    3617400
    ## 6286                 Kyrgyz Republic   year81    3685800
    ## 6287                 Kyrgyz Republic   year82    3759300
    ## 6288                 Kyrgyz Republic   year83    3838300
    ## 6289                 Kyrgyz Republic   year84    3916400
    ## 6290                 Kyrgyz Republic   year85    3990300
    ## 6291                 Kyrgyz Republic   year86    4066500
    ## 6292                 Kyrgyz Republic   year87    4144600
    ## 6293                 Kyrgyz Republic   year88    4218400
    ## 6294                 Kyrgyz Republic   year89    4307500
    ## 6295                 Kyrgyz Republic   year90    4391200
    ## 6296                 Kyrgyz Republic   year91    4463600
    ## 6297                 Kyrgyz Republic   year92    4515400
    ## 6298                 Kyrgyz Republic   year93    4516700
    ## 6299                 Kyrgyz Republic   year94    4515100
    ## 6300                 Kyrgyz Republic   year95    4560400
    ## 6301                 Kyrgyz Republic   year96    4628400
    ## 6302                 Kyrgyz Republic   year97    4696400
    ## 6303                 Kyrgyz Republic   year98    4769000
    ## 6304                 Kyrgyz Republic   year99    4840400
    ## 6305                 Kyrgyz Republic year2000    4898400
    ## 6306                 Kyrgyz Republic year2001    4945100
    ## 6307                 Kyrgyz Republic year2002    4990700
    ## 6308                 Kyrgyz Republic year2003    5043300
    ## 6309                 Kyrgyz Republic year2004    5104700
    ## 6310                 Kyrgyz Republic year2005    5162600
    ## 6311                 Kyrgyz Republic year2006    5218400
    ## 6312                 Kyrgyz Republic year2007    5268400
    ## 6313                 Kyrgyz Republic year2008    5318700
    ## 6314                 Kyrgyz Republic year2009    5383300
    ## 6315                 Kyrgyz Republic year2010    5447900
    ## 6316                 Kyrgyz Republic year2011    5514600
    ## 6317                 Kyrgyz Republic year2012    5607200
    ## 6318                 Kyrgyz Republic year2013    5719600
    ## 6319                 Kyrgyz Republic year2014    5835500
    ## 6320                 Kyrgyz Republic year2015    5956900
    ## 6321                 Kyrgyz Republic year2016    6079500
    ## 6322                 Kyrgyz Republic year2017    6201500
    ## 6323                         Lao PDR   year60    2120896
    ## 6324                         Lao PDR   year61    2170343
    ## 6325                         Lao PDR   year62    2221122
    ## 6326                         Lao PDR   year63    2273349
    ## 6327                         Lao PDR   year64    2327137
    ## 6328                         Lao PDR   year65    2382594
    ## 6329                         Lao PDR   year66    2439196
    ## 6330                         Lao PDR   year67    2496920
    ## 6331                         Lao PDR   year68    2556852
    ## 6332                         Lao PDR   year69    2620434
    ## 6333                         Lao PDR   year70    2688428
    ## 6334                         Lao PDR   year71    2762265
    ## 6335                         Lao PDR   year72    2840841
    ## 6336                         Lao PDR   year73    2919287
    ## 6337                         Lao PDR   year74    2990965
    ## 6338                         Lao PDR   year75    3051577
    ## 6339                         Lao PDR   year76    3098973
    ## 6340                         Lao PDR   year77    3135842
    ## 6341                         Lao PDR   year78    3168843
    ## 6342                         Lao PDR   year79    3207328
    ## 6343                         Lao PDR   year80    3258144
    ## 6344                         Lao PDR   year81    3323377
    ## 6345                         Lao PDR   year82    3401242
    ## 6346                         Lao PDR   year83    3489977
    ## 6347                         Lao PDR   year84    3586381
    ## 6348                         Lao PDR   year85    3687898
    ## 6349                         Lao PDR   year86    3794043
    ## 6350                         Lao PDR   year87    3905163
    ## 6351                         Lao PDR   year88    4020295
    ## 6352                         Lao PDR   year89    4138408
    ## 6353                         Lao PDR   year90    4258472
    ## 6354                         Lao PDR   year91    4380073
    ## 6355                         Lao PDR   year92    4502363
    ## 6356                         Lao PDR   year93    4623280
    ## 6357                         Lao PDR   year94    4740380
    ## 6358                         Lao PDR   year95    4851923
    ## 6359                         Lao PDR   year96    4957180
    ## 6360                         Lao PDR   year97    5056519
    ## 6361                         Lao PDR   year98    5150763
    ## 6362                         Lao PDR   year99    5241284
    ## 6363                         Lao PDR year2000    5329304
    ## 6364                         Lao PDR year2001    5414568
    ## 6365                         Lao PDR year2002    5497273
    ## 6366                         Lao PDR year2003    5579656
    ## 6367                         Lao PDR year2004    5664605
    ## 6368                         Lao PDR year2005    5754026
    ## 6369                         Lao PDR year2006    5849356
    ## 6370                         Lao PDR year2007    5949787
    ## 6371                         Lao PDR year2008    6052190
    ## 6372                         Lao PDR year2009    6152036
    ## 6373                         Lao PDR year2010    6246274
    ## 6374                         Lao PDR year2011    6333487
    ## 6375                         Lao PDR year2012    6415169
    ## 6376                         Lao PDR year2013    6494557
    ## 6377                         Lao PDR year2014    6576397
    ## 6378                         Lao PDR year2015    6663967
    ## 6379                         Lao PDR year2016    6758353
    ## 6380                         Lao PDR year2017    6858160
    ## 6381                          Latvia   year60    2120979
    ## 6382                          Latvia   year61    2152681
    ## 6383                          Latvia   year62    2181586
    ## 6384                          Latvia   year63    2210919
    ## 6385                          Latvia   year64    2240623
    ## 6386                          Latvia   year65    2265919
    ## 6387                          Latvia   year66    2283217
    ## 6388                          Latvia   year67    2301220
    ## 6389                          Latvia   year68    2323619
    ## 6390                          Latvia   year69    2343173
    ## 6391                          Latvia   year70    2359164
    ## 6392                          Latvia   year71    2376389
    ## 6393                          Latvia   year72    2395674
    ## 6394                          Latvia   year73    2415819
    ## 6395                          Latvia   year74    2437186
    ## 6396                          Latvia   year75    2456130
    ## 6397                          Latvia   year76    2470989
    ## 6398                          Latvia   year77    2485073
    ## 6399                          Latvia   year78    2497921
    ## 6400                          Latvia   year79    2505953
    ## 6401                          Latvia   year80    2511701
    ## 6402                          Latvia   year81    2519421
    ## 6403                          Latvia   year82    2531080
    ## 6404                          Latvia   year83    2546011
    ## 6405                          Latvia   year84    2562047
    ## 6406                          Latvia   year85    2578873
    ## 6407                          Latvia   year86    2599892
    ## 6408                          Latvia   year87    2626583
    ## 6409                          Latvia   year88    2653434
    ## 6410                          Latvia   year89    2666955
    ## 6411                          Latvia   year90    2663151
    ## 6412                          Latvia   year91    2650581
    ## 6413                          Latvia   year92    2614338
    ## 6414                          Latvia   year93    2563290
    ## 6415                          Latvia   year94    2520742
    ## 6416                          Latvia   year95    2485056
    ## 6417                          Latvia   year96    2457222
    ## 6418                          Latvia   year97    2432851
    ## 6419                          Latvia   year98    2410019
    ## 6420                          Latvia   year99    2390482
    ## 6421                          Latvia year2000    2367550
    ## 6422                          Latvia year2001    2337170
    ## 6423                          Latvia year2002    2310173
    ## 6424                          Latvia year2003    2287955
    ## 6425                          Latvia year2004    2263122
    ## 6426                          Latvia year2005    2238799
    ## 6427                          Latvia year2006    2218357
    ## 6428                          Latvia year2007    2200325
    ## 6429                          Latvia year2008    2177322
    ## 6430                          Latvia year2009    2141669
    ## 6431                          Latvia year2010    2097555
    ## 6432                          Latvia year2011    2059709
    ## 6433                          Latvia year2012    2034319
    ## 6434                          Latvia year2013    2012647
    ## 6435                          Latvia year2014    1993782
    ## 6436                          Latvia year2015    1977527
    ## 6437                          Latvia year2016    1959537
    ## 6438                          Latvia year2017    1940740
    ## 6439                         Lebanon   year60    1804926
    ## 6440                         Lebanon   year61    1864605
    ## 6441                         Lebanon   year62    1925276
    ## 6442                         Lebanon   year63    1984980
    ## 6443                         Lebanon   year64    2041207
    ## 6444                         Lebanon   year65    2092348
    ## 6445                         Lebanon   year66    2136636
    ## 6446                         Lebanon   year67    2174845
    ## 6447                         Lebanon   year68    2210959
    ## 6448                         Lebanon   year69    2250602
    ## 6449                         Lebanon   year70    2297389
    ## 6450                         Lebanon   year71    2353555
    ## 6451                         Lebanon   year72    2416735
    ## 6452                         Lebanon   year73    2480419
    ## 6453                         Lebanon   year74    2535497
    ## 6454                         Lebanon   year75    2575690
    ## 6455                         Lebanon   year76    2598354
    ## 6456                         Lebanon   year77    2606221
    ## 6457                         Lebanon   year78    2604865
    ## 6458                         Lebanon   year79    2602566
    ## 6459                         Lebanon   year80    2605293
    ## 6460                         Lebanon   year81    2615747
    ## 6461                         Lebanon   year82    2632276
    ## 6462                         Lebanon   year83    2651292
    ## 6463                         Lebanon   year84    2667220
    ## 6464                         Lebanon   year85    2676583
    ## 6465                         Lebanon   year86    2677280
    ## 6466                         Lebanon   year87    2672173
    ## 6467                         Lebanon   year88    2668585
    ## 6468                         Lebanon   year89    2676605
    ## 6469                         Lebanon   year90    2703016
    ## 6470                         Lebanon   year91    2752462
    ## 6471                         Lebanon   year92    2821862
    ## 6472                         Lebanon   year93    2900854
    ## 6473                         Lebanon   year94    2974640
    ## 6474                         Lebanon   year95    3033394
    ## 6475                         Lebanon   year96    3070960
    ## 6476                         Lebanon   year97    3092670
    ## 6477                         Lebanon   year98    3113951
    ## 6478                         Lebanon   year99    3156646
    ## 6479                         Lebanon year2000    3235366
    ## 6480                         Lebanon year2001    3359859
    ## 6481                         Lebanon year2002    3522837
    ## 6482                         Lebanon year2003    3701464
    ## 6483                         Lebanon year2004    3863267
    ## 6484                         Lebanon year2005    3986852
    ## 6485                         Lebanon year2006    4057350
    ## 6486                         Lebanon year2007    4086466
    ## 6487                         Lebanon year2008    4111047
    ## 6488                         Lebanon year2009    4183156
    ## 6489                         Lebanon year2010    4337141
    ## 6490                         Lebanon year2011    4588368
    ## 6491                         Lebanon year2012    4916404
    ## 6492                         Lebanon year2013    5276102
    ## 6493                         Lebanon year2014    5603279
    ## 6494                         Lebanon year2015    5851479
    ## 6495                         Lebanon year2016    6006668
    ## 6496                         Lebanon year2017    6082357
    ## 6497                         Lesotho   year60     851591
    ## 6498                         Lesotho   year61     866462
    ## 6499                         Lesotho   year62     882170
    ## 6500                         Lesotho   year63     898647
    ## 6501                         Lesotho   year64     915822
    ## 6502                         Lesotho   year65     933655
    ## 6503                         Lesotho   year66     952206
    ## 6504                         Lesotho   year67     971512
    ## 6505                         Lesotho   year68     991491
    ## 6506                         Lesotho   year69    1012015
    ## 6507                         Lesotho   year70    1033050
    ## 6508                         Lesotho   year71    1054453
    ## 6509                         Lesotho   year72    1076340
    ## 6510                         Lesotho   year73    1099235
    ## 6511                         Lesotho   year74    1123855
    ## 6512                         Lesotho   year75    1150635
    ## 6513                         Lesotho   year76    1179723
    ## 6514                         Lesotho   year77    1210799
    ## 6515                         Lesotho   year78    1243352
    ## 6516                         Lesotho   year79    1276663
    ## 6517                         Lesotho   year80    1310118
    ## 6518                         Lesotho   year81    1343690
    ## 6519                         Lesotho   year82    1377346
    ## 6520                         Lesotho   year83    1410439
    ## 6521                         Lesotho   year84    1442212
    ## 6522                         Lesotho   year85    1472192
    ## 6523                         Lesotho   year86    1499861
    ## 6524                         Lesotho   year87    1525460
    ## 6525                         Lesotho   year88    1550262
    ## 6526                         Lesotho   year89    1576022
    ## 6527                         Lesotho   year90    1603938
    ## 6528                         Lesotho   year91    1634517
    ## 6529                         Lesotho   year92    1667121
    ## 6530                         Lesotho   year93    1700362
    ## 6531                         Lesotho   year94    1732257
    ## 6532                         Lesotho   year95    1761359
    ## 6533                         Lesotho   year96    1787273
    ## 6534                         Lesotho   year97    1810453
    ## 6535                         Lesotho   year98    1831298
    ## 6536                         Lesotho   year99    1850527
    ## 6537                         Lesotho year2000    1868699
    ## 6538                         Lesotho year2001    1885955
    ## 6539                         Lesotho year2002    1902312
    ## 6540                         Lesotho year2003    1918097
    ## 6541                         Lesotho year2004    1933728
    ## 6542                         Lesotho year2005    1949543
    ## 6543                         Lesotho year2006    1965662
    ## 6544                         Lesotho year2007    1982287
    ## 6545                         Lesotho year2008    1999930
    ## 6546                         Lesotho year2009    2019209
    ## 6547                         Lesotho year2010    2040551
    ## 6548                         Lesotho year2011    2064166
    ## 6549                         Lesotho year2012    2089928
    ## 6550                         Lesotho year2013    2117361
    ## 6551                         Lesotho year2014    2145785
    ## 6552                         Lesotho year2015    2174645
    ## 6553                         Lesotho year2016    2203821
    ## 6554                         Lesotho year2017    2233339
    ## 6555                         Liberia   year60    1120313
    ## 6556                         Liberia   year61    1144986
    ## 6557                         Liberia   year62    1170480
    ## 6558                         Liberia   year63    1196890
    ## 6559                         Liberia   year64    1224344
    ## 6560                         Liberia   year65    1252972
    ## 6561                         Liberia   year66    1282814
    ## 6562                         Liberia   year67    1313941
    ## 6563                         Liberia   year68    1346491
    ## 6564                         Liberia   year69    1380637
    ## 6565                         Liberia   year70    1416529
    ## 6566                         Liberia   year71    1454198
    ## 6567                         Liberia   year72    1493711
    ## 6568                         Liberia   year73    1535229
    ## 6569                         Liberia   year74    1578952
    ## 6570                         Liberia   year75    1625013
    ## 6571                         Liberia   year76    1672300
    ## 6572                         Liberia   year77    1720489
    ## 6573                         Liberia   year78    1771256
    ## 6574                         Liberia   year79    1826881
    ## 6575                         Liberia   year80    1888314
    ## 6576                         Liberia   year81    1957456
    ## 6577                         Liberia   year82    2031850
    ## 6578                         Liberia   year83    2102911
    ## 6579                         Liberia   year84    2159089
    ## 6580                         Liberia   year85    2192555
    ## 6581                         Liberia   year86    2201833
    ## 6582                         Liberia   year87    2191023
    ## 6583                         Liberia   year88    2165090
    ## 6584                         Liberia   year89    2131525
    ## 6585                         Liberia   year90    2097232
    ## 6586                         Liberia   year91    2060267
    ## 6587                         Liberia   year92    2022729
    ## 6588                         Liberia   year93    2000248
    ## 6589                         Liberia   year94    2012885
    ## 6590                         Liberia   year95    2073482
    ## 6591                         Liberia   year96    2191179
    ## 6592                         Liberia   year97    2358469
    ## 6593                         Liberia   year98    2551062
    ## 6594                         Liberia   year99    2734518
    ## 6595                         Liberia year2000    2884522
    ## 6596                         Liberia year2001    2991132
    ## 6597                         Liberia year2002    3062863
    ## 6598                         Liberia year2003    3116233
    ## 6599                         Liberia year2004    3176414
    ## 6600                         Liberia year2005    3261230
    ## 6601                         Liberia year2006    3375838
    ## 6602                         Liberia year2007    3512932
    ## 6603                         Liberia year2008    3662993
    ## 6604                         Liberia year2009    3811528
    ## 6605                         Liberia year2010    3948125
    ## 6606                         Liberia year2011    4070167
    ## 6607                         Liberia year2012    4181563
    ## 6608                         Liberia year2013    4286291
    ## 6609                         Liberia year2014    4390737
    ## 6610                         Liberia year2015    4499621
    ## 6611                         Liberia year2016    4613823
    ## 6612                         Liberia year2017    4731906
    ## 6613                           Libya   year60    1448417
    ## 6614                           Libya   year61    1498071
    ## 6615                           Libya   year62    1550813
    ## 6616                           Libya   year63    1607171
    ## 6617                           Libya   year64    1667825
    ## 6618                           Libya   year65    1733306
    ## 6619                           Libya   year66    1803683
    ## 6620                           Libya   year67    1878877
    ## 6621                           Libya   year68    1958914
    ## 6622                           Libya   year69    2043818
    ## 6623                           Libya   year70    2133526
    ## 6624                           Libya   year71    2228146
    ## 6625                           Libya   year72    2327490
    ## 6626                           Libya   year73    2430755
    ## 6627                           Libya   year74    2536888
    ## 6628                           Libya   year75    2645139
    ## 6629                           Libya   year76    2754696
    ## 6630                           Libya   year77    2865637
    ## 6631                           Libya   year78    2979093
    ## 6632                           Libya   year79    3096729
    ## 6633                           Libya   year80    3219466
    ## 6634                           Libya   year81    3347781
    ## 6635                           Libya   year82    3480454
    ## 6636                           Libya   year83    3614689
    ## 6637                           Libya   year84    3746715
    ## 6638                           Libya   year85    3873781
    ## 6639                           Libya   year86    3994591
    ## 6640                           Libya   year87    4109703
    ## 6641                           Libya   year88    4220418
    ## 6642                           Libya   year89    4328914
    ## 6643                           Libya   year90    4436661
    ## 6644                           Libya   year91    4544293
    ## 6645                           Libya   year92    4651004
    ## 6646                           Libya   year93    4755289
    ## 6647                           Libya   year94    4855003
    ## 6648                           Libya   year95    4948798
    ## 6649                           Libya   year96    5035884
    ## 6650                           Libya   year97    5117269
    ## 6651                           Libya   year98    5195502
    ## 6652                           Libya   year99    5274163
    ## 6653                           Libya year2000    5355751
    ## 6654                           Libya year2001    5440566
    ## 6655                           Libya year2002    5527515
    ## 6656                           Libya year2003    5615952
    ## 6657                           Libya year2004    5704759
    ## 6658                           Libya year2005    5792688
    ## 6659                           Libya year2006    5881435
    ## 6660                           Libya year2007    5970362
    ## 6661                           Libya year2008    6053078
    ## 6662                           Libya year2009    6121053
    ## 6663                           Libya year2010    6169140
    ## 6664                           Libya year2011    6193501
    ## 6665                           Libya year2012    6198258
    ## 6666                           Libya year2013    6195970
    ## 6667                           Libya year2014    6204108
    ## 6668                           Libya year2015    6234955
    ## 6669                           Libya year2016    6293253
    ## 6670                           Libya year2017    6374616
    ## 6671                   Liechtenstein   year60      16495
    ## 6672                   Liechtenstein   year61      16894
    ## 6673                   Liechtenstein   year62      17290
    ## 6674                   Liechtenstein   year63      17718
    ## 6675                   Liechtenstein   year64      18170
    ## 6676                   Liechtenstein   year65      18649
    ## 6677                   Liechtenstein   year66      19153
    ## 6678                   Liechtenstein   year67      19691
    ## 6679                   Liechtenstein   year68      20236
    ## 6680                   Liechtenstein   year69      20765
    ## 6681                   Liechtenstein   year70      21265
    ## 6682                   Liechtenstein   year71      21726
    ## 6683                   Liechtenstein   year72      22151
    ## 6684                   Liechtenstein   year73      22563
    ## 6685                   Liechtenstein   year74      22981
    ## 6686                   Liechtenstein   year75      23432
    ## 6687                   Liechtenstein   year76      23926
    ## 6688                   Liechtenstein   year77      24440
    ## 6689                   Liechtenstein   year78      24962
    ## 6690                   Liechtenstein   year79      25447
    ## 6691                   Liechtenstein   year80      25866
    ## 6692                   Liechtenstein   year81      26224
    ## 6693                   Liechtenstein   year82      26515
    ## 6694                   Liechtenstein   year83      26765
    ## 6695                   Liechtenstein   year84      27011
    ## 6696                   Liechtenstein   year85      27257
    ## 6697                   Liechtenstein   year86      27524
    ## 6698                   Liechtenstein   year87      27802
    ## 6699                   Liechtenstein   year88      28095
    ## 6700                   Liechtenstein   year89      28407
    ## 6701                   Liechtenstein   year90      28747
    ## 6702                   Liechtenstein   year91      29108
    ## 6703                   Liechtenstein   year92      29497
    ## 6704                   Liechtenstein   year93      29919
    ## 6705                   Liechtenstein   year94      30365
    ## 6706                   Liechtenstein   year95      30833
    ## 6707                   Liechtenstein   year96      31325
    ## 6708                   Liechtenstein   year97      31838
    ## 6709                   Liechtenstein   year98      32355
    ## 6710                   Liechtenstein   year99      32842
    ## 6711                   Liechtenstein year2000      33286
    ## 6712                   Liechtenstein year2001      33671
    ## 6713                   Liechtenstein year2002      34018
    ## 6714                   Liechtenstein year2003      34321
    ## 6715                   Liechtenstein year2004      34596
    ## 6716                   Liechtenstein year2005      34852
    ## 6717                   Liechtenstein year2006      35095
    ## 6718                   Liechtenstein year2007      35322
    ## 6719                   Liechtenstein year2008      35541
    ## 6720                   Liechtenstein year2009      35766
    ## 6721                   Liechtenstein year2010      36003
    ## 6722                   Liechtenstein year2011      36264
    ## 6723                   Liechtenstein year2012      36545
    ## 6724                   Liechtenstein year2013      36834
    ## 6725                   Liechtenstein year2014      37127
    ## 6726                   Liechtenstein year2015      37403
    ## 6727                   Liechtenstein year2016      37666
    ## 6728                   Liechtenstein year2017      37922
    ## 6729                       Lithuania   year60    2778550
    ## 6730                       Lithuania   year61    2823550
    ## 6731                       Lithuania   year62    2863350
    ## 6732                       Lithuania   year63    2898950
    ## 6733                       Lithuania   year64    2935200
    ## 6734                       Lithuania   year65    2971450
    ## 6735                       Lithuania   year66    3008050
    ## 6736                       Lithuania   year67    3044400
    ## 6737                       Lithuania   year68    3078850
    ## 6738                       Lithuania   year69    3107321
    ## 6739                       Lithuania   year70    3139689
    ## 6740                       Lithuania   year71    3179041
    ## 6741                       Lithuania   year72    3213622
    ## 6742                       Lithuania   year73    3244438
    ## 6743                       Lithuania   year74    3273894
    ## 6744                       Lithuania   year75    3301652
    ## 6745                       Lithuania   year76    3328664
    ## 6746                       Lithuania   year77    3355036
    ## 6747                       Lithuania   year78    3379514
    ## 6748                       Lithuania   year79    3397842
    ## 6749                       Lithuania   year80    3413202
    ## 6750                       Lithuania   year81    3432947
    ## 6751                       Lithuania   year82    3457179
    ## 6752                       Lithuania   year83    3485192
    ## 6753                       Lithuania   year84    3514205
    ## 6754                       Lithuania   year85    3544543
    ## 6755                       Lithuania   year86    3578914
    ## 6756                       Lithuania   year87    3616367
    ## 6757                       Lithuania   year88    3655049
    ## 6758                       Lithuania   year89    3684255
    ## 6759                       Lithuania   year90    3697838
    ## 6760                       Lithuania   year91    3704134
    ## 6761                       Lithuania   year92    3700114
    ## 6762                       Lithuania   year93    3682613
    ## 6763                       Lithuania   year94    3657144
    ## 6764                       Lithuania   year95    3629102
    ## 6765                       Lithuania   year96    3601613
    ## 6766                       Lithuania   year97    3575137
    ## 6767                       Lithuania   year98    3549331
    ## 6768                       Lithuania   year99    3524238
    ## 6769                       Lithuania year2000    3499536
    ## 6770                       Lithuania year2001    3470818
    ## 6771                       Lithuania year2002    3443067
    ## 6772                       Lithuania year2003    3415213
    ## 6773                       Lithuania year2004    3377075
    ## 6774                       Lithuania year2005    3322528
    ## 6775                       Lithuania year2006    3269909
    ## 6776                       Lithuania year2007    3231294
    ## 6777                       Lithuania year2008    3198231
    ## 6778                       Lithuania year2009    3162916
    ## 6779                       Lithuania year2010    3097282
    ## 6780                       Lithuania year2011    3028115
    ## 6781                       Lithuania year2012    2987773
    ## 6782                       Lithuania year2013    2957689
    ## 6783                       Lithuania year2014    2932367
    ## 6784                       Lithuania year2015    2904910
    ## 6785                       Lithuania year2016    2868231
    ## 6786                       Lithuania year2017    2827721
    ## 6787                      Luxembourg   year60     313970
    ## 6788                      Luxembourg   year61     316845
    ## 6789                      Luxembourg   year62     320750
    ## 6790                      Luxembourg   year63     324100
    ## 6791                      Luxembourg   year64     327750
    ## 6792                      Luxembourg   year65     331500
    ## 6793                      Luxembourg   year66     333895
    ## 6794                      Luxembourg   year67     334995
    ## 6795                      Luxembourg   year68     335850
    ## 6796                      Luxembourg   year69     337500
    ## 6797                      Luxembourg   year70     339171
    ## 6798                      Luxembourg   year71     342421
    ## 6799                      Luxembourg   year72     346600
    ## 6800                      Luxembourg   year73     350450
    ## 6801                      Luxembourg   year74     355050
    ## 6802                      Luxembourg   year75     358950
    ## 6803                      Luxembourg   year76     360731
    ## 6804                      Luxembourg   year77     361358
    ## 6805                      Luxembourg   year78     362007
    ## 6806                      Luxembourg   year79     362856
    ## 6807                      Luxembourg   year80     364150
    ## 6808                      Luxembourg   year81     365225
    ## 6809                      Luxembourg   year82     365525
    ## 6810                      Luxembourg   year83     365622
    ## 6811                      Luxembourg   year84     365998
    ## 6812                      Luxembourg   year85     366706
    ## 6813                      Luxembourg   year86     368355
    ## 6814                      Luxembourg   year87     370750
    ## 6815                      Luxembourg   year88     373450
    ## 6816                      Luxembourg   year89     377100
    ## 6817                      Luxembourg   year90     381850
    ## 6818                      Luxembourg   year91     387000
    ## 6819                      Luxembourg   year92     392175
    ## 6820                      Luxembourg   year93     397475
    ## 6821                      Luxembourg   year94     402925
    ## 6822                      Luxembourg   year95     408625
    ## 6823                      Luxembourg   year96     414225
    ## 6824                      Luxembourg   year97     419450
    ## 6825                      Luxembourg   year98     424700
    ## 6826                      Luxembourg   year99     430475
    ## 6827                      Luxembourg year2000     436300
    ## 6828                      Luxembourg year2001     441525
    ## 6829                      Luxembourg year2002     446175
    ## 6830                      Luxembourg year2003     451630
    ## 6831                      Luxembourg year2004     458095
    ## 6832                      Luxembourg year2005     465158
    ## 6833                      Luxembourg year2006     472637
    ## 6834                      Luxembourg year2007     479993
    ## 6835                      Luxembourg year2008     488650
    ## 6836                      Luxembourg year2009     497783
    ## 6837                      Luxembourg year2010     506953
    ## 6838                      Luxembourg year2011     518347
    ## 6839                      Luxembourg year2012     530946
    ## 6840                      Luxembourg year2013     543360
    ## 6841                      Luxembourg year2014     556319
    ## 6842                      Luxembourg year2015     569604
    ## 6843                      Luxembourg year2016     582014
    ## 6844                      Luxembourg year2017     599449
    ## 6845                Macao SAR, China   year60     167796
    ## 6846                Macao SAR, China   year61     170465
    ## 6847                Macao SAR, China   year62     176188
    ## 6848                Macao SAR, China   year63     184250
    ## 6849                Macao SAR, China   year64     193563
    ## 6850                Macao SAR, China   year65     203231
    ## 6851                Macao SAR, China   year66     213196
    ## 6852                Macao SAR, China   year67     223420
    ## 6853                Macao SAR, China   year68     233004
    ## 6854                Macao SAR, China   year69     240842
    ## 6855                Macao SAR, China   year70     246195
    ## 6856                Macao SAR, China   year71     248739
    ## 6857                Macao SAR, China   year72     248767
    ## 6858                Macao SAR, China   year73     246947
    ## 6859                Macao SAR, China   year74     244284
    ## 6860                Macao SAR, China   year75     241628
    ## 6861                Macao SAR, China   year76     239085
    ## 6862                Macao SAR, China   year77     236695
    ## 6863                Macao SAR, China   year78     235198
    ## 6864                Macao SAR, China   year79     235479
    ## 6865                Macao SAR, China   year80     238118
    ## 6866                Macao SAR, China   year81     243427
    ## 6867                Macao SAR, China   year82     251219
    ## 6868                Macao SAR, China   year83     260997
    ## 6869                Macao SAR, China   year84     271993
    ## 6870                Macao SAR, China   year85     283581
    ## 6871                Macao SAR, China   year86     295677
    ## 6872                Macao SAR, China   year87     308275
    ## 6873                Macao SAR, China   year88     320877
    ## 6874                Macao SAR, China   year89     332901
    ## 6875                Macao SAR, China   year90     343935
    ## 6876                Macao SAR, China   year91     353764
    ## 6877                Macao SAR, China   year92     362459
    ## 6878                Macao SAR, China   year93     370345
    ## 6879                Macao SAR, China   year94     377960
    ## 6880                Macao SAR, China   year95     385686
    ## 6881                Macao SAR, China   year96     393567
    ## 6882                Macao SAR, China   year97     401564
    ## 6883                Macao SAR, China   year98     409837
    ## 6884                Macao SAR, China   year99     418604
    ## 6885                Macao SAR, China year2000     427979
    ## 6886                Macao SAR, China year2001     438081
    ## 6887                Macao SAR, China year2002     448896
    ## 6888                Macao SAR, China year2003     460147
    ## 6889                Macao SAR, China year2004     471453
    ## 6890                Macao SAR, China year2005     482559
    ## 6891                Macao SAR, China year2006     493320
    ## 6892                Macao SAR, China year2007     503823
    ## 6893                Macao SAR, China year2008     514348
    ## 6894                Macao SAR, China year2009     525313
    ## 6895                Macao SAR, China year2010     536969
    ## 6896                Macao SAR, China year2011     549439
    ## 6897                Macao SAR, China year2012     562531
    ## 6898                Macao SAR, China year2013     575841
    ## 6899                Macao SAR, China year2014     588781
    ## 6900                Macao SAR, China year2015     600942
    ## 6901                Macao SAR, China year2016     612167
    ## 6902                Macao SAR, China year2017     622567
    ## 6903                  Macedonia, FYR   year60    1488667
    ## 6904                  Macedonia, FYR   year61    1507654
    ## 6905                  Macedonia, FYR   year62    1527111
    ## 6906                  Macedonia, FYR   year63    1547450
    ## 6907                  Macedonia, FYR   year64    1569141
    ## 6908                  Macedonia, FYR   year65    1592432
    ## 6909                  Macedonia, FYR   year66    1617794
    ## 6910                  Macedonia, FYR   year67    1644943
    ## 6911                  Macedonia, FYR   year68    1672399
    ## 6912                  Macedonia, FYR   year69    1698143
    ## 6913                  Macedonia, FYR   year70    1720800
    ## 6914                  Macedonia, FYR   year71    1739521
    ## 6915                  Macedonia, FYR   year72    1754956
    ## 6916                  Macedonia, FYR   year73    1768992
    ## 6917                  Macedonia, FYR   year74    1784398
    ## 6918                  Macedonia, FYR   year75    1803010
    ## 6919                  Macedonia, FYR   year76    1825552
    ## 6920                  Macedonia, FYR   year77    1851069
    ## 6921                  Macedonia, FYR   year78    1877688
    ## 6922                  Macedonia, FYR   year79    1902719
    ## 6923                  Macedonia, FYR   year80    1924197
    ## 6924                  Macedonia, FYR   year81    1941530
    ## 6925                  Macedonia, FYR   year82    1955243
    ## 6926                  Macedonia, FYR   year83    1965895
    ## 6927                  Macedonia, FYR   year84    1974415
    ## 6928                  Macedonia, FYR   year85    1981534
    ## 6929                  Macedonia, FYR   year86    1987538
    ## 6930                  Macedonia, FYR   year87    1992274
    ## 6931                  Macedonia, FYR   year88    1995513
    ## 6932                  Macedonia, FYR   year89    1996870
    ## 6933                  Macedonia, FYR   year90    1996228
    ## 6934                  Macedonia, FYR   year91    1993302
    ## 6935                  Macedonia, FYR   year92    1988659
    ## 6936                  Macedonia, FYR   year93    1984028
    ## 6937                  Macedonia, FYR   year94    1981703
    ## 6938                  Macedonia, FYR   year95    1983252
    ## 6939                  Macedonia, FYR   year96    1989443
    ## 6940                  Macedonia, FYR   year97    1999599
    ## 6941                  Macedonia, FYR   year98    2012057
    ## 6942                  Macedonia, FYR   year99    2024394
    ## 6943                  Macedonia, FYR year2000    2034819
    ## 6944                  Macedonia, FYR year2001    2042842
    ## 6945                  Macedonia, FYR year2002    2048928
    ## 6946                  Macedonia, FYR year2003    2053426
    ## 6947                  Macedonia, FYR year2004    2057047
    ## 6948                  Macedonia, FYR year2005    2060272
    ## 6949                  Macedonia, FYR year2006    2063145
    ## 6950                  Macedonia, FYR year2007    2065458
    ## 6951                  Macedonia, FYR year2008    2067378
    ## 6952                  Macedonia, FYR year2009    2069093
    ## 6953                  Macedonia, FYR year2010    2070739
    ## 6954                  Macedonia, FYR year2011    2072383
    ## 6955                  Macedonia, FYR year2012    2074036
    ## 6956                  Macedonia, FYR year2013    2075739
    ## 6957                  Macedonia, FYR year2014    2077495
    ## 6958                  Macedonia, FYR year2015    2079308
    ## 6959                  Macedonia, FYR year2016    2081206
    ## 6960                  Macedonia, FYR year2017    2083160
    ## 6961                      Madagascar   year60    5099373
    ## 6962                      Madagascar   year61    5223568
    ## 6963                      Madagascar   year62    5352503
    ## 6964                      Madagascar   year63    5486319
    ## 6965                      Madagascar   year64    5625164
    ## 6966                      Madagascar   year65    5769218
    ## 6967                      Madagascar   year66    5918595
    ## 6968                      Madagascar   year67    6073526
    ## 6969                      Madagascar   year68    6234465
    ## 6970                      Madagascar   year69    6401921
    ## 6971                      Madagascar   year70    6576305
    ## 6972                      Madagascar   year71    6757850
    ## 6973                      Madagascar   year72    6946620
    ## 6974                      Madagascar   year73    7142627
    ## 6975                      Madagascar   year74    7345780
    ## 6976                      Madagascar   year75    7556026
    ## 6977                      Madagascar   year76    7773449
    ## 6978                      Madagascar   year77    7998164
    ## 6979                      Madagascar   year78    8230218
    ## 6980                      Madagascar   year79    8469672
    ## 6981                      Madagascar   year80    8716553
    ## 6982                      Madagascar   year81    8971345
    ## 6983                      Madagascar   year82    9234129
    ## 6984                      Madagascar   year83    9504281
    ## 6985                      Madagascar   year84    9780872
    ## 6986                      Madagascar   year85   10063495
    ## 6987                      Madagascar   year86   10352120
    ## 6988                      Madagascar   year87   10647754
    ## 6989                      Madagascar   year88   10952395
    ## 6990                      Madagascar   year89   11268658
    ## 6991                      Madagascar   year90   11598633
    ## 6992                      Madagascar   year91   11942819
    ## 6993                      Madagascar   year92   12301336
    ## 6994                      Madagascar   year93   12675460
    ## 6995                      Madagascar   year94   13066543
    ## 6996                      Madagascar   year95   13475400
    ## 6997                      Madagascar   year96   13902688
    ## 6998                      Madagascar   year97   14347854
    ## 6999                      Madagascar   year98   14808791
    ## 7000                      Madagascar   year99   15282521
    ## 7001                      Madagascar year2000   15766806
    ## 7002                      Madagascar year2001   16260932
    ## 7003                      Madagascar year2002   16765117
    ## 7004                      Madagascar year2003   17279141
    ## 7005                      Madagascar year2004   17802997
    ## 7006                      Madagascar year2005   18336724
    ## 7007                      Madagascar year2006   18880268
    ## 7008                      Madagascar year2007   19433523
    ## 7009                      Madagascar year2008   19996469
    ## 7010                      Madagascar year2009   20569121
    ## 7011                      Madagascar year2010   21151640
    ## 7012                      Madagascar year2011   21743949
    ## 7013                      Madagascar year2012   22346573
    ## 7014                      Madagascar year2013   22961146
    ## 7015                      Madagascar year2014   23589801
    ## 7016                      Madagascar year2015   24234088
    ## 7017                      Madagascar year2016   24894551
    ## 7018                      Madagascar year2017   25570895
    ## 7019                          Malawi   year60    3618595
    ## 7020                          Malawi   year61    3700023
    ## 7021                          Malawi   year62    3784439
    ## 7022                          Malawi   year63    3872118
    ## 7023                          Malawi   year64    3963417
    ## 7024                          Malawi   year65    4058673
    ## 7025                          Malawi   year66    4158124
    ## 7026                          Malawi   year67    4262005
    ## 7027                          Malawi   year68    4370650
    ## 7028                          Malawi   year69    4484439
    ## 7029                          Malawi   year70    4603723
    ## 7030                          Malawi   year71    4728703
    ## 7031                          Malawi   year72    4859610
    ## 7032                          Malawi   year73    4996940
    ## 7033                          Malawi   year74    5141202
    ## 7034                          Malawi   year75    5292808
    ## 7035                          Malawi   year76    5454705
    ## 7036                          Malawi   year77    5627533
    ## 7037                          Malawi   year78    5806845
    ## 7038                          Malawi   year79    5986332
    ## 7039                          Malawi   year80    6163080
    ## 7040                          Malawi   year81    6327569
    ## 7041                          Malawi   year82    6484452
    ## 7042                          Malawi   year83    6661358
    ## 7043                          Malawi   year84    6895928
    ## 7044                          Malawi   year85    7211105
    ## 7045                          Malawi   year86    7625305
    ## 7046                          Malawi   year87    8120093
    ## 7047                          Malawi   year88    8636935
    ## 7048                          Malawi   year89    9094671
    ## 7049                          Malawi   year90    9437553
    ## 7050                          Malawi   year91    9641153
    ## 7051                          Malawi   year92    9729717
    ## 7052                          Malawi   year93    9755857
    ## 7053                          Malawi   year94    9796976
    ## 7054                          Malawi   year95    9909088
    ## 7055                          Malawi   year96   10109789
    ## 7056                          Malawi   year97   10381862
    ## 7057                          Malawi   year98   10704744
    ## 7058                          Malawi   year99   11044356
    ## 7059                          Malawi year2000   11376172
    ## 7060                          Malawi year2001   11695863
    ## 7061                          Malawi year2002   12013711
    ## 7062                          Malawi year2003   12336687
    ## 7063                          Malawi year2004   12676038
    ## 7064                          Malawi year2005   13039711
    ## 7065                          Malawi year2006   13429262
    ## 7066                          Malawi year2007   13840969
    ## 7067                          Malawi year2008   14271234
    ## 7068                          Malawi year2009   14714602
    ## 7069                          Malawi year2010   15167095
    ## 7070                          Malawi year2011   15627618
    ## 7071                          Malawi year2012   16097305
    ## 7072                          Malawi year2013   16577147
    ## 7073                          Malawi year2014   17068838
    ## 7074                          Malawi year2015   17573607
    ## 7075                          Malawi year2016   18091575
    ## 7076                          Malawi year2017   18622104
    ## 7077                        Malaysia   year60    8157106
    ## 7078                        Malaysia   year61    8418460
    ## 7079                        Malaysia   year62    8692815
    ## 7080                        Malaysia   year63    8974084
    ## 7081                        Malaysia   year64    9253963
    ## 7082                        Malaysia   year65    9526563
    ## 7083                        Malaysia   year66    9789982
    ## 7084                        Malaysia   year67   10046172
    ## 7085                        Malaysia   year68   10297801
    ## 7086                        Malaysia   year69   10549226
    ## 7087                        Malaysia   year70   10803978
    ## 7088                        Malaysia   year71   11062338
    ## 7089                        Malaysia   year72   11324251
    ## 7090                        Malaysia   year73   11592698
    ## 7091                        Malaysia   year74   11871233
    ## 7092                        Malaysia   year75   12162369
    ## 7093                        Malaysia   year76   12468893
    ## 7094                        Malaysia   year77   12790546
    ## 7095                        Malaysia   year78   13123069
    ## 7096                        Malaysia   year79   13460201
    ## 7097                        Malaysia   year80   13798125
    ## 7098                        Malaysia   year81   14133840
    ## 7099                        Malaysia   year82   14470633
    ## 7100                        Malaysia   year83   14818617
    ## 7101                        Malaysia   year84   15191625
    ## 7102                        Malaysia   year85   15598942
    ## 7103                        Malaysia   year86   16045047
    ## 7104                        Malaysia   year87   16525108
    ## 7105                        Malaysia   year88   17027588
    ## 7106                        Malaysia   year89   17535971
    ## 7107                        Malaysia   year90   18038321
    ## 7108                        Malaysia   year91   18529454
    ## 7109                        Malaysia   year92   19012724
    ## 7110                        Malaysia   year93   19494967
    ## 7111                        Malaysia   year94   19986894
    ## 7112                        Malaysia   year95   20495597
    ## 7113                        Malaysia   year96   21023321
    ## 7114                        Malaysia   year97   21565325
    ## 7115                        Malaysia   year98   22113464
    ## 7116                        Malaysia   year99   22656286
    ## 7117                        Malaysia year2000   23185608
    ## 7118                        Malaysia year2001   23698907
    ## 7119                        Malaysia year2002   24198811
    ## 7120                        Malaysia year2003   24688703
    ## 7121                        Malaysia year2004   25174109
    ## 7122                        Malaysia year2005   25659393
    ## 7123                        Malaysia year2006   26143566
    ## 7124                        Malaysia year2007   26625845
    ## 7125                        Malaysia year2008   27111069
    ## 7126                        Malaysia year2009   27605383
    ## 7127                        Malaysia year2010   28112289
    ## 7128                        Malaysia year2011   28635128
    ## 7129                        Malaysia year2012   29170456
    ## 7130                        Malaysia year2013   29706724
    ## 7131                        Malaysia year2014   30228017
    ## 7132                        Malaysia year2015   30723155
    ## 7133                        Malaysia year2016   31187265
    ## 7134                        Malaysia year2017   31624264
    ## 7135                        Maldives   year60      89887
    ## 7136                        Maldives   year61      92350
    ## 7137                        Maldives   year62      94938
    ## 7138                        Maldives   year63      97584
    ## 7139                        Maldives   year64     100214
    ## 7140                        Maldives   year65     102766
    ## 7141                        Maldives   year66     105190
    ## 7142                        Maldives   year67     107538
    ## 7143                        Maldives   year68     109959
    ## 7144                        Maldives   year69     112651
    ## 7145                        Maldives   year70     115768
    ## 7146                        Maldives   year71     119378
    ## 7147                        Maldives   year72     123441
    ## 7148                        Maldives   year73     127791
    ## 7149                        Maldives   year74     132195
    ## 7150                        Maldives   year75     136519
    ## 7151                        Maldives   year76     140665
    ## 7152                        Maldives   year77     144736
    ## 7153                        Maldives   year78     148892
    ## 7154                        Maldives   year79     153386
    ## 7155                        Maldives   year80     158385
    ## 7156                        Maldives   year81     163935
    ## 7157                        Maldives   year82     169960
    ## 7158                        Maldives   year83     176356
    ## 7159                        Maldives   year84     182953
    ## 7160                        Maldives   year85     189637
    ## 7161                        Maldives   year86     196357
    ## 7162                        Maldives   year87     203124
    ## 7163                        Maldives   year88     209885
    ## 7164                        Maldives   year89     216595
    ## 7165                        Maldives   year90     223215
    ## 7166                        Maldives   year91     229754
    ## 7167                        Maldives   year92     236190
    ## 7168                        Maldives   year93     242459
    ## 7169                        Maldives   year94     248433
    ## 7170                        Maldives   year95     254082
    ## 7171                        Maldives   year96     259327
    ## 7172                        Maldives   year97     264275
    ## 7173                        Maldives   year98     269206
    ## 7174                        Maldives   year99     274484
    ## 7175                        Maldives year2000     280384
    ## 7176                        Maldives year2001     287027
    ## 7177                        Maldives year2002     294341
    ## 7178                        Maldives year2003     302209
    ## 7179                        Maldives year2004     310423
    ## 7180                        Maldives year2005     318836
    ## 7181                        Maldives year2006     327371
    ## 7182                        Maldives year2007     336070
    ## 7183                        Maldives year2008     345054
    ## 7184                        Maldives year2009     354501
    ## 7185                        Maldives year2010     364511
    ## 7186                        Maldives year2011     375131
    ## 7187                        Maldives year2012     386203
    ## 7188                        Maldives year2013     397397
    ## 7189                        Maldives year2014     408247
    ## 7190                        Maldives year2015     418403
    ## 7191                        Maldives year2016     427756
    ## 7192                        Maldives year2017     436330
    ## 7193                            Mali   year60    5263733
    ## 7194                            Mali   year61    5322266
    ## 7195                            Mali   year62    5381368
    ## 7196                            Mali   year63    5441613
    ## 7197                            Mali   year64    5503752
    ## 7198                            Mali   year65    5568484
    ## 7199                            Mali   year66    5635859
    ## 7200                            Mali   year67    5706199
    ## 7201                            Mali   year68    5780835
    ## 7202                            Mali   year69    5861412
    ## 7203                            Mali   year70    5949045
    ## 7204                            Mali   year71    6044530
    ## 7205                            Mali   year72    6147458
    ## 7206                            Mali   year73    6256187
    ## 7207                            Mali   year74    6368348
    ## 7208                            Mali   year75    6482278
    ## 7209                            Mali   year76    6596773
    ## 7210                            Mali   year77    6712401
    ## 7211                            Mali   year78    6831295
    ## 7212                            Mali   year79    6956579
    ## 7213                            Mali   year80    7090126
    ## 7214                            Mali   year81    7234303
    ## 7215                            Mali   year82    7387656
    ## 7216                            Mali   year83    7543743
    ## 7217                            Mali   year84    7693667
    ## 7218                            Mali   year85    7831889
    ## 7219                            Mali   year86    7955164
    ## 7220                            Mali   year87    8067758
    ## 7221                            Mali   year88    8180728
    ## 7222                            Mali   year89    8309531
    ## 7223                            Mali   year90    8465188
    ## 7224                            Mali   year91    8652514
    ## 7225                            Mali   year92    8868263
    ## 7226                            Mali   year93    9105472
    ## 7227                            Mali   year94    9353385
    ## 7228                            Mali   year95    9604450
    ## 7229                            Mali   year96    9856810
    ## 7230                            Mali   year97   10114094
    ## 7231                            Mali   year98   10380835
    ## 7232                            Mali   year99   10663723
    ## 7233                            Mali year2000   10967690
    ## 7234                            Mali year2001   11293258
    ## 7235                            Mali year2002   11638929
    ## 7236                            Mali year2003   12005128
    ## 7237                            Mali year2004   12391906
    ## 7238                            Mali year2005   12798763
    ## 7239                            Mali year2006   13227064
    ## 7240                            Mali year2007   13675606
    ## 7241                            Mali year2008   14138216
    ## 7242                            Mali year2009   14606597
    ## 7243                            Mali year2010   15075085
    ## 7244                            Mali year2011   15540989
    ## 7245                            Mali year2012   16006670
    ## 7246                            Mali year2013   16477818
    ## 7247                            Mali year2014   16962846
    ## 7248                            Mali year2015   17467905
    ## 7249                            Mali year2016   17994837
    ## 7250                            Mali year2017   18541980
    ## 7251                           Malta   year60     326550
    ## 7252                           Malta   year61     325250
    ## 7253                           Malta   year62     323900
    ## 7254                           Malta   year63     322550
    ## 7255                           Malta   year64     321250
    ## 7256                           Malta   year65     318800
    ## 7257                           Malta   year66     315200
    ## 7258                           Malta   year67     311550
    ## 7259                           Malta   year68     307900
    ## 7260                           Malta   year69     304300
    ## 7261                           Malta   year70     302650
    ## 7262                           Malta   year71     302700
    ## 7263                           Malta   year72     302450
    ## 7264                           Malta   year73     302200
    ## 7265                           Malta   year74     301996
    ## 7266                           Malta   year75     304222
    ## 7267                           Malta   year76     305774
    ## 7268                           Malta   year77     306970
    ## 7269                           Malta   year78     310182
    ## 7270                           Malta   year79     313342
    ## 7271                           Malta   year80     316645
    ## 7272                           Malta   year81     318982
    ## 7273                           Malta   year82     325898
    ## 7274                           Malta   year83     330524
    ## 7275                           Malta   year84     330593
    ## 7276                           Malta   year85     336452
    ## 7277                           Malta   year86     342121
    ## 7278                           Malta   year87     344485
    ## 7279                           Malta   year88     347325
    ## 7280                           Malta   year89     350722
    ## 7281                           Malta   year90     354170
    ## 7282                           Malta   year91     363845
    ## 7283                           Malta   year92     367618
    ## 7284                           Malta   year93     371308
    ## 7285                           Malta   year94     374797
    ## 7286                           Malta   year95     377419
    ## 7287                           Malta   year96     379905
    ## 7288                           Malta   year97     382791
    ## 7289                           Malta   year98     385287
    ## 7290                           Malta   year99     387578
    ## 7291                           Malta year2000     390087
    ## 7292                           Malta year2001     393028
    ## 7293                           Malta year2002     395969
    ## 7294                           Malta year2003     398582
    ## 7295                           Malta year2004     401268
    ## 7296                           Malta year2005     403834
    ## 7297                           Malta year2006     405308
    ## 7298                           Malta year2007     406724
    ## 7299                           Malta year2008     409379
    ## 7300                           Malta year2009     412477
    ## 7301                           Malta year2010     414508
    ## 7302                           Malta year2011     416268
    ## 7303                           Malta year2012     420028
    ## 7304                           Malta year2013     425967
    ## 7305                           Malta year2014     434558
    ## 7306                           Malta year2015     445053
    ## 7307                           Malta year2016     455356
    ## 7308                           Malta year2017     465292
    ## 7309                Marshall Islands   year60      14662
    ## 7310                Marshall Islands   year61      15051
    ## 7311                Marshall Islands   year62      15547
    ## 7312                Marshall Islands   year63      16114
    ## 7313                Marshall Islands   year64      16710
    ## 7314                Marshall Islands   year65      17284
    ## 7315                Marshall Islands   year66      17842
    ## 7316                Marshall Islands   year67      18388
    ## 7317                Marshall Islands   year68      18961
    ## 7318                Marshall Islands   year69      19622
    ## 7319                Marshall Islands   year70      20395
    ## 7320                Marshall Islands   year71      21313
    ## 7321                Marshall Islands   year72      22341
    ## 7322                Marshall Islands   year73      23439
    ## 7323                Marshall Islands   year74      24531
    ## 7324                Marshall Islands   year75      25576
    ## 7325                Marshall Islands   year76      26552
    ## 7326                Marshall Islands   year77      27470
    ## 7327                Marshall Islands   year78      28405
    ## 7328                Marshall Islands   year79      29418
    ## 7329                Marshall Islands   year80      30576
    ## 7330                Marshall Islands   year81      31893
    ## 7331                Marshall Islands   year82      33330
    ## 7332                Marshall Islands   year83      34892
    ## 7333                Marshall Islands   year84      36561
    ## 7334                Marshall Islands   year85      38333
    ## 7335                Marshall Islands   year86      40204
    ## 7336                Marshall Islands   year87      42153
    ## 7337                Marshall Islands   year88      44063
    ## 7338                Marshall Islands   year89      45814
    ## 7339                Marshall Islands   year90      47298
    ## 7340                Marshall Islands   year91      48475
    ## 7341                Marshall Islands   year92      49378
    ## 7342                Marshall Islands   year93      50048
    ## 7343                Marshall Islands   year94      50575
    ## 7344                Marshall Islands   year95      51015
    ## 7345                Marshall Islands   year96      51401
    ## 7346                Marshall Islands   year97      51692
    ## 7347                Marshall Islands   year98      51925
    ## 7348                Marshall Islands   year99      52079
    ## 7349                Marshall Islands year2000      52159
    ## 7350                Marshall Islands year2001      52183
    ## 7351                Marshall Islands year2002      52158
    ## 7352                Marshall Islands year2003      52116
    ## 7353                Marshall Islands year2004      52074
    ## 7354                Marshall Islands year2005      52055
    ## 7355                Marshall Islands year2006      52078
    ## 7356                Marshall Islands year2007      52137
    ## 7357                Marshall Islands year2008      52218
    ## 7358                Marshall Islands year2009      52320
    ## 7359                Marshall Islands year2010      52425
    ## 7360                Marshall Islands year2011      52542
    ## 7361                Marshall Islands year2012      52663
    ## 7362                Marshall Islands year2013      52793
    ## 7363                Marshall Islands year2014      52898
    ## 7364                Marshall Islands year2015      52994
    ## 7365                Marshall Islands year2016      53066
    ## 7366                Marshall Islands year2017      53127
    ## 7367                      Mauritania   year60     858168
    ## 7368                      Mauritania   year61     883221
    ## 7369                      Mauritania   year62     909174
    ## 7370                      Mauritania   year63     936016
    ## 7371                      Mauritania   year64     963747
    ## 7372                      Mauritania   year65     992367
    ## 7373                      Mauritania   year66    1021882
    ## 7374                      Mauritania   year67    1052286
    ## 7375                      Mauritania   year68    1083583
    ## 7376                      Mauritania   year69    1115788
    ## 7377                      Mauritania   year70    1148908
    ## 7378                      Mauritania   year71    1182954
    ## 7379                      Mauritania   year72    1217941
    ## 7380                      Mauritania   year73    1253874
    ## 7381                      Mauritania   year74    1290790
    ## 7382                      Mauritania   year75    1328686
    ## 7383                      Mauritania   year76    1367563
    ## 7384                      Mauritania   year77    1407436
    ## 7385                      Mauritania   year78    1448414
    ## 7386                      Mauritania   year79    1490603
    ## 7387                      Mauritania   year80    1534085
    ## 7388                      Mauritania   year81    1578938
    ## 7389                      Mauritania   year82    1625124
    ## 7390                      Mauritania   year83    1672496
    ## 7391                      Mauritania   year84    1720812
    ## 7392                      Mauritania   year85    1769942
    ## 7393                      Mauritania   year86    1819954
    ## 7394                      Mauritania   year87    1870978
    ## 7395                      Mauritania   year88    1923002
    ## 7396                      Mauritania   year89    1976030
    ## 7397                      Mauritania   year90    2030140
    ## 7398                      Mauritania   year91    2085202
    ## 7399                      Mauritania   year92    2141445
    ## 7400                      Mauritania   year93    2199791
    ## 7401                      Mauritania   year94    2261403
    ## 7402                      Mauritania   year95    2327075
    ## 7403                      Mauritania   year96    2397245
    ## 7404                      Mauritania   year97    2471598
    ## 7405                      Mauritania   year98    2549223
    ## 7406                      Mauritania   year99    2628803
    ## 7407                      Mauritania year2000    2709359
    ## 7408                      Mauritania year2001    2790729
    ## 7409                      Mauritania year2002    2873228
    ## 7410                      Mauritania year2003    2957117
    ## 7411                      Mauritania year2004    3042823
    ## 7412                      Mauritania year2005    3130720
    ## 7413                      Mauritania year2006    3220653
    ## 7414                      Mauritania year2007    3312665
    ## 7415                      Mauritania year2008    3407541
    ## 7416                      Mauritania year2009    3506288
    ## 7417                      Mauritania year2010    3609543
    ## 7418                      Mauritania year2011    3717672
    ## 7419                      Mauritania year2012    3830239
    ## 7420                      Mauritania year2013    3946170
    ## 7421                      Mauritania year2014    4063920
    ## 7422                      Mauritania year2015    4182341
    ## 7423                      Mauritania year2016    4301018
    ## 7424                      Mauritania year2017    4420184
    ## 7425                       Mauritius   year60     659351
    ## 7426                       Mauritius   year61     680757
    ## 7427                       Mauritius   year62     700349
    ## 7428                       Mauritius   year63     718861
    ## 7429                       Mauritius   year64     736381
    ## 7430                       Mauritius   year65     753000
    ## 7431                       Mauritius   year66     768813
    ## 7432                       Mauritius   year67     783917
    ## 7433                       Mauritius   year68     798413
    ## 7434                       Mauritius   year69     812405
    ## 7435                       Mauritius   year70     826000
    ## 7436                       Mauritius   year71     839230
    ## 7437                       Mauritius   year72     852053
    ## 7438                       Mauritius   year73     864819
    ## 7439                       Mauritius   year74     878042
    ## 7440                       Mauritius   year75     892000
    ## 7441                       Mauritius   year76     906507
    ## 7442                       Mauritius   year77     921379
    ## 7443                       Mauritius   year78     933499
    ## 7444                       Mauritius   year79     949888
    ## 7445                       Mauritius   year80     966039
    ## 7446                       Mauritius   year81     980462
    ## 7447                       Mauritius   year82     992521
    ## 7448                       Mauritius   year83    1001691
    ## 7449                       Mauritius   year84    1012221
    ## 7450                       Mauritius   year85    1020528
    ## 7451                       Mauritius   year86    1028360
    ## 7452                       Mauritius   year87    1036082
    ## 7453                       Mauritius   year88    1043239
    ## 7454                       Mauritius   year89    1051260
    ## 7455                       Mauritius   year90    1058775
    ## 7456                       Mauritius   year91    1070266
    ## 7457                       Mauritius   year92    1084441
    ## 7458                       Mauritius   year93    1097374
    ## 7459                       Mauritius   year94    1112846
    ## 7460                       Mauritius   year95    1122457
    ## 7461                       Mauritius   year96    1133996
    ## 7462                       Mauritius   year97    1148284
    ## 7463                       Mauritius   year98    1160421
    ## 7464                       Mauritius   year99    1175267
    ## 7465                       Mauritius year2000    1186873
    ## 7466                       Mauritius year2001    1196287
    ## 7467                       Mauritius year2002    1204621
    ## 7468                       Mauritius year2003    1213370
    ## 7469                       Mauritius year2004    1221003
    ## 7470                       Mauritius year2005    1228254
    ## 7471                       Mauritius year2006    1233996
    ## 7472                       Mauritius year2007    1239630
    ## 7473                       Mauritius year2008    1244121
    ## 7474                       Mauritius year2009    1247429
    ## 7475                       Mauritius year2010    1250400
    ## 7476                       Mauritius year2011    1252404
    ## 7477                       Mauritius year2012    1255882
    ## 7478                       Mauritius year2013    1258653
    ## 7479                       Mauritius year2014    1260934
    ## 7480                       Mauritius year2015    1262605
    ## 7481                       Mauritius year2016    1263473
    ## 7482                       Mauritius year2017    1264613
    ## 7483                          Mexico   year60   38174112
    ## 7484                          Mexico   year61   39394126
    ## 7485                          Mexico   year62   40649588
    ## 7486                          Mexico   year63   41939880
    ## 7487                          Mexico   year64   43264272
    ## 7488                          Mexico   year65   44623043
    ## 7489                          Mexico   year66   46011038
    ## 7490                          Mexico   year67   47429812
    ## 7491                          Mexico   year68   48894019
    ## 7492                          Mexico   year69   50423481
    ## 7493                          Mexico   year70   52029861
    ## 7494                          Mexico   year71   53718724
    ## 7495                          Mexico   year72   55478151
    ## 7496                          Mexico   year73   57280587
    ## 7497                          Mexico   year74   59088193
    ## 7498                          Mexico   year75   60872399
    ## 7499                          Mexico   year76   62623763
    ## 7500                          Mexico   year77   64345884
    ## 7501                          Mexico   year78   66039488
    ## 7502                          Mexico   year79   67709689
    ## 7503                          Mexico   year80   69360871
    ## 7504                          Mexico   year81   70992195
    ## 7505                          Mexico   year82   72602533
    ## 7506                          Mexico   year83   74196548
    ## 7507                          Mexico   year84   75780605
    ## 7508                          Mexico   year85   77360707
    ## 7509                          Mexico   year86   78934125
    ## 7510                          Mexico   year87   80503052
    ## 7511                          Mexico   year88   82083919
    ## 7512                          Mexico   year89   83697891
    ## 7513                          Mexico   year90   85357874
    ## 7514                          Mexico   year91   87071512
    ## 7515                          Mexico   year92   88828310
    ## 7516                          Mexico   year93   90600453
    ## 7517                          Mexico   year94   92349147
    ## 7518                          Mexico   year95   94045579
    ## 7519                          Mexico   year96   95687452
    ## 7520                          Mexico   year97   97281739
    ## 7521                          Mexico   year98   98821456
    ## 7522                          Mexico   year99  100300579
    ## 7523                          Mexico year2000  101719673
    ## 7524                          Mexico year2001  103067068
    ## 7525                          Mexico year2002  104355608
    ## 7526                          Mexico year2003  105640453
    ## 7527                          Mexico year2004  106995583
    ## 7528                          Mexico year2005  108472228
    ## 7529                          Mexico year2006  110092378
    ## 7530                          Mexico year2007  111836346
    ## 7531                          Mexico year2008  113661809
    ## 7532                          Mexico year2009  115505228
    ## 7533                          Mexico year2010  117318941
    ## 7534                          Mexico year2011  119090017
    ## 7535                          Mexico year2012  120828307
    ## 7536                          Mexico year2013  122535969
    ## 7537                          Mexico year2014  124221600
    ## 7538                          Mexico year2015  125890949
    ## 7539                          Mexico year2016  127540423
    ## 7540                          Mexico year2017  129163276
    ## 7541           Micronesia, Fed. Sts.   year60      44537
    ## 7542           Micronesia, Fed. Sts.   year61      45955
    ## 7543           Micronesia, Fed. Sts.   year62      47388
    ## 7544           Micronesia, Fed. Sts.   year63      48876
    ## 7545           Micronesia, Fed. Sts.   year64      50487
    ## 7546           Micronesia, Fed. Sts.   year65      52242
    ## 7547           Micronesia, Fed. Sts.   year66      54199
    ## 7548           Micronesia, Fed. Sts.   year67      56319
    ## 7549           Micronesia, Fed. Sts.   year68      58403
    ## 7550           Micronesia, Fed. Sts.   year69      60170
    ## 7551           Micronesia, Fed. Sts.   year70      61431
    ## 7552           Micronesia, Fed. Sts.   year71      62108
    ## 7553           Micronesia, Fed. Sts.   year72      62298
    ## 7554           Micronesia, Fed. Sts.   year73      62290
    ## 7555           Micronesia, Fed. Sts.   year74      62476
    ## 7556           Micronesia, Fed. Sts.   year75      63144
    ## 7557           Micronesia, Fed. Sts.   year76      64386
    ## 7558           Micronesia, Fed. Sts.   year77      66105
    ## 7559           Micronesia, Fed. Sts.   year78      68222
    ## 7560           Micronesia, Fed. Sts.   year79      70550
    ## 7561           Micronesia, Fed. Sts.   year80      72964
    ## 7562           Micronesia, Fed. Sts.   year81      75462
    ## 7563           Micronesia, Fed. Sts.   year82      78059
    ## 7564           Micronesia, Fed. Sts.   year83      80678
    ## 7565           Micronesia, Fed. Sts.   year84      83240
    ## 7566           Micronesia, Fed. Sts.   year85      85686
    ## 7567           Micronesia, Fed. Sts.   year86      87948
    ## 7568           Micronesia, Fed. Sts.   year87      90020
    ## 7569           Micronesia, Fed. Sts.   year88      92021
    ## 7570           Micronesia, Fed. Sts.   year89      94091
    ## 7571           Micronesia, Fed. Sts.   year90      96331
    ## 7572           Micronesia, Fed. Sts.   year91      98799
    ## 7573           Micronesia, Fed. Sts.   year92     101413
    ## 7574           Micronesia, Fed. Sts.   year93     103934
    ## 7575           Micronesia, Fed. Sts.   year94     106057
    ## 7576           Micronesia, Fed. Sts.   year95     107556
    ## 7577           Micronesia, Fed. Sts.   year96     108344
    ## 7578           Micronesia, Fed. Sts.   year97     108502
    ## 7579           Micronesia, Fed. Sts.   year98     108238
    ## 7580           Micronesia, Fed. Sts.   year99     107816
    ## 7581           Micronesia, Fed. Sts. year2000     107432
    ## 7582           Micronesia, Fed. Sts. year2001     107165
    ## 7583           Micronesia, Fed. Sts. year2002     106983
    ## 7584           Micronesia, Fed. Sts. year2003     106816
    ## 7585           Micronesia, Fed. Sts. year2004     106577
    ## 7586           Micronesia, Fed. Sts. year2005     106196
    ## 7587           Micronesia, Fed. Sts. year2006     105684
    ## 7588           Micronesia, Fed. Sts. year2007     105078
    ## 7589           Micronesia, Fed. Sts. year2008     104478
    ## 7590           Micronesia, Fed. Sts. year2009     103960
    ## 7591           Micronesia, Fed. Sts. year2010     103616
    ## 7592           Micronesia, Fed. Sts. year2011     103468
    ## 7593           Micronesia, Fed. Sts. year2012     103503
    ## 7594           Micronesia, Fed. Sts. year2013     103702
    ## 7595           Micronesia, Fed. Sts. year2014     104015
    ## 7596           Micronesia, Fed. Sts. year2015     104433
    ## 7597           Micronesia, Fed. Sts. year2016     104937
    ## 7598           Micronesia, Fed. Sts. year2017     105544
    ## 7599                         Moldova   year60    2544000
    ## 7600                         Moldova   year61    2605000
    ## 7601                         Moldova   year62    2664000
    ## 7602                         Moldova   year63    2721000
    ## 7603                         Moldova   year64    2774000
    ## 7604                         Moldova   year65    2825000
    ## 7605                         Moldova   year66    2873000
    ## 7606                         Moldova   year67    2918000
    ## 7607                         Moldova   year68    2960000
    ## 7608                         Moldova   year69    3002000
    ## 7609                         Moldova   year70    3044000
    ## 7610                         Moldova   year71    3088000
    ## 7611                         Moldova   year72    3131000
    ## 7612                         Moldova   year73    3174000
    ## 7613                         Moldova   year74    3215000
    ## 7614                         Moldova   year75    3251000
    ## 7615                         Moldova   year76    3284000
    ## 7616                         Moldova   year77    3312000
    ## 7617                         Moldova   year78    3339000
    ## 7618                         Moldova   year79    3366000
    ## 7619                         Moldova   year80    3396000
    ## 7620                         Moldova   year81    3429000
    ## 7621                         Moldova   year82    3464000
    ## 7622                         Moldova   year83    3500000
    ## 7623                         Moldova   year84    3536000
    ## 7624                         Moldova   year85    3570000
    ## 7625                         Moldova   year86    3602000
    ## 7626                         Moldova   year87    3633000
    ## 7627                         Moldova   year88    3660000
    ## 7628                         Moldova   year89    3681000
    ## 7629                         Moldova   year90    3696000
    ## 7630                         Moldova   year91    3704000
    ## 7631                         Moldova   year92    3706000
    ## 7632                         Moldova   year93    3701000
    ## 7633                         Moldova   year94    3691000
    ## 7634                         Moldova   year95    3675099
    ## 7635                         Moldova   year96    3667748
    ## 7636                         Moldova   year97    3654208
    ## 7637                         Moldova   year98    3652732
    ## 7638                         Moldova   year99    3647001
    ## 7639                         Moldova year2000    3639592
    ## 7640                         Moldova year2001    3631462
    ## 7641                         Moldova year2002    3623062
    ## 7642                         Moldova year2003    3612874
    ## 7643                         Moldova year2004    3603945
    ## 7644                         Moldova year2005    3595187
    ## 7645                         Moldova year2006    3585209
    ## 7646                         Moldova year2007    3576910
    ## 7647                         Moldova year2008    3570108
    ## 7648                         Moldova year2009    3565604
    ## 7649                         Moldova year2010    3562045
    ## 7650                         Moldova year2011    3559986
    ## 7651                         Moldova year2012    3559519
    ## 7652                         Moldova year2013    3558566
    ## 7653                         Moldova year2014    3556397
    ## 7654                         Moldova year2015    3554108
    ## 7655                         Moldova year2016    3551954
    ## 7656                         Moldova year2017    3549750
    ## 7657                          Monaco   year60      22452
    ## 7658                          Monaco   year61      22808
    ## 7659                          Monaco   year62      23039
    ## 7660                          Monaco   year63      23168
    ## 7661                          Monaco   year64      23236
    ## 7662                          Monaco   year65      23282
    ## 7663                          Monaco   year66      23305
    ## 7664                          Monaco   year67      23292
    ## 7665                          Monaco   year68      23304
    ## 7666                          Monaco   year69      23346
    ## 7667                          Monaco   year70      23484
    ## 7668                          Monaco   year71      23720
    ## 7669                          Monaco   year72      24051
    ## 7670                          Monaco   year73      24439
    ## 7671                          Monaco   year74      24835
    ## 7672                          Monaco   year75      25197
    ## 7673                          Monaco   year76      25523
    ## 7674                          Monaco   year77      25809
    ## 7675                          Monaco   year78      26087
    ## 7676                          Monaco   year79      26395
    ## 7677                          Monaco   year80      26745
    ## 7678                          Monaco   year81      27164
    ## 7679                          Monaco   year82      27624
    ## 7680                          Monaco   year83      28095
    ## 7681                          Monaco   year84      28512
    ## 7682                          Monaco   year85      28835
    ## 7683                          Monaco   year86      29041
    ## 7684                          Monaco   year87      29172
    ## 7685                          Monaco   year88      29235
    ## 7686                          Monaco   year89      29312
    ## 7687                          Monaco   year90      29439
    ## 7688                          Monaco   year91      29624
    ## 7689                          Monaco   year92      29863
    ## 7690                          Monaco   year93      30138
    ## 7691                          Monaco   year94      30427
    ## 7692                          Monaco   year95      30691
    ## 7693                          Monaco   year96      30967
    ## 7694                          Monaco   year97      31251
    ## 7695                          Monaco   year98      31523
    ## 7696                          Monaco   year99      31800
    ## 7697                          Monaco year2000      32082
    ## 7698                          Monaco year2001      32360
    ## 7699                          Monaco year2002      32629
    ## 7700                          Monaco year2003      32933
    ## 7701                          Monaco year2004      33314
    ## 7702                          Monaco year2005      33793
    ## 7703                          Monaco year2006      34408
    ## 7704                          Monaco year2007      35111
    ## 7705                          Monaco year2008      35853
    ## 7706                          Monaco year2009      36534
    ## 7707                          Monaco year2010      37094
    ## 7708                          Monaco year2011      37497
    ## 7709                          Monaco year2012      37783
    ## 7710                          Monaco year2013      37971
    ## 7711                          Monaco year2014      38132
    ## 7712                          Monaco year2015      38307
    ## 7713                          Monaco year2016      38499
    ## 7714                          Monaco year2017      38695
    ## 7715                        Mongolia   year60     955505
    ## 7716                        Mongolia   year61     982178
    ## 7717                        Mongolia   year62    1011324
    ## 7718                        Mongolia   year63    1042383
    ## 7719                        Mongolia   year64    1074514
    ## 7720                        Mongolia   year65    1107124
    ## 7721                        Mongolia   year66    1139961
    ## 7722                        Mongolia   year67    1173191
    ## 7723                        Mongolia   year68    1207104
    ## 7724                        Mongolia   year69    1242214
    ## 7725                        Mongolia   year70    1278825
    ## 7726                        Mongolia   year71    1317050
    ## 7727                        Mongolia   year72    1356670
    ## 7728                        Mongolia   year73    1397304
    ## 7729                        Mongolia   year74    1438425
    ## 7730                        Mongolia   year75    1479651
    ## 7731                        Mongolia   year76    1520865
    ## 7732                        Mongolia   year77    1562209
    ## 7733                        Mongolia   year78    1603906
    ## 7734                        Mongolia   year79    1646291
    ## 7735                        Mongolia   year80    1689622
    ## 7736                        Mongolia   year81    1733475
    ## 7737                        Mongolia   year82    1777727
    ## 7738                        Mongolia   year83    1823216
    ## 7739                        Mongolia   year84    1871090
    ## 7740                        Mongolia   year85    1921881
    ## 7741                        Mongolia   year86    1976309
    ## 7742                        Mongolia   year87    2033343
    ## 7743                        Mongolia   year88    2089714
    ## 7744                        Mongolia   year89    2141008
    ## 7745                        Mongolia   year90    2184145
    ## 7746                        Mongolia   year91    2217918
    ## 7747                        Mongolia   year92    2243502
    ## 7748                        Mongolia   year93    2263200
    ## 7749                        Mongolia   year94    2280496
    ## 7750                        Mongolia   year95    2298039
    ## 7751                        Mongolia   year96    2316567
    ## 7752                        Mongolia   year97    2335695
    ## 7753                        Mongolia   year98    2355590
    ## 7754                        Mongolia   year99    2376162
    ## 7755                        Mongolia year2000    2397436
    ## 7756                        Mongolia year2001    2419776
    ## 7757                        Mongolia year2002    2443659
    ## 7758                        Mongolia year2003    2469286
    ## 7759                        Mongolia year2004    2496832
    ## 7760                        Mongolia year2005    2526446
    ## 7761                        Mongolia year2006    2558012
    ## 7762                        Mongolia year2007    2591670
    ## 7763                        Mongolia year2008    2628131
    ## 7764                        Mongolia year2009    2668289
    ## 7765                        Mongolia year2010    2712650
    ## 7766                        Mongolia year2011    2761516
    ## 7767                        Mongolia year2012    2814226
    ## 7768                        Mongolia year2013    2869107
    ## 7769                        Mongolia year2014    2923896
    ## 7770                        Mongolia year2015    2976877
    ## 7771                        Mongolia year2016    3027398
    ## 7772                        Mongolia year2017    3075647
    ## 7773                      Montenegro   year60     480579
    ## 7774                      Montenegro   year61     491140
    ## 7775                      Montenegro   year62     502558
    ## 7776                      Montenegro   year63     513409
    ## 7777                      Montenegro   year64     521753
    ## 7778                      Montenegro   year65     526327
    ## 7779                      Montenegro   year66     526419
    ## 7780                      Montenegro   year67     522796
    ## 7781                      Montenegro   year68     517481
    ## 7782                      Montenegro   year69     513340
    ## 7783                      Montenegro   year70     512407
    ## 7784                      Montenegro   year71     515449
    ## 7785                      Montenegro   year72     521785
    ## 7786                      Montenegro   year73     530220
    ## 7787                      Montenegro   year74     538902
    ## 7788                      Montenegro   year75     546487
    ## 7789                      Montenegro   year76     552562
    ## 7790                      Montenegro   year77     557576
    ## 7791                      Montenegro   year78     562065
    ## 7792                      Montenegro   year79     566888
    ## 7793                      Montenegro   year80     572608
    ## 7794                      Montenegro   year81     579445
    ## 7795                      Montenegro   year82     587001
    ## 7796                      Montenegro   year83     594506
    ## 7797                      Montenegro   year84     600884
    ## 7798                      Montenegro   year85     605398
    ## 7799                      Montenegro   year86     607711
    ## 7800                      Montenegro   year87     608144
    ## 7801                      Montenegro   year88     607413
    ## 7802                      Montenegro   year89     606571
    ## 7803                      Montenegro   year90     606372
    ## 7804                      Montenegro   year91     607105
    ## 7805                      Montenegro   year92     608516
    ## 7806                      Montenegro   year93     610170
    ## 7807                      Montenegro   year94     611389
    ## 7808                      Montenegro   year95     611712
    ## 7809                      Montenegro   year96     611003
    ## 7810                      Montenegro   year97     609520
    ## 7811                      Montenegro   year98     607662
    ## 7812                      Montenegro   year99     606001
    ## 7813                      Montenegro year2000     604950
    ## 7814                      Montenegro year2001     607389
    ## 7815                      Montenegro year2002     609828
    ## 7816                      Montenegro year2003     612267
    ## 7817                      Montenegro year2004     613353
    ## 7818                      Montenegro year2005     614261
    ## 7819                      Montenegro year2006     615025
    ## 7820                      Montenegro year2007     615875
    ## 7821                      Montenegro year2008     616969
    ## 7822                      Montenegro year2009     618294
    ## 7823                      Montenegro year2010     619428
    ## 7824                      Montenegro year2011     620079
    ## 7825                      Montenegro year2012     620601
    ## 7826                      Montenegro year2013     621207
    ## 7827                      Montenegro year2014     621810
    ## 7828                      Montenegro year2015     622159
    ## 7829                      Montenegro year2016     622303
    ## 7830                      Montenegro year2017     622471
    ## 7831                         Morocco   year60   12328532
    ## 7832                         Morocco   year61   12710547
    ## 7833                         Morocco   year62   13094818
    ## 7834                         Morocco   year63   13478232
    ## 7835                         Morocco   year64   13857142
    ## 7836                         Morocco   year65   14229044
    ## 7837                         Morocco   year66   14593284
    ## 7838                         Morocco   year67   14950803
    ## 7839                         Morocco   year68   15302947
    ## 7840                         Morocco   year69   15651924
    ## 7841                         Morocco   year70   16000008
    ## 7842                         Morocco   year71   16347198
    ## 7843                         Morocco   year72   16695003
    ## 7844                         Morocco   year73   17049165
    ## 7845                         Morocco   year74   17416964
    ## 7846                         Morocco   year75   17803698
    ## 7847                         Morocco   year76   18210754
    ## 7848                         Morocco   year77   18636977
    ## 7849                         Morocco   year78   19081718
    ## 7850                         Morocco   year79   19543347
    ## 7851                         Morocco   year80   20019847
    ## 7852                         Morocco   year81   20511601
    ## 7853                         Morocco   year82   21016818
    ## 7854                         Morocco   year83   21528502
    ## 7855                         Morocco   year84   22037610
    ## 7856                         Morocco   year85   22537376
    ## 7857                         Morocco   year86   23023935
    ## 7858                         Morocco   year87   23497766
    ## 7859                         Morocco   year88   23961820
    ## 7860                         Morocco   year89   24421191
    ## 7861                         Morocco   year90   24879136
    ## 7862                         Morocco   year91   25336862
    ## 7863                         Morocco   year92   25791494
    ## 7864                         Morocco   year93   26237417
    ## 7865                         Morocco   year94   26667048
    ## 7866                         Morocco   year95   27075232
    ## 7867                         Morocco   year96   27460603
    ## 7868                         Morocco   year97   27825901
    ## 7869                         Morocco   year98   28175263
    ## 7870                         Morocco   year99   28514798
    ## 7871                         Morocco year2000   28849621
    ## 7872                         Morocco year2001   29181832
    ## 7873                         Morocco year2002   29512368
    ## 7874                         Morocco year2003   29843937
    ## 7875                         Morocco year2004   30179285
    ## 7876                         Morocco year2005   30521070
    ## 7877                         Morocco year2006   30869346
    ## 7878                         Morocco year2007   31225881
    ## 7879                         Morocco year2008   31596855
    ## 7880                         Morocco year2009   31989897
    ## 7881                         Morocco year2010   32409639
    ## 7882                         Morocco year2011   32858823
    ## 7883                         Morocco year2012   33333789
    ## 7884                         Morocco year2013   33824769
    ## 7885                         Morocco year2014   34318082
    ## 7886                         Morocco year2015   34803322
    ## 7887                         Morocco year2016   35276786
    ## 7888                         Morocco year2017   35739580
    ## 7889                      Mozambique   year60    7388695
    ## 7890                      Mozambique   year61    7541325
    ## 7891                      Mozambique   year62    7699139
    ## 7892                      Mozambique   year63    7862072
    ## 7893                      Mozambique   year64    8030025
    ## 7894                      Mozambique   year65    8203076
    ## 7895                      Mozambique   year66    8381455
    ## 7896                      Mozambique   year67    8565674
    ## 7897                      Mozambique   year68    8756481
    ## 7898                      Mozambique   year69    8954809
    ## 7899                      Mozambique   year70    9161534
    ## 7900                      Mozambique   year71    9375144
    ## 7901                      Mozambique   year72    9595762
    ## 7902                      Mozambique   year73    9827580
    ## 7903                      Mozambique   year74   10076172
    ## 7904                      Mozambique   year75   10344494
    ## 7905                      Mozambique   year76   10632932
    ## 7906                      Mozambique   year77   10936936
    ## 7907                      Mozambique   year78   11248046
    ## 7908                      Mozambique   year79   11554979
    ## 7909                      Mozambique   year80   11848331
    ## 7910                      Mozambique   year81   12133074
    ## 7911                      Mozambique   year82   12409243
    ## 7912                      Mozambique   year83   12657708
    ## 7913                      Mozambique   year84   12853780
    ## 7914                      Mozambique   year85   12984405
    ## 7915                      Mozambique   year86   13034385
    ## 7916                      Mozambique   year87   13020861
    ## 7917                      Mozambique   year88   13002553
    ## 7918                      Mozambique   year89   13059613
    ## 7919                      Mozambique   year90   13247649
    ## 7920                      Mozambique   year91   13591970
    ## 7921                      Mozambique   year92   14071231
    ## 7922                      Mozambique   year93   14636995
    ## 7923                      Mozambique   year94   15217044
    ## 7924                      Mozambique   year95   15759132
    ## 7925                      Mozambique   year96   16248232
    ## 7926                      Mozambique   year97   16701351
    ## 7927                      Mozambique   year98   17136780
    ## 7928                      Mozambique   year99   17584869
    ## 7929                      Mozambique year2000   18067687
    ## 7930                      Mozambique year2001   18588758
    ## 7931                      Mozambique year2002   19139658
    ## 7932                      Mozambique year2003   19716598
    ## 7933                      Mozambique year2004   20312705
    ## 7934                      Mozambique year2005   20923070
    ## 7935                      Mozambique year2006   21547463
    ## 7936                      Mozambique year2007   22188387
    ## 7937                      Mozambique year2008   22846758
    ## 7938                      Mozambique year2009   23524063
    ## 7939                      Mozambique year2010   24221405
    ## 7940                      Mozambique year2011   24939005
    ## 7941                      Mozambique year2012   25676606
    ## 7942                      Mozambique year2013   26434372
    ## 7943                      Mozambique year2014   27212382
    ## 7944                      Mozambique year2015   28010691
    ## 7945                      Mozambique year2016   28829476
    ## 7946                      Mozambique year2017   29668834
    ## 7947                         Myanmar   year60   20986123
    ## 7948                         Myanmar   year61   21438025
    ## 7949                         Myanmar   year62   21898020
    ## 7950                         Myanmar   year63   22371902
    ## 7951                         Myanmar   year64   22867741
    ## 7952                         Myanmar   year65   23391145
    ## 7953                         Myanmar   year66   23944178
    ## 7954                         Myanmar   year67   24524548
    ## 7955                         Myanmar   year68   25128116
    ## 7956                         Myanmar   year69   25748643
    ## 7957                         Myanmar   year70   26381431
    ## 7958                         Myanmar   year71   27024985
    ## 7959                         Myanmar   year72   27680144
    ## 7960                         Myanmar   year73   28347341
    ## 7961                         Myanmar   year74   29027734
    ## 7962                         Myanmar   year75   29721967
    ## 7963                         Myanmar   year76   30428034
    ## 7964                         Myanmar   year77   31144324
    ## 7965                         Myanmar   year78   31872230
    ## 7966                         Myanmar   year79   32613888
    ## 7967                         Myanmar   year80   33369712
    ## 7968                         Myanmar   year81   34139130
    ## 7969                         Myanmar   year82   34917895
    ## 7970                         Myanmar   year83   35697943
    ## 7971                         Myanmar   year84   36468888
    ## 7972                         Myanmar   year85   37222296
    ## 7973                         Myanmar   year86   37957332
    ## 7974                         Myanmar   year87   38673241
    ## 7975                         Myanmar   year88   39362142
    ## 7976                         Myanmar   year89   40014862
    ## 7977                         Myanmar   year90   40626250
    ## 7978                         Myanmar   year91   41190156
    ## 7979                         Myanmar   year92   41711465
    ## 7980                         Myanmar   year93   42209778
    ## 7981                         Myanmar   year94   42712223
    ## 7982                         Myanmar   year95   43237792
    ## 7983                         Myanmar   year96   43793310
    ## 7984                         Myanmar   year97   44371525
    ## 7985                         Myanmar   year98   44959935
    ## 7986                         Myanmar   year99   45539435
    ## 7987                         Myanmar year2000   46095462
    ## 7988                         Myanmar year2001   46627994
    ## 7989                         Myanmar year2002   47140220
    ## 7990                         Myanmar year2003   47624894
    ## 7991                         Myanmar year2004   48073707
    ## 7992                         Myanmar year2005   48482614
    ## 7993                         Myanmar year2006   48846474
    ## 7994                         Myanmar year2007   49171586
    ## 7995                         Myanmar year2008   49479752
    ## 7996                         Myanmar year2009   49800690
    ## 7997                         Myanmar year2010   50155896
    ## 7998                         Myanmar year2011   50553031
    ## 7999                         Myanmar year2012   50986514
    ## 8000                         Myanmar year2013   51448196
    ## 8001                         Myanmar year2014   51924182
    ## 8002                         Myanmar year2015   52403669
    ## 8003                         Myanmar year2016   52885223
    ## 8004                         Myanmar year2017   53370609
    ## 8005                         Namibia   year60     602544
    ## 8006                         Namibia   year61     617277
    ## 8007                         Namibia   year62     632654
    ## 8008                         Namibia   year63     648661
    ## 8009                         Namibia   year64     665282
    ## 8010                         Namibia   year65     682551
    ## 8011                         Namibia   year66     700341
    ## 8012                         Namibia   year67     718685
    ## 8013                         Namibia   year68     737886
    ## 8014                         Namibia   year69     758377
    ## 8015                         Namibia   year70     780384
    ## 8016                         Namibia   year71     804157
    ## 8017                         Namibia   year72     829441
    ## 8018                         Namibia   year73     855380
    ## 8019                         Namibia   year74     880785
    ## 8020                         Namibia   year75     904839
    ## 8021                         Namibia   year76     927503
    ## 8022                         Namibia   year77     949193
    ## 8023                         Namibia   year78     970258
    ## 8024                         Namibia   year79     991226
    ## 8025                         Namibia   year80    1012672
    ## 8026                         Namibia   year81    1034264
    ## 8027                         Namibia   year82    1056366
    ## 8028                         Namibia   year83    1081081
    ## 8029                         Namibia   year84    1111132
    ## 8030                         Namibia   year85    1148302
    ## 8031                         Namibia   year86    1193592
    ## 8032                         Namibia   year87    1245990
    ## 8033                         Namibia   year88    1302741
    ## 8034                         Namibia   year89    1359933
    ## 8035                         Namibia   year90    1414692
    ## 8036                         Namibia   year91    1465740
    ## 8037                         Namibia   year92    1513721
    ## 8038                         Namibia   year93    1559983
    ## 8039                         Namibia   year94    1606718
    ## 8040                         Namibia   year95    1655359
    ## 8041                         Namibia   year96    1706489
    ## 8042                         Namibia   year97    1758994
    ## 8043                         Namibia   year98    1810566
    ## 8044                         Namibia   year99    1858042
    ## 8045                         Namibia year2000    1899257
    ## 8046                         Namibia year2001    1933596
    ## 8047                         Namibia year2002    1962147
    ## 8048                         Namibia year2003    1986535
    ## 8049                         Namibia year2004    2009228
    ## 8050                         Namibia year2005    2032196
    ## 8051                         Namibia year2006    2055734
    ## 8052                         Namibia year2007    2079915
    ## 8053                         Namibia year2008    2106375
    ## 8054                         Namibia year2009    2137040
    ## 8055                         Namibia year2010    2173170
    ## 8056                         Namibia year2011    2215621
    ## 8057                         Namibia year2012    2263934
    ## 8058                         Namibia year2013    2316520
    ## 8059                         Namibia year2014    2370992
    ## 8060                         Namibia year2015    2425561
    ## 8061                         Namibia year2016    2479713
    ## 8062                         Namibia year2017    2533794
    ## 8063                           Nauru   year60       4433
    ## 8064                           Nauru   year61       4676
    ## 8065                           Nauru   year62       4948
    ## 8066                           Nauru   year63       5228
    ## 8067                           Nauru   year64       5500
    ## 8068                           Nauru   year65       5740
    ## 8069                           Nauru   year66       5933
    ## 8070                           Nauru   year67       6103
    ## 8071                           Nauru   year68       6237
    ## 8072                           Nauru   year69       6371
    ## 8073                           Nauru   year70       6496
    ## 8074                           Nauru   year71       6617
    ## 8075                           Nauru   year72       6743
    ## 8076                           Nauru   year73       6863
    ## 8077                           Nauru   year74       6972
    ## 8078                           Nauru   year75       7068
    ## 8079                           Nauru   year76       7150
    ## 8080                           Nauru   year77       7232
    ## 8081                           Nauru   year78       7309
    ## 8082                           Nauru   year79       7397
    ## 8083                           Nauru   year80       7488
    ## 8084                           Nauru   year81       7592
    ## 8085                           Nauru   year82       7717
    ## 8086                           Nauru   year83       7854
    ## 8087                           Nauru   year84       8005
    ## 8088                           Nauru   year85       8173
    ## 8089                           Nauru   year86       8353
    ## 8090                           Nauru   year87       8554
    ## 8091                           Nauru   year88       8755
    ## 8092                           Nauru   year89       8954
    ## 8093                           Nauru   year90       9155
    ## 8094                           Nauru   year91       9348
    ## 8095                           Nauru   year92       9546
    ## 8096                           Nauru   year93       9719
    ## 8097                           Nauru   year94       9857
    ## 8098                           Nauru   year95       9969
    ## 8099                           Nauru   year96      10029
    ## 8100                           Nauru   year97      10057
    ## 8101                           Nauru   year98      10046
    ## 8102                           Nauru   year99      10040
    ## 8103                           Nauru year2000      10037
    ## 8104                           Nauru year2001      10052
    ## 8105                           Nauru year2002      10080
    ## 8106                           Nauru year2003      10106
    ## 8107                           Nauru year2004      10126
    ## 8108                           Nauru year2005      10114
    ## 8109                           Nauru year2006      10071
    ## 8110                           Nauru year2007      10002
    ## 8111                           Nauru year2008       9947
    ## 8112                           Nauru year2009       9945
    ## 8113                           Nauru year2010      10025
    ## 8114                           Nauru year2011      10057
    ## 8115                           Nauru year2012      10279
    ## 8116                           Nauru year2013      10821
    ## 8117                           Nauru year2014      11853
    ## 8118                           Nauru year2015      12475
    ## 8119                           Nauru year2016      13049
    ## 8120                           Nauru year2017      13649
    ## 8121                           Nepal   year60   10063011
    ## 8122                           Nepal   year61   10221759
    ## 8123                           Nepal   year62   10384204
    ## 8124                           Nepal   year63   10552267
    ## 8125                           Nepal   year64   10728197
    ## 8126                           Nepal   year65   10913724
    ## 8127                           Nepal   year66   11109884
    ## 8128                           Nepal   year67   11316826
    ## 8129                           Nepal   year68   11534264
    ## 8130                           Nepal   year69   11761473
    ## 8131                           Nepal   year70   11997929
    ## 8132                           Nepal   year71   12243768
    ## 8133                           Nepal   year72   12499429
    ## 8134                           Nepal   year73   12764957
    ## 8135                           Nepal   year74   13040404
    ## 8136                           Nepal   year75   13325814
    ## 8137                           Nepal   year76   13621110
    ## 8138                           Nepal   year77   13926260
    ## 8139                           Nepal   year78   14241403
    ## 8140                           Nepal   year79   14566691
    ## 8141                           Nepal   year80   14902163
    ## 8142                           Nepal   year81   15249010
    ## 8143                           Nepal   year82   15607236
    ## 8144                           Nepal   year83   15974420
    ## 8145                           Nepal   year84   16347242
    ## 8146                           Nepal   year85   16723956
    ## 8147                           Nepal   year86   17101136
    ## 8148                           Nepal   year87   17480921
    ## 8149                           Nepal   year88   17873667
    ## 8150                           Nepal   year89   18293514
    ## 8151                           Nepal   year90   18749406
    ## 8152                           Nepal   year91   19245054
    ## 8153                           Nepal   year92   19773772
    ## 8154                           Nepal   year93   20321175
    ## 8155                           Nepal   year94   20867130
    ## 8156                           Nepal   year95   21396384
    ## 8157                           Nepal   year96   21903379
    ## 8158                           Nepal   year97   22389803
    ## 8159                           Nepal   year98   22856305
    ## 8160                           Nepal   year99   23305994
    ## 8161                           Nepal year2000   23740911
    ## 8162                           Nepal year2001   24161777
    ## 8163                           Nepal year2002   24566342
    ## 8164                           Nepal year2003   24950623
    ## 8165                           Nepal year2004   25309449
    ## 8166                           Nepal year2005   25640287
    ## 8167                           Nepal year2006   25940618
    ## 8168                           Nepal year2007   26214847
    ## 8169                           Nepal year2008   26475859
    ## 8170                           Nepal year2009   26741103
    ## 8171                           Nepal year2010   27023137
    ## 8172                           Nepal year2011   27327147
    ## 8173                           Nepal year2012   27649925
    ## 8174                           Nepal year2013   27985310
    ## 8175                           Nepal year2014   28323241
    ## 8176                           Nepal year2015   28656282
    ## 8177                           Nepal year2016   28982771
    ## 8178                           Nepal year2017   29304998
    ## 8179                     Netherlands   year60   11486631
    ## 8180                     Netherlands   year61   11638712
    ## 8181                     Netherlands   year62   11805689
    ## 8182                     Netherlands   year63   11965966
    ## 8183                     Netherlands   year64   12127120
    ## 8184                     Netherlands   year65   12294732
    ## 8185                     Netherlands   year66   12456251
    ## 8186                     Netherlands   year67   12598201
    ## 8187                     Netherlands   year68   12729721
    ## 8188                     Netherlands   year69   12877984
    ## 8189                     Netherlands   year70   13038526
    ## 8190                     Netherlands   year71   13194497
    ## 8191                     Netherlands   year72   13328593
    ## 8192                     Netherlands   year73   13439322
    ## 8193                     Netherlands   year74   13545056
    ## 8194                     Netherlands   year75   13666335
    ## 8195                     Netherlands   year76   13774037
    ## 8196                     Netherlands   year77   13856185
    ## 8197                     Netherlands   year78   13941700
    ## 8198                     Netherlands   year79   14038270
    ## 8199                     Netherlands   year80   14149800
    ## 8200                     Netherlands   year81   14247208
    ## 8201                     Netherlands   year82   14312690
    ## 8202                     Netherlands   year83   14367070
    ## 8203                     Netherlands   year84   14424211
    ## 8204                     Netherlands   year85   14491632
    ## 8205                     Netherlands   year86   14572278
    ## 8206                     Netherlands   year87   14665037
    ## 8207                     Netherlands   year88   14760094
    ## 8208                     Netherlands   year89   14848907
    ## 8209                     Netherlands   year90   14951510
    ## 8210                     Netherlands   year91   15069798
    ## 8211                     Netherlands   year92   15184166
    ## 8212                     Netherlands   year93   15290368
    ## 8213                     Netherlands   year94   15382838
    ## 8214                     Netherlands   year95   15459006
    ## 8215                     Netherlands   year96   15530498
    ## 8216                     Netherlands   year97   15610650
    ## 8217                     Netherlands   year98   15707209
    ## 8218                     Netherlands   year99   15812088
    ## 8219                     Netherlands year2000   15925513
    ## 8220                     Netherlands year2001   16046180
    ## 8221                     Netherlands year2002   16148929
    ## 8222                     Netherlands year2003   16225302
    ## 8223                     Netherlands year2004   16281779
    ## 8224                     Netherlands year2005   16319868
    ## 8225                     Netherlands year2006   16346101
    ## 8226                     Netherlands year2007   16381696
    ## 8227                     Netherlands year2008   16445593
    ## 8228                     Netherlands year2009   16530388
    ## 8229                     Netherlands year2010   16615394
    ## 8230                     Netherlands year2011   16693074
    ## 8231                     Netherlands year2012   16754962
    ## 8232                     Netherlands year2013   16804432
    ## 8233                     Netherlands year2014   16865008
    ## 8234                     Netherlands year2015   16939923
    ## 8235                     Netherlands year2016   17030314
    ## 8236                     Netherlands year2017   17132854
    ## 8237                   New Caledonia   year60      79000
    ## 8238                   New Caledonia   year61      81200
    ## 8239                   New Caledonia   year62      83400
    ## 8240                   New Caledonia   year63      85700
    ## 8241                   New Caledonia   year64      88100
    ## 8242                   New Caledonia   year65      90500
    ## 8243                   New Caledonia   year66      93500
    ## 8244                   New Caledonia   year67      96500
    ## 8245                   New Caledonia   year68      99500
    ## 8246                   New Caledonia   year69     104000
    ## 8247                   New Caledonia   year70     112000
    ## 8248                   New Caledonia   year71     120000
    ## 8249                   New Caledonia   year72     125500
    ## 8250                   New Caledonia   year73     128500
    ## 8251                   New Caledonia   year74     131000
    ## 8252                   New Caledonia   year75     132500
    ## 8253                   New Caledonia   year76     134000
    ## 8254                   New Caledonia   year77     136000
    ## 8255                   New Caledonia   year78     137500
    ## 8256                   New Caledonia   year79     138500
    ## 8257                   New Caledonia   year80     140050
    ## 8258                   New Caledonia   year81     142650
    ## 8259                   New Caledonia   year82     145700
    ## 8260                   New Caledonia   year83     148700
    ## 8261                   New Caledonia   year84     151650
    ## 8262                   New Caledonia   year85     154450
    ## 8263                   New Caledonia   year86     157350
    ## 8264                   New Caledonia   year87     160500
    ## 8265                   New Caledonia   year88     163650
    ## 8266                   New Caledonia   year89     166898
    ## 8267                   New Caledonia   year90     170899
    ## 8268                   New Caledonia   year91     175362
    ## 8269                   New Caledonia   year92     179799
    ## 8270                   New Caledonia   year93     184496
    ## 8271                   New Caledonia   year94     189482
    ## 8272                   New Caledonia   year95     193816
    ## 8273                   New Caledonia   year96     197564
    ## 8274                   New Caledonia   year97     201418
    ## 8275                   New Caledonia   year98     205279
    ## 8276                   New Caledonia   year99     209214
    ## 8277                   New Caledonia year2000     213230
    ## 8278                   New Caledonia year2001     217324
    ## 8279                   New Caledonia year2002     221490
    ## 8280                   New Caledonia year2003     225296
    ## 8281                   New Caledonia year2004     228750
    ## 8282                   New Caledonia year2005     232250
    ## 8283                   New Caledonia year2006     235750
    ## 8284                   New Caledonia year2007     239250
    ## 8285                   New Caledonia year2008     242750
    ## 8286                   New Caledonia year2009     245950
    ## 8287                   New Caledonia year2010     249750
    ## 8288                   New Caledonia year2011     254350
    ## 8289                   New Caledonia year2012     259000
    ## 8290                   New Caledonia year2013     263650
    ## 8291                   New Caledonia year2014     268050
    ## 8292                   New Caledonia year2015     272400
    ## 8293                   New Caledonia year2016     276550
    ## 8294                   New Caledonia year2017     280460
    ## 8295                     New Zealand   year60    2371800
    ## 8296                     New Zealand   year61    2419700
    ## 8297                     New Zealand   year62    2482000
    ## 8298                     New Zealand   year63    2531800
    ## 8299                     New Zealand   year64    2585400
    ## 8300                     New Zealand   year65    2628400
    ## 8301                     New Zealand   year66    2675900
    ## 8302                     New Zealand   year67    2724100
    ## 8303                     New Zealand   year68    2748100
    ## 8304                     New Zealand   year69    2772800
    ## 8305                     New Zealand   year70    2810700
    ## 8306                     New Zealand   year71    2853000
    ## 8307                     New Zealand   year72    2903900
    ## 8308                     New Zealand   year73    2961300
    ## 8309                     New Zealand   year74    3023700
    ## 8310                     New Zealand   year75    3083100
    ## 8311                     New Zealand   year76    3110500
    ## 8312                     New Zealand   year77    3120200
    ## 8313                     New Zealand   year78    3121200
    ## 8314                     New Zealand   year79    3109000
    ## 8315                     New Zealand   year80    3112900
    ## 8316                     New Zealand   year81    3124900
    ## 8317                     New Zealand   year82    3156100
    ## 8318                     New Zealand   year83    3199300
    ## 8319                     New Zealand   year84    3227100
    ## 8320                     New Zealand   year85    3247100
    ## 8321                     New Zealand   year86    3246300
    ## 8322                     New Zealand   year87    3274400
    ## 8323                     New Zealand   year88    3283400
    ## 8324                     New Zealand   year89    3299200
    ## 8325                     New Zealand   year90    3329800
    ## 8326                     New Zealand   year91    3495100
    ## 8327                     New Zealand   year92    3531700
    ## 8328                     New Zealand   year93    3572200
    ## 8329                     New Zealand   year94    3620000
    ## 8330                     New Zealand   year95    3673400
    ## 8331                     New Zealand   year96    3732000
    ## 8332                     New Zealand   year97    3781300
    ## 8333                     New Zealand   year98    3815000
    ## 8334                     New Zealand   year99    3835100
    ## 8335                     New Zealand year2000    3857700
    ## 8336                     New Zealand year2001    3880500
    ## 8337                     New Zealand year2002    3948500
    ## 8338                     New Zealand year2003    4027200
    ## 8339                     New Zealand year2004    4087500
    ## 8340                     New Zealand year2005    4133900
    ## 8341                     New Zealand year2006    4184600
    ## 8342                     New Zealand year2007    4223800
    ## 8343                     New Zealand year2008    4259800
    ## 8344                     New Zealand year2009    4302600
    ## 8345                     New Zealand year2010    4350700
    ## 8346                     New Zealand year2011    4384000
    ## 8347                     New Zealand year2012    4408100
    ## 8348                     New Zealand year2013    4442100
    ## 8349                     New Zealand year2014    4509700
    ## 8350                     New Zealand year2015    4595700
    ## 8351                     New Zealand year2016    4693200
    ## 8352                     New Zealand year2017    4793900
    ## 8353                       Nicaragua   year60    1774699
    ## 8354                       Nicaragua   year61    1830400
    ## 8355                       Nicaragua   year62    1886562
    ## 8356                       Nicaragua   year63    1943590
    ## 8357                       Nicaragua   year64    2002119
    ## 8358                       Nicaragua   year65    2062630
    ## 8359                       Nicaragua   year66    2125240
    ## 8360                       Nicaragua   year67    2189882
    ## 8361                       Nicaragua   year68    2256782
    ## 8362                       Nicaragua   year69    2326139
    ## 8363                       Nicaragua   year70    2398096
    ## 8364                       Nicaragua   year71    2472656
    ## 8365                       Nicaragua   year72    2549774
    ## 8366                       Nicaragua   year73    2629505
    ## 8367                       Nicaragua   year74    2711848
    ## 8368                       Nicaragua   year75    2796746
    ## 8369                       Nicaragua   year76    2884155
    ## 8370                       Nicaragua   year77    2973806
    ## 8371                       Nicaragua   year78    3065117
    ## 8372                       Nicaragua   year79    3157355
    ## 8373                       Nicaragua   year80    3249910
    ## 8374                       Nicaragua   year81    3342669
    ## 8375                       Nicaragua   year82    3435525
    ## 8376                       Nicaragua   year83    3527939
    ## 8377                       Nicaragua   year84    3619253
    ## 8378                       Nicaragua   year85    3709091
    ## 8379                       Nicaragua   year86    3796917
    ## 8380                       Nicaragua   year87    3882943
    ## 8381                       Nicaragua   year88    3968454
    ## 8382                       Nicaragua   year89    4055265
    ## 8383                       Nicaragua   year90    4144565
    ## 8384                       Nicaragua   year91    4236801
    ## 8385                       Nicaragua   year92    4331277
    ## 8386                       Nicaragua   year93    4426580
    ## 8387                       Nicaragua   year94    4520725
    ## 8388                       Nicaragua   year95    4612228
    ## 8389                       Nicaragua   year96    4700779
    ## 8390                       Nicaragua   year97    4786640
    ## 8391                       Nicaragua   year98    4869626
    ## 8392                       Nicaragua   year99    4949660
    ## 8393                       Nicaragua year2000    5026796
    ## 8394                       Nicaragua year2001    5100750
    ## 8395                       Nicaragua year2002    5171734
    ## 8396                       Nicaragua year2003    5240879
    ## 8397                       Nicaragua year2004    5309703
    ## 8398                       Nicaragua year2005    5379328
    ## 8399                       Nicaragua year2006    5450211
    ## 8400                       Nicaragua year2007    5522106
    ## 8401                       Nicaragua year2008    5594506
    ## 8402                       Nicaragua year2009    5666581
    ## 8403                       Nicaragua year2010    5737723
    ## 8404                       Nicaragua year2011    5807820
    ## 8405                       Nicaragua year2012    5877108
    ## 8406                       Nicaragua year2013    5945747
    ## 8407                       Nicaragua year2014    6013997
    ## 8408                       Nicaragua year2015    6082035
    ## 8409                       Nicaragua year2016    6149928
    ## 8410                       Nicaragua year2017    6217581
    ## 8411                           Niger   year60    3388764
    ## 8412                           Niger   year61    3486295
    ## 8413                           Niger   year62    3588156
    ## 8414                           Niger   year63    3693866
    ## 8415                           Niger   year64    3802640
    ## 8416                           Niger   year65    3913934
    ## 8417                           Niger   year66    4027758
    ## 8418                           Niger   year67    4144395
    ## 8419                           Niger   year68    4263745
    ## 8420                           Niger   year69    4385758
    ## 8421                           Niger   year70    4510479
    ## 8422                           Niger   year71    4637829
    ## 8423                           Niger   year72    4768078
    ## 8424                           Niger   year73    4902006
    ## 8425                           Niger   year74    5040656
    ## 8426                           Niger   year75    5184811
    ## 8427                           Niger   year76    5334918
    ## 8428                           Niger   year77    5490921
    ## 8429                           Niger   year78    5652355
    ## 8430                           Niger   year79    5818506
    ## 8431                           Niger   year80    5988904
    ## 8432                           Niger   year81    6164006
    ## 8433                           Niger   year82    6344382
    ## 8434                           Niger   year83    6529894
    ## 8435                           Niger   year84    6720344
    ## 8436                           Niger   year85    6915927
    ## 8437                           Niger   year86    7116744
    ## 8438                           Niger   year87    7323969
    ## 8439                           Niger   year88    7540253
    ## 8440                           Niger   year89    7768995
    ## 8441                           Niger   year90    8012861
    ## 8442                           Niger   year91    8272976
    ## 8443                           Niger   year92    8549424
    ## 8444                           Niger   year93    8842415
    ## 8445                           Niger   year94    9151763
    ## 8446                           Niger   year95    9477333
    ## 8447                           Niger   year96    9819964
    ## 8448                           Niger   year97   10180061
    ## 8449                           Niger   year98   10556549
    ## 8450                           Niger   year99   10947829
    ## 8451                           Niger year2000   11352973
    ## 8452                           Niger year2001   11771976
    ## 8453                           Niger year2002   12206002
    ## 8454                           Niger year2003   12656870
    ## 8455                           Niger year2004   13127012
    ## 8456                           Niger year2005   13618449
    ## 8457                           Niger year2006   14132064
    ## 8458                           Niger year2007   14668338
    ## 8459                           Niger year2008   15228525
    ## 8460                           Niger year2009   15813913
    ## 8461                           Niger year2010   16425578
    ## 8462                           Niger year2011   17064636
    ## 8463                           Niger year2012   17731634
    ## 8464                           Niger year2013   18426372
    ## 8465                           Niger year2014   19148219
    ## 8466                           Niger year2015   19896965
    ## 8467                           Niger year2016   20672987
    ## 8468                           Niger year2017   21477348
    ## 8469                         Nigeria   year60   45137812
    ## 8470                         Nigeria   year61   46062905
    ## 8471                         Nigeria   year62   47029140
    ## 8472                         Nigeria   year63   48032246
    ## 8473                         Nigeria   year64   49066059
    ## 8474                         Nigeria   year65   50127214
    ## 8475                         Nigeria   year66   51217359
    ## 8476                         Nigeria   year67   52341834
    ## 8477                         Nigeria   year68   53505978
    ## 8478                         Nigeria   year69   54716735
    ## 8479                         Nigeria   year70   55981400
    ## 8480                         Nigeria   year71   57295210
    ## 8481                         Nigeria   year72   58662603
    ## 8482                         Nigeria   year73   60110433
    ## 8483                         Nigeria   year74   61673559
    ## 8484                         Nigeria   year75   63373572
    ## 8485                         Nigeria   year76   65226229
    ## 8486                         Nigeria   year77   67215805
    ## 8487                         Nigeria   year78   69293550
    ## 8488                         Nigeria   year79   71391290
    ## 8489                         Nigeria   year80   73460724
    ## 8490                         Nigeria   year81   75482552
    ## 8491                         Nigeria   year82   77472907
    ## 8492                         Nigeria   year83   79462277
    ## 8493                         Nigeria   year84   81497739
    ## 8494                         Nigeria   year85   83613300
    ## 8495                         Nigeria   year86   85818502
    ## 8496                         Nigeria   year87   88101628
    ## 8497                         Nigeria   year88   90450281
    ## 8498                         Nigeria   year89   92844353
    ## 8499                         Nigeria   year90   95269988
    ## 8500                         Nigeria   year91   97726323
    ## 8501                         Nigeria   year92  100221563
    ## 8502                         Nigeria   year93  102761737
    ## 8503                         Nigeria   year94  105355783
    ## 8504                         Nigeria   year95  108011465
    ## 8505                         Nigeria   year96  110732904
    ## 8506                         Nigeria   year97  113522705
    ## 8507                         Nigeria   year98  116385750
    ## 8508                         Nigeria   year99  119327073
    ## 8509                         Nigeria year2000  122352009
    ## 8510                         Nigeria year2001  125463434
    ## 8511                         Nigeria year2002  128666710
    ## 8512                         Nigeria year2003  131972533
    ## 8513                         Nigeria year2004  135393616
    ## 8514                         Nigeria year2005  138939478
    ## 8515                         Nigeria year2006  142614094
    ## 8516                         Nigeria year2007  146417024
    ## 8517                         Nigeria year2008  150347390
    ## 8518                         Nigeria year2009  154402181
    ## 8519                         Nigeria year2010  158578261
    ## 8520                         Nigeria year2011  162877076
    ## 8521                         Nigeria year2012  167297284
    ## 8522                         Nigeria year2013  171829303
    ## 8523                         Nigeria year2014  176460502
    ## 8524                         Nigeria year2015  181181744
    ## 8525                         Nigeria year2016  185989640
    ## 8526                         Nigeria year2017  190886311
    ## 8527                   North America   year60  198624409
    ## 8528                   North America   year61  202007500
    ## 8529                   North America   year62  205198600
    ## 8530                   North America   year63  208253700
    ## 8531                   North America   year64  211262900
    ## 8532                   North America   year65  214031100
    ## 8533                   North America   year66  216659000
    ## 8534                   North America   year67  219176000
    ## 8535                   North America   year68  221503000
    ## 8536                   North America   year69  223759000
    ## 8537                   North America   year70  226431000
    ## 8538                   North America   year71  229361135
    ## 8539                   North America   year72  231943831
    ## 8540                   North America   year73  234332208
    ## 8541                   North America   year74  236681487
    ## 8542                   North America   year75  239235000
    ## 8543                   North America   year76  241606200
    ## 8544                   North America   year77  244088400
    ## 8545                   North America   year78  246674600
    ## 8546                   North America   year79  249385800
    ## 8547                   North America   year80  251872670
    ## 8548                   North America   year81  254421050
    ## 8549                   North America   year82  256921449
    ## 8550                   North America   year83  259303930
    ## 8551                   North America   year84  261583423
    ## 8552                   North America   year85  263922898
    ## 8553                   North America   year86  266394382
    ## 8554                   North America   year87  268896849
    ## 8555                   North America   year88  271452347
    ## 8556                   North America   year89  274256841
    ## 8557                   North America   year90  277473326
    ## 8558                   North America   year91  281211703
    ## 8559                   North America   year92  285092192
    ## 8560                   North America   year93  288811320
    ## 8561                   North America   year94  292297226
    ## 8562                   North America   year95  295691746
    ## 8563                   North America   year96  299126029
    ## 8564                   North America   year97  302704697
    ## 8565                   North America   year98  306162843
    ## 8566                   North America   year99  309600485
    ## 8567                   North America year2000  312993944
    ## 8568                   North America year2001  316113359
    ## 8569                   North America year2002  319050105
    ## 8570                   North America year2003  321847258
    ## 8571                   North America year2004  324864038
    ## 8572                   North America year2005  327892753
    ## 8573                   North America year2006  331014940
    ## 8574                   North America year2007  334184023
    ## 8575                   North America year2008  337405012
    ## 8576                   North America year2009  340465736
    ## 8577                   North America year2010  343408819
    ## 8578                   North America year2011  346051624
    ## 8579                   North America year2012  348808615
    ## 8580                   North America year2013  351451876
    ## 8581                   North America year2014  354223012
    ## 8582                   North America year2015  356937591
    ## 8583                   North America year2016  359735880
    ## 8584                   North America year2017  362492702
    ## 8585        Northern Mariana Islands   year60      10035
    ## 8586        Northern Mariana Islands   year61      10302
    ## 8587        Northern Mariana Islands   year62      10499
    ## 8588        Northern Mariana Islands   year63      10667
    ## 8589        Northern Mariana Islands   year64      10857
    ## 8590        Northern Mariana Islands   year65      11105
    ## 8591        Northern Mariana Islands   year66      11435
    ## 8592        Northern Mariana Islands   year67      11823
    ## 8593        Northern Mariana Islands   year68      12257
    ## 8594        Northern Mariana Islands   year69      12691
    ## 8595        Northern Mariana Islands   year70      13127
    ## 8596        Northern Mariana Islands   year71      13569
    ## 8597        Northern Mariana Islands   year72      14040
    ## 8598        Northern Mariana Islands   year73      14492
    ## 8599        Northern Mariana Islands   year74      14859
    ## 8600        Northern Mariana Islands   year75      15117
    ## 8601        Northern Mariana Islands   year76      15234
    ## 8602        Northern Mariana Islands   year77      15251
    ## 8603        Northern Mariana Islands   year78      15372
    ## 8604        Northern Mariana Islands   year79      15862
    ## 8605        Northern Mariana Islands   year80      16920
    ## 8606        Northern Mariana Islands   year81      18604
    ## 8607        Northern Mariana Islands   year82      20856
    ## 8608        Northern Mariana Islands   year83      23503
    ## 8609        Northern Mariana Islands   year84      26302
    ## 8610        Northern Mariana Islands   year85      29092
    ## 8611        Northern Mariana Islands   year86      31802
    ## 8612        Northern Mariana Islands   year87      34480
    ## 8613        Northern Mariana Islands   year88      37134
    ## 8614        Northern Mariana Islands   year89      39808
    ## 8615        Northern Mariana Islands   year90      42538
    ## 8616        Northern Mariana Islands   year91      45249
    ## 8617        Northern Mariana Islands   year92      47919
    ## 8618        Northern Mariana Islands   year93      50602
    ## 8619        Northern Mariana Islands   year94      53380
    ## 8620        Northern Mariana Islands   year95      56278
    ## 8621        Northern Mariana Islands   year96      59364
    ## 8622        Northern Mariana Islands   year97      62528
    ## 8623        Northern Mariana Islands   year98      65474
    ## 8624        Northern Mariana Islands   year99      67755
    ## 8625        Northern Mariana Islands year2000      69094
    ## 8626        Northern Mariana Islands year2001      69388
    ## 8627        Northern Mariana Islands year2002      68763
    ## 8628        Northern Mariana Islands year2003      67422
    ## 8629        Northern Mariana Islands year2004      65663
    ## 8630        Northern Mariana Islands year2005      63744
    ## 8631        Northern Mariana Islands year2006      61688
    ## 8632        Northern Mariana Islands year2007      59513
    ## 8633        Northern Mariana Islands year2008      57431
    ## 8634        Northern Mariana Islands year2009      55674
    ## 8635        Northern Mariana Islands year2010      54424
    ## 8636        Northern Mariana Islands year2011      53786
    ## 8637        Northern Mariana Islands year2012      53718
    ## 8638        Northern Mariana Islands year2013      54036
    ## 8639        Northern Mariana Islands year2014      54468
    ## 8640        Northern Mariana Islands year2015      54816
    ## 8641        Northern Mariana Islands year2016      55023
    ## 8642        Northern Mariana Islands year2017      55144
    ## 8643                          Norway   year60    3581239
    ## 8644                          Norway   year61    3609800
    ## 8645                          Norway   year62    3638918
    ## 8646                          Norway   year63    3666537
    ## 8647                          Norway   year64    3694339
    ## 8648                          Norway   year65    3723168
    ## 8649                          Norway   year66    3753012
    ## 8650                          Norway   year67    3784539
    ## 8651                          Norway   year68    3816486
    ## 8652                          Norway   year69    3847707
    ## 8653                          Norway   year70    3875763
    ## 8654                          Norway   year71    3903039
    ## 8655                          Norway   year72    3933004
    ## 8656                          Norway   year73    3960612
    ## 8657                          Norway   year74    3985258
    ## 8658                          Norway   year75    4007313
    ## 8659                          Norway   year76    4026152
    ## 8660                          Norway   year77    4043205
    ## 8661                          Norway   year78    4058671
    ## 8662                          Norway   year79    4072517
    ## 8663                          Norway   year80    4085620
    ## 8664                          Norway   year81    4099702
    ## 8665                          Norway   year82    4114787
    ## 8666                          Norway   year83    4128432
    ## 8667                          Norway   year84    4140099
    ## 8668                          Norway   year85    4152516
    ## 8669                          Norway   year86    4167354
    ## 8670                          Norway   year87    4186905
    ## 8671                          Norway   year88    4209488
    ## 8672                          Norway   year89    4226901
    ## 8673                          Norway   year90    4241473
    ## 8674                          Norway   year91    4261732
    ## 8675                          Norway   year92    4286401
    ## 8676                          Norway   year93    4311991
    ## 8677                          Norway   year94    4336613
    ## 8678                          Norway   year95    4359184
    ## 8679                          Norway   year96    4381336
    ## 8680                          Norway   year97    4405157
    ## 8681                          Norway   year98    4431464
    ## 8682                          Norway   year99    4461913
    ## 8683                          Norway year2000    4490967
    ## 8684                          Norway year2001    4513751
    ## 8685                          Norway year2002    4538159
    ## 8686                          Norway year2003    4564855
    ## 8687                          Norway year2004    4591910
    ## 8688                          Norway year2005    4623291
    ## 8689                          Norway year2006    4660677
    ## 8690                          Norway year2007    4709153
    ## 8691                          Norway year2008    4768212
    ## 8692                          Norway year2009    4828726
    ## 8693                          Norway year2010    4889252
    ## 8694                          Norway year2011    4953088
    ## 8695                          Norway year2012    5018573
    ## 8696                          Norway year2013    5079623
    ## 8697                          Norway year2014    5137232
    ## 8698                          Norway year2015    5190239
    ## 8699                          Norway year2016    5234519
    ## 8700                          Norway year2017    5282223
    ## 8701                            Oman   year60     551740
    ## 8702                            Oman   year61     564890
    ## 8703                            Oman   year62     578824
    ## 8704                            Oman   year63     593501
    ## 8705                            Oman   year64     608887
    ## 8706                            Oman   year65     625009
    ## 8707                            Oman   year66     642003
    ## 8708                            Oman   year67     660119
    ## 8709                            Oman   year68     679597
    ## 8710                            Oman   year69     700725
    ## 8711                            Oman   year70     723852
    ## 8712                            Oman   year71     748973
    ## 8713                            Oman   year72     776383
    ## 8714                            Oman   year73     806991
    ## 8715                            Oman   year74     841948
    ## 8716                            Oman   year75     882044
    ## 8717                            Oman   year76     927439
    ## 8718                            Oman   year77     977808
    ## 8719                            Oman   year78    1032800
    ## 8720                            Oman   year79    1091853
    ## 8721                            Oman   year80    1154379
    ## 8722                            Oman   year81    1220587
    ## 8723                            Oman   year82    1290111
    ## 8724                            Oman   year83    1361097
    ## 8725                            Oman   year84    1431077
    ## 8726                            Oman   year85    1498417
    ## 8727                            Oman   year86    1561185
    ## 8728                            Oman   year87    1619864
    ## 8729                            Oman   year88    1678116
    ## 8730                            Oman   year89    1741160
    ## 8731                            Oman   year90    1812160
    ## 8732                            Oman   year91    1893771
    ## 8733                            Oman   year92    1983277
    ## 8734                            Oman   year93    2072111
    ## 8735                            Oman   year94    2148428
    ## 8736                            Oman   year95    2204283
    ## 8737                            Oman   year96    2236666
    ## 8738                            Oman   year97    2249773
    ## 8739                            Oman   year98    2251875
    ## 8740                            Oman   year99    2254918
    ## 8741                            Oman year2000    2267991
    ## 8742                            Oman year2001    2294787
    ## 8743                            Oman year2002    2334285
    ## 8744                            Oman year2003    2385255
    ## 8745                            Oman year2004    2444751
    ## 8746                            Oman year2005    2511269
    ## 8747                            Oman year2006    2582991
    ## 8748                            Oman year2007    2662762
    ## 8749                            Oman year2008    2759014
    ## 8750                            Oman year2009    2882942
    ## 8751                            Oman year2010    3041460
    ## 8752                            Oman year2011    3237268
    ## 8753                            Oman year2012    3464644
    ## 8754                            Oman year2013    3711481
    ## 8755                            Oman year2014    3960925
    ## 8756                            Oman year2015    4199810
    ## 8757                            Oman year2016    4424762
    ## 8758                            Oman year2017    4636262
    ## 8759                        Pakistan   year60   44908293
    ## 8760                        Pakistan   year61   45984892
    ## 8761                        Pakistan   year62   47119361
    ## 8762                        Pakistan   year63   48309315
    ## 8763                        Pakistan   year64   49551904
    ## 8764                        Pakistan   year65   50845221
    ## 8765                        Pakistan   year66   52191095
    ## 8766                        Pakistan   year67   53590929
    ## 8767                        Pakistan   year68   55042397
    ## 8768                        Pakistan   year69   56542434
    ## 8769                        Pakistan   year70   58090759
    ## 8770                        Pakistan   year71   59687140
    ## 8771                        Pakistan   year72   61338261
    ## 8772                        Pakistan   year73   63059481
    ## 8773                        Pakistan   year74   64870833
    ## 8774                        Pakistan   year75   66787901
    ## 8775                        Pakistan   year76   68813220
    ## 8776                        Pakistan   year77   70946231
    ## 8777                        Pakistan   year78   73194937
    ## 8778                        Pakistan   year79   75567682
    ## 8779                        Pakistan   year80   78068144
    ## 8780                        Pakistan   year81   80696945
    ## 8781                        Pakistan   year82   83445863
    ## 8782                        Pakistan   year83   86297640
    ## 8783                        Pakistan   year84   89228949
    ## 8784                        Pakistan   year85   92219488
    ## 8785                        Pakistan   year86   95264460
    ## 8786                        Pakistan   year87   98357473
    ## 8787                        Pakistan   year88  101474835
    ## 8788                        Pakistan   year89  104588490
    ## 8789                        Pakistan   year90  107678614
    ## 8790                        Pakistan   year91  110730420
    ## 8791                        Pakistan   year92  113747135
    ## 8792                        Pakistan   year93  116749560
    ## 8793                        Pakistan   year94  119769556
    ## 8794                        Pakistan   year95  122829148
    ## 8795                        Pakistan   year96  125938339
    ## 8796                        Pakistan   year97  129086987
    ## 8797                        Pakistan   year98  132253264
    ## 8798                        Pakistan   year99  135405584
    ## 8799                        Pakistan year2000  138523285
    ## 8800                        Pakistan year2001  141601437
    ## 8801                        Pakistan year2002  144654143
    ## 8802                        Pakistan year2003  147703401
    ## 8803                        Pakistan year2004  150780300
    ## 8804                        Pakistan year2005  153909667
    ## 8805                        Pakistan year2006  157093993
    ## 8806                        Pakistan year2007  160332974
    ## 8807                        Pakistan year2008  163644603
    ## 8808                        Pakistan year2009  167049580
    ## 8809                        Pakistan year2010  170560182
    ## 8810                        Pakistan year2011  174184265
    ## 8811                        Pakistan year2012  177911533
    ## 8812                        Pakistan year2013  181712595
    ## 8813                        Pakistan year2014  185546257
    ## 8814                        Pakistan year2015  189380513
    ## 8815                        Pakistan year2016  193203476
    ## 8816                        Pakistan year2017  197015955
    ## 8817                           Palau   year60       9642
    ## 8818                           Palau   year61       9900
    ## 8819                           Palau   year62      10151
    ## 8820                           Palau   year63      10378
    ## 8821                           Palau   year64      10593
    ## 8822                           Palau   year65      10782
    ## 8823                           Palau   year66      10946
    ## 8824                           Palau   year67      11080
    ## 8825                           Palau   year68      11205
    ## 8826                           Palau   year69      11331
    ## 8827                           Palau   year70      11480
    ## 8828                           Palau   year71      11654
    ## 8829                           Palau   year72      11852
    ## 8830                           Palau   year73      12046
    ## 8831                           Palau   year74      12197
    ## 8832                           Palau   year75      12278
    ## 8833                           Palau   year76      12285
    ## 8834                           Palau   year77      12225
    ## 8835                           Palau   year78      12153
    ## 8836                           Palau   year79      12124
    ## 8837                           Palau   year80      12194
    ## 8838                           Palau   year81      12387
    ## 8839                           Palau   year82      12663
    ## 8840                           Palau   year83      13012
    ## 8841                           Palau   year84      13372
    ## 8842                           Palau   year85      13696
    ## 8843                           Palau   year86      13985
    ## 8844                           Palau   year87      14240
    ## 8845                           Palau   year88      14490
    ## 8846                           Palau   year89      14757
    ## 8847                           Palau   year90      15088
    ## 8848                           Palau   year91      15474
    ## 8849                           Palau   year92      15894
    ## 8850                           Palau   year93      16342
    ## 8851                           Palau   year94      16806
    ## 8852                           Palau   year95      17253
    ## 8853                           Palau   year96      17691
    ## 8854                           Palau   year97      18123
    ## 8855                           Palau   year98      18524
    ## 8856                           Palau   year99      18879
    ## 8857                           Palau year2000      19175
    ## 8858                           Palau year2001      19404
    ## 8859                           Palau year2002      19574
    ## 8860                           Palau year2003      19700
    ## 8861                           Palau year2004      19804
    ## 8862                           Palau year2005      19906
    ## 8863                           Palau year2006      20012
    ## 8864                           Palau year2007      20116
    ## 8865                           Palau year2008      20228
    ## 8866                           Palau year2009      20342
    ## 8867                           Palau year2010      20470
    ## 8868                           Palau year2011      20599
    ## 8869                           Palau year2012      20758
    ## 8870                           Palau year2013      20920
    ## 8871                           Palau year2014      21094
    ## 8872                           Palau year2015      21288
    ## 8873                           Palau year2016      21503
    ## 8874                           Palau year2017      21729
    ## 8875                          Panama   year60    1132921
    ## 8876                          Panama   year61    1167035
    ## 8877                          Panama   year62    1202373
    ## 8878                          Panama   year63    1238823
    ## 8879                          Panama   year64    1276276
    ## 8880                          Panama   year65    1314626
    ## 8881                          Panama   year66    1353804
    ## 8882                          Panama   year67    1393799
    ## 8883                          Panama   year68    1434657
    ## 8884                          Panama   year69    1476479
    ## 8885                          Panama   year70    1519299
    ## 8886                          Panama   year71    1563115
    ## 8887                          Panama   year72    1607834
    ## 8888                          Panama   year73    1653256
    ## 8889                          Panama   year74    1699113
    ## 8890                          Panama   year75    1745205
    ## 8891                          Panama   year76    1791453
    ## 8892                          Panama   year77    1837890
    ## 8893                          Panama   year78    1884515
    ## 8894                          Panama   year79    1931389
    ## 8895                          Panama   year80    1978578
    ## 8896                          Panama   year81    2026065
    ## 8897                          Panama   year82    2073844
    ## 8898                          Panama   year83    2121939
    ## 8899                          Panama   year84    2170409
    ## 8900                          Panama   year85    2219276
    ## 8901                          Panama   year86    2268574
    ## 8902                          Panama   year87    2318332
    ## 8903                          Panama   year88    2368618
    ## 8904                          Panama   year89    2419491
    ## 8905                          Panama   year90    2471009
    ## 8906                          Panama   year91    2523181
    ## 8907                          Panama   year92    2576018
    ## 8908                          Panama   year93    2629644
    ## 8909                          Panama   year94    2684183
    ## 8910                          Panama   year95    2739730
    ## 8911                          Panama   year96    2796344
    ## 8912                          Panama   year97    2853941
    ## 8913                          Panama   year98    2912328
    ## 8914                          Panama   year99    2971197
    ## 8915                          Panama year2000    3030347
    ## 8916                          Panama year2001    3089684
    ## 8917                          Panama year2002    3149265
    ## 8918                          Panama year2003    3209174
    ## 8919                          Panama year2004    3269541
    ## 8920                          Panama year2005    3330465
    ## 8921                          Panama year2006    3391905
    ## 8922                          Panama year2007    3453807
    ## 8923                          Panama year2008    3516268
    ## 8924                          Panama year2009    3579385
    ## 8925                          Panama year2010    3643222
    ## 8926                          Panama year2011    3707782
    ## 8927                          Panama year2012    3772938
    ## 8928                          Panama year2013    3838462
    ## 8929                          Panama year2014    3903986
    ## 8930                          Panama year2015    3969249
    ## 8931                          Panama year2016    4034119
    ## 8932                          Panama year2017    4098587
    ## 8933                Papua New Guinea   year60    2010677
    ## 8934                Papua New Guinea   year61    2051947
    ## 8935                Papua New Guinea   year62    2094687
    ## 8936                Papua New Guinea   year63    2139303
    ## 8937                Papua New Guinea   year64    2186340
    ## 8938                Papua New Guinea   year65    2236206
    ## 8939                Papua New Guinea   year66    2289109
    ## 8940                Papua New Guinea   year67    2344977
    ## 8941                Papua New Guinea   year68    2403595
    ## 8942                Papua New Guinea   year69    2464548
    ## 8943                Papua New Guinea   year70    2527586
    ## 8944                Papua New Guinea   year71    2592628
    ## 8945                Papua New Guinea   year72    2659851
    ## 8946                Papua New Guinea   year73    2729580
    ## 8947                Papua New Guinea   year74    2802243
    ## 8948                Papua New Guinea   year75    2878156
    ## 8949                Papua New Guinea   year76    2957339
    ## 8950                Papua New Guinea   year77    3039660
    ## 8951                Papua New Guinea   year78    3125034
    ## 8952                Papua New Guinea   year79    3213360
    ## 8953                Papua New Guinea   year80    3304473
    ## 8954                Papua New Guinea   year81    3398469
    ## 8955                Papua New Guinea   year82    3495199
    ## 8956                Papua New Guinea   year83    3594004
    ## 8957                Papua New Guinea   year84    3694041
    ## 8958                Papua New Guinea   year85    3794720
    ## 8959                Papua New Guinea   year86    3895852
    ## 8960                Papua New Guinea   year87    3997702
    ## 8961                Papua New Guinea   year88    4100729
    ## 8962                Papua New Guinea   year89    4205654
    ## 8963                Papua New Guinea   year90    4313059
    ## 8964                Papua New Guinea   year91    4423007
    ## 8965                Papua New Guinea   year92    4535520
    ## 8966                Papua New Guinea   year93    4651169
    ## 8967                Papua New Guinea   year94    4770606
    ## 8968                Papua New Guinea   year95    4894276
    ## 8969                Papua New Guinea   year96    5022437
    ## 8970                Papua New Guinea   year97    5154910
    ## 8971                Papua New Guinea   year98    5291178
    ## 8972                Papua New Guinea   year99    5430479
    ## 8973                Papua New Guinea year2000    5572222
    ## 8974                Papua New Guinea year2001    5716152
    ## 8975                Papua New Guinea year2002    5862316
    ## 8976                Papua New Guinea year2003    6010724
    ## 8977                Papua New Guinea year2004    6161517
    ## 8978                Papua New Guinea year2005    6314709
    ## 8979                Papua New Guinea year2006    6470272
    ## 8980                Papua New Guinea year2007    6627922
    ## 8981                Papua New Guinea year2008    6787187
    ## 8982                Papua New Guinea year2009    6947447
    ## 8983                Papua New Guinea year2010    7108239
    ## 8984                Papua New Guinea year2011    7269348
    ## 8985                Papua New Guinea year2012    7430836
    ## 8986                Papua New Guinea year2013    7592865
    ## 8987                Papua New Guinea year2014    7755785
    ## 8988                Papua New Guinea year2015    7919825
    ## 8989                Papua New Guinea year2016    8084991
    ## 8990                Papua New Guinea year2017    8251162
    ## 8991                        Paraguay   year60    1902875
    ## 8992                        Paraguay   year61    1953328
    ## 8993                        Paraguay   year62    2005337
    ## 8994                        Paraguay   year63    2058915
    ## 8995                        Paraguay   year64    2114095
    ## 8996                        Paraguay   year65    2170859
    ## 8997                        Paraguay   year66    2229376
    ## 8998                        Paraguay   year67    2289582
    ## 8999                        Paraguay   year68    2350901
    ## 9000                        Paraguay   year69    2412566
    ## 9001                        Paraguay   year70    2474106
    ## 9002                        Paraguay   year71    2535359
    ## 9003                        Paraguay   year72    2596739
    ## 9004                        Paraguay   year73    2659088
    ## 9005                        Paraguay   year74    2723523
    ## 9006                        Paraguay   year75    2790962
    ## 9007                        Paraguay   year76    2861581
    ## 9008                        Paraguay   year77    2935375
    ## 9009                        Paraguay   year78    3012829
    ## 9010                        Paraguay   year79    3094482
    ## 9011                        Paraguay   year80    3180630
    ## 9012                        Paraguay   year81    3271456
    ## 9013                        Paraguay   year82    3366719
    ## 9014                        Paraguay   year83    3465793
    ## 9015                        Paraguay   year84    3567752
    ## 9016                        Paraguay   year85    3671826
    ## 9017                        Paraguay   year86    3777763
    ## 9018                        Paraguay   year87    3885436
    ## 9019                        Paraguay   year88    3994331
    ## 9020                        Paraguay   year89    4103911
    ## 9021                        Paraguay   year90    4213742
    ## 9022                        Paraguay   year91    4323410
    ## 9023                        Paraguay   year92    4432736
    ## 9024                        Paraguay   year93    4541902
    ## 9025                        Paraguay   year94    4651225
    ## 9026                        Paraguay   year95    4760850
    ## 9027                        Paraguay   year96    4870694
    ## 9028                        Paraguay   year97    4980344
    ## 9029                        Paraguay   year98    5089310
    ## 9030                        Paraguay   year99    5196937
    ## 9031                        Paraguay year2000    5302700
    ## 9032                        Paraguay year2001    5406624
    ## 9033                        Paraguay year2002    5508611
    ## 9034                        Paraguay year2003    5607950
    ## 9035                        Paraguay year2004    5703740
    ## 9036                        Paraguay year2005    5795494
    ## 9037                        Paraguay year2006    5882796
    ## 9038                        Paraguay year2007    5966159
    ## 9039                        Paraguay year2008    6047117
    ## 9040                        Paraguay year2009    6127837
    ## 9041                        Paraguay year2010    6209877
    ## 9042                        Paraguay year2011    6293783
    ## 9043                        Paraguay year2012    6379219
    ## 9044                        Paraguay year2013    6465740
    ## 9045                        Paraguay year2014    6552584
    ## 9046                        Paraguay year2015    6639119
    ## 9047                        Paraguay year2016    6725308
    ## 9048                        Paraguay year2017    6811297
    ## 9049                            Peru   year60   10061515
    ## 9050                            Peru   year61   10350242
    ## 9051                            Peru   year62   10650667
    ## 9052                            Peru   year63   10961540
    ## 9053                            Peru   year64   11281015
    ## 9054                            Peru   year65   11607681
    ## 9055                            Peru   year66   11941325
    ## 9056                            Peru   year67   12282082
    ## 9057                            Peru   year68   12629329
    ## 9058                            Peru   year69   12982449
    ## 9059                            Peru   year70   13341069
    ## 9060                            Peru   year71   13704335
    ## 9061                            Peru   year72   14072476
    ## 9062                            Peru   year73   14447648
    ## 9063                            Peru   year74   14832841
    ## 9064                            Peru   year75   15229947
    ## 9065                            Peru   year76   15639901
    ## 9066                            Peru   year77   16061323
    ## 9067                            Peru   year78   16491083
    ## 9068                            Peru   year79   16924753
    ## 9069                            Peru   year80   17359120
    ## 9070                            Peru   year81   17792549
    ## 9071                            Peru   year82   18225730
    ## 9072                            Peru   year83   18660439
    ## 9073                            Peru   year84   19099584
    ## 9074                            Peru   year85   19544956
    ## 9075                            Peru   year86   19996253
    ## 9076                            Peru   year87   20451710
    ## 9077                            Peru   year88   20909895
    ## 9078                            Peru   year89   21368859
    ## 9079                            Peru   year90   21826658
    ## 9080                            Peru   year91   22283128
    ## 9081                            Peru   year92   22737056
    ## 9082                            Peru   year93   23184228
    ## 9083                            Peru   year94   23619356
    ## 9084                            Peru   year95   24038760
    ## 9085                            Peru   year96   24441074
    ## 9086                            Peru   year97   24827406
    ## 9087                            Peru   year98   25199748
    ## 9088                            Peru   year99   25561299
    ## 9089                            Peru year2000   25914879
    ## 9090                            Peru year2001   26261363
    ## 9091                            Peru year2002   26601467
    ## 9092                            Peru year2003   26937738
    ## 9093                            Peru year2004   27273194
    ## 9094                            Peru year2005   27610410
    ## 9095                            Peru year2006   27949944
    ## 9096                            Peru year2007   28292724
    ## 9097                            Peru year2008   28641980
    ## 9098                            Peru year2009   29001507
    ## 9099                            Peru year2010   29373646
    ## 9100                            Peru year2011   29759989
    ## 9101                            Peru year2012   30158966
    ## 9102                            Peru year2013   30565716
    ## 9103                            Peru year2014   30973354
    ## 9104                            Peru year2015   31376671
    ## 9105                            Peru year2016   31773839
    ## 9106                            Peru year2017   32165485
    ## 9107                     Philippines   year60   26273025
    ## 9108                     Philippines   year61   27164617
    ## 9109                     Philippines   year62   28081231
    ## 9110                     Philippines   year63   29016771
    ## 9111                     Philippines   year64   29962876
    ## 9112                     Philippines   year65   30913933
    ## 9113                     Philippines   year66   31867563
    ## 9114                     Philippines   year67   32826599
    ## 9115                     Philippines   year68   33797042
    ## 9116                     Philippines   year69   34787588
    ## 9117                     Philippines   year70   35804729
    ## 9118                     Philippines   year71   36851055
    ## 9119                     Philippines   year72   37925400
    ## 9120                     Philippines   year73   39026082
    ## 9121                     Philippines   year74   40149961
    ## 9122                     Philippines   year75   41295124
    ## 9123                     Philippines   year76   42461193
    ## 9124                     Philippines   year77   43650333
    ## 9125                     Philippines   year78   44866273
    ## 9126                     Philippines   year79   46113995
    ## 9127                     Philippines   year80   47396968
    ## 9128                     Philippines   year81   48715592
    ## 9129                     Philippines   year82   50068493
    ## 9130                     Philippines   year83   51455033
    ## 9131                     Philippines   year84   52873974
    ## 9132                     Philippines   year85   54323648
    ## 9133                     Philippines   year86   55804072
    ## 9134                     Philippines   year87   57313311
    ## 9135                     Philippines   year88   58845205
    ## 9136                     Philippines   year89   60391867
    ## 9137                     Philippines   year90   61947348
    ## 9138                     Philippines   year91   63508459
    ## 9139                     Philippines   year92   65075486
    ## 9140                     Philippines   year93   66650247
    ## 9141                     Philippines   year94   68236230
    ## 9142                     Philippines   year95   69835715
    ## 9143                     Philippines   year96   71446107
    ## 9144                     Philippines   year97   73064764
    ## 9145                     Philippines   year98   74693695
    ## 9146                     Philippines   year99   76335812
    ## 9147                     Philippines year2000   77991569
    ## 9148                     Philippines year2001   79665315
    ## 9149                     Philippines year2002   81352060
    ## 9150                     Philippines year2003   83031954
    ## 9151                     Philippines year2004   84678493
    ## 9152                     Philippines year2005   86274237
    ## 9153                     Philippines year2006   87809419
    ## 9154                     Philippines year2007   89293490
    ## 9155                     Philippines year2008   90751864
    ## 9156                     Philippines year2009   92220879
    ## 9157                     Philippines year2010   93726624
    ## 9158                     Philippines year2011   95277940
    ## 9159                     Philippines year2012   96866642
    ## 9160                     Philippines year2013   98481032
    ## 9161                     Philippines year2014  100102249
    ## 9162                     Philippines year2015  101716359
    ## 9163                     Philippines year2016  103320222
    ## 9164                     Philippines year2017  104918090
    ## 9165                          Poland   year60   29637450
    ## 9166                          Poland   year61   29964000
    ## 9167                          Poland   year62   30308500
    ## 9168                          Poland   year63   30712000
    ## 9169                          Poland   year64   31139450
    ## 9170                          Poland   year65   31444950
    ## 9171                          Poland   year66   31681000
    ## 9172                          Poland   year67   31987155
    ## 9173                          Poland   year68   32294655
    ## 9174                          Poland   year69   32548300
    ## 9175                          Poland   year70   32664300
    ## 9176                          Poland   year71   32783500
    ## 9177                          Poland   year72   33055650
    ## 9178                          Poland   year73   33357200
    ## 9179                          Poland   year74   33678899
    ## 9180                          Poland   year75   34015199
    ## 9181                          Poland   year76   34356300
    ## 9182                          Poland   year77   34689050
    ## 9183                          Poland   year78   34965600
    ## 9184                          Poland   year79   35247217
    ## 9185                          Poland   year80   35574150
    ## 9186                          Poland   year81   35898587
    ## 9187                          Poland   year82   36230481
    ## 9188                          Poland   year83   36571808
    ## 9189                          Poland   year84   36904134
    ## 9190                          Poland   year85   37201885
    ## 9191                          Poland   year86   37456119
    ## 9192                          Poland   year87   37668045
    ## 9193                          Poland   year88   37824487
    ## 9194                          Poland   year89   37961529
    ## 9195                          Poland   year90   38110782
    ## 9196                          Poland   year91   38246193
    ## 9197                          Poland   year92   38363667
    ## 9198                          Poland   year93   38461408
    ## 9199                          Poland   year94   38542652
    ## 9200                          Poland   year95   38594998
    ## 9201                          Poland   year96   38624370
    ## 9202                          Poland   year97   38649660
    ## 9203                          Poland   year98   38663481
    ## 9204                          Poland   year99   38660271
    ## 9205                          Poland year2000   38258629
    ## 9206                          Poland year2001   38248076
    ## 9207                          Poland year2002   38230364
    ## 9208                          Poland year2003   38204570
    ## 9209                          Poland year2004   38182222
    ## 9210                          Poland year2005   38165445
    ## 9211                          Poland year2006   38141267
    ## 9212                          Poland year2007   38120560
    ## 9213                          Poland year2008   38125759
    ## 9214                          Poland year2009   38151603
    ## 9215                          Poland year2010   38042794
    ## 9216                          Poland year2011   38063255
    ## 9217                          Poland year2012   38063164
    ## 9218                          Poland year2013   38040196
    ## 9219                          Poland year2014   38011735
    ## 9220                          Poland year2015   37986412
    ## 9221                          Poland year2016   37970087
    ## 9222                          Poland year2017   37975841
    ## 9223                        Portugal   year60    8857716
    ## 9224                        Portugal   year61    8929316
    ## 9225                        Portugal   year62    8993985
    ## 9226                        Portugal   year63    9030355
    ## 9227                        Portugal   year64    9035365
    ## 9228                        Portugal   year65    8998595
    ## 9229                        Portugal   year66    8930990
    ## 9230                        Portugal   year67    8874520
    ## 9231                        Portugal   year68    8836650
    ## 9232                        Portugal   year69    8757705
    ## 9233                        Portugal   year70    8680431
    ## 9234                        Portugal   year71    8643756
    ## 9235                        Portugal   year72    8630430
    ## 9236                        Portugal   year73    8633100
    ## 9237                        Portugal   year74    8754365
    ## 9238                        Portugal   year75    9093470
    ## 9239                        Portugal   year76    9355810
    ## 9240                        Portugal   year77    9455675
    ## 9241                        Portugal   year78    9558250
    ## 9242                        Portugal   year79    9661265
    ## 9243                        Portugal   year80    9766312
    ## 9244                        Portugal   year81    9851362
    ## 9245                        Portugal   year82    9911771
    ## 9246                        Portugal   year83    9957865
    ## 9247                        Portugal   year84    9996232
    ## 9248                        Portugal   year85   10023613
    ## 9249                        Portugal   year86   10032734
    ## 9250                        Portugal   year87   10030031
    ## 9251                        Portugal   year88   10019610
    ## 9252                        Portugal   year89   10005000
    ## 9253                        Portugal   year90    9983218
    ## 9254                        Portugal   year91    9960235
    ## 9255                        Portugal   year92    9952494
    ## 9256                        Portugal   year93    9964675
    ## 9257                        Portugal   year94    9991525
    ## 9258                        Portugal   year95   10026176
    ## 9259                        Portugal   year96   10063945
    ## 9260                        Portugal   year97   10108977
    ## 9261                        Portugal   year98   10160196
    ## 9262                        Portugal   year99   10217828
    ## 9263                        Portugal year2000   10289898
    ## 9264                        Portugal year2001   10362722
    ## 9265                        Portugal year2002   10419631
    ## 9266                        Portugal year2003   10458821
    ## 9267                        Portugal year2004   10483861
    ## 9268                        Portugal year2005   10503330
    ## 9269                        Portugal year2006   10522288
    ## 9270                        Portugal year2007   10542964
    ## 9271                        Portugal year2008   10558177
    ## 9272                        Portugal year2009   10568247
    ## 9273                        Portugal year2010   10573100
    ## 9274                        Portugal year2011   10557560
    ## 9275                        Portugal year2012   10514844
    ## 9276                        Portugal year2013   10457295
    ## 9277                        Portugal year2014   10401062
    ## 9278                        Portugal year2015   10358076
    ## 9279                        Portugal year2016   10325452
    ## 9280                        Portugal year2017   10293718
    ## 9281                     Puerto Rico   year60    2358000
    ## 9282                     Puerto Rico   year61    2399722
    ## 9283                     Puerto Rico   year62    2450322
    ## 9284                     Puerto Rico   year63    2504530
    ## 9285                     Puerto Rico   year64    2554066
    ## 9286                     Puerto Rico   year65    2594000
    ## 9287                     Puerto Rico   year66    2624995
    ## 9288                     Puerto Rico   year67    2645674
    ## 9289                     Puerto Rico   year68    2662064
    ## 9290                     Puerto Rico   year69    2684150
    ## 9291                     Puerto Rico   year70    2718000
    ## 9292                     Puerto Rico   year71    2762190
    ## 9293                     Puerto Rico   year72    2817256
    ## 9294                     Puerto Rico   year73    2878786
    ## 9295                     Puerto Rico   year74    2939299
    ## 9296                     Puerto Rico   year75    2994000
    ## 9297                     Puerto Rico   year76    3043854
    ## 9298                     Puerto Rico   year77    3088690
    ## 9299                     Puerto Rico   year78    3129421
    ## 9300                     Puerto Rico   year79    3168088
    ## 9301                     Puerto Rico   year80    3206000
    ## 9302                     Puerto Rico   year81    3242552
    ## 9303                     Puerto Rico   year82    3277453
    ## 9304                     Puerto Rico   year83    3311138
    ## 9305                     Puerto Rico   year84    3344190
    ## 9306                     Puerto Rico   year85    3377000
    ## 9307                     Puerto Rico   year86    3409554
    ## 9308                     Puerto Rico   year87    3441850
    ## 9309                     Puerto Rico   year88    3473898
    ## 9310                     Puerto Rico   year89    3505650
    ## 9311                     Puerto Rico   year90    3537000
    ## 9312                     Puerto Rico   year91    3562110
    ## 9313                     Puerto Rico   year92    3585176
    ## 9314                     Puerto Rico   year93    3615497
    ## 9315                     Puerto Rico   year94    3649237
    ## 9316                     Puerto Rico   year95    3683103
    ## 9317                     Puerto Rico   year96    3724655
    ## 9318                     Puerto Rico   year97    3759430
    ## 9319                     Puerto Rico   year98    3781101
    ## 9320                     Puerto Rico   year99    3800081
    ## 9321                     Puerto Rico year2000    3810605
    ## 9322                     Puerto Rico year2001    3818774
    ## 9323                     Puerto Rico year2002    3823701
    ## 9324                     Puerto Rico year2003    3826095
    ## 9325                     Puerto Rico year2004    3826878
    ## 9326                     Puerto Rico year2005    3821362
    ## 9327                     Puerto Rico year2006    3805214
    ## 9328                     Puerto Rico year2007    3782995
    ## 9329                     Puerto Rico year2008    3760866
    ## 9330                     Puerto Rico year2009    3740410
    ## 9331                     Puerto Rico year2010    3721525
    ## 9332                     Puerto Rico year2011    3678732
    ## 9333                     Puerto Rico year2012    3634488
    ## 9334                     Puerto Rico year2013    3593077
    ## 9335                     Puerto Rico year2014    3534874
    ## 9336                     Puerto Rico year2015    3473177
    ## 9337                     Puerto Rico year2016    3406520
    ## 9338                     Puerto Rico year2017    3337177
    ## 9339                           Qatar   year60      47384
    ## 9340                           Qatar   year61      51421
    ## 9341                           Qatar   year62      56263
    ## 9342                           Qatar   year63      61717
    ## 9343                           Qatar   year64      67567
    ## 9344                           Qatar   year65      73633
    ## 9345                           Qatar   year66      79844
    ## 9346                           Qatar   year67      86295
    ## 9347                           Qatar   year68      93201
    ## 9348                           Qatar   year69     100874
    ## 9349                           Qatar   year70     109514
    ## 9350                           Qatar   year71     119424
    ## 9351                           Qatar   year72     130534
    ## 9352                           Qatar   year73     142241
    ## 9353                           Qatar   year74     153704
    ## 9354                           Qatar   year75     164413
    ## 9355                           Qatar   year76     173836
    ## 9356                           Qatar   year77     182443
    ## 9357                           Qatar   year78     192093
    ## 9358                           Qatar   year79     205313
    ## 9359                           Qatar   year80     223775
    ## 9360                           Qatar   year81     248144
    ## 9361                           Qatar   year82     277396
    ## 9362                           Qatar   year83     309479
    ## 9363                           Qatar   year84     341455
    ## 9364                           Qatar   year85     371081
    ## 9365                           Qatar   year86     397932
    ## 9366                           Qatar   year87     422341
    ## 9367                           Qatar   year88     443794
    ## 9368                           Qatar   year89     461870
    ## 9369                           Qatar   year90     476445
    ## 9370                           Qatar   year91     487491
    ## 9371                           Qatar   year92     495517
    ## 9372                           Qatar   year93     501566
    ## 9373                           Qatar   year94     507095
    ## 9374                           Qatar   year95     513455
    ## 9375                           Qatar   year96     522304
    ## 9376                           Qatar   year97     534608
    ## 9377                           Qatar   year98     550430
    ## 9378                           Qatar   year99     569447
    ## 9379                           Qatar year2000     592267
    ## 9380                           Qatar year2001     616886
    ## 9381                           Qatar year2002     645659
    ## 9382                           Qatar year2003     688586
    ## 9383                           Qatar year2004     758855
    ## 9384                           Qatar year2005     864863
    ## 9385                           Qatar year2006    1010382
    ## 9386                           Qatar year2007    1189633
    ## 9387                           Qatar year2008    1389342
    ## 9388                           Qatar year2009    1590780
    ## 9389                           Qatar year2010    1779676
    ## 9390                           Qatar year2011    1952054
    ## 9391                           Qatar year2012    2109568
    ## 9392                           Qatar year2013    2250473
    ## 9393                           Qatar year2014    2374419
    ## 9394                           Qatar year2015    2481539
    ## 9395                           Qatar year2016    2569804
    ## 9396                           Qatar year2017    2639211
    ## 9397                         Romania   year60   18406905
    ## 9398                         Romania   year61   18555250
    ## 9399                         Romania   year62   18676550
    ## 9400                         Romania   year63   18797850
    ## 9401                         Romania   year64   18919126
    ## 9402                         Romania   year65   19031576
    ## 9403                         Romania   year66   19215450
    ## 9404                         Romania   year67   19534242
    ## 9405                         Romania   year68   19799831
    ## 9406                         Romania   year69   20009141
    ## 9407                         Romania   year70   20250398
    ## 9408                         Romania   year71   20461567
    ## 9409                         Romania   year72   20657957
    ## 9410                         Romania   year73   20835681
    ## 9411                         Romania   year74   21029429
    ## 9412                         Romania   year75   21293583
    ## 9413                         Romania   year76   21551634
    ## 9414                         Romania   year77   21756096
    ## 9415                         Romania   year78   21951464
    ## 9416                         Romania   year79   22090488
    ## 9417                         Romania   year80   22242653
    ## 9418                         Romania   year81   22415169
    ## 9419                         Romania   year82   22515389
    ## 9420                         Romania   year83   22588790
    ## 9421                         Romania   year84   22655940
    ## 9422                         Romania   year85   22755427
    ## 9423                         Romania   year86   22859269
    ## 9424                         Romania   year87   22949430
    ## 9425                         Romania   year88   23057662
    ## 9426                         Romania   year89   23161458
    ## 9427                         Romania   year90   23201835
    ## 9428                         Romania   year91   23001155
    ## 9429                         Romania   year92   22794284
    ## 9430                         Romania   year93   22763280
    ## 9431                         Romania   year94   22730211
    ## 9432                         Romania   year95   22684270
    ## 9433                         Romania   year96   22619004
    ## 9434                         Romania   year97   22553978
    ## 9435                         Romania   year98   22507344
    ## 9436                         Romania   year99   22472040
    ## 9437                         Romania year2000   22442971
    ## 9438                         Romania year2001   22131970
    ## 9439                         Romania year2002   21730496
    ## 9440                         Romania year2003   21574326
    ## 9441                         Romania year2004   21451748
    ## 9442                         Romania year2005   21319685
    ## 9443                         Romania year2006   21193760
    ## 9444                         Romania year2007   20882982
    ## 9445                         Romania year2008   20537875
    ## 9446                         Romania year2009   20367487
    ## 9447                         Romania year2010   20246871
    ## 9448                         Romania year2011   20147528
    ## 9449                         Romania year2012   20058035
    ## 9450                         Romania year2013   19983693
    ## 9451                         Romania year2014   19908979
    ## 9452                         Romania year2015   19815481
    ## 9453                         Romania year2016   19702332
    ## 9454                         Romania year2017   19586539
    ## 9455              Russian Federation   year60  119897000
    ## 9456              Russian Federation   year61  121236000
    ## 9457              Russian Federation   year62  122591000
    ## 9458              Russian Federation   year63  123960000
    ## 9459              Russian Federation   year64  125345000
    ## 9460              Russian Federation   year65  126745000
    ## 9461              Russian Federation   year66  127468000
    ## 9462              Russian Federation   year67  128196000
    ## 9463              Russian Federation   year68  128928000
    ## 9464              Russian Federation   year69  129664000
    ## 9465              Russian Federation   year70  130404000
    ## 9466              Russian Federation   year71  131155000
    ## 9467              Russian Federation   year72  131909000
    ## 9468              Russian Federation   year73  132669000
    ## 9469              Russian Federation   year74  133432000
    ## 9470              Russian Federation   year75  134200000
    ## 9471              Russian Federation   year76  135147000
    ## 9472              Russian Federation   year77  136100000
    ## 9473              Russian Federation   year78  137060000
    ## 9474              Russian Federation   year79  138027000
    ## 9475              Russian Federation   year80  139010000
    ## 9476              Russian Federation   year81  139941000
    ## 9477              Russian Federation   year82  140823000
    ## 9478              Russian Federation   year83  141668000
    ## 9479              Russian Federation   year84  142745000
    ## 9480              Russian Federation   year85  143858000
    ## 9481              Russian Federation   year86  144894000
    ## 9482              Russian Federation   year87  145908000
    ## 9483              Russian Federation   year88  146857000
    ## 9484              Russian Federation   year89  147721000
    ## 9485              Russian Federation   year90  148292000
    ## 9486              Russian Federation   year91  148624000
    ## 9487              Russian Federation   year92  148689000
    ## 9488              Russian Federation   year93  148520000
    ## 9489              Russian Federation   year94  148336000
    ## 9490              Russian Federation   year95  148375726
    ## 9491              Russian Federation   year96  148160042
    ## 9492              Russian Federation   year97  147915307
    ## 9493              Russian Federation   year98  147670692
    ## 9494              Russian Federation   year99  147214392
    ## 9495              Russian Federation year2000  146596557
    ## 9496              Russian Federation year2001  145976083
    ## 9497              Russian Federation year2002  145306046
    ## 9498              Russian Federation year2003  144648257
    ## 9499              Russian Federation year2004  144067054
    ## 9500              Russian Federation year2005  143518523
    ## 9501              Russian Federation year2006  143049528
    ## 9502              Russian Federation year2007  142805088
    ## 9503              Russian Federation year2008  142742350
    ## 9504              Russian Federation year2009  142785342
    ## 9505              Russian Federation year2010  142849449
    ## 9506              Russian Federation year2011  142960868
    ## 9507              Russian Federation year2012  143201676
    ## 9508              Russian Federation year2013  143506911
    ## 9509              Russian Federation year2014  143819666
    ## 9510              Russian Federation year2015  144096870
    ## 9511              Russian Federation year2016  144342396
    ## 9512              Russian Federation year2017  144495044
    ## 9513                          Rwanda   year60    2933428
    ## 9514                          Rwanda   year61    2996096
    ## 9515                          Rwanda   year62    3050604
    ## 9516                          Rwanda   year63    3102972
    ## 9517                          Rwanda   year64    3161724
    ## 9518                          Rwanda   year65    3232934
    ## 9519                          Rwanda   year66    3319082
    ## 9520                          Rwanda   year67    3418317
    ## 9521                          Rwanda   year68    3527263
    ## 9522                          Rwanda   year69    3640591
    ## 9523                          Rwanda   year70    3754541
    ## 9524                          Rwanda   year71    3868337
    ## 9525                          Rwanda   year72    3983700
    ## 9526                          Rwanda   year73    4102321
    ## 9527                          Rwanda   year74    4226799
    ## 9528                          Rwanda   year75    4359092
    ## 9529                          Rwanda   year76    4499509
    ## 9530                          Rwanda   year77    4647615
    ## 9531                          Rwanda   year78    4803725
    ## 9532                          Rwanda   year79    4968074
    ## 9533                          Rwanda   year80    5140716
    ## 9534                          Rwanda   year81    5315032
    ## 9535                          Rwanda   year82    5489322
    ## 9536                          Rwanda   year83    5673614
    ## 9537                          Rwanda   year84    5881906
    ## 9538                          Rwanda   year85    6120107
    ## 9539                          Rwanda   year86    6407672
    ## 9540                          Rwanda   year87    6732131
    ## 9541                          Rwanda   year88    7030179
    ## 9542                          Rwanda   year89    7216028
    ## 9543                          Rwanda   year90    7235798
    ## 9544                          Rwanda   year91    7051759
    ## 9545                          Rwanda   year92    6701851
    ## 9546                          Rwanda   year93    6299909
    ## 9547                          Rwanda   year94    6005095
    ## 9548                          Rwanda   year95    5928078
    ## 9549                          Rwanda   year96    6115168
    ## 9550                          Rwanda   year97    6522382
    ## 9551                          Rwanda   year98    7059813
    ## 9552                          Rwanda   year99    7593239
    ## 9553                          Rwanda year2000    8025703
    ## 9554                          Rwanda year2001    8329406
    ## 9555                          Rwanda year2002    8536205
    ## 9556                          Rwanda year2003    8680346
    ## 9557                          Rwanda year2004    8818438
    ## 9558                          Rwanda year2005    8991735
    ## 9559                          Rwanda year2006    9206580
    ## 9560                          Rwanda year2007    9447402
    ## 9561                          Rwanda year2008    9708169
    ## 9562                          Rwanda year2009    9977446
    ## 9563                          Rwanda year2010   10246842
    ## 9564                          Rwanda year2011   10516071
    ## 9565                          Rwanda year2012   10788853
    ## 9566                          Rwanda year2013   11065151
    ## 9567                          Rwanda year2014   11345357
    ## 9568                          Rwanda year2015   11629553
    ## 9569                          Rwanda year2016   11917508
    ## 9570                          Rwanda year2017   12208407
    ## 9571                           Samoa   year60     108646
    ## 9572                           Samoa   year61     112119
    ## 9573                           Samoa   year62     115788
    ## 9574                           Samoa   year63     119561
    ## 9575                           Samoa   year64     123354
    ## 9576                           Samoa   year65     127068
    ## 9577                           Samoa   year66     130688
    ## 9578                           Samoa   year67     134193
    ## 9579                           Samoa   year68     137506
    ## 9580                           Samoa   year69     140518
    ## 9581                           Samoa   year70     143176
    ## 9582                           Samoa   year71     145439
    ## 9583                           Samoa   year72     147321
    ## 9584                           Samoa   year73     148889
    ## 9585                           Samoa   year74     150221
    ## 9586                           Samoa   year75     151387
    ## 9587                           Samoa   year76     152390
    ## 9588                           Samoa   year77     153247
    ## 9589                           Samoa   year78     154007
    ## 9590                           Samoa   year79     154760
    ## 9591                           Samoa   year80     155557
    ## 9592                           Samoa   year81     156428
    ## 9593                           Samoa   year82     157403
    ## 9594                           Samoa   year83     158384
    ## 9595                           Samoa   year84     159283
    ## 9596                           Samoa   year85     160031
    ## 9597                           Samoa   year86     160592
    ## 9598                           Samoa   year87     161015
    ## 9599                           Samoa   year88     161421
    ## 9600                           Samoa   year89     161998
    ## 9601                           Samoa   year90     162866
    ## 9602                           Samoa   year91     164076
    ## 9603                           Samoa   year92     165570
    ## 9604                           Samoa   year93     167207
    ## 9605                           Samoa   year94     168788
    ## 9606                           Samoa   year95     170157
    ## 9607                           Samoa   year96     171283
    ## 9608                           Samoa   year97     172198
    ## 9609                           Samoa   year98     172981
    ## 9610                           Samoa   year99     173755
    ## 9611                           Samoa year2000     174610
    ## 9612                           Samoa year2001     175566
    ## 9613                           Samoa year2002     176582
    ## 9614                           Samoa year2003     177662
    ## 9615                           Samoa year2004     178781
    ## 9616                           Samoa year2005     179929
    ## 9617                           Samoa year2006     181094
    ## 9618                           Samoa year2007     182286
    ## 9619                           Samoa year2008     183526
    ## 9620                           Samoa year2009     184826
    ## 9621                           Samoa year2010     186205
    ## 9622                           Samoa year2011     187665
    ## 9623                           Samoa year2012     189194
    ## 9624                           Samoa year2013     190757
    ## 9625                           Samoa year2014     192290
    ## 9626                           Samoa year2015     193759
    ## 9627                           Samoa year2016     195125
    ## 9628                           Samoa year2017     196440
    ## 9629                      San Marino   year60      15397
    ## 9630                      San Marino   year61      15789
    ## 9631                      San Marino   year62      16199
    ## 9632                      San Marino   year63      16621
    ## 9633                      San Marino   year64      17032
    ## 9634                      San Marino   year65      17441
    ## 9635                      San Marino   year66      17835
    ## 9636                      San Marino   year67      18229
    ## 9637                      San Marino   year68      18589
    ## 9638                      San Marino   year69      18895
    ## 9639                      San Marino   year70      19138
    ## 9640                      San Marino   year71      19303
    ## 9641                      San Marino   year72      19398
    ## 9642                      San Marino   year73      19466
    ## 9643                      San Marino   year74      19562
    ## 9644                      San Marino   year75      19735
    ## 9645                      San Marino   year76      19980
    ## 9646                      San Marino   year77      20296
    ## 9647                      San Marino   year78      20660
    ## 9648                      San Marino   year79      21030
    ## 9649                      San Marino   year80      21361
    ## 9650                      San Marino   year81      21666
    ## 9651                      San Marino   year82      21943
    ## 9652                      San Marino   year83      22210
    ## 9653                      San Marino   year84      22455
    ## 9654                      San Marino   year85      22708
    ## 9655                      San Marino   year86      22961
    ## 9656                      San Marino   year87      23210
    ## 9657                      San Marino   year88      23466
    ## 9658                      San Marino   year89      23740
    ## 9659                      San Marino   year90      24043
    ## 9660                      San Marino   year91      24386
    ## 9661                      San Marino   year92      24749
    ## 9662                      San Marino   year93      25141
    ## 9663                      San Marino   year94      25516
    ## 9664                      San Marino   year95      25877
    ## 9665                      San Marino   year96      26209
    ## 9666                      San Marino   year97      26508
    ## 9667                      San Marino   year98      26799
    ## 9668                      San Marino   year99      27096
    ## 9669                      San Marino year2000      27418
    ## 9670                      San Marino year2001      27762
    ## 9671                      San Marino year2002      28121
    ## 9672                      San Marino year2003      28494
    ## 9673                      San Marino year2004      28866
    ## 9674                      San Marino year2005      29240
    ## 9675                      San Marino year2006      29614
    ## 9676                      San Marino year2007      29977
    ## 9677                      San Marino year2008      30351
    ## 9678                      San Marino year2009      30723
    ## 9679                      San Marino year2010      31110
    ## 9680                      San Marino year2011      31504
    ## 9681                      San Marino year2012      31914
    ## 9682                      San Marino year2013      32303
    ## 9683                      San Marino year2014      32657
    ## 9684                      San Marino year2015      32960
    ## 9685                      San Marino year2016      33203
    ## 9686                      San Marino year2017      33400
    ## 9687           Sao Tome and Principe   year60      64253
    ## 9688           Sao Tome and Principe   year61      64551
    ## 9689           Sao Tome and Principe   year62      64432
    ## 9690           Sao Tome and Principe   year63      64177
    ## 9691           Sao Tome and Principe   year64      64212
    ## 9692           Sao Tome and Principe   year65      64796
    ## 9693           Sao Tome and Principe   year66      66063
    ## 9694           Sao Tome and Principe   year67      67873
    ## 9695           Sao Tome and Principe   year68      70046
    ## 9696           Sao Tome and Principe   year69      72241
    ## 9697           Sao Tome and Principe   year70      74253
    ## 9698           Sao Tome and Principe   year71      75988
    ## 9699           Sao Tome and Principe   year72      77537
    ## 9700           Sao Tome and Principe   year73      79022
    ## 9701           Sao Tome and Principe   year74      80670
    ## 9702           Sao Tome and Principe   year75      82607
    ## 9703           Sao Tome and Principe   year76      84885
    ## 9704           Sao Tome and Principe   year77      87434
    ## 9705           Sao Tome and Principe   year78      90089
    ## 9706           Sao Tome and Principe   year79      92649
    ## 9707           Sao Tome and Principe   year80      94949
    ## 9708           Sao Tome and Principe   year81      96950
    ## 9709           Sao Tome and Principe   year82      98706
    ## 9710           Sao Tome and Principe   year83     100318
    ## 9711           Sao Tome and Principe   year84     101915
    ## 9712           Sao Tome and Principe   year85     103634
    ## 9713           Sao Tome and Principe   year86     105474
    ## 9714           Sao Tome and Principe   year87     107415
    ## 9715           Sao Tome and Principe   year88     109470
    ## 9716           Sao Tome and Principe   year89     111627
    ## 9717           Sao Tome and Principe   year90     113893
    ## 9718           Sao Tome and Principe   year91     116294
    ## 9719           Sao Tome and Principe   year92     118816
    ## 9720           Sao Tome and Principe   year93     121407
    ## 9721           Sao Tome and Principe   year94     123973
    ## 9722           Sao Tome and Principe   year95     126454
    ## 9723           Sao Tome and Principe   year96     128821
    ## 9724           Sao Tome and Principe   year97     131107
    ## 9725           Sao Tome and Principe   year98     133418
    ## 9726           Sao Tome and Principe   year99     135886
    ## 9727           Sao Tome and Principe year2000     138606
    ## 9728           Sao Tome and Principe year2001     141622
    ## 9729           Sao Tome and Principe year2002     144889
    ## 9730           Sao Tome and Principe year2003     148372
    ## 9731           Sao Tome and Principe year2004     151969
    ## 9732           Sao Tome and Principe year2005     155630
    ## 9733           Sao Tome and Principe year2006     159328
    ## 9734           Sao Tome and Principe year2007     163101
    ## 9735           Sao Tome and Principe year2008     166913
    ## 9736           Sao Tome and Principe year2009     170813
    ## 9737           Sao Tome and Principe year2010     174776
    ## 9738           Sao Tome and Principe year2011     178800
    ## 9739           Sao Tome and Principe year2012     182889
    ## 9740           Sao Tome and Principe year2013     187045
    ## 9741           Sao Tome and Principe year2014     191266
    ## 9742           Sao Tome and Principe year2015     195553
    ## 9743           Sao Tome and Principe year2016     199910
    ## 9744           Sao Tome and Principe year2017     204327
    ## 9745                    Saudi Arabia   year60    4086539
    ## 9746                    Saudi Arabia   year61    4218879
    ## 9747                    Saudi Arabia   year62    4362864
    ## 9748                    Saudi Arabia   year63    4516659
    ## 9749                    Saudi Arabia   year64    4677404
    ## 9750                    Saudi Arabia   year65    4843635
    ## 9751                    Saudi Arabia   year66    5015204
    ## 9752                    Saudi Arabia   year67    5194846
    ## 9753                    Saudi Arabia   year68    5387486
    ## 9754                    Saudi Arabia   year69    5599628
    ## 9755                    Saudi Arabia   year70    5836389
    ## 9756                    Saudi Arabia   year71    6100994
    ## 9757                    Saudi Arabia   year72    6393894
    ## 9758                    Saudi Arabia   year73    6714095
    ## 9759                    Saudi Arabia   year74    7059334
    ## 9760                    Saudi Arabia   year75    7428703
    ## 9761                    Saudi Arabia   year76    7818613
    ## 9762                    Saudi Arabia   year77    8231604
    ## 9763                    Saudi Arabia   year78    8679840
    ## 9764                    Saudi Arabia   year79    9179621
    ## 9765                    Saudi Arabia   year80    9740599
    ## 9766                    Saudi Arabia   year81   10366661
    ## 9767                    Saudi Arabia   year82   11048080
    ## 9768                    Saudi Arabia   year83   11763837
    ## 9769                    Saudi Arabia   year84   12484967
    ## 9770                    Saudi Arabia   year85   13189115
    ## 9771                    Saudi Arabia   year86   13869012
    ## 9772                    Saudi Arabia   year87   14525660
    ## 9773                    Saudi Arabia   year88   15155223
    ## 9774                    Saudi Arabia   year89   15755944
    ## 9775                    Saudi Arabia   year90   16326815
    ## 9776                    Saudi Arabia   year91   16867829
    ## 9777                    Saudi Arabia   year92   17378833
    ## 9778                    Saudi Arabia   year93   17859750
    ## 9779                    Saudi Arabia   year94   18311090
    ## 9780                    Saudi Arabia   year95   18735841
    ## 9781                    Saudi Arabia   year96   19131578
    ## 9782                    Saudi Arabia   year97   19505576
    ## 9783                    Saudi Arabia   year98   19882458
    ## 9784                    Saudi Arabia   year99   20294406
    ## 9785                    Saudi Arabia year2000   20764312
    ## 9786                    Saudi Arabia year2001   21303592
    ## 9787                    Saudi Arabia year2002   21906308
    ## 9788                    Saudi Arabia year2003   22556425
    ## 9789                    Saudi Arabia year2004   23228890
    ## 9790                    Saudi Arabia year2005   23905654
    ## 9791                    Saudi Arabia year2006   24578301
    ## 9792                    Saudi Arabia year2007   25252569
    ## 9793                    Saudi Arabia year2008   25940770
    ## 9794                    Saudi Arabia year2009   26661492
    ## 9795                    Saudi Arabia year2010   27425676
    ## 9796                    Saudi Arabia year2011   28238020
    ## 9797                    Saudi Arabia year2012   29086357
    ## 9798                    Saudi Arabia year2013   29944476
    ## 9799                    Saudi Arabia year2014   30776722
    ## 9800                    Saudi Arabia year2015   31557144
    ## 9801                    Saudi Arabia year2016   32275687
    ## 9802                    Saudi Arabia year2017   32938213
    ## 9803                         Senegal   year60    3206749
    ## 9804                         Senegal   year61    3295293
    ## 9805                         Senegal   year62    3386863
    ## 9806                         Senegal   year63    3481745
    ## 9807                         Senegal   year64    3580312
    ## 9808                         Senegal   year65    3682876
    ## 9809                         Senegal   year66    3789211
    ## 9810                         Senegal   year67    3899237
    ## 9811                         Senegal   year68    4013539
    ## 9812                         Senegal   year69    4132844
    ## 9813                         Senegal   year70    4257505
    ## 9814                         Senegal   year71    4388458
    ## 9815                         Senegal   year72    4525114
    ## 9816                         Senegal   year73    4664444
    ## 9817                         Senegal   year74    4802348
    ## 9818                         Senegal   year75    4936209
    ## 9819                         Senegal   year76    5064674
    ## 9820                         Senegal   year77    5189539
    ## 9821                         Senegal   year78    5315265
    ## 9822                         Senegal   year79    5448110
    ## 9823                         Senegal   year80    5592646
    ## 9824                         Senegal   year81    5750338
    ## 9825                         Senegal   year82    5920059
    ## 9826                         Senegal   year83    6100495
    ## 9827                         Senegal   year84    6289327
    ## 9828                         Senegal   year85    6484738
    ## 9829                         Senegal   year86    6686159
    ## 9830                         Senegal   year87    6893896
    ## 9831                         Senegal   year88    7107976
    ## 9832                         Senegal   year89    7328600
    ## 9833                         Senegal   year90    7555617
    ## 9834                         Senegal   year91    7789653
    ## 9835                         Senegal   year92    8029725
    ## 9836                         Senegal   year93    8272170
    ## 9837                         Senegal   year94    8512173
    ## 9838                         Senegal   year95    8746606
    ## 9839                         Senegal   year96    8974077
    ## 9840                         Senegal   year97    9196528
    ## 9841                         Senegal   year98    9418393
    ## 9842                         Senegal   year99    9645957
    ## 9843                         Senegal year2000    9884052
    ## 9844                         Senegal year2001   10134497
    ## 9845                         Senegal year2002   10396861
    ## 9846                         Senegal year2003   10670990
    ## 9847                         Senegal year2004   10955944
    ## 9848                         Senegal year2005   11251266
    ## 9849                         Senegal year2006   11556763
    ## 9850                         Senegal year2007   11873557
    ## 9851                         Senegal year2008   12203957
    ## 9852                         Senegal year2009   12550917
    ## 9853                         Senegal year2010   12916229
    ## 9854                         Senegal year2011   13300910
    ## 9855                         Senegal year2012   13703513
    ## 9856                         Senegal year2013   14120320
    ## 9857                         Senegal year2014   14546111
    ## 9858                         Senegal year2015   14976994
    ## 9859                         Senegal year2016   15411614
    ## 9860                         Senegal year2017   15850567
    ## 9861                          Serbia   year60         NA
    ## 9862                          Serbia   year61         NA
    ## 9863                          Serbia   year62         NA
    ## 9864                          Serbia   year63         NA
    ## 9865                          Serbia   year64         NA
    ## 9866                          Serbia   year65         NA
    ## 9867                          Serbia   year66         NA
    ## 9868                          Serbia   year67         NA
    ## 9869                          Serbia   year68         NA
    ## 9870                          Serbia   year69         NA
    ## 9871                          Serbia   year70         NA
    ## 9872                          Serbia   year71         NA
    ## 9873                          Serbia   year72         NA
    ## 9874                          Serbia   year73         NA
    ## 9875                          Serbia   year74         NA
    ## 9876                          Serbia   year75         NA
    ## 9877                          Serbia   year76         NA
    ## 9878                          Serbia   year77         NA
    ## 9879                          Serbia   year78         NA
    ## 9880                          Serbia   year79         NA
    ## 9881                          Serbia   year80         NA
    ## 9882                          Serbia   year81         NA
    ## 9883                          Serbia   year82         NA
    ## 9884                          Serbia   year83         NA
    ## 9885                          Serbia   year84         NA
    ## 9886                          Serbia   year85         NA
    ## 9887                          Serbia   year86         NA
    ## 9888                          Serbia   year87         NA
    ## 9889                          Serbia   year88         NA
    ## 9890                          Serbia   year89         NA
    ## 9891                          Serbia   year90    7586000
    ## 9892                          Serbia   year91    7595636
    ## 9893                          Serbia   year92    7646424
    ## 9894                          Serbia   year93    7699307
    ## 9895                          Serbia   year94    7734639
    ## 9896                          Serbia   year95    7625357
    ## 9897                          Serbia   year96    7617794
    ## 9898                          Serbia   year97    7596501
    ## 9899                          Serbia   year98    7567745
    ## 9900                          Serbia   year99    7540401
    ## 9901                          Serbia year2000    7516346
    ## 9902                          Serbia year2001    7503433
    ## 9903                          Serbia year2002    7496522
    ## 9904                          Serbia year2003    7480591
    ## 9905                          Serbia year2004    7463157
    ## 9906                          Serbia year2005    7440769
    ## 9907                          Serbia year2006    7411569
    ## 9908                          Serbia year2007    7381579
    ## 9909                          Serbia year2008    7350222
    ## 9910                          Serbia year2009    7320807
    ## 9911                          Serbia year2010    7291436
    ## 9912                          Serbia year2011    7234099
    ## 9913                          Serbia year2012    7199077
    ## 9914                          Serbia year2013    7164132
    ## 9915                          Serbia year2014    7130576
    ## 9916                          Serbia year2015    7095383
    ## 9917                          Serbia year2016    7058322
    ## 9918                          Serbia year2017    7022268
    ## 9919                      Seychelles   year60      41700
    ## 9920                      Seychelles   year61      42889
    ## 9921                      Seychelles   year62      44042
    ## 9922                      Seychelles   year63      45176
    ## 9923                      Seychelles   year64      46322
    ## 9924                      Seychelles   year65      47500
    ## 9925                      Seychelles   year66      48699
    ## 9926                      Seychelles   year67      49911
    ## 9927                      Seychelles   year68      51134
    ## 9928                      Seychelles   year69      52365
    ## 9929                      Seychelles   year70      53600
    ## 9930                      Seychelles   year71      54695
    ## 9931                      Seychelles   year72      56029
    ## 9932                      Seychelles   year73      56892
    ## 9933                      Seychelles   year74      57937
    ## 9934                      Seychelles   year75      59292
    ## 9935                      Seychelles   year76      60504
    ## 9936                      Seychelles   year77      61786
    ## 9937                      Seychelles   year78      62150
    ## 9938                      Seychelles   year79      62686
    ## 9939                      Seychelles   year80      63261
    ## 9940                      Seychelles   year81      64035
    ## 9941                      Seychelles   year82      64413
    ## 9942                      Seychelles   year83      64335
    ## 9943                      Seychelles   year84      64717
    ## 9944                      Seychelles   year85      65244
    ## 9945                      Seychelles   year86      65652
    ## 9946                      Seychelles   year87      68499
    ## 9947                      Seychelles   year88      68755
    ## 9948                      Seychelles   year89      69167
    ## 9949                      Seychelles   year90      69507
    ## 9950                      Seychelles   year91      70439
    ## 9951                      Seychelles   year92      70763
    ## 9952                      Seychelles   year93      72253
    ## 9953                      Seychelles   year94      74205
    ## 9954                      Seychelles   year95      75304
    ## 9955                      Seychelles   year96      76417
    ## 9956                      Seychelles   year97      77319
    ## 9957                      Seychelles   year98      78846
    ## 9958                      Seychelles   year99      80410
    ## 9959                      Seychelles year2000      81131
    ## 9960                      Seychelles year2001      81202
    ## 9961                      Seychelles year2002      83723
    ## 9962                      Seychelles year2003      82781
    ## 9963                      Seychelles year2004      82475
    ## 9964                      Seychelles year2005      82858
    ## 9965                      Seychelles year2006      84600
    ## 9966                      Seychelles year2007      85033
    ## 9967                      Seychelles year2008      86956
    ## 9968                      Seychelles year2009      87298
    ## 9969                      Seychelles year2010      89770
    ## 9970                      Seychelles year2011      87441
    ## 9971                      Seychelles year2012      88303
    ## 9972                      Seychelles year2013      89949
    ## 9973                      Seychelles year2014      91359
    ## 9974                      Seychelles year2015      93419
    ## 9975                      Seychelles year2016      94677
    ## 9976                      Seychelles year2017      95843
    ## 9977                    Sierra Leone   year60    2297110
    ## 9978                    Sierra Leone   year61    2329204
    ## 9979                    Sierra Leone   year62    2363013
    ## 9980                    Sierra Leone   year63    2398414
    ## 9981                    Sierra Leone   year64    2435204
    ## 9982                    Sierra Leone   year65    2473294
    ## 9983                    Sierra Leone   year66    2512652
    ## 9984                    Sierra Leone   year67    2553529
    ## 9985                    Sierra Leone   year68    2596568
    ## 9986                    Sierra Leone   year69    2642608
    ## 9987                    Sierra Leone   year70    2692259
    ## 9988                    Sierra Leone   year71    2745779
    ## 9989                    Sierra Leone   year72    2803031
    ## 9990                    Sierra Leone   year73    2863739
    ## 9991                    Sierra Leone   year74    2927468
    ## 9992                    Sierra Leone   year75    2993876
    ## 9993                    Sierra Leone   year76    3062956
    ## 9994                    Sierra Leone   year77    3134800
    ## 9995                    Sierra Leone   year78    3209263
    ## 9996                    Sierra Leone   year79    3286179
    ## 9997                    Sierra Leone   year80    3365441
    ## 9998                    Sierra Leone   year81    3445277
    ## 9999                    Sierra Leone   year82    3525399
    ## 10000                   Sierra Leone   year83    3608751
    ## 10001                   Sierra Leone   year84    3699467
    ## 10002                   Sierra Leone   year85    3799550
    ## 10003                   Sierra Leone   year86    3912438
    ## 10004                   Sierra Leone   year87    4034668
    ## 10005                   Sierra Leone   year88    4152984
    ## 10006                   Sierra Leone   year89    4249468
    ## 10007                   Sierra Leone   year90    4312246
    ## 10008                   Sierra Leone   year91    4337239
    ## 10009                   Sierra Leone   year92    4331332
    ## 10010                   Sierra Leone   year93    4307299
    ## 10011                   Sierra Leone   year94    4283621
    ## 10012                   Sierra Leone   year95    4274819
    ## 10013                   Sierra Leone   year96    4282350
    ## 10014                   Sierra Leone   year97    4305455
    ## 10015                   Sierra Leone   year98    4353646
    ## 10016                   Sierra Leone   year99    4437803
    ## 10017                   Sierra Leone year2000    4564297
    ## 10018                   Sierra Leone year2001    4739147
    ## 10019                   Sierra Leone year2002    4957216
    ## 10020                   Sierra Leone year2003    5199549
    ## 10021                   Sierra Leone year2004    5439695
    ## 10022                   Sierra Leone year2005    5658379
    ## 10023                   Sierra Leone year2006    5848692
    ## 10024                   Sierra Leone year2007    6015417
    ## 10025                   Sierra Leone year2008    6165372
    ## 10026                   Sierra Leone year2009    6310260
    ## 10027                   Sierra Leone year2010    6458720
    ## 10028                   Sierra Leone year2011    6611692
    ## 10029                   Sierra Leone year2012    6766103
    ## 10030                   Sierra Leone year2013    6922079
    ## 10031                   Sierra Leone year2014    7079162
    ## 10032                   Sierra Leone year2015    7237025
    ## 10033                   Sierra Leone year2016    7396190
    ## 10034                   Sierra Leone year2017    7557212
    ## 10035                      Singapore   year60    1646400
    ## 10036                      Singapore   year61    1702400
    ## 10037                      Singapore   year62    1750200
    ## 10038                      Singapore   year63    1795000
    ## 10039                      Singapore   year64    1841600
    ## 10040                      Singapore   year65    1886900
    ## 10041                      Singapore   year66    1934400
    ## 10042                      Singapore   year67    1977600
    ## 10043                      Singapore   year68    2012000
    ## 10044                      Singapore   year69    2042500
    ## 10045                      Singapore   year70    2074500
    ## 10046                      Singapore   year71    2112900
    ## 10047                      Singapore   year72    2152400
    ## 10048                      Singapore   year73    2193000
    ## 10049                      Singapore   year74    2229800
    ## 10050                      Singapore   year75    2262600
    ## 10051                      Singapore   year76    2293300
    ## 10052                      Singapore   year77    2325300
    ## 10053                      Singapore   year78    2353600
    ## 10054                      Singapore   year79    2383500
    ## 10055                      Singapore   year80    2413945
    ## 10056                      Singapore   year81    2532835
    ## 10057                      Singapore   year82    2646466
    ## 10058                      Singapore   year83    2681061
    ## 10059                      Singapore   year84    2732221
    ## 10060                      Singapore   year85    2735957
    ## 10061                      Singapore   year86    2733373
    ## 10062                      Singapore   year87    2774789
    ## 10063                      Singapore   year88    2846108
    ## 10064                      Singapore   year89    2930901
    ## 10065                      Singapore   year90    3047132
    ## 10066                      Singapore   year91    3135083
    ## 10067                      Singapore   year92    3230698
    ## 10068                      Singapore   year93    3313471
    ## 10069                      Singapore   year94    3419048
    ## 10070                      Singapore   year95    3524506
    ## 10071                      Singapore   year96    3670704
    ## 10072                      Singapore   year97    3796038
    ## 10073                      Singapore   year98    3927213
    ## 10074                      Singapore   year99    3958723
    ## 10075                      Singapore year2000    4027887
    ## 10076                      Singapore year2001    4138012
    ## 10077                      Singapore year2002    4175950
    ## 10078                      Singapore year2003    4114826
    ## 10079                      Singapore year2004    4166664
    ## 10080                      Singapore year2005    4265762
    ## 10081                      Singapore year2006    4401365
    ## 10082                      Singapore year2007    4588599
    ## 10083                      Singapore year2008    4839396
    ## 10084                      Singapore year2009    4987573
    ## 10085                      Singapore year2010    5076732
    ## 10086                      Singapore year2011    5183688
    ## 10087                      Singapore year2012    5312437
    ## 10088                      Singapore year2013    5399162
    ## 10089                      Singapore year2014    5469724
    ## 10090                      Singapore year2015    5535002
    ## 10091                      Singapore year2016    5607283
    ## 10092                      Singapore year2017    5612253
    ## 10093      Sint Maarten (Dutch part)   year60         NA
    ## 10094      Sint Maarten (Dutch part)   year61         NA
    ## 10095      Sint Maarten (Dutch part)   year62         NA
    ## 10096      Sint Maarten (Dutch part)   year63         NA
    ## 10097      Sint Maarten (Dutch part)   year64         NA
    ## 10098      Sint Maarten (Dutch part)   year65         NA
    ## 10099      Sint Maarten (Dutch part)   year66         NA
    ## 10100      Sint Maarten (Dutch part)   year67         NA
    ## 10101      Sint Maarten (Dutch part)   year68         NA
    ## 10102      Sint Maarten (Dutch part)   year69         NA
    ## 10103      Sint Maarten (Dutch part)   year70         NA
    ## 10104      Sint Maarten (Dutch part)   year71         NA
    ## 10105      Sint Maarten (Dutch part)   year72         NA
    ## 10106      Sint Maarten (Dutch part)   year73         NA
    ## 10107      Sint Maarten (Dutch part)   year74         NA
    ## 10108      Sint Maarten (Dutch part)   year75         NA
    ## 10109      Sint Maarten (Dutch part)   year76         NA
    ## 10110      Sint Maarten (Dutch part)   year77         NA
    ## 10111      Sint Maarten (Dutch part)   year78         NA
    ## 10112      Sint Maarten (Dutch part)   year79         NA
    ## 10113      Sint Maarten (Dutch part)   year80         NA
    ## 10114      Sint Maarten (Dutch part)   year81         NA
    ## 10115      Sint Maarten (Dutch part)   year82         NA
    ## 10116      Sint Maarten (Dutch part)   year83         NA
    ## 10117      Sint Maarten (Dutch part)   year84         NA
    ## 10118      Sint Maarten (Dutch part)   year85         NA
    ## 10119      Sint Maarten (Dutch part)   year86         NA
    ## 10120      Sint Maarten (Dutch part)   year87         NA
    ## 10121      Sint Maarten (Dutch part)   year88         NA
    ## 10122      Sint Maarten (Dutch part)   year89         NA
    ## 10123      Sint Maarten (Dutch part)   year90         NA
    ## 10124      Sint Maarten (Dutch part)   year91         NA
    ## 10125      Sint Maarten (Dutch part)   year92         NA
    ## 10126      Sint Maarten (Dutch part)   year93         NA
    ## 10127      Sint Maarten (Dutch part)   year94         NA
    ## 10128      Sint Maarten (Dutch part)   year95         NA
    ## 10129      Sint Maarten (Dutch part)   year96         NA
    ## 10130      Sint Maarten (Dutch part)   year97         NA
    ## 10131      Sint Maarten (Dutch part)   year98      31240
    ## 10132      Sint Maarten (Dutch part)   year99      31084
    ## 10133      Sint Maarten (Dutch part) year2000      30519
    ## 10134      Sint Maarten (Dutch part) year2001      30600
    ## 10135      Sint Maarten (Dutch part) year2002      30777
    ## 10136      Sint Maarten (Dutch part) year2003      31472
    ## 10137      Sint Maarten (Dutch part) year2004      32488
    ## 10138      Sint Maarten (Dutch part) year2005      33011
    ## 10139      Sint Maarten (Dutch part) year2006      33441
    ## 10140      Sint Maarten (Dutch part) year2007      33811
    ## 10141      Sint Maarten (Dutch part) year2008      33964
    ## 10142      Sint Maarten (Dutch part) year2009      34238
    ## 10143      Sint Maarten (Dutch part) year2010      34056
    ## 10144      Sint Maarten (Dutch part) year2011      33435
    ## 10145      Sint Maarten (Dutch part) year2012      34640
    ## 10146      Sint Maarten (Dutch part) year2013      36607
    ## 10147      Sint Maarten (Dutch part) year2014      37685
    ## 10148      Sint Maarten (Dutch part) year2015      38824
    ## 10149      Sint Maarten (Dutch part) year2016      39969
    ## 10150      Sint Maarten (Dutch part) year2017      41109
    ## 10151                Slovak Republic   year60    4068095
    ## 10152                Slovak Republic   year61    4191667
    ## 10153                Slovak Republic   year62    4238188
    ## 10154                Slovak Republic   year63    4282017
    ## 10155                Slovak Republic   year64    4327341
    ## 10156                Slovak Republic   year65    4370983
    ## 10157                Slovak Republic   year66    4411666
    ## 10158                Slovak Republic   year67    4449367
    ## 10159                Slovak Republic   year68    4483915
    ## 10160                Slovak Republic   year69    4518607
    ## 10161                Slovak Republic   year70    4538223
    ## 10162                Slovak Republic   year71    4557449
    ## 10163                Slovak Republic   year72    4596622
    ## 10164                Slovak Republic   year73    4641445
    ## 10165                Slovak Republic   year74    4689623
    ## 10166                Slovak Republic   year75    4739105
    ## 10167                Slovak Republic   year76    4789507
    ## 10168                Slovak Republic   year77    4840501
    ## 10169                Slovak Republic   year78    4890125
    ## 10170                Slovak Republic   year79    4938973
    ## 10171                Slovak Republic   year80    4979815
    ## 10172                Slovak Republic   year81    5016105
    ## 10173                Slovak Republic   year82    5055099
    ## 10174                Slovak Republic   year83    5091971
    ## 10175                Slovak Republic   year84    5127097
    ## 10176                Slovak Republic   year85    5161768
    ## 10177                Slovak Republic   year86    5193838
    ## 10178                Slovak Republic   year87    5222840
    ## 10179                Slovak Republic   year88    5250596
    ## 10180                Slovak Republic   year89    5275942
    ## 10181                Slovak Republic   year90    5299187
    ## 10182                Slovak Republic   year91    5303294
    ## 10183                Slovak Republic   year92    5305016
    ## 10184                Slovak Republic   year93    5325305
    ## 10185                Slovak Republic   year94    5346331
    ## 10186                Slovak Republic   year95    5361999
    ## 10187                Slovak Republic   year96    5373361
    ## 10188                Slovak Republic   year97    5383291
    ## 10189                Slovak Republic   year98    5390516
    ## 10190                Slovak Republic   year99    5396020
    ## 10191                Slovak Republic year2000    5388720
    ## 10192                Slovak Republic year2001    5378867
    ## 10193                Slovak Republic year2002    5376912
    ## 10194                Slovak Republic year2003    5373374
    ## 10195                Slovak Republic year2004    5372280
    ## 10196                Slovak Republic year2005    5372807
    ## 10197                Slovak Republic year2006    5373054
    ## 10198                Slovak Republic year2007    5374622
    ## 10199                Slovak Republic year2008    5379233
    ## 10200                Slovak Republic year2009    5386406
    ## 10201                Slovak Republic year2010    5391428
    ## 10202                Slovak Republic year2011    5398384
    ## 10203                Slovak Republic year2012    5407579
    ## 10204                Slovak Republic year2013    5413393
    ## 10205                Slovak Republic year2014    5418649
    ## 10206                Slovak Republic year2015    5423801
    ## 10207                Slovak Republic year2016    5430798
    ## 10208                Slovak Republic year2017    5439892
    ## 10209                       Slovenia   year60    1584720
    ## 10210                       Slovenia   year61    1594131
    ## 10211                       Slovenia   year62    1603649
    ## 10212                       Slovenia   year63    1616971
    ## 10213                       Slovenia   year64    1632114
    ## 10214                       Slovenia   year65    1649160
    ## 10215                       Slovenia   year66    1669905
    ## 10216                       Slovenia   year67    1689528
    ## 10217                       Slovenia   year68    1704546
    ## 10218                       Slovenia   year69    1713874
    ## 10219                       Slovenia   year70    1724891
    ## 10220                       Slovenia   year71    1738335
    ## 10221                       Slovenia   year72    1752233
    ## 10222                       Slovenia   year73    1766697
    ## 10223                       Slovenia   year74    1776132
    ## 10224                       Slovenia   year75    1793581
    ## 10225                       Slovenia   year76    1820249
    ## 10226                       Slovenia   year77    1842377
    ## 10227                       Slovenia   year78    1862548
    ## 10228                       Slovenia   year79    1882599
    ## 10229                       Slovenia   year80    1901315
    ## 10230                       Slovenia   year81    1906531
    ## 10231                       Slovenia   year82    1910334
    ## 10232                       Slovenia   year83    1922321
    ## 10233                       Slovenia   year84    1932154
    ## 10234                       Slovenia   year85    1941641
    ## 10235                       Slovenia   year86    1965964
    ## 10236                       Slovenia   year87    1989776
    ## 10237                       Slovenia   year88    1995196
    ## 10238                       Slovenia   year89    1996351
    ## 10239                       Slovenia   year90    1998161
    ## 10240                       Slovenia   year91    1999429
    ## 10241                       Slovenia   year92    1996498
    ## 10242                       Slovenia   year93    1991746
    ## 10243                       Slovenia   year94    1989443
    ## 10244                       Slovenia   year95    1989872
    ## 10245                       Slovenia   year96    1988628
    ## 10246                       Slovenia   year97    1985956
    ## 10247                       Slovenia   year98    1981629
    ## 10248                       Slovenia   year99    1983045
    ## 10249                       Slovenia year2000    1988925
    ## 10250                       Slovenia year2001    1992060
    ## 10251                       Slovenia year2002    1994530
    ## 10252                       Slovenia year2003    1995733
    ## 10253                       Slovenia year2004    1997012
    ## 10254                       Slovenia year2005    2000474
    ## 10255                       Slovenia year2006    2006868
    ## 10256                       Slovenia year2007    2018122
    ## 10257                       Slovenia year2008    2021316
    ## 10258                       Slovenia year2009    2039669
    ## 10259                       Slovenia year2010    2048583
    ## 10260                       Slovenia year2011    2052843
    ## 10261                       Slovenia year2012    2057159
    ## 10262                       Slovenia year2013    2059953
    ## 10263                       Slovenia year2014    2061980
    ## 10264                       Slovenia year2015    2063531
    ## 10265                       Slovenia year2016    2065042
    ## 10266                       Slovenia year2017    2066748
    ## 10267                   Small states   year60   14260440
    ## 10268                   Small states   year61   14538217
    ## 10269                   Small states   year62   14821270
    ## 10270                   Small states   year63   15107653
    ## 10271                   Small states   year64   15397706
    ## 10272                   Small states   year65   15688248
    ## 10273                   Small states   year66   15976545
    ## 10274                   Small states   year67   16263697
    ## 10275                   Small states   year68   16556001
    ## 10276                   Small states   year69   16853980
    ## 10277                   Small states   year70   17159590
    ## 10278                   Small states   year71   17476361
    ## 10279                   Small states   year72   17800127
    ## 10280                   Small states   year73   18127044
    ## 10281                   Small states   year74   18456078
    ## 10282                   Small states   year75   18789042
    ## 10283                   Small states   year76   19120333
    ## 10284                   Small states   year77   19453890
    ## 10285                   Small states   year78   19794779
    ## 10286                   Small states   year79   20157728
    ## 10287                   Small states   year80   20547258
    ## 10288                   Small states   year81   20963859
    ## 10289                   Small states   year82   21407906
    ## 10290                   Small states   year83   21865552
    ## 10291                   Small states   year84   22331260
    ## 10292                   Small states   year85   22806174
    ## 10293                   Small states   year86   23285884
    ## 10294                   Small states   year87   23770486
    ## 10295                   Small states   year88   24251665
    ## 10296                   Small states   year89   24726226
    ## 10297                   Small states   year90   25186921
    ## 10298                   Small states   year91   25642376
    ## 10299                   Small states   year92   26066187
    ## 10300                   Small states   year93   26471464
    ## 10301                   Small states   year94   26880995
    ## 10302                   Small states   year95   27284098
    ## 10303                   Small states   year96   27691618
    ## 10304                   Small states   year97   28107601
    ## 10305                   Small states   year98   28528151
    ## 10306                   Small states   year99   28976065
    ## 10307                   Small states year2000   29434279
    ## 10308                   Small states year2001   29887049
    ## 10309                   Small states year2002   30352305
    ## 10310                   Small states year2003   30838828
    ## 10311                   Small states year2004   31365918
    ## 10312                   Small states year2005   31947386
    ## 10313                   Small states year2006   32586895
    ## 10314                   Small states year2007   33278247
    ## 10315                   Small states year2008   34006389
    ## 10316                   Small states year2009   34739790
    ## 10317                   Small states year2010   35465245
    ## 10318                   Small states year2011   36171934
    ## 10319                   Small states year2012   36875022
    ## 10320                   Small states year2013   37572012
    ## 10321                   Small states year2014   38266156
    ## 10322                   Small states year2015   38960406
    ## 10323                   Small states year2016   39646847
    ## 10324                   Small states year2017   40324496
    ## 10325                Solomon Islands   year60     117866
    ## 10326                Solomon Islands   year61     121396
    ## 10327                Solomon Islands   year62     125064
    ## 10328                Solomon Islands   year63     128866
    ## 10329                Solomon Islands   year64     132782
    ## 10330                Solomon Islands   year65     136847
    ## 10331                Solomon Islands   year66     141026
    ## 10332                Solomon Islands   year67     145351
    ## 10333                Solomon Islands   year68     149921
    ## 10334                Solomon Islands   year69     154875
    ## 10335                Solomon Islands   year70     160290
    ## 10336                Solomon Islands   year71     166212
    ## 10337                Solomon Islands   year72     172598
    ## 10338                Solomon Islands   year73     179349
    ## 10339                Solomon Islands   year74     186332
    ## 10340                Solomon Islands   year75     193445
    ## 10341                Solomon Islands   year76     200640
    ## 10342                Solomon Islands   year77     207937
    ## 10343                Solomon Islands   year78     215347
    ## 10344                Solomon Islands   year79     222897
    ## 10345                Solomon Islands   year80     230607
    ## 10346                Solomon Islands   year81     238479
    ## 10347                Solomon Islands   year82     246493
    ## 10348                Solomon Islands   year83     254596
    ## 10349                Solomon Islands   year84     262709
    ## 10350                Solomon Islands   year85     270801
    ## 10351                Solomon Islands   year86     278838
    ## 10352                Solomon Islands   year87     286863
    ## 10353                Solomon Islands   year88     294964
    ## 10354                Solomon Islands   year89     303253
    ## 10355                Solomon Islands   year90     311840
    ## 10356                Solomon Islands   year91     320753
    ## 10357                Solomon Islands   year92     329953
    ## 10358                Solomon Islands   year93     339456
    ## 10359                Solomon Islands   year94     349225
    ## 10360                Solomon Islands   year95     359225
    ## 10361                Solomon Islands   year96     369469
    ## 10362                Solomon Islands   year97     379947
    ## 10363                Solomon Islands   year98     390643
    ## 10364                Solomon Islands   year99     401538
    ## 10365                Solomon Islands year2000     412609
    ## 10366                Solomon Islands year2001     423853
    ## 10367                Solomon Islands year2002     435262
    ## 10368                Solomon Islands year2003     446769
    ## 10369                Solomon Islands year2004     458324
    ## 10370                Solomon Islands year2005     469885
    ## 10371                Solomon Islands year2006     481422
    ## 10372                Solomon Islands year2007     492940
    ## 10373                Solomon Islands year2008     504477
    ## 10374                Solomon Islands year2009     516079
    ## 10375                Solomon Islands year2010     527790
    ## 10376                Solomon Islands year2011     539614
    ## 10377                Solomon Islands year2012     551531
    ## 10378                Solomon Islands year2013     563513
    ## 10379                Solomon Islands year2014     575504
    ## 10380                Solomon Islands year2015     587482
    ## 10381                Solomon Islands year2016     599419
    ## 10382                Solomon Islands year2017     611343
    ## 10383                        Somalia   year60    2755947
    ## 10384                        Somalia   year61    2814096
    ## 10385                        Somalia   year62    2874190
    ## 10386                        Somalia   year63    2936443
    ## 10387                        Somalia   year64    3001126
    ## 10388                        Somalia   year65    3068437
    ## 10389                        Somalia   year66    3143836
    ## 10390                        Somalia   year67    3228495
    ## 10391                        Somalia   year68    3313786
    ## 10392                        Somalia   year69    3387632
    ## 10393                        Somalia   year70    3444553
    ## 10394                        Somalia   year71    3470324
    ## 10395                        Somalia   year72    3475022
    ## 10396                        Somalia   year73    3506008
    ## 10397                        Somalia   year74    3627504
    ## 10398                        Somalia   year75    3880320
    ## 10399                        Somalia   year76    4289469
    ## 10400                        Somalia   year77    4827362
    ## 10401                        Somalia   year78    5417740
    ## 10402                        Somalia   year79    5953615
    ## 10403                        Somalia   year80    6359126
    ## 10404                        Somalia   year81    6604872
    ## 10405                        Somalia   year82    6716448
    ## 10406                        Somalia   year83    6740220
    ## 10407                        Somalia   year84    6747932
    ## 10408                        Somalia   year85    6791716
    ## 10409                        Somalia   year86    6887372
    ## 10410                        Somalia   year87    7018109
    ## 10411                        Somalia   year88    7165295
    ## 10412                        Somalia   year89    7298417
    ## 10413                        Somalia   year90    7397347
    ## 10414                        Somalia   year91    7455936
    ## 10415                        Somalia   year92    7488544
    ## 10416                        Somalia   year93    7519811
    ## 10417                        Somalia   year94    7583954
    ## 10418                        Somalia   year95    7704894
    ## 10419                        Somalia   year96    7892389
    ## 10420                        Somalia   year97    8137475
    ## 10421                        Somalia   year98    8422372
    ## 10422                        Somalia   year99    8720231
    ## 10423                        Somalia year2000    9011479
    ## 10424                        Somalia year2001    9290823
    ## 10425                        Somalia year2002    9564167
    ## 10426                        Somalia year2003    9836397
    ## 10427                        Somalia year2004   10116228
    ## 10428                        Somalia year2005   10409925
    ## 10429                        Somalia year2006   10718317
    ## 10430                        Somalia year2007   11038596
    ## 10431                        Somalia year2008   11369276
    ## 10432                        Somalia year2009   11707990
    ## 10433                        Somalia year2010   12053223
    ## 10434                        Somalia year2011   12404725
    ## 10435                        Somalia year2012   12763776
    ## 10436                        Somalia year2013   13132349
    ## 10437                        Somalia year2014   13513125
    ## 10438                        Somalia year2015   13908129
    ## 10439                        Somalia year2016   14317996
    ## 10440                        Somalia year2017   14742523
    ## 10441                   South Africa   year60   17456855
    ## 10442                   South Africa   year61   17920673
    ## 10443                   South Africa   year62   18401608
    ## 10444                   South Africa   year63   18899275
    ## 10445                   South Africa   year64   19412975
    ## 10446                   South Africa   year65   19942303
    ## 10447                   South Africa   year66   20486439
    ## 10448                   South Africa   year67   21045785
    ## 10449                   South Africa   year68   21622590
    ## 10450                   South Africa   year69   22219897
    ## 10451                   South Africa   year70   22839451
    ## 10452                   South Africa   year71   23482813
    ## 10453                   South Africa   year72   24148137
    ## 10454                   South Africa   year73   24829693
    ## 10455                   South Africa   year74   25519604
    ## 10456                   South Africa   year75   26212405
    ## 10457                   South Africa   year76   26904349
    ## 10458                   South Africa   year77   27597297
    ## 10459                   South Africa   year78   28298150
    ## 10460                   South Africa   year79   29017049
    ## 10461                   South Africa   year80   29760471
    ## 10462                   South Africa   year81   30532954
    ## 10463                   South Africa   year82   31330259
    ## 10464                   South Africa   year83   32139708
    ## 10465                   South Africa   year84   32943584
    ## 10466                   South Africa   year85   33730148
    ## 10467                   South Africa   year86   34490419
    ## 10468                   South Africa   year87   35230249
    ## 10469                   South Africa   year88   35970537
    ## 10470                   South Africa   year89   36740883
    ## 10471                   South Africa   year90   37560525
    ## 10472                   South Africa   year91   38437855
    ## 10473                   South Africa   year92   39360225
    ## 10474                   South Africa   year93   40300161
    ## 10475                   South Africa   year94   41218901
    ## 10476                   South Africa   year95   42088165
    ## 10477                   South Africa   year96   42898520
    ## 10478                   South Africa   year97   43657024
    ## 10479                   South Africa   year98   44372112
    ## 10480                   South Africa   year99   45058775
    ## 10481                   South Africa year2000   45728315
    ## 10482                   South Africa year2001   46385006
    ## 10483                   South Africa year2002   47026173
    ## 10484                   South Africa year2003   47648727
    ## 10485                   South Africa year2004   48247395
    ## 10486                   South Africa year2005   48820586
    ## 10487                   South Africa year2006   49364582
    ## 10488                   South Africa year2007   49887181
    ## 10489                   South Africa year2008   50412129
    ## 10490                   South Africa year2009   50970818
    ## 10491                   South Africa year2010   51584663
    ## 10492                   South Africa year2011   52263516
    ## 10493                   South Africa year2012   52998213
    ## 10494                   South Africa year2013   53767396
    ## 10495                   South Africa year2014   54539571
    ## 10496                   South Africa year2015   55291225
    ## 10497                   South Africa year2016   56015473
    ## 10498                   South Africa year2017   56717156
    ## 10499                     South Asia   year60  571835666
    ## 10500                     South Asia   year61  583894094
    ## 10501                     South Asia   year62  596413939
    ## 10502                     South Asia   year63  609391805
    ## 10503                     South Asia   year64  622822615
    ## 10504                     South Asia   year65  636701820
    ## 10505                     South Asia   year66  651036352
    ## 10506                     South Asia   year67  665826653
    ## 10507                     South Asia   year68  681054882
    ## 10508                     South Asia   year69  696697198
    ## 10509                     South Asia   year70  712740919
    ## 10510                     South Asia   year71  729173562
    ## 10511                     South Asia   year72  746012374
    ## 10512                     South Asia   year73  763310561
    ## 10513                     South Asia   year74  781140577
    ## 10514                     South Asia   year75  799553306
    ## 10515                     South Asia   year76  818560436
    ## 10516                     South Asia   year77  838142287
    ## 10517                     South Asia   year78  858277856
    ## 10518                     South Asia   year79  878933031
    ## 10519                     South Asia   year80  900076467
    ## 10520                     South Asia   year81  921696915
    ## 10521                     South Asia   year82  943781613
    ## 10522                     South Asia   year83  966293643
    ## 10523                     South Asia   year84  989188965
    ## 10524                     South Asia   year85 1012429641
    ## 10525                     South Asia   year86 1035982524
    ## 10526                     South Asia   year87 1059829211
    ## 10527                     South Asia   year88 1083963380
    ## 10528                     South Asia   year89 1108386444
    ## 10529                     South Asia   year90 1133089464
    ## 10530                     South Asia   year91 1158058109
    ## 10531                     South Asia   year92 1183253534
    ## 10532                     South Asia   year93 1208612942
    ## 10533                     South Asia   year94 1234059205
    ## 10534                     South Asia   year95 1259530819
    ## 10535                     South Asia   year96 1284978193
    ## 10536                     South Asia   year97 1310387887
    ## 10537                     South Asia   year98 1335777637
    ## 10538                     South Asia   year99 1361185289
    ## 10539                     South Asia year2000 1386625845
    ## 10540                     South Asia year2001 1412104373
    ## 10541                     South Asia year2002 1437568227
    ## 10542                     South Asia year2003 1462906674
    ## 10543                     South Asia year2004 1487975237
    ## 10544                     South Asia year2005 1512670560
    ## 10545                     South Asia year2006 1536943534
    ## 10546                     South Asia year2007 1560818860
    ## 10547                     South Asia year2008 1584359049
    ## 10548                     South Asia year2009 1607663899
    ## 10549                     South Asia year2010 1630806784
    ## 10550                     South Asia year2011 1653798614
    ## 10551                     South Asia year2012 1676615491
    ## 10552                     South Asia year2013 1699310450
    ## 10553                     South Asia year2014 1721847786
    ## 10554                     South Asia year2015 1744199944
    ## 10555                     South Asia year2016 1766393714
    ## 10556                     South Asia year2017 1788388852
    ## 10557                    South Sudan   year60    2955152
    ## 10558                    South Sudan   year61    3011110
    ## 10559                    South Sudan   year62    3069913
    ## 10560                    South Sudan   year63    3131557
    ## 10561                    South Sudan   year64    3196113
    ## 10562                    South Sudan   year65    3263638
    ## 10563                    South Sudan   year66    3334191
    ## 10564                    South Sudan   year67    3407800
    ## 10565                    South Sudan   year68    3484537
    ## 10566                    South Sudan   year69    3564465
    ## 10567                    South Sudan   year70    3647709
    ## 10568                    South Sudan   year71    3734418
    ## 10569                    South Sudan   year72    3824762
    ## 10570                    South Sudan   year73    3918922
    ## 10571                    South Sudan   year74    4017075
    ## 10572                    South Sudan   year75    4119438
    ## 10573                    South Sudan   year76    4224529
    ## 10574                    South Sudan   year77    4332287
    ## 10575                    South Sudan   year78    4445826
    ## 10576                    South Sudan   year79    4569423
    ## 10577                    South Sudan   year80    4705224
    ## 10578                    South Sudan   year81    4853927
    ## 10579                    South Sudan   year82    5011726
    ## 10580                    South Sudan   year83    5170558
    ## 10581                    South Sudan   year84    5319609
    ## 10582                    South Sudan   year85    5450424
    ## 10583                    South Sudan   year86    5565545
    ## 10584                    South Sudan   year87    5666078
    ## 10585                    South Sudan   year88    5741235
    ## 10586                    South Sudan   year89    5777498
    ## 10587                    South Sudan   year90    5768481
    ## 10588                    South Sudan   year91    5705378
    ## 10589                    South Sudan   year92    5599814
    ## 10590                    South Sudan   year93    5490915
    ## 10591                    South Sudan   year94    5431738
    ## 10592                    South Sudan   year95    5459519
    ## 10593                    South Sudan   year96    5591114
    ## 10594                    South Sudan   year97    5814006
    ## 10595                    South Sudan   year98    6099923
    ## 10596                    South Sudan   year99    6405864
    ## 10597                    South Sudan year2000    6700656
    ## 10598                    South Sudan year2001    6974442
    ## 10599                    South Sudan year2002    7237276
    ## 10600                    South Sudan year2003    7501642
    ## 10601                    South Sudan year2004    7787655
    ## 10602                    South Sudan year2005    8108877
    ## 10603                    South Sudan year2006    8468152
    ## 10604                    South Sudan year2007    8856800
    ## 10605                    South Sudan year2008    9263136
    ## 10606                    South Sudan year2009    9670667
    ## 10607                    South Sudan year2010   10067192
    ## 10608                    South Sudan year2011   10448857
    ## 10609                    South Sudan year2012   10818258
    ## 10610                    South Sudan year2013   11177490
    ## 10611                    South Sudan year2014   11530971
    ## 10612                    South Sudan year2015   11882136
    ## 10613                    South Sudan year2016   12230730
    ## 10614                    South Sudan year2017   12575714
    ## 10615                          Spain   year60   30455000
    ## 10616                          Spain   year61   30739250
    ## 10617                          Spain   year62   31023366
    ## 10618                          Spain   year63   31296651
    ## 10619                          Spain   year64   31609195
    ## 10620                          Spain   year65   31954292
    ## 10621                          Spain   year66   32283194
    ## 10622                          Spain   year67   32682947
    ## 10623                          Spain   year68   33113134
    ## 10624                          Spain   year69   33441054
    ## 10625                          Spain   year70   33814531
    ## 10626                          Spain   year71   34224490
    ## 10627                          Spain   year72   34604469
    ## 10628                          Spain   year73   34988947
    ## 10629                          Spain   year74   35373335
    ## 10630                          Spain   year75   35757900
    ## 10631                          Spain   year76   36137812
    ## 10632                          Spain   year77   36511638
    ## 10633                          Spain   year78   36864898
    ## 10634                          Spain   year79   37191330
    ## 10635                          Spain   year80   37491165
    ## 10636                          Spain   year81   37758631
    ## 10637                          Spain   year82   37986012
    ## 10638                          Spain   year83   38171525
    ## 10639                          Spain   year84   38330364
    ## 10640                          Spain   year85   38469512
    ## 10641                          Spain   year86   38584624
    ## 10642                          Spain   year87   38684815
    ## 10643                          Spain   year88   38766939
    ## 10644                          Spain   year89   38827764
    ## 10645                          Spain   year90   38867322
    ## 10646                          Spain   year91   38966376
    ## 10647                          Spain   year92   39157685
    ## 10648                          Spain   year93   39361262
    ## 10649                          Spain   year94   39549108
    ## 10650                          Spain   year95   39724050
    ## 10651                          Spain   year96   39889852
    ## 10652                          Spain   year97   40057389
    ## 10653                          Spain   year98   40223509
    ## 10654                          Spain   year99   40386875
    ## 10655                          Spain year2000   40567864
    ## 10656                          Spain year2001   40850412
    ## 10657                          Spain year2002   41431558
    ## 10658                          Spain year2003   42187645
    ## 10659                          Spain year2004   42921895
    ## 10660                          Spain year2005   43653155
    ## 10661                          Spain year2006   44397319
    ## 10662                          Spain year2007   45226803
    ## 10663                          Spain year2008   45954106
    ## 10664                          Spain year2009   46362946
    ## 10665                          Spain year2010   46576897
    ## 10666                          Spain year2011   46742697
    ## 10667                          Spain year2012   46773055
    ## 10668                          Spain year2013   46620045
    ## 10669                          Spain year2014   46480882
    ## 10670                          Spain year2015   46444832
    ## 10671                          Spain year2016   46484062
    ## 10672                          Spain year2017   46572028
    ## 10673                      Sri Lanka   year60    9874481
    ## 10674                      Sri Lanka   year61   10111646
    ## 10675                      Sri Lanka   year62   10352188
    ## 10676                      Sri Lanka   year63   10597520
    ## 10677                      Sri Lanka   year64   10849979
    ## 10678                      Sri Lanka   year65   11110828
    ## 10679                      Sri Lanka   year66   11380683
    ## 10680                      Sri Lanka   year67   11657660
    ## 10681                      Sri Lanka   year68   11937611
    ## 10682                      Sri Lanka   year69   12214968
    ## 10683                      Sri Lanka   year70   12485756
    ## 10684                      Sri Lanka   year71   12747842
    ## 10685                      Sri Lanka   year72   13002275
    ## 10686                      Sri Lanka   year73   13252087
    ## 10687                      Sri Lanka   year74   13501986
    ## 10688                      Sri Lanka   year75   13755161
    ## 10689                      Sri Lanka   year76   14012817
    ## 10690                      Sri Lanka   year77   14273272
    ## 10691                      Sri Lanka   year78   14533376
    ## 10692                      Sri Lanka   year79   14788607
    ## 10693                      Sri Lanka   year80   15035856
    ## 10694                      Sri Lanka   year81   15273391
    ## 10695                      Sri Lanka   year82   15502515
    ## 10696                      Sri Lanka   year83   15726802
    ## 10697                      Sri Lanka   year84   15951422
    ## 10698                      Sri Lanka   year85   16179796
    ## 10699                      Sri Lanka   year86   16412711
    ## 10700                      Sri Lanka   year87   16647945
    ## 10701                      Sri Lanka   year88   16882189
    ## 10702                      Sri Lanka   year89   17110713
    ## 10703                      Sri Lanka   year90   17329713
    ## 10704                      Sri Lanka   year91   17539633
    ## 10705                      Sri Lanka   year92   17740637
    ## 10706                      Sri Lanka   year93   17928576
    ## 10707                      Sri Lanka   year94   18098348
    ## 10708                      Sri Lanka   year95   18247121
    ## 10709                      Sri Lanka   year96   18372120
    ## 10710                      Sri Lanka   year97   18476505
    ## 10711                      Sri Lanka   year98   18570701
    ## 10712                      Sri Lanka   year99   18669103
    ## 10713                      Sri Lanka year2000   18781938
    ## 10714                      Sri Lanka year2001   18913054
    ## 10715                      Sri Lanka year2002   19059300
    ## 10716                      Sri Lanka year2003   19215307
    ## 10717                      Sri Lanka year2004   19372538
    ## 10718                      Sri Lanka year2005   19524558
    ## 10719                      Sri Lanka year2006   19670151
    ## 10720                      Sri Lanka year2007   19810789
    ## 10721                      Sri Lanka year2008   19945832
    ## 10722                      Sri Lanka year2009   20075086
    ## 10723                      Sri Lanka year2010   20198353
    ## 10724                      Sri Lanka year2011   20315017
    ## 10725                      Sri Lanka year2012   20425000
    ## 10726                      Sri Lanka year2013   20585000
    ## 10727                      Sri Lanka year2014   20771000
    ## 10728                      Sri Lanka year2015   20966000
    ## 10729                      Sri Lanka year2016   21203000
    ## 10730                      Sri Lanka year2017   21444000
    ## 10731            St. Kitts and Nevis   year60      51195
    ## 10732            St. Kitts and Nevis   year61      51193
    ## 10733            St. Kitts and Nevis   year62      50966
    ## 10734            St. Kitts and Nevis   year63      50525
    ## 10735            St. Kitts and Nevis   year64      49930
    ## 10736            St. Kitts and Nevis   year65      49214
    ## 10737            St. Kitts and Nevis   year66      48358
    ## 10738            St. Kitts and Nevis   year67      47380
    ## 10739            St. Kitts and Nevis   year68      46402
    ## 10740            St. Kitts and Nevis   year69      45534
    ## 10741            St. Kitts and Nevis   year70      44885
    ## 10742            St. Kitts and Nevis   year71      44495
    ## 10743            St. Kitts and Nevis   year72      44326
    ## 10744            St. Kitts and Nevis   year73      44316
    ## 10745            St. Kitts and Nevis   year74      44331
    ## 10746            St. Kitts and Nevis   year75      44276
    ## 10747            St. Kitts and Nevis   year76      44148
    ## 10748            St. Kitts and Nevis   year77      43942
    ## 10749            St. Kitts and Nevis   year78      43703
    ## 10750            St. Kitts and Nevis   year79      43457
    ## 10751            St. Kitts and Nevis   year80      43210
    ## 10752            St. Kitts and Nevis   year81      42976
    ## 10753            St. Kitts and Nevis   year82      42762
    ## 10754            St. Kitts and Nevis   year83      42542
    ## 10755            St. Kitts and Nevis   year84      42294
    ## 10756            St. Kitts and Nevis   year85      42013
    ## 10757            St. Kitts and Nevis   year86      41697
    ## 10758            St. Kitts and Nevis   year87      41351
    ## 10759            St. Kitts and Nevis   year88      41047
    ## 10760            St. Kitts and Nevis   year89      40852
    ## 10761            St. Kitts and Nevis   year90      40834
    ## 10762            St. Kitts and Nevis   year91      41013
    ## 10763            St. Kitts and Nevis   year92      41361
    ## 10764            St. Kitts and Nevis   year93      41846
    ## 10765            St. Kitts and Nevis   year94      42373
    ## 10766            St. Kitts and Nevis   year95      42891
    ## 10767            St. Kitts and Nevis   year96      43373
    ## 10768            St. Kitts and Nevis   year97      43846
    ## 10769            St. Kitts and Nevis   year98      44317
    ## 10770            St. Kitts and Nevis   year99      44824
    ## 10771            St. Kitts and Nevis year2000      45374
    ## 10772            St. Kitts and Nevis year2001      45990
    ## 10773            St. Kitts and Nevis year2002      46641
    ## 10774            St. Kitts and Nevis year2003      47306
    ## 10775            St. Kitts and Nevis year2004      47971
    ## 10776            St. Kitts and Nevis year2005      48611
    ## 10777            St. Kitts and Nevis year2006      49210
    ## 10778            St. Kitts and Nevis year2007      49783
    ## 10779            St. Kitts and Nevis year2008      50332
    ## 10780            St. Kitts and Nevis year2009      50886
    ## 10781            St. Kitts and Nevis year2010      51445
    ## 10782            St. Kitts and Nevis year2011      52006
    ## 10783            St. Kitts and Nevis year2012      52591
    ## 10784            St. Kitts and Nevis year2013      53169
    ## 10785            St. Kitts and Nevis year2014      53739
    ## 10786            St. Kitts and Nevis year2015      54288
    ## 10787            St. Kitts and Nevis year2016      54821
    ## 10788            St. Kitts and Nevis year2017      55345
    ## 10789                      St. Lucia   year60      89897
    ## 10790                      St. Lucia   year61      90914
    ## 10791                      St. Lucia   year62      92084
    ## 10792                      St. Lucia   year63      93399
    ## 10793                      St. Lucia   year64      94814
    ## 10794                      St. Lucia   year65      96302
    ## 10795                      St. Lucia   year66      97881
    ## 10796                      St. Lucia   year67      99527
    ## 10797                      St. Lucia   year68     101179
    ## 10798                      St. Lucia   year69     102749
    ## 10799                      St. Lucia   year70     104160
    ## 10800                      St. Lucia   year71     105390
    ## 10801                      St. Lucia   year72     106455
    ## 10802                      St. Lucia   year73     107466
    ## 10803                      St. Lucia   year74     108532
    ## 10804                      St. Lucia   year75     109769
    ## 10805                      St. Lucia   year76     111208
    ## 10806                      St. Lucia   year77     112831
    ## 10807                      St. Lucia   year78     114546
    ## 10808                      St. Lucia   year79     116290
    ## 10809                      St. Lucia   year80     117987
    ## 10810                      St. Lucia   year81     119594
    ## 10811                      St. Lucia   year82     121154
    ## 10812                      St. Lucia   year83     122740
    ## 10813                      St. Lucia   year84     124468
    ## 10814                      St. Lucia   year85     126418
    ## 10815                      St. Lucia   year86     128619
    ## 10816                      St. Lucia   year87     131034
    ## 10817                      St. Lucia   year88     133533
    ## 10818                      St. Lucia   year89     135956
    ## 10819                      St. Lucia   year90     138185
    ## 10820                      St. Lucia   year91     140156
    ## 10821                      St. Lucia   year92     141925
    ## 10822                      St. Lucia   year93     143565
    ## 10823                      St. Lucia   year94     145247
    ## 10824                      St. Lucia   year95     147044
    ## 10825                      St. Lucia   year96     149004
    ## 10826                      St. Lucia   year97     151086
    ## 10827                      St. Lucia   year98     153183
    ## 10828                      St. Lucia   year99     155172
    ## 10829                      St. Lucia year2000     156949
    ## 10830                      St. Lucia year2001     158464
    ## 10831                      St. Lucia year2002     159763
    ## 10832                      St. Lucia year2003     160973
    ## 10833                      St. Lucia year2004     162251
    ## 10834                      St. Lucia year2005     163714
    ## 10835                      St. Lucia year2006     165407
    ## 10836                      St. Lucia year2007     167288
    ## 10837                      St. Lucia year2008     169220
    ## 10838                      St. Lucia year2009     171022
    ## 10839                      St. Lucia year2010     172580
    ## 10840                      St. Lucia year2011     173832
    ## 10841                      St. Lucia year2012     174835
    ## 10842                      St. Lucia year2013     175660
    ## 10843                      St. Lucia year2014     176421
    ## 10844                      St. Lucia year2015     177206
    ## 10845                      St. Lucia year2016     178015
    ## 10846                      St. Lucia year2017     178844
    ## 10847       St. Martin (French part)   year60       4279
    ## 10848       St. Martin (French part)   year61       4453
    ## 10849       St. Martin (French part)   year62       4566
    ## 10850       St. Martin (French part)   year63       4656
    ## 10851       St. Martin (French part)   year64       4748
    ## 10852       St. Martin (French part)   year65       4841
    ## 10853       St. Martin (French part)   year66       4936
    ## 10854       St. Martin (French part)   year67       5033
    ## 10855       St. Martin (French part)   year68       5161
    ## 10856       St. Martin (French part)   year69       5303
    ## 10857       St. Martin (French part)   year70       5450
    ## 10858       St. Martin (French part)   year71       5601
    ## 10859       St. Martin (French part)   year72       5756
    ## 10860       St. Martin (French part)   year73       5915
    ## 10861       St. Martin (French part)   year74       6078
    ## 10862       St. Martin (French part)   year75       6291
    ## 10863       St. Martin (French part)   year76       6530
    ## 10864       St. Martin (French part)   year77       6778
    ## 10865       St. Martin (French part)   year78       7035
    ## 10866       St. Martin (French part)   year79       7303
    ## 10867       St. Martin (French part)   year80       7580
    ## 10868       St. Martin (French part)   year81       7868
    ## 10869       St. Martin (French part)   year82       8670
    ## 10870       St. Martin (French part)   year83      10547
    ## 10871       St. Martin (French part)   year84      12790
    ## 10872       St. Martin (French part)   year85      15392
    ## 10873       St. Martin (French part)   year86      18337
    ## 10874       St. Martin (French part)   year87      21628
    ## 10875       St. Martin (French part)   year88      24873
    ## 10876       St. Martin (French part)   year89      27676
    ## 10877       St. Martin (French part)   year90      30036
    ## 10878       St. Martin (French part)   year91      31821
    ## 10879       St. Martin (French part)   year92      32892
    ## 10880       St. Martin (French part)   year93      33238
    ## 10881       St. Martin (French part)   year94      33098
    ## 10882       St. Martin (French part)   year95      32712
    ## 10883       St. Martin (French part)   year96      32102
    ## 10884       St. Martin (French part)   year97      31304
    ## 10885       St. Martin (French part)   year98      30358
    ## 10886       St. Martin (French part)   year99      29305
    ## 10887       St. Martin (French part) year2000      28384
    ## 10888       St. Martin (French part) year2001      27782
    ## 10889       St. Martin (French part) year2002      27450
    ## 10890       St. Martin (French part) year2003      27363
    ## 10891       St. Martin (French part) year2004      27514
    ## 10892       St. Martin (French part) year2005      27906
    ## 10893       St. Martin (French part) year2006      28414
    ## 10894       St. Martin (French part) year2007      28905
    ## 10895       St. Martin (French part) year2008      29376
    ## 10896       St. Martin (French part) year2009      29820
    ## 10897       St. Martin (French part) year2010      30235
    ## 10898       St. Martin (French part) year2011      30615
    ## 10899       St. Martin (French part) year2012      30959
    ## 10900       St. Martin (French part) year2013      31264
    ## 10901       St. Martin (French part) year2014      31530
    ## 10902       St. Martin (French part) year2015      31754
    ## 10903       St. Martin (French part) year2016      31949
    ## 10904       St. Martin (French part) year2017      32125
    ## 10905 St. Vincent and the Grenadines   year60      80949
    ## 10906 St. Vincent and the Grenadines   year61      82142
    ## 10907 St. Vincent and the Grenadines   year62      83206
    ## 10908 St. Vincent and the Grenadines   year63      84167
    ## 10909 St. Vincent and the Grenadines   year64      85069
    ## 10910 St. Vincent and the Grenadines   year65      85970
    ## 10911 St. Vincent and the Grenadines   year66      86857
    ## 10912 St. Vincent and the Grenadines   year67      87736
    ## 10913 St. Vincent and the Grenadines   year68      88613
    ## 10914 St. Vincent and the Grenadines   year69      89516
    ## 10915 St. Vincent and the Grenadines   year70      90452
    ## 10916 St. Vincent and the Grenadines   year71      91440
    ## 10917 St. Vincent and the Grenadines   year72      92463
    ## 10918 St. Vincent and the Grenadines   year73      93517
    ## 10919 St. Vincent and the Grenadines   year74      94568
    ## 10920 St. Vincent and the Grenadines   year75      95611
    ## 10921 St. Vincent and the Grenadines   year76      96641
    ## 10922 St. Vincent and the Grenadines   year77      97649
    ## 10923 St. Vincent and the Grenadines   year78      98633
    ## 10924 St. Vincent and the Grenadines   year79      99590
    ## 10925 St. Vincent and the Grenadines   year80     100505
    ## 10926 St. Vincent and the Grenadines   year81     101379
    ## 10927 St. Vincent and the Grenadines   year82     102204
    ## 10928 St. Vincent and the Grenadines   year83     102984
    ## 10929 St. Vincent and the Grenadines   year84     103742
    ## 10930 St. Vincent and the Grenadines   year85     104477
    ## 10931 St. Vincent and the Grenadines   year86     105198
    ## 10932 St. Vincent and the Grenadines   year87     105896
    ## 10933 St. Vincent and the Grenadines   year88     106536
    ## 10934 St. Vincent and the Grenadines   year89     107084
    ## 10935 St. Vincent and the Grenadines   year90     107505
    ## 10936 St. Vincent and the Grenadines   year91     107814
    ## 10937 St. Vincent and the Grenadines   year92     108003
    ## 10938 St. Vincent and the Grenadines   year93     108092
    ## 10939 St. Vincent and the Grenadines   year94     108129
    ## 10940 St. Vincent and the Grenadines   year95     108122
    ## 10941 St. Vincent and the Grenadines   year96     108075
    ## 10942 St. Vincent and the Grenadines   year97     108004
    ## 10943 St. Vincent and the Grenadines   year98     107922
    ## 10944 St. Vincent and the Grenadines   year99     107880
    ## 10945 St. Vincent and the Grenadines year2000     107898
    ## 10946 St. Vincent and the Grenadines year2001     107988
    ## 10947 St. Vincent and the Grenadines year2002     108146
    ## 10948 St. Vincent and the Grenadines year2003     108350
    ## 10949 St. Vincent and the Grenadines year2004     108559
    ## 10950 St. Vincent and the Grenadines year2005     108744
    ## 10951 St. Vincent and the Grenadines year2006     108907
    ## 10952 St. Vincent and the Grenadines year2007     109047
    ## 10953 St. Vincent and the Grenadines year2008     109165
    ## 10954 St. Vincent and the Grenadines year2009     109253
    ## 10955 St. Vincent and the Grenadines year2010     109315
    ## 10956 St. Vincent and the Grenadines year2011     109341
    ## 10957 St. Vincent and the Grenadines year2012     109328
    ## 10958 St. Vincent and the Grenadines year2013     109320
    ## 10959 St. Vincent and the Grenadines year2014     109357
    ## 10960 St. Vincent and the Grenadines year2015     109455
    ## 10961 St. Vincent and the Grenadines year2016     109643
    ## 10962 St. Vincent and the Grenadines year2017     109897
    ## 10963                          Sudan   year60    7544491
    ## 10964                          Sudan   year61    7769482
    ## 10965                          Sudan   year62    8004121
    ## 10966                          Sudan   year63    8248812
    ## 10967                          Sudan   year64    8503994
    ## 10968                          Sudan   year65    8770097
    ## 10969                          Sudan   year66    9047798
    ## 10970                          Sudan   year67    9337657
    ## 10971                          Sudan   year68    9639840
    ## 10972                          Sudan   year69    9954410
    ## 10973                          Sudan   year70   10281700
    ## 10974                          Sudan   year71   10621472
    ## 10975                          Sudan   year72   10974622
    ## 10976                          Sudan   year73   11343926
    ## 10977                          Sudan   year74   11732958
    ## 10978                          Sudan   year75   12144135
    ## 10979                          Sudan   year76   12578407
    ## 10980                          Sudan   year77   13034625
    ## 10981                          Sudan   year78   13510421
    ## 10982                          Sudan   year79   14002303
    ## 10983                          Sudan   year80   14507468
    ## 10984                          Sudan   year81   15027270
    ## 10985                          Sudan   year82   15562194
    ## 10986                          Sudan   year83   16107730
    ## 10987                          Sudan   year84   16658054
    ## 10988                          Sudan   year85   17210187
    ## 10989                          Sudan   year86   17757169
    ## 10990                          Sudan   year87   18302587
    ## 10991                          Sudan   year88   18866319
    ## 10992                          Sudan   year89   19475609
    ## 10993                          Sudan   year90   20147590
    ## 10994                          Sudan   year91   20893625
    ## 10995                          Sudan   year92   21701476
    ## 10996                          Sudan   year93   22535937
    ## 10997                          Sudan   year94   23347885
    ## 10998                          Sudan   year95   24102986
    ## 10999                          Sudan   year96   24786190
    ## 11000                          Sudan   year97   25410451
    ## 11001                          Sudan   year98   26003542
    ## 11002                          Sudan   year99   26607042
    ## 11003                          Sudan year2000   27250535
    ## 11004                          Sudan year2001   27945005
    ## 11005                          Sudan year2002   28679565
    ## 11006                          Sudan year2003   29435944
    ## 11007                          Sudan year2004   30186341
    ## 11008                          Sudan year2005   30911914
    ## 11009                          Sudan year2006   31607064
    ## 11010                          Sudan year2007   32282526
    ## 11011                          Sudan year2008   32955496
    ## 11012                          Sudan year2009   33650619
    ## 11013                          Sudan year2010   34385963
    ## 11014                          Sudan year2011   35167314
    ## 11015                          Sudan year2012   35990192
    ## 11016                          Sudan year2013   36849918
    ## 11017                          Sudan year2014   37737913
    ## 11018                          Sudan year2015   38647803
    ## 11019                          Sudan year2016   39578828
    ## 11020                          Sudan year2017   40533330
    ## 11021                       Suriname   year60     289966
    ## 11022                       Suriname   year61     298188
    ## 11023                       Suriname   year62     306328
    ## 11024                       Suriname   year63     314528
    ## 11025                       Suriname   year64     322997
    ## 11026                       Suriname   year65     331793
    ## 11027                       Suriname   year66     341133
    ## 11028                       Suriname   year67     350751
    ## 11029                       Suriname   year68     359733
    ## 11030                       Suriname   year69     366848
    ## 11031                       Suriname   year70     371273
    ## 11032                       Suriname   year71     372623
    ## 11033                       Suriname   year72     371324
    ## 11034                       Suriname   year73     368344
    ## 11035                       Suriname   year74     365099
    ## 11036                       Suriname   year75     362654
    ## 11037                       Suriname   year76     361364
    ## 11038                       Suriname   year77     361043
    ## 11039                       Suriname   year78     361457
    ## 11040                       Suriname   year79     362125
    ## 11041                       Suriname   year80     362777
    ## 11042                       Suriname   year81     363325
    ## 11043                       Suriname   year82     364032
    ## 11044                       Suriname   year83     365300
    ## 11045                       Suriname   year84     367660
    ## 11046                       Suriname   year85     371470
    ## 11047                       Suriname   year86     376867
    ## 11048                       Suriname   year87     383654
    ## 11049                       Suriname   year88     391391
    ## 11050                       Suriname   year89     399492
    ## 11051                       Suriname   year90     407472
    ## 11052                       Suriname   year91     415216
    ## 11053                       Suriname   year92     422763
    ## 11054                       Suriname   year93     430039
    ## 11055                       Suriname   year94     437037
    ## 11056                       Suriname   year95     443724
    ## 11057                       Suriname   year96     450036
    ## 11058                       Suriname   year97     455954
    ## 11059                       Suriname   year98     461560
    ## 11060                       Suriname   year99     467003
    ## 11061                       Suriname year2000     472390
    ## 11062                       Suriname year2001     477740
    ## 11063                       Suriname year2002     483044
    ## 11064                       Suriname year2003     488332
    ## 11065                       Suriname year2004     493630
    ## 11066                       Suriname year2005     498946
    ## 11067                       Suriname year2006     504307
    ## 11068                       Suriname year2007     509705
    ## 11069                       Suriname year2008     515148
    ## 11070                       Suriname year2009     520619
    ## 11071                       Suriname year2010     526103
    ## 11072                       Suriname year2011     531589
    ## 11073                       Suriname year2012     537077
    ## 11074                       Suriname year2013     542540
    ## 11075                       Suriname year2014     547928
    ## 11076                       Suriname year2015     553208
    ## 11077                       Suriname year2016     558368
    ## 11078                       Suriname year2017     563402
    ## 11079                         Sweden   year60    7484656
    ## 11080                         Sweden   year61    7519998
    ## 11081                         Sweden   year62    7561588
    ## 11082                         Sweden   year63    7604328
    ## 11083                         Sweden   year64    7661354
    ## 11084                         Sweden   year65    7733853
    ## 11085                         Sweden   year66    7807797
    ## 11086                         Sweden   year67    7867931
    ## 11087                         Sweden   year68    7912273
    ## 11088                         Sweden   year69    7968072
    ## 11089                         Sweden   year70    8042801
    ## 11090                         Sweden   year71    8098334
    ## 11091                         Sweden   year72    8122300
    ## 11092                         Sweden   year73    8136312
    ## 11093                         Sweden   year74    8159955
    ## 11094                         Sweden   year75    8192437
    ## 11095                         Sweden   year76    8222286
    ## 11096                         Sweden   year77    8251540
    ## 11097                         Sweden   year78    8275599
    ## 11098                         Sweden   year79    8293678
    ## 11099                         Sweden   year80    8310531
    ## 11100                         Sweden   year81    8320503
    ## 11101                         Sweden   year82    8325263
    ## 11102                         Sweden   year83    8329033
    ## 11103                         Sweden   year84    8336605
    ## 11104                         Sweden   year85    8350386
    ## 11105                         Sweden   year86    8369829
    ## 11106                         Sweden   year87    8397804
    ## 11107                         Sweden   year88    8436489
    ## 11108                         Sweden   year89    8492964
    ## 11109                         Sweden   year90    8558835
    ## 11110                         Sweden   year91    8617375
    ## 11111                         Sweden   year92    8668067
    ## 11112                         Sweden   year93    8718561
    ## 11113                         Sweden   year94    8780745
    ## 11114                         Sweden   year95    8826939
    ## 11115                         Sweden   year96    8840998
    ## 11116                         Sweden   year97    8846062
    ## 11117                         Sweden   year98    8850974
    ## 11118                         Sweden   year99    8857874
    ## 11119                         Sweden year2000    8872109
    ## 11120                         Sweden year2001    8895960
    ## 11121                         Sweden year2002    8924958
    ## 11122                         Sweden year2003    8958229
    ## 11123                         Sweden year2004    8993531
    ## 11124                         Sweden year2005    9029572
    ## 11125                         Sweden year2006    9080505
    ## 11126                         Sweden year2007    9148092
    ## 11127                         Sweden year2008    9219637
    ## 11128                         Sweden year2009    9298515
    ## 11129                         Sweden year2010    9378126
    ## 11130                         Sweden year2011    9449213
    ## 11131                         Sweden year2012    9519374
    ## 11132                         Sweden year2013    9600379
    ## 11133                         Sweden year2014    9696110
    ## 11134                         Sweden year2015    9799186
    ## 11135                         Sweden year2016    9923085
    ## 11136                         Sweden year2017   10067744
    ## 11137                    Switzerland   year60    5327827
    ## 11138                    Switzerland   year61    5434294
    ## 11139                    Switzerland   year62    5573815
    ## 11140                    Switzerland   year63    5694247
    ## 11141                    Switzerland   year64    5789228
    ## 11142                    Switzerland   year65    5856472
    ## 11143                    Switzerland   year66    5918002
    ## 11144                    Switzerland   year67    5991785
    ## 11145                    Switzerland   year68    6067714
    ## 11146                    Switzerland   year69    6136387
    ## 11147                    Switzerland   year70    6180877
    ## 11148                    Switzerland   year71    6213399
    ## 11149                    Switzerland   year72    6260956
    ## 11150                    Switzerland   year73    6307347
    ## 11151                    Switzerland   year74    6341405
    ## 11152                    Switzerland   year75    6338632
    ## 11153                    Switzerland   year76    6302504
    ## 11154                    Switzerland   year77    6281174
    ## 11155                    Switzerland   year78    6281738
    ## 11156                    Switzerland   year79    6294365
    ## 11157                    Switzerland   year80    6319408
    ## 11158                    Switzerland   year81    6354074
    ## 11159                    Switzerland   year82    6391309
    ## 11160                    Switzerland   year83    6418773
    ## 11161                    Switzerland   year84    6441865
    ## 11162                    Switzerland   year85    6470365
    ## 11163                    Switzerland   year86    6504124
    ## 11164                    Switzerland   year87    6545106
    ## 11165                    Switzerland   year88    6593386
    ## 11166                    Switzerland   year89    6646912
    ## 11167                    Switzerland   year90    6715519
    ## 11168                    Switzerland   year91    6799978
    ## 11169                    Switzerland   year92    6875364
    ## 11170                    Switzerland   year93    6938265
    ## 11171                    Switzerland   year94    6993795
    ## 11172                    Switzerland   year95    7040687
    ## 11173                    Switzerland   year96    7071850
    ## 11174                    Switzerland   year97    7088906
    ## 11175                    Switzerland   year98    7110001
    ## 11176                    Switzerland   year99    7143991
    ## 11177                    Switzerland year2000    7184250
    ## 11178                    Switzerland year2001    7229854
    ## 11179                    Switzerland year2002    7284753
    ## 11180                    Switzerland year2003    7339001
    ## 11181                    Switzerland year2004    7389625
    ## 11182                    Switzerland year2005    7437115
    ## 11183                    Switzerland year2006    7483934
    ## 11184                    Switzerland year2007    7551117
    ## 11185                    Switzerland year2008    7647675
    ## 11186                    Switzerland year2009    7743831
    ## 11187                    Switzerland year2010    7824909
    ## 11188                    Switzerland year2011    7912398
    ## 11189                    Switzerland year2012    7996861
    ## 11190                    Switzerland year2013    8089346
    ## 11191                    Switzerland year2014    8188649
    ## 11192                    Switzerland year2015    8282396
    ## 11193                    Switzerland year2016    8373338
    ## 11194                    Switzerland year2017    8466017
    ## 11195           Syrian Arab Republic   year60    4573512
    ## 11196           Syrian Arab Republic   year61    4721896
    ## 11197           Syrian Arab Republic   year62    4875422
    ## 11198           Syrian Arab Republic   year63    5034646
    ## 11199           Syrian Arab Republic   year64    5200336
    ## 11200           Syrian Arab Republic   year65    5373137
    ## 11201           Syrian Arab Republic   year66    5553246
    ## 11202           Syrian Arab Republic   year67    5740710
    ## 11203           Syrian Arab Republic   year68    5935860
    ## 11204           Syrian Arab Republic   year69    6139048
    ## 11205           Syrian Arab Republic   year70    6350541
    ## 11206           Syrian Arab Republic   year71    6570857
    ## 11207           Syrian Arab Republic   year72    6800141
    ## 11208           Syrian Arab Republic   year73    7037851
    ## 11209           Syrian Arab Republic   year74    7283177
    ## 11210           Syrian Arab Republic   year75    7535714
    ## 11211           Syrian Arab Republic   year76    7794662
    ## 11212           Syrian Arab Republic   year77    8060649
    ## 11213           Syrian Arab Republic   year78    8336418
    ## 11214           Syrian Arab Republic   year79    8625690
    ## 11215           Syrian Arab Republic   year80    8930774
    ## 11216           Syrian Arab Republic   year81    9252851
    ## 11217           Syrian Arab Republic   year82    9590227
    ## 11218           Syrian Arab Republic   year83    9938847
    ## 11219           Syrian Arab Republic   year84   10293049
    ## 11220           Syrian Arab Republic   year85   10648632
    ## 11221           Syrian Arab Republic   year86   11004272
    ## 11222           Syrian Arab Republic   year87   11360852
    ## 11223           Syrian Arab Republic   year88   11719071
    ## 11224           Syrian Arab Republic   year89   12080444
    ## 11225           Syrian Arab Republic   year90   12446171
    ## 11226           Syrian Arab Republic   year91   12815219
    ## 11227           Syrian Arab Republic   year92   13187085
    ## 11228           Syrian Arab Republic   year93   13564167
    ## 11229           Syrian Arab Republic   year94   13949697
    ## 11230           Syrian Arab Republic   year95   14345492
    ## 11231           Syrian Arab Republic   year96   14755286
    ## 11232           Syrian Arab Republic   year97   15177456
    ## 11233           Syrian Arab Republic   year98   15602210
    ## 11234           Syrian Arab Republic   year99   16016092
    ## 11235           Syrian Arab Republic year2000   16410848
    ## 11236           Syrian Arab Republic year2001   16766899
    ## 11237           Syrian Arab Republic year2002   17087901
    ## 11238           Syrian Arab Republic year2003   17415266
    ## 11239           Syrian Arab Republic year2004   17806638
    ## 11240           Syrian Arab Republic year2005   18294611
    ## 11241           Syrian Arab Republic year2006   18914977
    ## 11242           Syrian Arab Republic year2007   19632806
    ## 11243           Syrian Arab Republic year2008   20325443
    ## 11244           Syrian Arab Republic year2009   20824893
    ## 11245           Syrian Arab Republic year2010   21018834
    ## 11246           Syrian Arab Republic year2011   20863993
    ## 11247           Syrian Arab Republic year2012   20420701
    ## 11248           Syrian Arab Republic year2013   19809141
    ## 11249           Syrian Arab Republic year2014   19203090
    ## 11250           Syrian Arab Republic year2015   18734987
    ## 11251           Syrian Arab Republic year2016   18430453
    ## 11252           Syrian Arab Republic year2017   18269868
    ## 11253                     Tajikistan   year60    2087038
    ## 11254                     Tajikistan   year61    2159123
    ## 11255                     Tajikistan   year62    2236559
    ## 11256                     Tajikistan   year63    2318234
    ## 11257                     Tajikistan   year64    2402455
    ## 11258                     Tajikistan   year65    2487953
    ## 11259                     Tajikistan   year66    2574478
    ## 11260                     Tajikistan   year67    2662230
    ## 11261                     Tajikistan   year68    2750894
    ## 11262                     Tajikistan   year69    2840228
    ## 11263                     Tajikistan   year70    2930079
    ## 11264                     Tajikistan   year71    3020391
    ## 11265                     Tajikistan   year72    3111264
    ## 11266                     Tajikistan   year73    3203019
    ## 11267                     Tajikistan   year74    3296095
    ## 11268                     Tajikistan   year75    3390935
    ## 11269                     Tajikistan   year76    3487644
    ## 11270                     Tajikistan   year77    3586499
    ## 11271                     Tajikistan   year78    3688385
    ## 11272                     Tajikistan   year79    3794420
    ## 11273                     Tajikistan   year80    3905413
    ## 11274                     Tajikistan   year81    4020778
    ## 11275                     Tajikistan   year82    4140258
    ## 11276                     Tajikistan   year83    4265247
    ## 11277                     Tajikistan   year84    4397525
    ## 11278                     Tajikistan   year85    4537789
    ## 11279                     Tajikistan   year86    4687283
    ## 11280                     Tajikistan   year87    4843951
    ## 11281                     Tajikistan   year88    5001110
    ## 11282                     Tajikistan   year89    5149803
    ## 11283                     Tajikistan   year90    5283728
    ## 11284                     Tajikistan   year91    5400714
    ## 11285                     Tajikistan   year92    5502976
    ## 11286                     Tajikistan   year93    5594114
    ## 11287                     Tajikistan   year94    5679832
    ## 11288                     Tajikistan   year95    5764712
    ## 11289                     Tajikistan   year96    5849540
    ## 11290                     Tajikistan   year97    5934282
    ## 11291                     Tajikistan   year98    6021691
    ## 11292                     Tajikistan   year99    6114886
    ## 11293                     Tajikistan year2000    6216205
    ## 11294                     Tajikistan year2001    6327125
    ## 11295                     Tajikistan year2002    6447688
    ## 11296                     Tajikistan year2003    6576877
    ## 11297                     Tajikistan year2004    6712841
    ## 11298                     Tajikistan year2005    6854176
    ## 11299                     Tajikistan year2006    7000557
    ## 11300                     Tajikistan year2007    7152385
    ## 11301                     Tajikistan year2008    7309728
    ## 11302                     Tajikistan year2009    7472819
    ## 11303                     Tajikistan year2010    7641630
    ## 11304                     Tajikistan year2011    7815949
    ## 11305                     Tajikistan year2012    7995062
    ## 11306                     Tajikistan year2013    8177809
    ## 11307                     Tajikistan year2014    8362745
    ## 11308                     Tajikistan year2015    8548651
    ## 11309                     Tajikistan year2016    8734951
    ## 11310                     Tajikistan year2017    8921343
    ## 11311                       Tanzania   year60   10074507
    ## 11312                       Tanzania   year61   10373398
    ## 11313                       Tanzania   year62   10683906
    ## 11314                       Tanzania   year63   11005905
    ## 11315                       Tanzania   year64   11339097
    ## 11316                       Tanzania   year65   11683528
    ## 11317                       Tanzania   year66   12038903
    ## 11318                       Tanzania   year67   12406040
    ## 11319                       Tanzania   year68   12787489
    ## 11320                       Tanzania   year69   13186557
    ## 11321                       Tanzania   year70   13605529
    ## 11322                       Tanzania   year71   14045824
    ## 11323                       Tanzania   year72   14506617
    ## 11324                       Tanzania   year73   14985131
    ## 11325                       Tanzania   year74   15477294
    ## 11326                       Tanzania   year75   15980301
    ## 11327                       Tanzania   year76   16493305
    ## 11328                       Tanzania   year77   17017670
    ## 11329                       Tanzania   year78   17555494
    ## 11330                       Tanzania   year79   18109884
    ## 11331                       Tanzania   year80   18683157
    ## 11332                       Tanzania   year81   19277108
    ## 11333                       Tanzania   year82   19891548
    ## 11334                       Tanzania   year83   20524666
    ## 11335                       Tanzania   year84   21173603
    ## 11336                       Tanzania   year85   21836999
    ## 11337                       Tanzania   year86   22511243
    ## 11338                       Tanzania   year87   23198533
    ## 11339                       Tanzania   year88   23909954
    ## 11340                       Tanzania   year89   24660575
    ## 11341                       Tanzania   year90   25459604
    ## 11342                       Tanzania   year91   26315013
    ## 11343                       Tanzania   year92   27219619
    ## 11344                       Tanzania   year93   28149328
    ## 11345                       Tanzania   year94   29070615
    ## 11346                       Tanzania   year95   29960776
    ## 11347                       Tanzania   year96   30811854
    ## 11348                       Tanzania   year97   31635251
    ## 11349                       Tanzania   year98   32451713
    ## 11350                       Tanzania   year99   33291540
    ## 11351                       Tanzania year2000   34178042
    ## 11352                       Tanzania year2001   35117019
    ## 11353                       Tanzania year2002   36105808
    ## 11354                       Tanzania year2003   37149072
    ## 11355                       Tanzania year2004   38249984
    ## 11356                       Tanzania year2005   39410545
    ## 11357                       Tanzania year2006   40634948
    ## 11358                       Tanzania year2007   41923715
    ## 11359                       Tanzania year2008   43270144
    ## 11360                       Tanzania year2009   44664231
    ## 11361                       Tanzania year2010   46098591
    ## 11362                       Tanzania year2011   47570902
    ## 11363                       Tanzania year2012   49082997
    ## 11364                       Tanzania year2013   50636595
    ## 11365                       Tanzania year2014   52234869
    ## 11366                       Tanzania year2015   53879957
    ## 11367                       Tanzania year2016   55572201
    ## 11368                       Tanzania year2017   57310019
    ## 11369                       Thailand   year60   27397175
    ## 11370                       Thailand   year61   28224204
    ## 11371                       Thailand   year62   29081034
    ## 11372                       Thailand   year63   29967041
    ## 11373                       Thailand   year64   30881332
    ## 11374                       Thailand   year65   31822796
    ## 11375                       Thailand   year66   32789096
    ## 11376                       Thailand   year67   33778504
    ## 11377                       Thailand   year68   34790945
    ## 11378                       Thailand   year69   35826804
    ## 11379                       Thailand   year70   36884913
    ## 11380                       Thailand   year71   37964925
    ## 11381                       Thailand   year72   39061994
    ## 11382                       Thailand   year73   40164966
    ## 11383                       Thailand   year74   41259536
    ## 11384                       Thailand   year75   42334954
    ## 11385                       Thailand   year76   43386841
    ## 11386                       Thailand   year77   44416010
    ## 11387                       Thailand   year78   45423436
    ## 11388                       Thailand   year79   46412307
    ## 11389                       Thailand   year80   47385323
    ## 11390                       Thailand   year81   48337503
    ## 11391                       Thailand   year82   49267560
    ## 11392                       Thailand   year83   50186199
    ## 11393                       Thailand   year84   51108082
    ## 11394                       Thailand   year85   52041469
    ## 11395                       Thailand   year86   52996467
    ## 11396                       Thailand   year87   53964406
    ## 11397                       Thailand   year88   54912334
    ## 11398                       Thailand   year89   55795106
    ## 11399                       Thailand   year90   56582821
    ## 11400                       Thailand   year91   57258401
    ## 11401                       Thailand   year92   57837878
    ## 11402                       Thailand   year93   58364891
    ## 11403                       Thailand   year94   58901666
    ## 11404                       Thailand   year95   59491790
    ## 11405                       Thailand   year96   60151472
    ## 11406                       Thailand   year97   60863506
    ## 11407                       Thailand   year98   61597283
    ## 11408                       Thailand   year99   62306651
    ## 11409                       Thailand year2000   62958021
    ## 11410                       Thailand year2001   63543322
    ## 11411                       Thailand year2002   64073164
    ## 11412                       Thailand year2003   64554952
    ## 11413                       Thailand year2004   65002231
    ## 11414                       Thailand year2005   65425470
    ## 11415                       Thailand year2006   65824164
    ## 11416                       Thailand year2007   66195615
    ## 11417                       Thailand year2008   66545760
    ## 11418                       Thailand year2009   66881867
    ## 11419                       Thailand year2010   67208808
    ## 11420                       Thailand year2011   67530130
    ## 11421                       Thailand year2012   67843979
    ## 11422                       Thailand year2013   68143065
    ## 11423                       Thailand year2014   68416772
    ## 11424                       Thailand year2015   68657600
    ## 11425                       Thailand year2016   68863514
    ## 11426                       Thailand year2017   69037513
    ## 11427                    Timor-Leste   year60     499950
    ## 11428                    Timor-Leste   year61     508845
    ## 11429                    Timor-Leste   year62     518107
    ## 11430                    Timor-Leste   year63     527749
    ## 11431                    Timor-Leste   year64     537786
    ## 11432                    Timor-Leste   year65     548218
    ## 11433                    Timor-Leste   year66     558676
    ## 11434                    Timor-Leste   year67     569031
    ## 11435                    Timor-Leste   year68     579807
    ## 11436                    Timor-Leste   year69     591739
    ## 11437                    Timor-Leste   year70     605125
    ## 11438                    Timor-Leste   year71     620945
    ## 11439                    Timor-Leste   year72     638499
    ## 11440                    Timor-Leste   year73     654437
    ## 11441                    Timor-Leste   year74     664223
    ## 11442                    Timor-Leste   year75     664984
    ## 11443                    Timor-Leste   year76     654947
    ## 11444                    Timor-Leste   year77     636096
    ## 11445                    Timor-Leste   year78     613857
    ## 11446                    Timor-Leste   year79     595872
    ## 11447                    Timor-Leste   year80     587563
    ## 11448                    Timor-Leste   year81     591005
    ## 11449                    Timor-Leste   year82     604430
    ## 11450                    Timor-Leste   year83     624648
    ## 11451                    Timor-Leste   year84     646688
    ## 11452                    Timor-Leste   year85     666945
    ## 11453                    Timor-Leste   year86     684184
    ## 11454                    Timor-Leste   year87     699522
    ## 11455                    Timor-Leste   year88     714474
    ## 11456                    Timor-Leste   year89     731444
    ## 11457                    Timor-Leste   year90     751933
    ## 11458                    Timor-Leste   year91     777011
    ## 11459                    Timor-Leste   year92     805435
    ## 11460                    Timor-Leste   year93     833611
    ## 11461                    Timor-Leste   year94     856684
    ## 11462                    Timor-Leste   year95     871447
    ## 11463                    Timor-Leste   year96     875916
    ## 11464                    Timor-Leste   year97     871994
    ## 11465                    Timor-Leste   year98     865194
    ## 11466                    Timor-Leste   year99     863269
    ## 11467                    Timor-Leste year2000     871607
    ## 11468                    Timor-Leste year2001     892531
    ## 11469                    Timor-Leste year2002     923825
    ## 11470                    Timor-Leste year2003     960852
    ## 11471                    Timor-Leste year2004     996698
    ## 11472                    Timor-Leste year2005    1026484
    ## 11473                    Timor-Leste year2006    1048621
    ## 11474                    Timor-Leste year2007    1064973
    ## 11475                    Timor-Leste year2008    1078110
    ## 11476                    Timor-Leste year2009    1092021
    ## 11477                    Timor-Leste year2010    1109591
    ## 11478                    Timor-Leste year2011    1131523
    ## 11479                    Timor-Leste year2012    1156760
    ## 11480                    Timor-Leste year2013    1184366
    ## 11481                    Timor-Leste year2014    1212814
    ## 11482                    Timor-Leste year2015    1240977
    ## 11483                    Timor-Leste year2016    1268671
    ## 11484                    Timor-Leste year2017    1296311
    ## 11485                           Togo   year60    1580513
    ## 11486                           Togo   year61    1597526
    ## 11487                           Togo   year62    1612755
    ## 11488                           Togo   year63    1631764
    ## 11489                           Togo   year64    1662073
    ## 11490                           Togo   year65    1708630
    ## 11491                           Togo   year66    1774029
    ## 11492                           Togo   year67    1855442
    ## 11493                           Togo   year68    1945780
    ## 11494                           Togo   year69    2034907
    ## 11495                           Togo   year70    2115522
    ## 11496                           Togo   year71    2185662
    ## 11497                           Togo   year72    2247582
    ## 11498                           Togo   year73    2303345
    ## 11499                           Togo   year74    2356622
    ## 11500                           Togo   year75    2410446
    ## 11501                           Togo   year76    2464455
    ## 11502                           Togo   year77    2518566
    ## 11503                           Togo   year78    2576469
    ## 11504                           Togo   year79    2642846
    ## 11505                           Togo   year80    2720839
    ## 11506                           Togo   year81    2812039
    ## 11507                           Togo   year82    2915066
    ## 11508                           Togo   year83    3026238
    ## 11509                           Togo   year84    3140237
    ## 11510                           Togo   year85    3252994
    ## 11511                           Togo   year86    3364020
    ## 11512                           Togo   year87    3474080
    ## 11513                           Togo   year88    3581928
    ## 11514                           Togo   year89    3686373
    ## 11515                           Togo   year90    3786940
    ## 11516                           Togo   year91    3882271
    ## 11517                           Togo   year92    3973327
    ## 11518                           Togo   year93    4064926
    ## 11519                           Togo   year94    4163642
    ## 11520                           Togo   year95    4274024
    ## 11521                           Togo   year96    4398238
    ## 11522                           Togo   year97    4534551
    ## 11523                           Togo   year98    4679023
    ## 11524                           Togo   year99    4825704
    ## 11525                           Togo year2000    4970367
    ## 11526                           Togo year2001    5111770
    ## 11527                           Togo year2002    5251472
    ## 11528                           Togo year2003    5391401
    ## 11529                           Togo year2004    5534598
    ## 11530                           Togo year2005    5683268
    ## 11531                           Togo year2006    5837792
    ## 11532                           Togo year2007    5997385
    ## 11533                           Togo year2008    6161796
    ## 11534                           Togo year2009    6330472
    ## 11535                           Togo year2010    6502952
    ## 11536                           Togo year2011    6679282
    ## 11537                           Togo year2012    6859482
    ## 11538                           Togo year2013    7042948
    ## 11539                           Togo year2014    7228915
    ## 11540                           Togo year2015    7416802
    ## 11541                           Togo year2016    7606374
    ## 11542                           Togo year2017    7797694
    ## 11543                          Tonga   year60      61601
    ## 11544                          Tonga   year61      63745
    ## 11545                          Tonga   year62      66259
    ## 11546                          Tonga   year63      69005
    ## 11547                          Tonga   year64      71757
    ## 11548                          Tonga   year65      74362
    ## 11549                          Tonga   year66      76779
    ## 11550                          Tonga   year67      79052
    ## 11551                          Tonga   year68      81097
    ## 11552                          Tonga   year69      82877
    ## 11553                          Tonga   year70      84369
    ## 11554                          Tonga   year71      85518
    ## 11555                          Tonga   year72      86347
    ## 11556                          Tonga   year73      86988
    ## 11557                          Tonga   year74      87609
    ## 11558                          Tonga   year75      88348
    ## 11559                          Tonga   year76      89254
    ## 11560                          Tonga   year77      90295
    ## 11561                          Tonga   year78      91364
    ## 11562                          Tonga   year79      92300
    ## 11563                          Tonga   year80      93007
    ## 11564                          Tonga   year81      93453
    ## 11565                          Tonga   year82      93681
    ## 11566                          Tonga   year83      93774
    ## 11567                          Tonga   year84      93842
    ## 11568                          Tonga   year85      93953
    ## 11569                          Tonga   year86      94145
    ## 11570                          Tonga   year87      94384
    ## 11571                          Tonga   year88      94667
    ## 11572                          Tonga   year89      94929
    ## 11573                          Tonga   year90      95153
    ## 11574                          Tonga   year91      95333
    ## 11575                          Tonga   year92      95496
    ## 11576                          Tonga   year93      95644
    ## 11577                          Tonga   year94      95833
    ## 11578                          Tonga   year95      96076
    ## 11579                          Tonga   year96      96369
    ## 11580                          Tonga   year97      96725
    ## 11581                          Tonga   year98      97135
    ## 11582                          Tonga   year99      97591
    ## 11583                          Tonga year2000      98082
    ## 11584                          Tonga year2001      98611
    ## 11585                          Tonga year2002      99184
    ## 11586                          Tonga year2003      99789
    ## 11587                          Tonga year2004     100406
    ## 11588                          Tonga year2005     101041
    ## 11589                          Tonga year2006     101689
    ## 11590                          Tonga year2007     102357
    ## 11591                          Tonga year2008     103005
    ## 11592                          Tonga year2009     103604
    ## 11593                          Tonga year2010     104137
    ## 11594                          Tonga year2011     104577
    ## 11595                          Tonga year2012     104951
    ## 11596                          Tonga year2013     105328
    ## 11597                          Tonga year2014     105782
    ## 11598                          Tonga year2015     106364
    ## 11599                          Tonga year2016     107122
    ## 11600                          Tonga year2017     108020
    ## 11601            Trinidad and Tobago   year60     848479
    ## 11602            Trinidad and Tobago   year61     865360
    ## 11603            Trinidad and Tobago   year62     880023
    ## 11604            Trinidad and Tobago   year63     892569
    ## 11605            Trinidad and Tobago   year64     903275
    ## 11606            Trinidad and Tobago   year65     912417
    ## 11607            Trinidad and Tobago   year66     919903
    ## 11608            Trinidad and Tobago   year67     925909
    ## 11609            Trinidad and Tobago   year68     931468
    ## 11610            Trinidad and Tobago   year69     937848
    ## 11611            Trinidad and Tobago   year70     945993
    ## 11612            Trinidad and Tobago   year71     956366
    ## 11613            Trinidad and Tobago   year72     968741
    ## 11614            Trinidad and Tobago   year73     982592
    ## 11615            Trinidad and Tobago   year74     997053
    ## 11616            Trinidad and Tobago   year75    1011490
    ## 11617            Trinidad and Tobago   year76    1025658
    ## 11618            Trinidad and Tobago   year77    1039761
    ## 11619            Trinidad and Tobago   year78    1054116
    ## 11620            Trinidad and Tobago   year79    1069202
    ## 11621            Trinidad and Tobago   year80    1085308
    ## 11622            Trinidad and Tobago   year81    1102556
    ## 11623            Trinidad and Tobago   year82    1120611
    ## 11624            Trinidad and Tobago   year83    1138676
    ## 11625            Trinidad and Tobago   year84    1155695
    ## 11626            Trinidad and Tobago   year85    1170928
    ## 11627            Trinidad and Tobago   year86    1184051
    ## 11628            Trinidad and Tobago   year87    1195247
    ## 11629            Trinidad and Tobago   year88    1204893
    ## 11630            Trinidad and Tobago   year89    1213624
    ## 11631            Trinidad and Tobago   year90    1221900
    ## 11632            Trinidad and Tobago   year91    1229907
    ## 11633            Trinidad and Tobago   year92    1237487
    ## 11634            Trinidad and Tobago   year93    1244407
    ## 11635            Trinidad and Tobago   year94    1250318
    ## 11636            Trinidad and Tobago   year95    1255001
    ## 11637            Trinidad and Tobago   year96    1258364
    ## 11638            Trinidad and Tobago   year97    1260678
    ## 11639            Trinidad and Tobago   year98    1262542
    ## 11640            Trinidad and Tobago   year99    1264775
    ## 11641            Trinidad and Tobago year2000    1267984
    ## 11642            Trinidad and Tobago year2001    1272380
    ## 11643            Trinidad and Tobago year2002    1277837
    ## 11644            Trinidad and Tobago year2003    1284052
    ## 11645            Trinidad and Tobago year2004    1290535
    ## 11646            Trinidad and Tobago year2005    1296934
    ## 11647            Trinidad and Tobago year2006    1303144
    ## 11648            Trinidad and Tobago year2007    1309260
    ## 11649            Trinidad and Tobago year2008    1315372
    ## 11650            Trinidad and Tobago year2009    1321618
    ## 11651            Trinidad and Tobago year2010    1328100
    ## 11652            Trinidad and Tobago year2011    1334788
    ## 11653            Trinidad and Tobago year2012    1341588
    ## 11654            Trinidad and Tobago year2013    1348248
    ## 11655            Trinidad and Tobago year2014    1354493
    ## 11656            Trinidad and Tobago year2015    1360092
    ## 11657            Trinidad and Tobago year2016    1364962
    ## 11658            Trinidad and Tobago year2017    1369125
    ## 11659                        Tunisia   year60    4176266
    ## 11660                        Tunisia   year61    4235937
    ## 11661                        Tunisia   year62    4303131
    ## 11662                        Tunisia   year63    4377637
    ## 11663                        Tunisia   year64    4458611
    ## 11664                        Tunisia   year65    4545339
    ## 11665                        Tunisia   year66    4638275
    ## 11666                        Tunisia   year67    4737627
    ## 11667                        Tunisia   year68    4842167
    ## 11668                        Tunisia   year69    4950153
    ## 11669                        Tunisia   year70    5060397
    ## 11670                        Tunisia   year71    5172691
    ## 11671                        Tunisia   year72    5287543
    ## 11672                        Tunisia   year73    5405355
    ## 11673                        Tunisia   year74    5526764
    ## 11674                        Tunisia   year75    5652476
    ## 11675                        Tunisia   year76    5781796
    ## 11676                        Tunisia   year77    5915006
    ## 11677                        Tunisia   year78    6054911
    ## 11678                        Tunisia   year79    6205212
    ## 11679                        Tunisia   year80    6368167
    ## 11680                        Tunisia   year81    6545024
    ## 11681                        Tunisia   year82    6733961
    ## 11682                        Tunisia   year83    6930387
    ## 11683                        Tunisia   year84    7127941
    ## 11684                        Tunisia   year85    7321876
    ## 11685                        Tunisia   year86    7509756
    ## 11686                        Tunisia   year87    7692254
    ## 11687                        Tunisia   year88    7871459
    ## 11688                        Tunisia   year89    8050932
    ## 11689                        Tunisia   year90    8232797
    ## 11690                        Tunisia   year91    8417684
    ## 11691                        Tunisia   year92    8603225
    ## 11692                        Tunisia   year93    8784888
    ## 11693                        Tunisia   year94    8956596
    ## 11694                        Tunisia   year95    9113975
    ## 11695                        Tunisia   year96    9256037
    ## 11696                        Tunisia   year97    9384152
    ## 11697                        Tunisia   year98    9499395
    ## 11698                        Tunisia   year99    9603742
    ## 11699                        Tunisia year2000    9699197
    ## 11700                        Tunisia year2001    9785701
    ## 11701                        Tunisia year2002    9864326
    ## 11702                        Tunisia year2003    9939678
    ## 11703                        Tunisia year2004   10017601
    ## 11704                        Tunisia year2005   10102482
    ## 11705                        Tunisia year2006   10196136
    ## 11706                        Tunisia year2007   10298087
    ## 11707                        Tunisia year2008   10407336
    ## 11708                        Tunisia year2009   10521834
    ## 11709                        Tunisia year2010   10639931
    ## 11710                        Tunisia year2011   10761467
    ## 11711                        Tunisia year2012   10886668
    ## 11712                        Tunisia year2013   11014558
    ## 11713                        Tunisia year2014   11143908
    ## 11714                        Tunisia year2015   11273661
    ## 11715                        Tunisia year2016   11403248
    ## 11716                        Tunisia year2017   11532127
    ## 11717                         Turkey   year60   27472331
    ## 11718                         Turkey   year61   28146893
    ## 11719                         Turkey   year62   28832805
    ## 11720                         Turkey   year63   29531342
    ## 11721                         Turkey   year64   30244232
    ## 11722                         Turkey   year65   30972965
    ## 11723                         Turkey   year66   31717477
    ## 11724                         Turkey   year67   32477961
    ## 11725                         Turkey   year68   33256432
    ## 11726                         Turkey   year69   34055361
    ## 11727                         Turkey   year70   34876267
    ## 11728                         Turkey   year71   35720568
    ## 11729                         Turkey   year72   36587225
    ## 11730                         Turkey   year73   37472298
    ## 11731                         Turkey   year74   38370241
    ## 11732                         Turkey   year75   39277211
    ## 11733                         Turkey   year76   40189511
    ## 11734                         Turkey   year77   41108248
    ## 11735                         Turkey   year78   42039935
    ## 11736                         Turkey   year79   42993991
    ## 11737                         Turkey   year80   43975921
    ## 11738                         Turkey   year81   44988356
    ## 11739                         Turkey   year82   46025357
    ## 11740                         Turkey   year83   47073422
    ## 11741                         Turkey   year84   48114105
    ## 11742                         Turkey   year85   49133883
    ## 11743                         Turkey   year86   50128489
    ## 11744                         Turkey   year87   51100878
    ## 11745                         Turkey   year88   52053704
    ## 11746                         Turkey   year89   52992429
    ## 11747                         Turkey   year90   53921699
    ## 11748                         Turkey   year91   54840531
    ## 11749                         Turkey   year92   55748875
    ## 11750                         Turkey   year93   56653729
    ## 11751                         Turkey   year94   57564132
    ## 11752                         Turkey   year95   58486381
    ## 11753                         Turkey   year96   59423208
    ## 11754                         Turkey   year97   60372499
    ## 11755                         Turkey   year98   61329590
    ## 11756                         Turkey   year99   62287326
    ## 11757                         Turkey year2000   63240121
    ## 11758                         Turkey year2001   64191474
    ## 11759                         Turkey year2002   65143054
    ## 11760                         Turkey year2003   66085803
    ## 11761                         Turkey year2004   67007855
    ## 11762                         Turkey year2005   67903406
    ## 11763                         Turkey year2006   68763405
    ## 11764                         Turkey year2007   69597281
    ## 11765                         Turkey year2008   70440032
    ## 11766                         Turkey year2009   71339185
    ## 11767                         Turkey year2010   72326914
    ## 11768                         Turkey year2011   73409455
    ## 11769                         Turkey year2012   74569867
    ## 11770                         Turkey year2013   75787333
    ## 11771                         Turkey year2014   77030628
    ## 11772                         Turkey year2015   78271472
    ## 11773                         Turkey year2016   79512426
    ## 11774                         Turkey year2017   80745020
    ## 11775                   Turkmenistan   year60    1603258
    ## 11776                   Turkmenistan   year61    1658362
    ## 11777                   Turkmenistan   year62    1715408
    ## 11778                   Turkmenistan   year63    1773853
    ## 11779                   Turkmenistan   year64    1833063
    ## 11780                   Turkmenistan   year65    1892599
    ## 11781                   Turkmenistan   year66    1952141
    ## 11782                   Turkmenistan   year67    2011763
    ## 11783                   Turkmenistan   year68    2071789
    ## 11784                   Turkmenistan   year69    2132799
    ## 11785                   Turkmenistan   year70    2195173
    ## 11786                   Turkmenistan   year71    2258964
    ## 11787                   Turkmenistan   year72    2324013
    ## 11788                   Turkmenistan   year73    2390213
    ## 11789                   Turkmenistan   year74    2457382
    ## 11790                   Turkmenistan   year75    2525361
    ## 11791                   Turkmenistan   year76    2594311
    ## 11792                   Turkmenistan   year77    2664257
    ## 11793                   Turkmenistan   year78    2734896
    ## 11794                   Turkmenistan   year79    2805818
    ## 11795                   Turkmenistan   year80    2876808
    ## 11796                   Turkmenistan   year81    2947779
    ## 11797                   Turkmenistan   year82    3019066
    ## 11798                   Turkmenistan   year83    3091511
    ## 11799                   Turkmenistan   year84    3166221
    ## 11800                   Turkmenistan   year85    3244018
    ## 11801                   Turkmenistan   year86    3324456
    ## 11802                   Turkmenistan   year87    3407319
    ## 11803                   Turkmenistan   year88    3493894
    ## 11804                   Turkmenistan   year89    3585867
    ## 11805                   Turkmenistan   year90    3683966
    ## 11806                   Turkmenistan   year91    3789185
    ## 11807                   Turkmenistan   year92    3899843
    ## 11808                   Turkmenistan   year93    4010789
    ## 11809                   Turkmenistan   year94    4115099
    ## 11810                   Turkmenistan   year95    4207840
    ## 11811                   Turkmenistan   year96    4287344
    ## 11812                   Turkmenistan   year97    4355114
    ## 11813                   Turkmenistan   year98    4413477
    ## 11814                   Turkmenistan   year99    4466132
    ## 11815                   Turkmenistan year2000    4516131
    ## 11816                   Turkmenistan year2001    4564080
    ## 11817                   Turkmenistan year2002    4610002
    ## 11818                   Turkmenistan year2003    4655741
    ## 11819                   Turkmenistan year2004    4703398
    ## 11820                   Turkmenistan year2005    4754641
    ## 11821                   Turkmenistan year2006    4810105
    ## 11822                   Turkmenistan year2007    4870137
    ## 11823                   Turkmenistan year2008    4935762
    ## 11824                   Turkmenistan year2009    5007950
    ## 11825                   Turkmenistan year2010    5087210
    ## 11826                   Turkmenistan year2011    5174061
    ## 11827                   Turkmenistan year2012    5267839
    ## 11828                   Turkmenistan year2013    5366277
    ## 11829                   Turkmenistan year2014    5466241
    ## 11830                   Turkmenistan year2015    5565284
    ## 11831                   Turkmenistan year2016    5662544
    ## 11832                   Turkmenistan year2017    5758075
    ## 11833       Turks and Caicos Islands   year60       5726
    ## 11834       Turks and Caicos Islands   year61       5763
    ## 11835       Turks and Caicos Islands   year62       5763
    ## 11836       Turks and Caicos Islands   year63       5740
    ## 11837       Turks and Caicos Islands   year64       5710
    ## 11838       Turks and Caicos Islands   year65       5672
    ## 11839       Turks and Caicos Islands   year66       5629
    ## 11840       Turks and Caicos Islands   year67       5590
    ## 11841       Turks and Caicos Islands   year68       5559
    ## 11842       Turks and Caicos Islands   year69       5571
    ## 11843       Turks and Caicos Islands   year70       5633
    ## 11844       Turks and Caicos Islands   year71       5756
    ## 11845       Turks and Caicos Islands   year72       5922
    ## 11846       Turks and Caicos Islands   year73       6126
    ## 11847       Turks and Caicos Islands   year74       6346
    ## 11848       Turks and Caicos Islands   year75       6548
    ## 11849       Turks and Caicos Islands   year76       6723
    ## 11850       Turks and Caicos Islands   year77       6886
    ## 11851       Turks and Caicos Islands   year78       7053
    ## 11852       Turks and Caicos Islands   year79       7264
    ## 11853       Turks and Caicos Islands   year80       7519
    ## 11854       Turks and Caicos Islands   year81       7858
    ## 11855       Turks and Caicos Islands   year82       8244
    ## 11856       Turks and Caicos Islands   year83       8669
    ## 11857       Turks and Caicos Islands   year84       9095
    ## 11858       Turks and Caicos Islands   year85       9506
    ## 11859       Turks and Caicos Islands   year86       9875
    ## 11860       Turks and Caicos Islands   year87      10224
    ## 11861       Turks and Caicos Islands   year88      10582
    ## 11862       Turks and Caicos Islands   year89      11017
    ## 11863       Turks and Caicos Islands   year90      11552
    ## 11864       Turks and Caicos Islands   year91      12206
    ## 11865       Turks and Caicos Islands   year92      12968
    ## 11866       Turks and Caicos Islands   year93      13789
    ## 11867       Turks and Caicos Islands   year94      14597
    ## 11868       Turks and Caicos Islands   year95      15332
    ## 11869       Turks and Caicos Islands   year96      15966
    ## 11870       Turks and Caicos Islands   year97      16528
    ## 11871       Turks and Caicos Islands   year98      17115
    ## 11872       Turks and Caicos Islands   year99      17864
    ## 11873       Turks and Caicos Islands year2000      18873
    ## 11874       Turks and Caicos Islands year2001      20185
    ## 11875       Turks and Caicos Islands year2002      21742
    ## 11876       Turks and Caicos Islands year2003      23410
    ## 11877       Turks and Caicos Islands year2004      25028
    ## 11878       Turks and Caicos Islands year2005      26448
    ## 11879       Turks and Caicos Islands year2006      27642
    ## 11880       Turks and Caicos Islands year2007      28640
    ## 11881       Turks and Caicos Islands year2008      29481
    ## 11882       Turks and Caicos Islands year2009      30245
    ## 11883       Turks and Caicos Islands year2010      30994
    ## 11884       Turks and Caicos Islands year2011      31731
    ## 11885       Turks and Caicos Islands year2012      32431
    ## 11886       Turks and Caicos Islands year2013      33108
    ## 11887       Turks and Caicos Islands year2014      33739
    ## 11888       Turks and Caicos Islands year2015      34339
    ## 11889       Turks and Caicos Islands year2016      34900
    ## 11890       Turks and Caicos Islands year2017      35446
    ## 11891                         Tuvalu   year60       6104
    ## 11892                         Tuvalu   year61       6246
    ## 11893                         Tuvalu   year62       6389
    ## 11894                         Tuvalu   year63       6538
    ## 11895                         Tuvalu   year64       6684
    ## 11896                         Tuvalu   year65       6815
    ## 11897                         Tuvalu   year66       6938
    ## 11898                         Tuvalu   year67       7040
    ## 11899                         Tuvalu   year68       7133
    ## 11900                         Tuvalu   year69       7214
    ## 11901                         Tuvalu   year70       7303
    ## 11902                         Tuvalu   year71       7381
    ## 11903                         Tuvalu   year72       7458
    ## 11904                         Tuvalu   year73       7537
    ## 11905                         Tuvalu   year74       7616
    ## 11906                         Tuvalu   year75       7677
    ## 11907                         Tuvalu   year76       7749
    ## 11908                         Tuvalu   year77       7816
    ## 11909                         Tuvalu   year78       7888
    ## 11910                         Tuvalu   year79       7962
    ## 11911                         Tuvalu   year80       8052
    ## 11912                         Tuvalu   year81       8154
    ## 11913                         Tuvalu   year82       8284
    ## 11914                         Tuvalu   year83       8413
    ## 11915                         Tuvalu   year84       8530
    ## 11916                         Tuvalu   year85       8650
    ## 11917                         Tuvalu   year86       8747
    ## 11918                         Tuvalu   year87       8820
    ## 11919                         Tuvalu   year88       8883
    ## 11920                         Tuvalu   year89       8947
    ## 11921                         Tuvalu   year90       9003
    ## 11922                         Tuvalu   year91       9053
    ## 11923                         Tuvalu   year92       9109
    ## 11924                         Tuvalu   year93       9156
    ## 11925                         Tuvalu   year94       9190
    ## 11926                         Tuvalu   year95       9230
    ## 11927                         Tuvalu   year96       9256
    ## 11928                         Tuvalu   year97       9277
    ## 11929                         Tuvalu   year98       9306
    ## 11930                         Tuvalu   year99       9345
    ## 11931                         Tuvalu year2000       9420
    ## 11932                         Tuvalu year2001       9512
    ## 11933                         Tuvalu year2002       9635
    ## 11934                         Tuvalu year2003       9767
    ## 11935                         Tuvalu year2004       9894
    ## 11936                         Tuvalu year2005      10027
    ## 11937                         Tuvalu year2006      10137
    ## 11938                         Tuvalu year2007      10243
    ## 11939                         Tuvalu year2008      10340
    ## 11940                         Tuvalu year2009      10441
    ## 11941                         Tuvalu year2010      10531
    ## 11942                         Tuvalu year2011      10628
    ## 11943                         Tuvalu year2012      10725
    ## 11944                         Tuvalu year2013      10819
    ## 11945                         Tuvalu year2014      10908
    ## 11946                         Tuvalu year2015      11001
    ## 11947                         Tuvalu year2016      11097
    ## 11948                         Tuvalu year2017      11192
    ## 11949                         Uganda   year60    6788214
    ## 11950                         Uganda   year61    7006633
    ## 11951                         Uganda   year62    7240174
    ## 11952                         Uganda   year63    7487429
    ## 11953                         Uganda   year64    7746198
    ## 11954                         Uganda   year65    8014401
    ## 11955                         Uganda   year66    8292776
    ## 11956                         Uganda   year67    8580676
    ## 11957                         Uganda   year68    8872920
    ## 11958                         Uganda   year69    9162833
    ## 11959                         Uganda   year70    9446064
    ## 11960                         Uganda   year71    9720399
    ## 11961                         Uganda   year72    9988380
    ## 11962                         Uganda   year73   10256429
    ## 11963                         Uganda   year74   10533716
    ## 11964                         Uganda   year75   10827147
    ## 11965                         Uganda   year76   11139833
    ## 11966                         Uganda   year77   11470867
    ## 11967                         Uganda   year78   11818307
    ## 11968                         Uganda   year79   12178544
    ## 11969                         Uganda   year80   12549540
    ## 11970                         Uganda   year81   12930209
    ## 11971                         Uganda   year82   13323332
    ## 11972                         Uganda   year83   13735271
    ## 11973                         Uganda   year84   14174470
    ## 11974                         Uganda   year85   14646624
    ## 11975                         Uganda   year86   15154521
    ## 11976                         Uganda   year87   15695411
    ## 11977                         Uganda   year88   16262533
    ## 11978                         Uganda   year89   16846090
    ## 11979                         Uganda   year90   17438907
    ## 11980                         Uganda   year91   18040438
    ## 11981                         Uganda   year92   18652889
    ## 11982                         Uganda   year93   19275422
    ## 11983                         Uganda   year94   19907634
    ## 11984                         Uganda   year95   20550291
    ## 11985                         Uganda   year96   21202118
    ## 11986                         Uganda   year97   21865931
    ## 11987                         Uganda   year98   22551789
    ## 11988                         Uganda   year99   23272995
    ## 11989                         Uganda year2000   24039274
    ## 11990                         Uganda year2001   24854892
    ## 11991                         Uganda year2002   25718048
    ## 11992                         Uganda year2003   26624820
    ## 11993                         Uganda year2004   27568436
    ## 11994                         Uganda year2005   28543940
    ## 11995                         Uganda year2006   29550662
    ## 11996                         Uganda year2007   30590487
    ## 11997                         Uganda year2008   31663896
    ## 11998                         Uganda year2009   32771895
    ## 11999                         Uganda year2010   33915133
    ## 12000                         Uganda year2011   35093648
    ## 12001                         Uganda year2012   36306796
    ## 12002                         Uganda year2013   37553726
    ## 12003                         Uganda year2014   38833338
    ## 12004                         Uganda year2015   40144870
    ## 12005                         Uganda year2016   41487965
    ## 12006                         Uganda year2017   42862958
    ## 12007                        Ukraine   year60   42662149
    ## 12008                        Ukraine   year61   43203635
    ## 12009                        Ukraine   year62   43749470
    ## 12010                        Ukraine   year63   44285899
    ## 12011                        Ukraine   year64   44794327
    ## 12012                        Ukraine   year65   45261935
    ## 12013                        Ukraine   year66   45682308
    ## 12014                        Ukraine   year67   46060452
    ## 12015                        Ukraine   year68   46409002
    ## 12016                        Ukraine   year69   46746669
    ## 12017                        Ukraine   year70   47086761
    ## 12018                        Ukraine   year71   47433805
    ## 12019                        Ukraine   year72   47783011
    ## 12020                        Ukraine   year73   48127172
    ## 12021                        Ukraine   year74   48455122
    ## 12022                        Ukraine   year75   48758987
    ## 12023                        Ukraine   year76   49036456
    ## 12024                        Ukraine   year77   49290905
    ## 12025                        Ukraine   year78   49526883
    ## 12026                        Ukraine   year79   49751257
    ## 12027                        Ukraine   year80   49968812
    ## 12028                        Ukraine   year81   50221000
    ## 12029                        Ukraine   year82   50384000
    ## 12030                        Ukraine   year83   50564000
    ## 12031                        Ukraine   year84   50754000
    ## 12032                        Ukraine   year85   50917000
    ## 12033                        Ukraine   year86   51097000
    ## 12034                        Ukraine   year87   51293000
    ## 12035                        Ukraine   year88   51521000
    ## 12036                        Ukraine   year89   51773000
    ## 12037                        Ukraine   year90   51892000
    ## 12038                        Ukraine   year91   52000470
    ## 12039                        Ukraine   year92   52150266
    ## 12040                        Ukraine   year93   52179210
    ## 12041                        Ukraine   year94   51921041
    ## 12042                        Ukraine   year95   51512299
    ## 12043                        Ukraine   year96   51057189
    ## 12044                        Ukraine   year97   50594105
    ## 12045                        Ukraine   year98   50143939
    ## 12046                        Ukraine   year99   49673350
    ## 12047                        Ukraine year2000   49175848
    ## 12048                        Ukraine year2001   48683865
    ## 12049                        Ukraine year2002   48202500
    ## 12050                        Ukraine year2003   47812950
    ## 12051                        Ukraine year2004   47451600
    ## 12052                        Ukraine year2005   47105150
    ## 12053                        Ukraine year2006   46787750
    ## 12054                        Ukraine year2007   46509350
    ## 12055                        Ukraine year2008   46258200
    ## 12056                        Ukraine year2009   46053300
    ## 12057                        Ukraine year2010   45870700
    ## 12058                        Ukraine year2011   45706100
    ## 12059                        Ukraine year2012   45593300
    ## 12060                        Ukraine year2013   45489600
    ## 12061                        Ukraine year2014   45271947
    ## 12062                        Ukraine year2015   45154029
    ## 12063                        Ukraine year2016   45004645
    ## 12064                        Ukraine year2017   44831159
    ## 12065           United Arab Emirates   year60      92634
    ## 12066           United Arab Emirates   year61     101078
    ## 12067           United Arab Emirates   year62     112472
    ## 12068           United Arab Emirates   year63     125566
    ## 12069           United Arab Emirates   year64     138529
    ## 12070           United Arab Emirates   year65     150362
    ## 12071           United Arab Emirates   year66     160481
    ## 12072           United Arab Emirates   year67     170283
    ## 12073           United Arab Emirates   year68     183194
    ## 12074           United Arab Emirates   year69     203820
    ## 12075           United Arab Emirates   year70     235499
    ## 12076           United Arab Emirates   year71     278808
    ## 12077           United Arab Emirates   year72     332760
    ## 12078           United Arab Emirates   year73     397174
    ## 12079           United Arab Emirates   year74     471364
    ## 12080           United Arab Emirates   year75     554324
    ## 12081           United Arab Emirates   year76     646943
    ## 12082           United Arab Emirates   year77     748117
    ## 12083           United Arab Emirates   year78     852262
    ## 12084           United Arab Emirates   year79     952040
    ## 12085           United Arab Emirates   year80    1042384
    ## 12086           United Arab Emirates   year81    1120900
    ## 12087           United Arab Emirates   year82    1189545
    ## 12088           United Arab Emirates   year83    1253060
    ## 12089           United Arab Emirates   year84    1318478
    ## 12090           United Arab Emirates   year85    1391052
    ## 12091           United Arab Emirates   year86    1472218
    ## 12092           United Arab Emirates   year87    1560718
    ## 12093           United Arab Emirates   year88    1655849
    ## 12094           United Arab Emirates   year89    1756043
    ## 12095           United Arab Emirates   year90    1860174
    ## 12096           United Arab Emirates   year91    1970026
    ## 12097           United Arab Emirates   year92    2086639
    ## 12098           United Arab Emirates   year93    2207405
    ## 12099           United Arab Emirates   year94    2328686
    ## 12100           United Arab Emirates   year95    2448820
    ## 12101           United Arab Emirates   year96    2571020
    ## 12102           United Arab Emirates   year97    2700010
    ## 12103           United Arab Emirates   year98    2838145
    ## 12104           United Arab Emirates   year99    2988162
    ## 12105           United Arab Emirates year2000    3154925
    ## 12106           United Arab Emirates year2001    3326032
    ## 12107           United Arab Emirates year2002    3507232
    ## 12108           United Arab Emirates year2003    3741932
    ## 12109           United Arab Emirates year2004    4087931
    ## 12110           United Arab Emirates year2005    4579562
    ## 12111           United Arab Emirates year2006    5242032
    ## 12112           United Arab Emirates year2007    6044067
    ## 12113           United Arab Emirates year2008    6894278
    ## 12114           United Arab Emirates year2009    7666393
    ## 12115           United Arab Emirates year2010    8270684
    ## 12116           United Arab Emirates year2011    8672475
    ## 12117           United Arab Emirates year2012    8900453
    ## 12118           United Arab Emirates year2013    9006263
    ## 12119           United Arab Emirates year2014    9070867
    ## 12120           United Arab Emirates year2015    9154302
    ## 12121           United Arab Emirates year2016    9269612
    ## 12122           United Arab Emirates year2017    9400145
    ## 12123                 United Kingdom   year60   52400000
    ## 12124                 United Kingdom   year61   52800000
    ## 12125                 United Kingdom   year62   53250000
    ## 12126                 United Kingdom   year63   53650000
    ## 12127                 United Kingdom   year64   54000000
    ## 12128                 United Kingdom   year65   54348050
    ## 12129                 United Kingdom   year66   54648500
    ## 12130                 United Kingdom   year67   54943600
    ## 12131                 United Kingdom   year68   55211700
    ## 12132                 United Kingdom   year69   55441750
    ## 12133                 United Kingdom   year70   55663250
    ## 12134                 United Kingdom   year71   55896223
    ## 12135                 United Kingdom   year72   56086065
    ## 12136                 United Kingdom   year73   56194527
    ## 12137                 United Kingdom   year74   56229974
    ## 12138                 United Kingdom   year75   56225800
    ## 12139                 United Kingdom   year76   56211968
    ## 12140                 United Kingdom   year77   56193492
    ## 12141                 United Kingdom   year78   56196504
    ## 12142                 United Kingdom   year79   56246951
    ## 12143                 United Kingdom   year80   56314216
    ## 12144                 United Kingdom   year81   56333829
    ## 12145                 United Kingdom   year82   56313641
    ## 12146                 United Kingdom   year83   56332848
    ## 12147                 United Kingdom   year84   56422072
    ## 12148                 United Kingdom   year85   56550268
    ## 12149                 United Kingdom   year86   56681396
    ## 12150                 United Kingdom   year87   56802050
    ## 12151                 United Kingdom   year88   56928327
    ## 12152                 United Kingdom   year89   57076711
    ## 12153                 United Kingdom   year90   57247586
    ## 12154                 United Kingdom   year91   57424897
    ## 12155                 United Kingdom   year92   57580402
    ## 12156                 United Kingdom   year93   57718614
    ## 12157                 United Kingdom   year94   57865745
    ## 12158                 United Kingdom   year95   58019030
    ## 12159                 United Kingdom   year96   58166950
    ## 12160                 United Kingdom   year97   58316954
    ## 12161                 United Kingdom   year98   58487141
    ## 12162                 United Kingdom   year99   58682466
    ## 12163                 United Kingdom year2000   58892514
    ## 12164                 United Kingdom year2001   59119673
    ## 12165                 United Kingdom year2002   59370479
    ## 12166                 United Kingdom year2003   59647577
    ## 12167                 United Kingdom year2004   59987905
    ## 12168                 United Kingdom year2005   60401206
    ## 12169                 United Kingdom year2006   60846820
    ## 12170                 United Kingdom year2007   61322463
    ## 12171                 United Kingdom year2008   61806995
    ## 12172                 United Kingdom year2009   62276270
    ## 12173                 United Kingdom year2010   62766365
    ## 12174                 United Kingdom year2011   63258918
    ## 12175                 United Kingdom year2012   63700300
    ## 12176                 United Kingdom year2013   64128226
    ## 12177                 United Kingdom year2014   64613160
    ## 12178                 United Kingdom year2015   65128861
    ## 12179                 United Kingdom year2016   65595565
    ## 12180                 United Kingdom year2017   66022273
    ## 12181                  United States   year60  180671000
    ## 12182                  United States   year61  183691000
    ## 12183                  United States   year62  186538000
    ## 12184                  United States   year63  189242000
    ## 12185                  United States   year64  191889000
    ## 12186                  United States   year65  194303000
    ## 12187                  United States   year66  196560000
    ## 12188                  United States   year67  198712000
    ## 12189                  United States   year68  200706000
    ## 12190                  United States   year69  202677000
    ## 12191                  United States   year70  205052000
    ## 12192                  United States   year71  207661000
    ## 12193                  United States   year72  209896000
    ## 12194                  United States   year73  211909000
    ## 12195                  United States   year74  213854000
    ## 12196                  United States   year75  215973000
    ## 12197                  United States   year76  218035000
    ## 12198                  United States   year77  220239000
    ## 12199                  United States   year78  222585000
    ## 12200                  United States   year79  225055000
    ## 12201                  United States   year80  227225000
    ## 12202                  United States   year81  229466000
    ## 12203                  United States   year82  231664000
    ## 12204                  United States   year83  233792000
    ## 12205                  United States   year84  235825000
    ## 12206                  United States   year85  237924000
    ## 12207                  United States   year86  240133000
    ## 12208                  United States   year87  242289000
    ## 12209                  United States   year88  244499000
    ## 12210                  United States   year89  246819000
    ## 12211                  United States   year90  249623000
    ## 12212                  United States   year91  252981000
    ## 12213                  United States   year92  256514000
    ## 12214                  United States   year93  259919000
    ## 12215                  United States   year94  263126000
    ## 12216                  United States   year95  266278000
    ## 12217                  United States   year96  269394000
    ## 12218                  United States   year97  272657000
    ## 12219                  United States   year98  275854000
    ## 12220                  United States   year99  279040000
    ## 12221                  United States year2000  282162411
    ## 12222                  United States year2001  284968955
    ## 12223                  United States year2002  287625193
    ## 12224                  United States year2003  290107933
    ## 12225                  United States year2004  292805298
    ## 12226                  United States year2005  295516599
    ## 12227                  United States year2006  298379912
    ## 12228                  United States year2007  301231207
    ## 12229                  United States year2008  304093966
    ## 12230                  United States year2009  306771529
    ## 12231                  United States year2010  309338421
    ## 12232                  United States year2011  311644280
    ## 12233                  United States year2012  313993272
    ## 12234                  United States year2013  316234505
    ## 12235                  United States year2014  318622525
    ## 12236                  United States year2015  321039839
    ## 12237                  United States year2016  323405935
    ## 12238                  United States year2017  325719178
    ## 12239                        Uruguay   year60    2538651
    ## 12240                        Uruguay   year61    2571690
    ## 12241                        Uruguay   year62    2603887
    ## 12242                        Uruguay   year63    2635129
    ## 12243                        Uruguay   year64    2665390
    ## 12244                        Uruguay   year65    2694537
    ## 12245                        Uruguay   year66    2722877
    ## 12246                        Uruguay   year67    2750093
    ## 12247                        Uruguay   year68    2774774
    ## 12248                        Uruguay   year69    2795046
    ## 12249                        Uruguay   year70    2809803
    ## 12250                        Uruguay   year71    2818270
    ## 12251                        Uruguay   year72    2821439
    ## 12252                        Uruguay   year73    2822081
    ## 12253                        Uruguay   year74    2824069
    ## 12254                        Uruguay   year75    2830172
    ## 12255                        Uruguay   year76    2841429
    ## 12256                        Uruguay   year77    2857105
    ## 12257                        Uruguay   year78    2875966
    ## 12258                        Uruguay   year79    2896023
    ## 12259                        Uruguay   year80    2915778
    ## 12260                        Uruguay   year81    2935036
    ## 12261                        Uruguay   year82    2954282
    ## 12262                        Uruguay   year83    2973463
    ## 12263                        Uruguay   year84    2992645
    ## 12264                        Uruguay   year85    3011908
    ## 12265                        Uruguay   year86    3031038
    ## 12266                        Uruguay   year87    3049966
    ## 12267                        Uruguay   year88    3069099
    ## 12268                        Uruguay   year89    3088984
    ## 12269                        Uruguay   year90    3109989
    ## 12270                        Uruguay   year91    3132050
    ## 12271                        Uruguay   year92    3154855
    ## 12272                        Uruguay   year93    3178155
    ## 12273                        Uruguay   year94    3201607
    ## 12274                        Uruguay   year95    3224804
    ## 12275                        Uruguay   year96    3248035
    ## 12276                        Uruguay   year97    3271010
    ## 12277                        Uruguay   year98    3292138
    ## 12278                        Uruguay   year99    3309318
    ## 12279                        Uruguay year2000    3321245
    ## 12280                        Uruguay year2001    3327103
    ## 12281                        Uruguay year2002    3327773
    ## 12282                        Uruguay year2003    3325637
    ## 12283                        Uruguay year2004    3324096
    ## 12284                        Uruguay year2005    3325612
    ## 12285                        Uruguay year2006    3331043
    ## 12286                        Uruguay year2007    3339741
    ## 12287                        Uruguay year2008    3350824
    ## 12288                        Uruguay year2009    3362755
    ## 12289                        Uruguay year2010    3374415
    ## 12290                        Uruguay year2011    3385624
    ## 12291                        Uruguay year2012    3396777
    ## 12292                        Uruguay year2013    3408005
    ## 12293                        Uruguay year2014    3419546
    ## 12294                        Uruguay year2015    3431552
    ## 12295                        Uruguay year2016    3444006
    ## 12296                        Uruguay year2017    3456750
    ## 12297                     Uzbekistan   year60    8549493
    ## 12298                     Uzbekistan   year61    8837349
    ## 12299                     Uzbekistan   year62    9138097
    ## 12300                     Uzbekistan   year63    9454250
    ## 12301                     Uzbekistan   year64    9788986
    ## 12302                     Uzbekistan   year65   10143740
    ## 12303                     Uzbekistan   year66   10520879
    ## 12304                     Uzbekistan   year67   10917446
    ## 12305                     Uzbekistan   year68   11323095
    ## 12306                     Uzbekistan   year69   11723846
    ## 12307                     Uzbekistan   year70   12110028
    ## 12308                     Uzbekistan   year71   12477058
    ## 12309                     Uzbekistan   year72   12828625
    ## 12310                     Uzbekistan   year73   13173590
    ## 12311                     Uzbekistan   year74   13525094
    ## 12312                     Uzbekistan   year75   13892638
    ## 12313                     Uzbekistan   year76   14279120
    ## 12314                     Uzbekistan   year77   14681459
    ## 12315                     Uzbekistan   year78   15096012
    ## 12316                     Uzbekistan   year79   15516862
    ## 12317                     Uzbekistan   year80   15939744
    ## 12318                     Uzbekistan   year81   16363562
    ## 12319                     Uzbekistan   year82   16790069
    ## 12320                     Uzbekistan   year83   17221212
    ## 12321                     Uzbekistan   year84   17659975
    ## 12322                     Uzbekistan   year85   18108300
    ## 12323                     Uzbekistan   year86   18565477
    ## 12324                     Uzbekistan   year87   19029877
    ## 12325                     Uzbekistan   year88   19501225
    ## 12326                     Uzbekistan   year89   19979127
    ## 12327                     Uzbekistan   year90   20510000
    ## 12328                     Uzbekistan   year91   20952000
    ## 12329                     Uzbekistan   year92   21449000
    ## 12330                     Uzbekistan   year93   21942000
    ## 12331                     Uzbekistan   year94   22377000
    ## 12332                     Uzbekistan   year95   22785000
    ## 12333                     Uzbekistan   year96   23225000
    ## 12334                     Uzbekistan   year97   23667000
    ## 12335                     Uzbekistan   year98   24051000
    ## 12336                     Uzbekistan   year99   24311650
    ## 12337                     Uzbekistan year2000   24650400
    ## 12338                     Uzbekistan year2001   24964450
    ## 12339                     Uzbekistan year2002   25271850
    ## 12340                     Uzbekistan year2003   25567650
    ## 12341                     Uzbekistan year2004   25864350
    ## 12342                     Uzbekistan year2005   26167000
    ## 12343                     Uzbekistan year2006   26488250
    ## 12344                     Uzbekistan year2007   26868000
    ## 12345                     Uzbekistan year2008   27302800
    ## 12346                     Uzbekistan year2009   27767400
    ## 12347                     Uzbekistan year2010   28562400
    ## 12348                     Uzbekistan year2011   29339400
    ## 12349                     Uzbekistan year2012   29774500
    ## 12350                     Uzbekistan year2013   30243200
    ## 12351                     Uzbekistan year2014   30757700
    ## 12352                     Uzbekistan year2015   31298900
    ## 12353                     Uzbekistan year2016   31847900
    ## 12354                     Uzbekistan year2017   32387200
    ## 12355                        Vanuatu   year60      63699
    ## 12356                        Vanuatu   year61      65713
    ## 12357                        Vanuatu   year62      67808
    ## 12358                        Vanuatu   year63      69964
    ## 12359                        Vanuatu   year64      72131
    ## 12360                        Vanuatu   year65      74289
    ## 12361                        Vanuatu   year66      76413
    ## 12362                        Vanuatu   year67      78522
    ## 12363                        Vanuatu   year68      80673
    ## 12364                        Vanuatu   year69      82940
    ## 12365                        Vanuatu   year70      85389
    ## 12366                        Vanuatu   year71      88022
    ## 12367                        Vanuatu   year72      90823
    ## 12368                        Vanuatu   year73      93765
    ## 12369                        Vanuatu   year74      96796
    ## 12370                        Vanuatu   year75      99872
    ## 12371                        Vanuatu   year76     103028
    ## 12372                        Vanuatu   year77     106222
    ## 12373                        Vanuatu   year78     109429
    ## 12374                        Vanuatu   year79     112580
    ## 12375                        Vanuatu   year80     115632
    ## 12376                        Vanuatu   year81     118580
    ## 12377                        Vanuatu   year82     121435
    ## 12378                        Vanuatu   year83     124249
    ## 12379                        Vanuatu   year84     127092
    ## 12380                        Vanuatu   year85     130027
    ## 12381                        Vanuatu   year86     133038
    ## 12382                        Vanuatu   year87     136125
    ## 12383                        Vanuatu   year88     139366
    ## 12384                        Vanuatu   year89     142849
    ## 12385                        Vanuatu   year90     146634
    ## 12386                        Vanuatu   year91     150778
    ## 12387                        Vanuatu   year92     155243
    ## 12388                        Vanuatu   year93     159814
    ## 12389                        Vanuatu   year94     164208
    ## 12390                        Vanuatu   year95     168235
    ## 12391                        Vanuatu   year96     171801
    ## 12392                        Vanuatu   year97     174999
    ## 12393                        Vanuatu   year98     178078
    ## 12394                        Vanuatu   year99     181345
    ## 12395                        Vanuatu year2000     185063
    ## 12396                        Vanuatu year2001     189290
    ## 12397                        Vanuatu year2002     193956
    ## 12398                        Vanuatu year2003     198964
    ## 12399                        Vanuatu year2004     204143
    ## 12400                        Vanuatu year2005     209370
    ## 12401                        Vanuatu year2006     214634
    ## 12402                        Vanuatu year2007     219953
    ## 12403                        Vanuatu year2008     225340
    ## 12404                        Vanuatu year2009     230785
    ## 12405                        Vanuatu year2010     236295
    ## 12406                        Vanuatu year2011     241871
    ## 12407                        Vanuatu year2012     247485
    ## 12408                        Vanuatu year2013     253142
    ## 12409                        Vanuatu year2014     258850
    ## 12410                        Vanuatu year2015     264603
    ## 12411                        Vanuatu year2016     270402
    ## 12412                        Vanuatu year2017     276244
    ## 12413                  Venezuela, RB   year60    8146847
    ## 12414                  Venezuela, RB   year61    8461685
    ## 12415                  Venezuela, RB   year62    8790589
    ## 12416                  Venezuela, RB   year63    9130349
    ## 12417                  Venezuela, RB   year64    9476252
    ## 12418                  Venezuela, RB   year65    9824692
    ## 12419                  Venezuela, RB   year66   10175140
    ## 12420                  Venezuela, RB   year67   10528054
    ## 12421                  Venezuela, RB   year68   10881995
    ## 12422                  Venezuela, RB   year69   11235491
    ## 12423                  Venezuela, RB   year70   11587761
    ## 12424                  Venezuela, RB   year71   11937805
    ## 12425                  Venezuela, RB   year72   12286439
    ## 12426                  Venezuela, RB   year73   12636969
    ## 12427                  Venezuela, RB   year74   12994025
    ## 12428                  Venezuela, RB   year75   13360987
    ## 12429                  Venezuela, RB   year76   13739142
    ## 12430                  Venezuela, RB   year77   14127787
    ## 12431                  Venezuela, RB   year78   14525931
    ## 12432                  Venezuela, RB   year79   14931739
    ## 12433                  Venezuela, RB   year80   15343916
    ## 12434                  Venezuela, RB   year81   15761799
    ## 12435                  Venezuela, RB   year82   16185894
    ## 12436                  Venezuela, RB   year83   16617346
    ## 12437                  Venezuela, RB   year84   17057785
    ## 12438                  Venezuela, RB   year85   17508059
    ## 12439                  Venezuela, RB   year86   17968552
    ## 12440                  Venezuela, RB   year87   18437794
    ## 12441                  Venezuela, RB   year88   18912526
    ## 12442                  Venezuela, RB   year89   19388342
    ## 12443                  Venezuela, RB   year90   19861956
    ## 12444                  Venezuela, RB   year91   20332079
    ## 12445                  Venezuela, RB   year92   20799075
    ## 12446                  Venezuela, RB   year93   21263443
    ## 12447                  Venezuela, RB   year94   21726352
    ## 12448                  Venezuela, RB   year95   22188667
    ## 12449                  Venezuela, RB   year96   22650102
    ## 12450                  Venezuela, RB   year97   23110178
    ## 12451                  Venezuela, RB   year98   23569454
    ## 12452                  Venezuela, RB   year99   24028689
    ## 12453                  Venezuela, RB year2000   24488340
    ## 12454                  Venezuela, RB year2001   24948476
    ## 12455                  Venezuela, RB year2002   25408700
    ## 12456                  Venezuela, RB year2003   25868523
    ## 12457                  Venezuela, RB year2004   26327225
    ## 12458                  Venezuela, RB year2005   26784161
    ## 12459                  Venezuela, RB year2006   27239168
    ## 12460                  Venezuela, RB year2007   27691965
    ## 12461                  Venezuela, RB year2008   28141701
    ## 12462                  Venezuela, RB year2009   28587323
    ## 12463                  Venezuela, RB year2010   29028033
    ## 12464                  Venezuela, RB year2011   29463291
    ## 12465                  Venezuela, RB year2012   29893080
    ## 12466                  Venezuela, RB year2013   30317848
    ## 12467                  Venezuela, RB year2014   30738378
    ## 12468                  Venezuela, RB year2015   31155134
    ## 12469                  Venezuela, RB year2016   31568179
    ## 12470                  Venezuela, RB year2017   31977065
    ## 12471                        Vietnam   year60   32670629
    ## 12472                        Vietnam   year61   33666772
    ## 12473                        Vietnam   year62   34684165
    ## 12474                        Vietnam   year63   35722091
    ## 12475                        Vietnam   year64   36780985
    ## 12476                        Vietnam   year65   37860012
    ## 12477                        Vietnam   year66   38959334
    ## 12478                        Vietnam   year67   40074699
    ## 12479                        Vietnam   year68   41195835
    ## 12480                        Vietnam   year69   42309665
    ## 12481                        Vietnam   year70   43407287
    ## 12482                        Vietnam   year71   44485908
    ## 12483                        Vietnam   year72   45549483
    ## 12484                        Vietnam   year73   46604726
    ## 12485                        Vietnam   year74   47661773
    ## 12486                        Vietnam   year75   48729392
    ## 12487                        Vietnam   year76   49808071
    ## 12488                        Vietnam   year77   50899504
    ## 12489                        Vietnam   year78   52015281
    ## 12490                        Vietnam   year79   53169673
    ## 12491                        Vietnam   year80   54372514
    ## 12492                        Vietnam   year81   55627746
    ## 12493                        Vietnam   year82   56931824
    ## 12494                        Vietnam   year83   58277387
    ## 12495                        Vietnam   year84   59653090
    ## 12496                        Vietnam   year85   61049373
    ## 12497                        Vietnam   year86   62459560
    ## 12498                        Vietnam   year87   63881297
    ## 12499                        Vietnam   year88   65313708
    ## 12500                        Vietnam   year89   66757402
    ## 12501                        Vietnam   year90   68209605
    ## 12502                        Vietnam   year91   69670902
    ## 12503                        Vietnam   year92   71130448
    ## 12504                        Vietnam   year93   72560427
    ## 12505                        Vietnam   year94   73925082
    ## 12506                        Vietnam   year95   75198977
    ## 12507                        Vietnam   year96   76372719
    ## 12508                        Vietnam   year97   77453335
    ## 12509                        Vietnam   year98   78452897
    ## 12510                        Vietnam   year99   79391374
    ## 12511                        Vietnam year2000   80285562
    ## 12512                        Vietnam year2001   81139919
    ## 12513                        Vietnam year2002   81956496
    ## 12514                        Vietnam year2003   82747662
    ## 12515                        Vietnam year2004   83527678
    ## 12516                        Vietnam year2005   84308843
    ## 12517                        Vietnam year2006   85094617
    ## 12518                        Vietnam year2007   85889590
    ## 12519                        Vietnam year2008   86707801
    ## 12520                        Vietnam year2009   87565407
    ## 12521                        Vietnam year2010   88472512
    ## 12522                        Vietnam year2011   89436644
    ## 12523                        Vietnam year2012   90451881
    ## 12524                        Vietnam year2013   91497725
    ## 12525                        Vietnam year2014   92544915
    ## 12526                        Vietnam year2015   93571567
    ## 12527                        Vietnam year2016   94569072
    ## 12528                        Vietnam year2017   95540800
    ## 12529          Virgin Islands (U.S.)   year60      32500
    ## 12530          Virgin Islands (U.S.)   year61      34300
    ## 12531          Virgin Islands (U.S.)   year62      35000
    ## 12532          Virgin Islands (U.S.)   year63      39800
    ## 12533          Virgin Islands (U.S.)   year64      40800
    ## 12534          Virgin Islands (U.S.)   year65      43500
    ## 12535          Virgin Islands (U.S.)   year66      46200
    ## 12536          Virgin Islands (U.S.)   year67      49100
    ## 12537          Virgin Islands (U.S.)   year68      55700
    ## 12538          Virgin Islands (U.S.)   year69      60300
    ## 12539          Virgin Islands (U.S.)   year70      63476
    ## 12540          Virgin Islands (U.S.)   year71      70937
    ## 12541          Virgin Islands (U.S.)   year72      76319
    ## 12542          Virgin Islands (U.S.)   year73      84121
    ## 12543          Virgin Islands (U.S.)   year74      89941
    ## 12544          Virgin Islands (U.S.)   year75      94484
    ## 12545          Virgin Islands (U.S.)   year76      96166
    ## 12546          Virgin Islands (U.S.)   year77      93203
    ## 12547          Virgin Islands (U.S.)   year78      95929
    ## 12548          Virgin Islands (U.S.)   year79      96183
    ## 12549          Virgin Islands (U.S.)   year80      99636
    ## 12550          Virgin Islands (U.S.)   year81      99853
    ## 12551          Virgin Islands (U.S.)   year82     100068
    ## 12552          Virgin Islands (U.S.)   year83     100348
    ## 12553          Virgin Islands (U.S.)   year84     100600
    ## 12554          Virgin Islands (U.S.)   year85     100760
    ## 12555          Virgin Islands (U.S.)   year86     100842
    ## 12556          Virgin Islands (U.S.)   year87     100901
    ## 12557          Virgin Islands (U.S.)   year88     100952
    ## 12558          Virgin Islands (U.S.)   year89     101041
    ## 12559          Virgin Islands (U.S.)   year90     103963
    ## 12560          Virgin Islands (U.S.)   year91     104807
    ## 12561          Virgin Islands (U.S.)   year92     105712
    ## 12562          Virgin Islands (U.S.)   year93     106578
    ## 12563          Virgin Islands (U.S.)   year94     107318
    ## 12564          Virgin Islands (U.S.)   year95     107818
    ## 12565          Virgin Islands (U.S.)   year96     108095
    ## 12566          Virgin Islands (U.S.)   year97     108357
    ## 12567          Virgin Islands (U.S.)   year98     108537
    ## 12568          Virgin Islands (U.S.)   year99     108599
    ## 12569          Virgin Islands (U.S.) year2000     108642
    ## 12570          Virgin Islands (U.S.) year2001     108549
    ## 12571          Virgin Islands (U.S.) year2002     108510
    ## 12572          Virgin Islands (U.S.) year2003     108506
    ## 12573          Virgin Islands (U.S.) year2004     108467
    ## 12574          Virgin Islands (U.S.) year2005     108454
    ## 12575          Virgin Islands (U.S.) year2006     108371
    ## 12576          Virgin Islands (U.S.) year2007     108339
    ## 12577          Virgin Islands (U.S.) year2008     108399
    ## 12578          Virgin Islands (U.S.) year2009     108405
    ## 12579          Virgin Islands (U.S.) year2010     108358
    ## 12580          Virgin Islands (U.S.) year2011     108292
    ## 12581          Virgin Islands (U.S.) year2012     108191
    ## 12582          Virgin Islands (U.S.) year2013     108044
    ## 12583          Virgin Islands (U.S.) year2014     107884
    ## 12584          Virgin Islands (U.S.) year2015     107710
    ## 12585          Virgin Islands (U.S.) year2016     107510
    ## 12586          Virgin Islands (U.S.) year2017     107268
    ## 12587             West Bank and Gaza   year60         NA
    ## 12588             West Bank and Gaza   year61         NA
    ## 12589             West Bank and Gaza   year62         NA
    ## 12590             West Bank and Gaza   year63         NA
    ## 12591             West Bank and Gaza   year64         NA
    ## 12592             West Bank and Gaza   year65         NA
    ## 12593             West Bank and Gaza   year66         NA
    ## 12594             West Bank and Gaza   year67         NA
    ## 12595             West Bank and Gaza   year68         NA
    ## 12596             West Bank and Gaza   year69         NA
    ## 12597             West Bank and Gaza   year70         NA
    ## 12598             West Bank and Gaza   year71         NA
    ## 12599             West Bank and Gaza   year72         NA
    ## 12600             West Bank and Gaza   year73         NA
    ## 12601             West Bank and Gaza   year74         NA
    ## 12602             West Bank and Gaza   year75         NA
    ## 12603             West Bank and Gaza   year76         NA
    ## 12604             West Bank and Gaza   year77         NA
    ## 12605             West Bank and Gaza   year78         NA
    ## 12606             West Bank and Gaza   year79         NA
    ## 12607             West Bank and Gaza   year80         NA
    ## 12608             West Bank and Gaza   year81         NA
    ## 12609             West Bank and Gaza   year82         NA
    ## 12610             West Bank and Gaza   year83         NA
    ## 12611             West Bank and Gaza   year84         NA
    ## 12612             West Bank and Gaza   year85         NA
    ## 12613             West Bank and Gaza   year86         NA
    ## 12614             West Bank and Gaza   year87         NA
    ## 12615             West Bank and Gaza   year88         NA
    ## 12616             West Bank and Gaza   year89         NA
    ## 12617             West Bank and Gaza   year90    1978248
    ## 12618             West Bank and Gaza   year91    2068845
    ## 12619             West Bank and Gaza   year92    2163591
    ## 12620             West Bank and Gaza   year93    2262676
    ## 12621             West Bank and Gaza   year94    2366298
    ## 12622             West Bank and Gaza   year95    2474666
    ## 12623             West Bank and Gaza   year96    2587997
    ## 12624             West Bank and Gaza   year97    2706518
    ## 12625             West Bank and Gaza   year98    2776568
    ## 12626             West Bank and Gaza   year99    2848431
    ## 12627             West Bank and Gaza year2000    2922153
    ## 12628             West Bank and Gaza year2001    2997784
    ## 12629             West Bank and Gaza year2002    3075373
    ## 12630             West Bank and Gaza year2003    3154969
    ## 12631             West Bank and Gaza year2004    3236626
    ## 12632             West Bank and Gaza year2005    3320396
    ## 12633             West Bank and Gaza year2006    3406334
    ## 12634             West Bank and Gaza year2007    3494496
    ## 12635             West Bank and Gaza year2008    3596688
    ## 12636             West Bank and Gaza year2009    3702218
    ## 12637             West Bank and Gaza year2010    3811102
    ## 12638             West Bank and Gaza year2011    3927051
    ## 12639             West Bank and Gaza year2012    4046901
    ## 12640             West Bank and Gaza year2013    4169506
    ## 12641             West Bank and Gaza year2014    4294682
    ## 12642             West Bank and Gaza year2015    4422143
    ## 12643             West Bank and Gaza year2016    4551566
    ## 12644             West Bank and Gaza year2017    4684777
    ## 12645                    Yemen, Rep.   year60    5172135
    ## 12646                    Yemen, Rep.   year61    5260501
    ## 12647                    Yemen, Rep.   year62    5351799
    ## 12648                    Yemen, Rep.   year63    5446063
    ## 12649                    Yemen, Rep.   year64    5543339
    ## 12650                    Yemen, Rep.   year65    5643643
    ## 12651                    Yemen, Rep.   year66    5748588
    ## 12652                    Yemen, Rep.   year67    5858638
    ## 12653                    Yemen, Rep.   year68    5971407
    ## 12654                    Yemen, Rep.   year69    6083619
    ## 12655                    Yemen, Rep.   year70    6193810
    ## 12656                    Yemen, Rep.   year71    6300554
    ## 12657                    Yemen, Rep.   year72    6407295
    ## 12658                    Yemen, Rep.   year73    6523452
    ## 12659                    Yemen, Rep.   year74    6661566
    ## 12660                    Yemen, Rep.   year75    6830692
    ## 12661                    Yemen, Rep.   year76    7034868
    ## 12662                    Yemen, Rep.   year77    7271872
    ## 12663                    Yemen, Rep.   year78    7536764
    ## 12664                    Yemen, Rep.   year79    7821552
    ## 12665                    Yemen, Rep.   year80    8120497
    ## 12666                    Yemen, Rep.   year81    8434017
    ## 12667                    Yemen, Rep.   year82    8764621
    ## 12668                    Yemen, Rep.   year83    9111097
    ## 12669                    Yemen, Rep.   year84    9472170
    ## 12670                    Yemen, Rep.   year85    9847899
    ## 12671                    Yemen, Rep.   year86   10232733
    ## 12672                    Yemen, Rep.   year87   10628585
    ## 12673                    Yemen, Rep.   year88   11051504
    ## 12674                    Yemen, Rep.   year89   11523267
    ## 12675                    Yemen, Rep.   year90   12057039
    ## 12676                    Yemen, Rep.   year91   12661614
    ## 12677                    Yemen, Rep.   year92   13325583
    ## 12678                    Yemen, Rep.   year93   14017239
    ## 12679                    Yemen, Rep.   year94   14692686
    ## 12680                    Yemen, Rep.   year95   15320653
    ## 12681                    Yemen, Rep.   year96   15889449
    ## 12682                    Yemen, Rep.   year97   16408954
    ## 12683                    Yemen, Rep.   year98   16896210
    ## 12684                    Yemen, Rep.   year99   17378098
    ## 12685                    Yemen, Rep. year2000   17874725
    ## 12686                    Yemen, Rep. year2001   18390135
    ## 12687                    Yemen, Rep. year2002   18919179
    ## 12688                    Yemen, Rep. year2003   19462086
    ## 12689                    Yemen, Rep. year2004   20017068
    ## 12690                    Yemen, Rep. year2005   20582927
    ## 12691                    Yemen, Rep. year2006   21160534
    ## 12692                    Yemen, Rep. year2007   21751605
    ## 12693                    Yemen, Rep. year2008   22356391
    ## 12694                    Yemen, Rep. year2009   22974929
    ## 12695                    Yemen, Rep. year2010   23606779
    ## 12696                    Yemen, Rep. year2011   24252206
    ## 12697                    Yemen, Rep. year2012   24909969
    ## 12698                    Yemen, Rep. year2013   25576322
    ## 12699                    Yemen, Rep. year2014   26246327
    ## 12700                    Yemen, Rep. year2015   26916207
    ## 12701                    Yemen, Rep. year2016   27584213
    ## 12702                    Yemen, Rep. year2017   28250420
    ## 12703                         Zambia   year60    3044846
    ## 12704                         Zambia   year61    3140264
    ## 12705                         Zambia   year62    3240587
    ## 12706                         Zambia   year63    3345145
    ## 12707                         Zambia   year64    3452942
    ## 12708                         Zambia   year65    3563407
    ## 12709                         Zambia   year66    3676189
    ## 12710                         Zambia   year67    3791887
    ## 12711                         Zambia   year68    3912085
    ## 12712                         Zambia   year69    4038923
    ## 12713                         Zambia   year70    4173928
    ## 12714                         Zambia   year71    4317748
    ## 12715                         Zambia   year72    4469895
    ## 12716                         Zambia   year73    4629402
    ## 12717                         Zambia   year74    4794754
    ## 12718                         Zambia   year75    4964831
    ## 12719                         Zambia   year76    5139030
    ## 12720                         Zambia   year77    5317631
    ## 12721                         Zambia   year78    5501445
    ## 12722                         Zambia   year79    5691749
    ## 12723                         Zambia   year80    5889230
    ## 12724                         Zambia   year81    6094206
    ## 12725                         Zambia   year82    6305709
    ## 12726                         Zambia   year83    6521542
    ## 12727                         Zambia   year84    6738765
    ## 12728                         Zambia   year85    6955212
    ## 12729                         Zambia   year86    7170656
    ## 12730                         Zambia   year87    7385686
    ## 12731                         Zambia   year88    7600072
    ## 12732                         Zambia   year89    7813808
    ## 12733                         Zambia   year90    8027253
    ## 12734                         Zambia   year91    8239732
    ## 12735                         Zambia   year92    8452275
    ## 12736                         Zambia   year93    8669168
    ## 12737                         Zambia   year94    8896109
    ## 12738                         Zambia   year95    9137077
    ## 12739                         Zambia   year96    9394304
    ## 12740                         Zambia   year97    9666578
    ## 12741                         Zambia   year98    9950224
    ## 12742                         Zambia   year99   10239714
    ## 12743                         Zambia year2000   10531221
    ## 12744                         Zambia year2001   10824125
    ## 12745                         Zambia year2002   11120409
    ## 12746                         Zambia year2003   11421984
    ## 12747                         Zambia year2004   11731746
    ## 12748                         Zambia year2005   12052156
    ## 12749                         Zambia year2006   12383446
    ## 12750                         Zambia year2007   12725974
    ## 12751                         Zambia year2008   13082517
    ## 12752                         Zambia year2009   13456417
    ## 12753                         Zambia year2010   13850033
    ## 12754                         Zambia year2011   14264756
    ## 12755                         Zambia year2012   14699937
    ## 12756                         Zambia year2013   15153210
    ## 12757                         Zambia year2014   15620974
    ## 12758                         Zambia year2015   16100587
    ## 12759                         Zambia year2016   16591390
    ## 12760                         Zambia year2017   17094130
    ## 12761                       Zimbabwe   year60    3747369
    ## 12762                       Zimbabwe   year61    3870756
    ## 12763                       Zimbabwe   year62    3999419
    ## 12764                       Zimbabwe   year63    4132756
    ## 12765                       Zimbabwe   year64    4269863
    ## 12766                       Zimbabwe   year65    4410212
    ## 12767                       Zimbabwe   year66    4553433
    ## 12768                       Zimbabwe   year67    4700041
    ## 12769                       Zimbabwe   year68    4851431
    ## 12770                       Zimbabwe   year69    5009514
    ## 12771                       Zimbabwe   year70    5175618
    ## 12772                       Zimbabwe   year71    5351195
    ## 12773                       Zimbabwe   year72    5535874
    ## 12774                       Zimbabwe   year73    5727044
    ## 12775                       Zimbabwe   year74    5920943
    ## 12776                       Zimbabwe   year75    6115370
    ## 12777                       Zimbabwe   year76    6308300
    ## 12778                       Zimbabwe   year77    6501893
    ## 12779                       Zimbabwe   year78    6703182
    ## 12780                       Zimbabwe   year79    6921790
    ## 12781                       Zimbabwe   year80    7164172
    ## 12782                       Zimbabwe   year81    7431940
    ## 12783                       Zimbabwe   year82    7721536
    ## 12784                       Zimbabwe   year83    8027565
    ## 12785                       Zimbabwe   year84    8342195
    ## 12786                       Zimbabwe   year85    8658857
    ## 12787                       Zimbabwe   year86    8976205
    ## 12788                       Zimbabwe   year87    9293283
    ## 12789                       Zimbabwe   year88    9604302
    ## 12790                       Zimbabwe   year89    9902540
    ## 12791                       Zimbabwe   year90   10183113
    ## 12792                       Zimbabwe   year91   10443043
    ## 12793                       Zimbabwe   year92   10682868
    ## 12794                       Zimbabwe   year93   10905756
    ## 12795                       Zimbabwe   year94   11116948
    ## 12796                       Zimbabwe   year95   11320346
    ## 12797                       Zimbabwe   year96   11518262
    ## 12798                       Zimbabwe   year97   11709997
    ## 12799                       Zimbabwe   year98   11893272
    ## 12800                       Zimbabwe   year99   12064537
    ## 12801                       Zimbabwe year2000   12222251
    ## 12802                       Zimbabwe year2001   12366165
    ## 12803                       Zimbabwe year2002   12500525
    ## 12804                       Zimbabwe year2003   12633897
    ## 12805                       Zimbabwe year2004   12777511
    ## 12806                       Zimbabwe year2005   12940032
    ## 12807                       Zimbabwe year2006   13124267
    ## 12808                       Zimbabwe year2007   13329909
    ## 12809                       Zimbabwe year2008   13558469
    ## 12810                       Zimbabwe year2009   13810599
    ## 12811                       Zimbabwe year2010   14086317
    ## 12812                       Zimbabwe year2011   14386649
    ## 12813                       Zimbabwe year2012   14710826
    ## 12814                       Zimbabwe year2013   15054506
    ## 12815                       Zimbabwe year2014   15411675
    ## 12816                       Zimbabwe year2015   15777451
    ## 12817                       Zimbabwe year2016   16150362
    ## 12818                       Zimbabwe year2017   16529904

``` r
write_xlsx(total_pop, "C:/Users/anton/OneDrive/Escritorio/Clustering-Algorithms/data/totalpop.xlsx")
```
