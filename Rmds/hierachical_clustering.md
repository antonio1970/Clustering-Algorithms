Hiearachical clustering
================

Preparing the dataset for the cluster analysis
----------------------------------------------

``` r
d2<-iris[,-c(5)]
d2 <- scale(d2)
```

Compute the distance matrix and hierarchical clustering algorithm
-----------------------------------------------------------------

``` r
d <- dist(d2, method = "euclidean")
 fit <- hclust(d, method="average")
```

Plot dendogram
--------------

``` r
plot(fit)
rect.hclust(fit, k=2, border="green")
```

![](hierachical_clustering_files/figure-markdown_github/plot%20dendogram-1.png)
