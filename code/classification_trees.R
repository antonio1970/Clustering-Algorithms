# Decision trees using cluster labels
# Data: 21.10.2019

# Install the right packages.

install.packages(c("rpart", "rpart.plot", "caret", "rattle"))

# Load the corresponding libraries and train/split partition

library(rpart) # Build-up the classification tree
library (rpart.plot) # Generate a graph for the classification tree
library(rattle)
library(caret) # Data partition- train.vs test

#Load the data with the proper labels (4 clusters: very good, good, bad, and worst)

datos_trees <- read.csv("C:/Users/anton/OneDrive/Escritorio/Clustering-Algorithms/data/afdata_imputed_ClsterDescription.csv")

# Proportion of cluster labels

prop.table(table(datos_trees$MergedClusterDescription))*100

# Create data partition (generate index with observations in each
# dataset)

set.seed (2019)
trainIndex <- createDataPartition(datos_trees$MergedClusterDescription, p = .7, 
                                  list = FALSE, 
                                  times = 1)
datos_trees_train <- datos_trees[trainIndex,]
datos_trees_test <- datos_trees[-trainIndex,]


# Classification tree

mod <- rpart(MergedClusterDescription~ PRIMARY + SECONDARY + TELEP3 +FIXBI2 + INTERN3 + PATEN2 + STJOU2 +
               REGQU + RULEL + TNTBA, data = datos_trees_train, method = 'class',
             control = rpart.control(minsplit = 20, cp = 0.01))

mod
prp(mod, type = 1, extra = 104, nn = TRUE,
    fallen.leaves = TRUE, faclen = 0, varlen = 4,
    shadow.col = "black")

mod$cptable

mod.pruned<-prune(mod, mod$cptable[3, "CP"])

prp(mod.pruned, type = 1, extra = 104, nn = TRUE,
    fallen.leaves = TRUE, faclen = 0, varlen = 4,
    shadow.col = "gray")
