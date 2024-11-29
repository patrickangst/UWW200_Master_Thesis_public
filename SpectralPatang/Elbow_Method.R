library(factoextra)
library(cluster)

# Sample data (replace with your data)
set.seed(123)
X <- matrix(rnorm(200), ncol=2)

# Compute k-means clustering for a range of cluster numbers
wss <- function(k) {
  kmeans(X, k, nstart = 10)$tot.withinss
}

# Compute and plot the within-cluster sum of squares (wss)
k.values <- 1:15
wss_values <- sapply(k.values, wss)

# Elbow method to find the optimal number of clusters
fviz_nbclust(X, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# Optionally, use the NbClust package to determine the optimal number of clusters
# install.packages("NbClust")
# library(NbClust)
# nb <- NbClust(X, min.nc = 2, max.nc = 15, method = "kmeans")
# fviz_nbclust(nb)
