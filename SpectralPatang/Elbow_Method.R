library(factoextra)
library(cluster)
library(terra)
library(parallel)


# # Sample data (replace with your data)
# set.seed(123)
# X <- matrix(rnorm(200), ncol=2)
#
# # Compute k-means clustering for a range of cluster numbers
# wss <- function(k) {
#   kmeans(X, k, nstart = 10)$tot.withinss
# }
#
# # Compute and plot the within-cluster sum of squares (wss)
# k.values <- 1:15
# wss_values <- sapply(k.values, wss)
#
# # Elbow method to find the optimal number of clusters
# fviz_nbclust(X, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
#
# # Optionally, use the NbClust package to determine the optimal number of clusters
# install.packages("NbClust")
# library(NbClust)
# nb <- NbClust(X, min.nc = 2, max.nc = 15, method = "kmeans")
# fviz_nbclust(nb)



# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(factoextra)
library(cluster)
library(terra)
library(parallel)
library(stats)

## Patang test
pca_hs_image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20180729t212542rfl/result/ang20180729t212542_rfl_v2r2_img_rectified/SPCA/PCA/OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- detectCores()

pca_hs_image_subset <- terra::subset(pca_hs_image, 1:4)
# Downsample the raster (reduce spatial resolution)
pca_hs_image_downsampled <- terra::aggregate(pca_hs_image_subset, fact = 5, fun = mean, cores = num_cores)
pca_data <- as.matrix(terra::values(pca_hs_image_downsampled))
pca_data <- na.omit(pca_data)
#terra::nlyr(pca_hs_image_subset)


# Extract raster values to a matrix (bypassing data frame conversion)
#pca_data <- as.matrix(terra::values(pca_hs_image_subset))
pca_data2 <- terra::values(pca_hs_image_downsampled, dataframe = TRUE)
pca_data2 <- na.omit(pca_data2)

# Remove rows with NA values
#pca_data <- na.omit(pca_data)
#pca_data2 <- na.omit(pca_data2)

# Downsample the raster (reduce spatial resolution)
#pca_hs_image_downsampled <- terra::aggregate(pca_data, fact = 100, fun = mean, cores = num_cores)

#pca_data <- as.matrix(terra::values(pca_hs_image_downsampled))


# Compute k-means clustering for a range of cluster numbers
wss <- function(k) {
  kmeans(pca_data, k, nstart = 10)$tot.withinss
}
k.values <- 1:15
wss_values <- mclapply(k.values, wss, mc.cores = parallel::detectCores())


plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Elbow method to find the optimal number of clusters
fviz_nbclust(pca_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)


# Setup cluster
cl <- makeCluster(num_cores)

# Export data and required function to cluster
clusterExport(cl, varlist = c("pca_data"))

# Export required libraries in cluster
clusterEvalQ(cl, library(stats))

# Run parallel computation
wss_values <- parLapply(cl, k.values, function(k) {
  kmeans(pca_data, k, nstart = 10)$tot.withinss
})

# Stop cluster after computation
stopCluster(cl)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")





























# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(factoextra)
library(cluster)
library(terra)
library(parallel)
library(stats)

## Patang test
pca_hs_image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20180729t212542rfl/result/ang20180729t212542_rfl_v2r2_img_rectified/SPCA/PCA/OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- detectCores()

pca_hs_image_subset <- terra::subset(pca_hs_image, 1:4)
# Downsample the raster (reduce spatial resolution)
pca_hs_image_downsampled <- terra::aggregate(pca_hs_image_subset, fact = 5, fun = mean, cores = num_cores)
pca_data <- terra::values(pca_hs_image_downsampled, dataframe = TRUE)
pca_data <- na.omit(pca_data)


nb <- NbClust(pca_data, min.nc = 2, max.nc = 15, method = "kmeans")
fviz_nbclust(nb)



k.values <- 10:30

# Setup cluster
cl <- makeCluster(num_cores)

# Export data and required function to cluster
clusterExport(cl, varlist = c("pca_data"))

# Export required libraries in cluster
clusterEvalQ(cl, library(stats))

# Run parallel computation
wss_values <- parLapply(cl, k.values, function(k) {
  kmeans(pca_data, k, nstart = 10)$tot.withinss
})

# Stop cluster after computation
stopCluster(cl)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#fviz_nbclust(pca_data, kmeans, method = "wss")

fviz_nbclust(
  pca_data,
  method = c("wss"),
  k.max = 30,
  verbose = interactive(),
  barfill = "steelblue",
  barcolor = "steelblue",
  linecolor = "steelblue",
  print.summary = TRUE)
