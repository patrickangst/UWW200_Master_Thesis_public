# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(factoextra)
library(cluster)
library(terra)
library(parallel)
library(stats)

## Patang test
pca_hs_image_path <- 'D:\\MasterThesis\\test_data_elbow\\ang20180729t212542rfl\\result\\ang20180729t212542_rfl_v2r2_img_rectified\\SPCA\\PCA\\OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores()

pca_hs_image_subset <- terra::subset(pca_hs_image, 1:4)
# Downsample the raster (reduce spatial resolution)
pca_hs_image_downsampled <- terra::aggregate(pca_hs_image_subset, fact = 5, fun = mean, cores = num_cores)
pca_data <- as.matrix(terra::values(pca_hs_image_downsampled))
pca_data <- na.omit(pca_data)
#terra::nlyr(pca_hs_image_subset)

k.values <- 1:30

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

wss_values_unlisted <- unlist(wss_values)
# Find the optimal number of clusters using the Elbow Method
diff_wss <- diff(wss_values_unlisted)  # First derivative
diff2_wss <- diff(diff_wss)  # Second derivative
optimal_clusters <- which.max(-diff2_wss) + 1 + min(k.values) - 1

cat("Optimal number of clusters (Elbow Method):", optimal_clusters, "\n")

# Find the optimal number of clusters (elbow point)
optimal_clusters <- which.max(diff(diff(wss_values))) + 1
cat("Optimal number of clusters (Elbow Method):", optimal_clusters, "\n")

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust_result <- fviz_nbclust(
  pca_data,
  kmeans,
  method = "wss", # Elbow Method
  k.max = 30,
  verbose = interactive(),
  barfill = "steelblue",
  barcolor = "steelblue",
  linecolor = "steelblue",
  print.summary = TRUE
)

plot(fviz_nbclust_result)





























# # clean environment
# rm(list=ls(all=TRUE));gc()
# graphics.off()
#
# library(factoextra)
# library(cluster)
# library(terra)
# library(parallel)
# library(stats)
# library(NbClust)
#
# ## Patang test
# pca_hs_image_path <- 'D:\\MasterThesis\\test_data_elbow\\ang20180729t212542rfl\\result\\ang20180729t212542_rfl_v2r2_img_rectified\\SPCA\\PCA\\OutputPCA_30_PCs'
# pca_hs_image <- terra::rast(pca_hs_image_path)
# num_cores <- detectCores()
#
# pca_hs_image_subset <- terra::subset(pca_hs_image, 1:4)
# # Downsample the raster (reduce spatial resolution)
# pca_hs_image_downsampled <- terra::aggregate(pca_hs_image_subset, fact = 5, fun = mean, cores = num_cores)
# pca_data <- terra::values(pca_hs_image_downsampled, dataframe = TRUE)
# pca_data <- na.omit(pca_data)
#
#
# nb <- NbClust(pca_data, min.nc = 2, max.nc = 15, method = "kmeans")
# fviz_nbclust(nb)
#
#
#
# k.values <- 10:30
#
# # Setup cluster
# cl <- makeCluster(num_cores)
#
# # Export data and required function to cluster
# clusterExport(cl, varlist = c("pca_data"))
#
# # Export required libraries in cluster
# clusterEvalQ(cl, library(stats))
#
# # Run parallel computation
# wss_values <- parLapply(cl, k.values, function(k) {
#   kmeans(pca_data, k, nstart = 10)$tot.withinss
# })
#
# # Stop cluster after computation
# stopCluster(cl)
#
# plot(k.values, wss_values,
#      type="b", pch = 19, frame = FALSE,
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
#
# #fviz_nbclust(pca_data, kmeans, method = "wss")
#
# fviz_nbclust(
#   pca_data,
#   method = c("wss"),
#   k.max = 30,
#   verbose = interactive(),
#   barfill = "steelblue",
#   barcolor = "steelblue",
#   linecolor = "steelblue",
#   print.summary = TRUE)


















# Clean Environment
rm(list = ls(all = TRUE)); gc()
graphics.off()

# Load Required Libraries
library(factoextra)
library(terra)
library(parallel)
library(NbClust)

# Load PCA Raster Image
pca_hs_image_path <- 'D:\\MasterThesis\\test_data_elbow\\ang20180729t212542rfl\\result\\ang20180729t212542_rfl_v2r2_img_rectified\\SPCA\\PCA\\OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)

# Use only the first 4 PCA bands and downsample
pca_hs_image_subset <- terra::subset(pca_hs_image, 1:4)
pca_hs_image_downsampled <- terra::aggregate(pca_hs_image_subset, fact = 5, fun = mean)

# Extract Raster Values and Remove NAs
pca_data <- na.omit(terra::values(pca_hs_image_downsampled, dataframe = TRUE))
pca_data <- na.omit(terra::values(pca_hs_image_subset, dataframe = TRUE))

# Perform NbClust Analysis to Determine Optimal Clusters
optimal_clusters <- NbClust(
  data = pca_data,
  min.nc = 2,
  max.nc = 5,
  method = "kmeans"
)
fviz_nbclust(optimal_clusters)

# Parallel K-Means Clustering for Range of Clusters (Optional if NbClust is enough)
num_cores <- detectCores()
k.values <- 1:35

# Setup Parallel Cluster
cl <- makeCluster(num_cores)
clusterExport(cl, varlist = "pca_data")
clusterEvalQ(cl, library(stats))

# Calculate Total Within-Cluster Sum of Squares (WSS) in Parallel
wss_values <- parLapply(cl, k.values, function(k) {
  kmeans(pca_data, k, nstart = 10)$tot.withinss
})

# Stop the Cluster
stopCluster(cl)

# Plot WSS for Elbow Method
plot(
  k.values, wss_values, type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters (K)", ylab = "Total Within-Cluster Sum of Squares"
)

fviz_nbclust(iris.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)





library(NbClust)
library(parallel)

# Define the range of clusters
min_nc <- 2
max_nc <- 15

# Number of cores to use
num_cores <- detectCores()

# Create cluster
cl <- makeCluster(num_cores)
clusterExport(cl, varlist = c("pca_data", "NbClust"))
clusterEvalQ(cl, library(NbClust))

# Split the range into overlapping chunks to satisfy NbClust requirements
cluster_ranges <- split(seq(min_nc, max_nc, by = 2), seq_len(num_cores))

# Adjust ranges to have at least 2 elements in each chunk
cluster_ranges <- lapply(cluster_ranges, function(range) {
  if (length(range) == 1) {
    range <- c(range[1], range[1] + 1)
  }
  range
})

# Run NbClust in parallel for each chunk
results <- parLapply(cl, cluster_ranges, function(range) {
  NbClust(
    data = pca_data,
    min.nc = range[1],
    max.nc = tail(range, 1),
    method = "kmeans"
  )
})

# Stop cluster
stopCluster(cl)

# Combine results
optimal_clusters <- do.call(rbind, lapply(results, function(res) res$Best.nc))

# Visualize or analyze the results
print(optimal_clusters)




















# Clean Environment
rm(list = ls(all = TRUE)); gc()
graphics.off()

# Load Required Libraries
library(factoextra)
library(terra)
library(parallel)
library(NbClust)

# Load PCA Raster Image
pca_hs_image_path <- 'D:\\MasterThesis\\test_data_elbow\\ang20180729t212542rfl\\result\\ang20180729t212542_rfl_v2r2_img_rectified\\SPCA\\PCA\\OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)

num_cores <- detectCores()

# Use only the first 4 PCA bands and downsample
pca_hs_image_subset <- terra::subset(pca_hs_image, 1:4)
pca_hs_image_downsampled <- terra::aggregate(pca_hs_image_subset, fact = 2, fun = mean, cores = num_cores)

# Extract Raster Values and Remove NAs
#pca_data <- na.omit(terra::values(pca_hs_image_downsampled, dataframe = TRUE))
pca_data <- na.omit(terra::values(pca_hs_image_downsampled, dataframe = TRUE))


fviz_nbclust_result <- fviz_nbclust(
  pca_data,
  kmeans,
  method = "wss", # Elbow Method
  k.max = 10,
  verbose = interactive(),
  barfill = "steelblue",
  barcolor = "steelblue",
  linecolor = "steelblue",
  print.summary = TRUE
)

library(cluster)
library(factoextra)

# Compute silhouette values for a range of clusters
sil <- sapply(2:10, function(k) {
  model <- pam(pca_data, k)
  mean(silhouette(model$clustering, dist(pca_data))[, 3])
})

# Find the optimal number of clusters
optimal_clusters <- which.max(sil) + 1  # +1 since range starts at 2
cat("Optimal number of clusters (Silhouette Method):", optimal_clusters, "\n")


# Compute WSS for a range of clusters
wss <- sapply(1:30, function(k) {
  kmeans(pca_data, centers = k, nstart = 10)$tot.withinss
})

# Find the optimal number of clusters (elbow point)
optimal_clusters_wss <- which.max(diff(diff(wss))) + 1
cat("Optimal number of clusters (Elbow Method):", optimal_clusters_wss, "\n")




library(NbClust)

# Run NbClust to determine the optimal number of clusters
set.seed(123)
nb <- NbClust(pca_data, min.nc = 2, max.nc = 10, method = "kmeans")

# Extract the optimal number of clusters
optimal_clusters <- as.integer(names(which.max(table(nb$Best.nc[1, ]))))
cat("Optimal number of clusters (NbClust):", optimal_clusters, "\n")

