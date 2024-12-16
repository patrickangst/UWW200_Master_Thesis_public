# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(NbClust)
library(factoextra)
library(parallel)
library(terra)


## Patang test
pca_hs_image_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/result/ang20190706t235120_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection_cutline.tif'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores() - 2


pca_data <- as.matrix(terra::values(pca_hs_image))

# Prepare the data
pca_data_na_omitted <- na.omit(pca_data)   # Clean the data (remove NAs)
pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)     # Standardize data for clustering
kmeans_clustering_data <- pca_data_na_omitted_scaled

# Define Range of Clusters
k.values <- 2:30

# Define Clustering Metrics Functions
compute_wss <- function(data, k, seed = 123) {
  set.seed(seed)
  kmeans(data, centers = k, nstart = 10)$tot.withinss
}

compute_silhouette <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}


# Setup Parallel Cluster
cl <- makeCluster(num_cores)
# clusterExport(cl, varlist = c("kmeans_clustering_data", "compute_wss"))
clusterExport(cl, varlist = c("kmeans_clustering_data", "compute_wss", "compute_silhouette"))
clusterEvalQ(cl, library(cluster))  # Load `cluster` library on workers

# Compute Metrics in Parallel
wss_values <- parSapply(cl, k.values, function(k) compute_wss(kmeans_clustering_data, k))
sil_values <- parSapply(cl, k.values, function(k) compute_silhouette(kmeans_clustering_data, k))

# Stop Cluster
stopCluster(cl)

# Create Data Frame with Metrics
cluster_metrics <- data.frame(
  k = k.values,
  WSS = wss_values,
  Silhouette = sil_values
)

# Determine Optimal Clusters
# Elbow Method: Find the point where the second derivative is minimized
diff_wss <- diff(cluster_metrics$WSS)
diff2_wss <- diff(diff_wss)
optimal_clusters_elbow <- which.min(diff2_wss) + 2  # +2 to account for the two diffs

# Silhouette Method: Find the maximum silhouette width
optimal_clusters_sil <- which.max(cluster_metrics$Silhouette) + 1  # +1 because k starts at 2

cat("Optimal number of clusters (Elbow Method):", optimal_clusters_elbow, "\n")
cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_sil, "\n")

# Visualize the Metrics
# Elbow Method Plot
ggplot(cluster_metrics, aes(x = k, y = WSS)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_elbow, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_clusters_elbow, y = max(cluster_metrics$WSS), label = paste("Optimal k =", optimal_clusters_elbow), vjust = -1, color = "red") +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

# Silhouette Method Plot
ggplot(cluster_metrics, aes(x = k, y = Silhouette)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_sil, linetype = "dashed", color = "blue") +
  annotate("text", x = optimal_clusters_sil, y = max(cluster_metrics$Silhouette), label = paste("Optimal k =", optimal_clusters_sil), vjust = -1, color = "blue") +
  labs(title = "Silhouette Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()








# Clean the environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required libraries
library(NbClust)
library(factoextra)
library(terra)
library(cluster)
library(ggplot2)

## Patang test
# Path to the PCA image
pca_hs_image_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/result/ang20190706t235120_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection_cutline.tif'

# Load the raster and extract values as a matrix
pca_hs_image <- terra::rast(pca_hs_image_path)
pca_data <- as.matrix(terra::values(pca_hs_image))

# Prepare the data
pca_data_na_omitted <- na.omit(pca_data)              # Remove missing values (NA)
pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data
kmeans_clustering_data <- pca_data_na_omitted_scaled

# Define Range of Clusters
k.values <- 2:30

# Define Clustering Metrics Functions
compute_wss <- function(data, k, seed = 123) {
  set.seed(seed)
  kmeans(data, centers = k, nstart = 10)$tot.withinss
}

compute_silhouette <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}

# Initialize lists to store metrics
wss_values <- numeric(length(k.values))
sil_values <- numeric(length(k.values))

# Compute metrics using a for loop
for (i in seq_along(k.values)) {
  k <- k.values[i]
  cat("Processing k =", k, "\n")  # Print progress
  wss_values[i] <- compute_wss(kmeans_clustering_data, k)
  sil_values[i] <- compute_silhouette(kmeans_clustering_data, k)
}

# Create Data Frame with Metrics
cluster_metrics <- data.frame(
  k = k.values,
  WSS = wss_values,
  Silhouette = sil_values
)

# Determine Optimal Clusters
# Elbow Method: Find the point where the second derivative is minimized
diff_wss <- diff(cluster_metrics$WSS)
diff2_wss <- diff(diff_wss)
optimal_clusters_elbow <- which.min(diff2_wss) + 2  # +2 to account for the two diffs

# Silhouette Method: Find the maximum silhouette width
optimal_clusters_sil <- which.max(cluster_metrics$Silhouette) + 1  # +1 because k starts at 2

cat("Optimal number of clusters (Elbow Method):", optimal_clusters_elbow, "\n")
cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_sil, "\n")

# Visualize the Metrics
# Elbow Method Plot
ggplot(cluster_metrics, aes(x = k, y = WSS)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_elbow, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_clusters_elbow, y = max(cluster_metrics$WSS), label = paste("Optimal k =", optimal_clusters_elbow), vjust = -1, color = "red") +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

# Silhouette Method Plot
ggplot(cluster_metrics, aes(x = k, y = Silhouette)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_sil, linetype = "dashed", color = "blue") +
  annotate("text", x = optimal_clusters_sil, y = max(cluster_metrics$Silhouette), label = paste("Optimal k =", optimal_clusters_sil), vjust = -1, color = "blue") +
  labs(title = "Silhouette Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()


















# Clean the environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required libraries
library(NbClust)
library(factoextra)
library(terra)
library(cluster)
library(ggplot2)
library(bigmemory)

## Patang test
# Path to the PCA image
pca_hs_image_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/result/ang20190706t235120_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection_cutline.tif'

# Load the raster and extract values as a matrix
pca_hs_image <- terra::rast(pca_hs_image_path)
pca_data <- as.matrix(terra::values(pca_hs_image))

# Prepare the data
pca_data_na_omitted <- na.omit(pca_data)  # Remove missing values (NA)
pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data

# Convert the data to a big.matrix for memory-efficient processing
kmeans_clustering_data <- as.big.matrix(pca_data_na_omitted_scaled)

# Define Range of Clusters
k.values <- 2:30

# Define Clustering Metrics Functions
compute_wss <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data[, ], centers = k, nstart = 10)  # Access big.matrix using [, ]
  km$tot.withinss
}

compute_silhouette <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data[, ], centers = k, nstart = 10)  # Access big.matrix using [, ]
  ss <- silhouette(km$cluster, dist(data[, ]))
  mean(ss[, 3])
}

# Initialize lists to store metrics
wss_values <- numeric(length(k.values))
sil_values <- numeric(length(k.values))

# Compute metrics using a for loop
for (i in seq_along(k.values)) {
  k <- k.values[i]
  cat("Processing k =", k, "\n")  # Print progress
  wss_values[i] <- compute_wss(kmeans_clustering_data, k)
  sil_values[i] <- compute_silhouette(kmeans_clustering_data, k)
}

# Create Data Frame with Metrics
cluster_metrics <- data.frame(
  k = k.values,
  WSS = wss_values,
  Silhouette = sil_values
)

# Determine Optimal Clusters
# Elbow Method: Find the point where the second derivative is minimized
diff_wss <- diff(cluster_metrics$WSS)
diff2_wss <- diff(diff_wss)
optimal_clusters_elbow <- which.min(diff2_wss) + 2  # +2 to account for the two diffs

# Silhouette Method: Find the maximum silhouette width
optimal_clusters_sil <- which.max(cluster_metrics$Silhouette) + 1  # +1 because k starts at 2

cat("Optimal number of clusters (Elbow Method):", optimal_clusters_elbow, "\n")
cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_sil, "\n")

# Visualize the Metrics
# Elbow Method Plot
ggplot(cluster_metrics, aes(x = k, y = WSS)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_elbow, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_clusters_elbow, y = max(cluster_metrics$WSS), label = paste("Optimal k =", optimal_clusters_elbow), vjust = -1, color = "red") +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

# Silhouette Method Plot
ggplot(cluster_metrics, aes(x = k, y = Silhouette)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_sil, linetype = "dashed", color = "blue") +
  annotate("text", x = optimal_clusters_sil, y = max(cluster_metrics$Silhouette), label = paste("Optimal k =", optimal_clusters_sil), vjust = -1, color = "blue") +
  labs(title = "Silhouette Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()













# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required libraries
library(NbClust)
library(factoextra)
library(terra)
library(bigmemory)
library(cluster)
library(parallel)
library(ggplot2)
library(biganalytics)

# Path to the PCA image
pca_hs_image_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/result/ang20190706t235120_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection_cutline.tif'

# Load raster and extract values as a matrix
pca_hs_image <- terra::rast(pca_hs_image_path)
pca_data <- as.matrix(terra::values(pca_hs_image))

# Prepare the data
pca_data_na_omitted <- na.omit(pca_data)              # Remove missing values (NA)
pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data

# Convert the data to a big.matrix for memory-efficient processing
kmeans_clustering_data <- as.big.matrix(pca_data_na_omitted_scaled)

# Define range of clusters
k.values <- 2:30

# Define clustering metrics functions
compute_wss <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- bigkmeans(data[, ], centers = k, nstart = 10)  # Access big.matrix using [, ]
  km$tot.withinss
}

compute_silhouette <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data[, ], centers = k, nstart = 10)  # Access big.matrix using [, ]
  ss <- silhouette(km$cluster, dist(data[, ]))
  mean(ss[, 3])
}

# Setup parallel cluster
num_cores <- parallel::detectCores() - 2
cl <- makeCluster(num_cores)

# Export variables and libraries to workers
clusterExport(cl, varlist = c("kmeans_clustering_data", "compute_wss", "compute_silhouette"))
clusterEvalQ(cl, library(cluster), library(biganalytics))  # Load `cluster` library on workers

# Compute metrics in parallel
wss_values <- parSapply(cl, k.values, function(k) compute_wss(kmeans_clustering_data, k))
sil_values <- parSapply(cl, k.values, function(k) compute_silhouette(kmeans_clustering_data, k))

# Stop cluster
stopCluster(cl)

# Create data frame with metrics
cluster_metrics <- data.frame(
  k = k.values,
  WSS = wss_values,
  Silhouette = sil_values
)

# Determine optimal clusters
# Elbow Method: Find the point where the second derivative is minimized
diff_wss <- diff(cluster_metrics$WSS)
diff2_wss <- diff(diff_wss)
optimal_clusters_elbow <- which.min(diff2_wss) + 2  # +2 to account for the two diffs

# Silhouette Method: Find the maximum silhouette width
optimal_clusters_sil <- which.max(cluster_metrics$Silhouette) + 1  # +1 because k starts at 2

cat("Optimal number of clusters (Elbow Method):", optimal_clusters_elbow, "\n")
cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_sil, "\n")

# Visualize the metrics
# Elbow Method Plot
ggplot(cluster_metrics, aes(x = k, y = WSS)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_elbow, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_clusters_elbow, y = max(cluster_metrics$WSS), label = paste("Optimal k =", optimal_clusters_elbow), vjust = -1, color = "red") +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

# Silhouette Method Plot
ggplot(cluster_metrics, aes(x = k, y = Silhouette)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_sil, linetype = "dashed", color = "blue") +
  annotate("text", x = optimal_clusters_sil, y = max(cluster_metrics$Silhouette), label = paste("Optimal k =", optimal_clusters_sil), vjust = -1, color = "blue") +
  labs(title = "Silhouette Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()
