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

compute_silhouette <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}

# Initialize lists to store metrics
sil_values <- numeric(length(k.values))

# Compute metrics using a for loop
for (i in seq_along(k.values)) {
  k <- k.values[i]
  cat("Processing k =", k, "\n")  # Print progress
  sil_values[i] <- compute_silhouette(kmeans_clustering_data, k)
}

# Create Data Frame with Metrics
cluster_metrics <- data.frame(
  k = k.values,
  Silhouette = sil_values
)

# Determine Optimal Clusters
# Silhouette Method: Find the maximum silhouette width
optimal_clusters_sil <- which.max(cluster_metrics$Silhouette) + 1  # +1 because k starts at 2

cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_sil, "\n")

# Visualize the Metrics
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
