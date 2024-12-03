# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(NbClust)
library(factoextra)
library(parallel)


## Patang test
pca_hs_image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20190712t231624cut/result/ang20190712t231624_rfl_v2v2_img_rectified_cut/SPCA/PCA/OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores()

pca_hs_image_subset <- terra::subset(pca_hs_image, 1:9)
pca_data <- as.matrix(terra::values(pca_hs_image_subset))

# Prepare the data
pca_data <- na.omit(pca_data)   # Clean the data (remove NAs)
pca_data <- scale(pca_data)     # Standardize data for clustering

set.seed(123)  # Set the seed for reproducibility within each worker
nbclust_result <- NbClust(
  data = pca_data,
  distance = "euclidean",
  min.nc = 1,
  max.nc = 30,
  method = "kmeans",
  index = "all"
)


# Extract the number of clusters suggested by each index
optimal_clusters <- nbclust_result$Best.nc

# Create a frequency table for the suggested cluster numbers
cluster_votes <- table(optimal_clusters)

# Determine the optimal number of clusters based on the majority vote
best_number_of_clusters <- as.numeric(names(which.max(cluster_votes)))
cat("The optimal number of clusters is:", best_number_of_clusters, "\n")


# Barplot to visualize votes for the number of clusters
barplot(
  cluster_votes,
  main = "Number of Clusters Suggested by NbClust Indices",
  xlab = "Number of Clusters",
  ylab = "Frequency",
  col = "skyblue",
  border = "black"
)

fviz_nbclust(nbclust_result) +
  labs(title = "Optimal Number of Clusters Using NbClust",
       subtitle = paste("Optimal Clusters:", optimal_clusters)) +
  theme_minimal()




# Define Optimal Number of Clusters (Replace with your computed optimal value)
optimal_k <- 5

# Perform K-means Clustering
set.seed(123)  # Ensure reproducibility
kmeans_result <- kmeans(pca_data, centers = optimal_k, nstart = 10)

# View the clustering result
print(kmeans_result)


# Cluster Assignments
head(kmeans_result$cluster)
# Cluster Centers
kmeans_result$centers


fviz_cluster(kmeans_result, data = pca_data, geom = "point", ellipse.type = "convex") +
  labs(title = paste("K-means Clustering with k =", optimal_k))



































# Clean Environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load Required Libraries
library(factoextra)
library(cluster)
library(terra)
library(parallel)
library(stats)
library(ggplot2)

# Load and Prepare Data
pca_hs_image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20190712t231624cut/result/ang20190712t231624_rfl_v2v2_img_rectified_cut/SPCA/PCA/OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores()

pca_hs_image_subset <- terra::subset(pca_hs_image, 1:9)
pca_data <- as.matrix(terra::values(pca_hs_image_subset))

# Prepare the data
pca_data <- na.omit(pca_data)   # Clean the data (remove NAs)
pca_data <- scale(pca_data)     # Standardize data for clustering

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
clusterExport(cl, varlist = c("pca_data", "compute_wss", "compute_silhouette"))
clusterEvalQ(cl, library(cluster))  # Load `cluster` library on workers

# Compute Metrics in Parallel
wss_values <- parSapply(cl, k.values, function(k) compute_wss(pca_data, k))
sil_values <- parSapply(cl, k.values, function(k) compute_silhouette(pca_data, k))

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









