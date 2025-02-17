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

# Define the directory containing .tiff files
pca_hs_image_dir <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/08_principle_components_selection/'

# Get all .tiff files in the directory
tiff_files <- list.files(pca_hs_image_dir, pattern = "\\.tif$", full.names = TRUE)

# Define Range of Clusters
k.values <- 2:50

# Function to compute silhouette scores
compute_silhouette <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}

# Initialize a results dataframe
results <- data.frame(File = character(), Optimal_K = numeric(), stringsAsFactors = FALSE)

# Process each .tiff file
for (file in tiff_files) {
  cat("Processing:", basename(file), "\n")

  # Load the raster and extract values
  pca_hs_image <- terra::rast(file)

  # Extract raster values as a matrix but sample only 10,000 pixels
  pca_data <- as.matrix(terra::values(pca_hs_image))


  # Prepare the data
  pca_data_na_omitted <- na.omit(pca_data)  # Remove missing values

  threshold <- 40000

  print(paste0('nrow: ', nrow(pca_data_na_omitted)))
  if (nrow(pca_data_na_omitted) > threshold) {
    set.seed(123)  # Ensure reproducibility
    pca_data_na_omitted <- pca_data_na_omitted[sample(1:nrow(pca_data_na_omitted), threshold), ]
  }

  pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize
  kmeans_clustering_data <- pca_data_na_omitted_scaled

  # Compute silhouette values
  sil_values <- numeric(length(k.values))
  for (i in seq_along(k.values)) {
    k <- k.values[i]
    print(paste0('Start processing k=',k))
    sil_values[i] <- compute_silhouette(kmeans_clustering_data, k)
  }

  # Determine the optimal number of clusters
  optimal_clusters_sil <- which.max(sil_values) + 1  # +1 because k starts at 2

  # Store results
  results <- rbind(results, data.frame(File = basename(file), Optimal_K = optimal_clusters_sil))
}

# Print the results
print(results)

# Plot the final results
silhouette_plot <- ggplot(results, aes(x = File, y = Optimal_K)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Optimal Number of Clusters for Each PCA Image",
       x = "PCA Image",
       y = "Optimal k") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(silhouette_plot)

silhouette_plot_file_path <- file.path('~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/07_Testsite_Metrics/plots', paste0('silhouette_plot_',threshold,'.png'))
ggsave(
  filename = silhouette_plot_file_path,
  plot = silhouette_plot,
  width = 9,
  height = 6,
  dpi = 400
)












#
# # Clean the environment
# rm(list = ls(all = TRUE))
# gc()
# graphics.off()
#
# # Load required libraries
# library(NbClust)
# library(factoextra)
# library(terra)
# library(cluster)
# library(ggplot2)
#
# # Define the directory containing .tiff files
# pca_hs_image_dir <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/08_principle_components_selection/'
#
# # Get all .tiff files in the directory
# tiff_files <- list.files(pca_hs_image_dir, pattern = "\\.tif$", full.names = TRUE)
#
# # Define Range of Clusters
# k.values <- 2:4
#
# # Function to compute silhouette scores
# compute_silhouette <- function(data, k, seed = 123) {
#   set.seed(seed)
#   km <- kmeans(data, centers = k, nstart = 10)
#   ss <- silhouette(km$cluster, dist(data))
#   mean(ss[, 3])
# }
#
# # Initialize a results dataframe
# results <- data.frame(File = character(), Optimal_K = numeric(), stringsAsFactors = FALSE)
#
# # Loop over each .tiff file
# for (file in tiff_files) {
#   cat("Processing:", basename(file), "\n")
#
#   # Load the raster and extract values as a matrix
#   pca_hs_image <- terra::rast(file)
#   pca_data <- as.matrix(terra::values(pca_hs_image))
#
#   # Prepare the data
#   pca_data_na_omitted <- na.omit(pca_data)  # Remove missing values (NA)
#   pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data
#   kmeans_clustering_data <- pca_data_na_omitted_scaled
#
#   # Compute silhouette values
#   sil_values <- numeric(length(k.values))
#   for (i in seq_along(k.values)) {
#     k <- k.values[i]
#     sil_values[i] <- compute_silhouette(kmeans_clustering_data, k)
#   }
#
#   # Determine the optimal number of clusters
#   optimal_clusters_sil <- which.max(sil_values) + 1  # +1 because k starts at 2
#
#   # Store results
#   results <- rbind(results, data.frame(File = basename(file), Optimal_K = optimal_clusters_sil))
# }
#
# # Print the results
# print(results)
#
# # Plot the final results
# ggplot(results, aes(x = File, y = Optimal_K)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   labs(title = "Optimal Number of Clusters for Each PCA Image",
#        x = "PCA Image",
#        y = "Optimal k") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#



# # Clean the environment
# rm(list = ls(all = TRUE))
# gc()
# graphics.off()
#
#
#
# # Load required libraries
# library(NbClust)
# library(factoextra)
# library(terra)
# library(cluster)
# library(ggplot2)
#
# ## Patang test
# # Path to the PCA image
# pca_hs_image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/08_principle_components_selection/AN_TJ_1_pc_selection.tif'
#
# # Load the raster and extract values as a matrix
# pca_hs_image <- terra::rast(pca_hs_image_path)
# pca_data <- as.matrix(terra::values(pca_hs_image))
#
# # Prepare the data
# pca_data_na_omitted <- na.omit(pca_data)              # Remove missing values (NA)
# pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data
# kmeans_clustering_data <- pca_data_na_omitted_scaled
#
# # Define Range of Clusters
# k.values <- 2:10
#
# compute_silhouette <- function(data, k, seed = 123) {
#   set.seed(seed)
#   km <- kmeans(data, centers = k, nstart = 10)
#   ss <- silhouette(km$cluster, dist(data))
#   mean(ss[, 3])
# }
#
# # Initialize lists to store metrics
# sil_values <- numeric(length(k.values))
#
# # Compute metrics using a for loop
# for (i in seq_along(k.values)) {
#   k <- k.values[i]
#   cat("Processing k =", k, "\n")  # Print progress
#   sil_values[i] <- compute_silhouette(kmeans_clustering_data, k)
# }
#
# # Create Data Frame with Metrics
# cluster_metrics <- data.frame(
#   k = k.values,
#   Silhouette = sil_values
# )
#
# # Determine Optimal Clusters
# # Silhouette Method: Find the maximum silhouette width
# optimal_clusters_sil <- which.max(cluster_metrics$Silhouette) + 1  # +1 because k starts at 2
#
# cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_sil, "\n")
#
# # Visualize the Metrics
# # Silhouette Method Plot
# ggplot(cluster_metrics, aes(x = k, y = Silhouette)) +
#   geom_point() +
#   geom_line() +
#   geom_vline(xintercept = optimal_clusters_sil, linetype = "dashed", color = "blue") +
#   annotate("text", x = optimal_clusters_sil, y = max(cluster_metrics$Silhouette), label = paste("Optimal k =", optimal_clusters_sil), vjust = -1, color = "blue") +
#   labs(title = "Silhouette Method for Optimal Clusters",
#        x = "Number of Clusters (k)",
#        y = "Average Silhouette Width") +
#   theme_minimal()
