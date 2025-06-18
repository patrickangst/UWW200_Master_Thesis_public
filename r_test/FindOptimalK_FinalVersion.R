# ------------------------------------------------------------------------------
# Description:
# This script performs automated k-means clustering on a batch of PCA-transformed 
# hyperspectral images (GeoTIFFs) located in a specified folder. For each image, 
# the script first removes any rows with missing (NA) values, scales the data, 
# and evaluates multiple values of the `nstart` parameter to identify the one 
# that produces the lowest within-cluster sum of squares (WSS) for a fixed number 
# of clusters. Once the optimal `nstart` value is selected, it evaluates the WSS 
# for a range of cluster numbers (`min_clusters` to `max_clusters`), using the 
# Elbow Method (based on the second derivative of the WSS curve) to determine the 
# optimal number of clusters.
#
# The final k-means clustering is then applied to the cleaned and scaled PCA matrix 
# using the selected optimal parameters. Cluster assignments are reshaped back into 
# the original image dimensions and saved as GeoTIFF rasters to a dedicated 
# output folder ("kmeans_analysis/"). This process is repeated for each image in 
# the input directory (`hs/`).
#
# Parameters:
# - `hs_folder`: Path to the folder containing PCA .tif files.
# - `min_clusters`, `max_clusters`: Range of k values to consider.
# - `nstart_values`: Vector of `nstart` values to test for stability of clustering.
#
# Returns:
# - A set of clustered raster files (GeoTIFF) named according to the source file 
#   and optimal cluster count, stored in the output folder.
#
# Dependencies:
# - terra: For raster handling and manipulation.
# - tools: For file path manipulation.
# - ggplot2: (Loaded but not actively used in this script.)
#
# Notes:
# - The script assumes that input rasters are PCA-reduced hyperspectral images.
# - Clustering is performed only if the image has sufficient non-NA pixels 
#   (more than `max_clusters`).
# - The Elbow Method assumes the WSS curve is smooth and convex. In cases where 
#   the curve is noisy or lacks an elbow, the minimum number of clusters is used 
#   as fallback.
# ------------------------------------------------------------------------------


# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required libraries
library(terra)
library(tools)
library(ggplot2)

# Parameters
hs_folder <- "hs/"  # Folder containing .tif files
min_clusters <- 2
max_clusters <- 50
nstart_values <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)  # Range of nstart values to test
set.seed(123)

# Create output folder if it doesn't exist
if (!dir.exists("kmeans_analysis")) {
  dir.create("kmeans_analysis")
}

# Get list of all .tif files in "hs/"
tif_files <- list.files(hs_folder, pattern = "\\.tif$", full.names = TRUE)

# Function to calculate WSS for a given number of clusters and nstart
calculate_wss <- function(data, k, nstart) {
  kmeans_result <- kmeans(data, centers = k, nstart = nstart)
  return(kmeans_result$tot.withinss)
}

# Iterate over each .tif file
for (hyperspectral_path in tif_files) {
  
  cat("\nProcessing file:", hyperspectral_path, "\n")
  
  # Load the PCA GeoTIFF
  pca_data <- rast(hyperspectral_path)
  
  # Convert to 2D matrix (pixels x principal components)
  pca_matrix <- as.matrix(terra::values(pca_data))
  
  # Track NA locations before removing missing values
  na_rows <- apply(pca_matrix, 1, function(x) any(is.na(x)))
  
  # Remove rows with NA for clustering
  pca_matrix_clean <- na.omit(pca_matrix)
  
  if (nrow(pca_matrix_clean) < max_clusters) {
    cat("Skipping file due to insufficient data points.\n")
    next
  }
  
  # Scale the cleaned matrix
  scaled_pca_matrix <- scale(pca_matrix_clean)
  
  # Step 1: Find the best nstart value
  best_nstart <- nstart_values[1]
  lowest_wss <- Inf
  
  cat("Evaluating different nstart values...\n")
  for (nstart in nstart_values) {
    wss_test <- calculate_wss(scaled_pca_matrix, k = min_clusters, nstart = nstart)
    if (wss_test < lowest_wss) {
      lowest_wss <- wss_test
      best_nstart <- nstart
    }
  }
  
  cat("Optimal nstart for", hyperspectral_path, ":", best_nstart, "\n")
  
  # Step 2: Evaluate WSS for different numbers of clusters using the best nstart
  potential_k_values <- min_clusters:max_clusters
  wss_values <- numeric(length(potential_k_values))
  
  cat("Evaluating WSS for different numbers of clusters...\n")
  
  for (i in seq_along(potential_k_values)) {
    k <- potential_k_values[i]
    wss_values[i] <- calculate_wss(scaled_pca_matrix, k, best_nstart)
  }
  
  # Create a data frame for plotting WSS
  wss_df_optimal_k <- data.frame(clusters = potential_k_values, WSS = wss_values)
  
  # Determine Optimal Clusters - Elbow Method (Second Derivative)
  diff_wss <- diff(wss_df_optimal_k$WSS)
  diff2_wss <- diff(diff_wss)
  
  optimal_k_elbow <- min_clusters
  if (length(diff2_wss) > 0) {
    optimal_k_elbow_index <- which.min(diff2_wss) + 1
    optimal_k_elbow <- potential_k_values[optimal_k_elbow_index]
  } else {
    cat("Not enough k values to calculate the second derivative. Suggesting minimum clusters.\n")
  }
  
  cat("Optimal k for", hyperspectral_path, ":", optimal_k_elbow, "\n")
  
  # Step 3: Perform final k-means clustering
  final_kmeans_result <- kmeans(scaled_pca_matrix, centers = optimal_k_elbow, nstart = best_nstart)
  cluster_assignments <- final_kmeans_result$cluster
  
  # Restore original shape: create a full vector with NA where necessary
  full_clusters <- rep(NA_integer_, nrow(pca_matrix)) # Initialize with NA
  full_clusters[!na_rows] <- cluster_assignments  # Fill in only non-NA rows
  
  # Reshape back into raster
  cluster_raster <- rast(pca_data, nlyr = 1)
  terra::values(cluster_raster) <- full_clusters
  
  # Save final clustered raster
  output_filename_cluster <- file.path("kmeans_analysis", paste0(file_path_sans_ext(basename(hyperspectral_path)), "_kmeans_clusters_k_", optimal_k_elbow, ".tif"))
  writeRaster(cluster_raster, filename = output_filename_cluster, overwrite = TRUE)
  cat(paste0("K-means cluster raster (k=", optimal_k_elbow, ") saved to: ", output_filename_cluster, "\n"))
  
}

cat("\n Batch k-means analysis complete.\n")
