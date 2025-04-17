# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load packages
library(terra)
library(NbClust)
library(dplyr)
library(ggplot2)

# --- Parameters ---
hyperspectral_path <- "hs/AN_TJ_1_pc_selection.tif"  # ← Change as needed
sample_size_max <- 1000
min_clusters <- 2
max_clusters <- 10
set.seed(123)

# --- Load raster ---
raster_image <- tryCatch(rast(hyperspectral_path), 
                         error = function(e) stop("Error loading raster: ", e$message))

# --- Sample raster ---
if (ncell(raster_image) == 0) stop("Raster has no cells.")

sample_size <- min(ncell(raster_image), sample_size_max)
sampled_matrix <- tryCatch(
  as.matrix(spatSample(raster_image, size = sample_size, method = "random")),
  error = function(e) stop("Sampling error: ", e$message)
)

if (nrow(sampled_matrix) == 0) stop("Sampling returned no data.")
cat("Sampled matrix dimensions:", nrow(sampled_matrix), "x", ncol(sampled_matrix), "\n")

# --- Run NbClust ---
nb_result <- tryCatch({
  NbClust(data = sampled_matrix,
          distance = "euclidean",
          min.nc = min_clusters,
          max.nc = max_clusters,
          method = "kmeans",
          index = "alllong")
}, error = function(e) stop("NbClust failed: ", e$message))

# --- Extract results ---
best_k <- nb_result$Best.nc["Number_clusters"]
cat("Best number of clusters suggested:", best_k, "\n")

# --- KMeans clustering ---
kmeans_model <- kmeans(sampled_matrix, centers = best_k)
clustered_data <- cbind(sampled_matrix, cluster = kmeans_model$cluster)

# --- Visualization (optional) ---
clustered_df <- as.data.frame(clustered_data)

if (ncol(sampled_matrix) >= 2) {
  ggplot(clustered_df, aes(x = V1, y = V2, color = factor(cluster))) +
    geom_point(alpha = 0.6) +
    labs(title = paste("KMeans Clustering (k =", best_k, ")"),
         x = "Band 1", y = "Band 2", color = "Cluster") +
    theme_minimal()
} else {
  message("Not enough bands for 2D visualization.")
}
