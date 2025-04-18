# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load packages
library(terra)
library(NbClust)
library(dplyr)
library(ggplot2)
library(statip)

# --- Parameters ---
hyperspectral_path <-
  "hs/AN_TJ_2_pc_selection.tif"  # ← Change as needed
memory_limit_gb <- 64
bytes_per_value <- 4  # Assuming float32
nbclust_sample_size <- 10000  # Further limit for NbClust
min_clusters <- 2
max_clusters <- 50
set.seed(123)

# --- Load raster ---
raster_image <- tryCatch(
  rast(hyperspectral_path),
  error = function(e)
    stop("Error loading raster: ", e$message)
)

# --- Estimate max sample size based on memory limit ---
n_bands <- nlyr(raster_image)
total_cells <- ncell(raster_image)

memory_limit_bytes <- memory_limit_gb * 1024 ^ 3
max_sample_size <-
  floor(memory_limit_bytes / (n_bands * bytes_per_value))

sample_size <- min(total_cells, max_sample_size)

cat("Sampling",
    sample_size,
    "pixels based on memory cap of",
    memory_limit_gb,
    "GB...\n")

# --- Sample raster using terra ---
if (sample_size == 0)
  stop("Sample size is zero — check raster size or memory limit.")

sampled_raster <- tryCatch(
  spatSample(
    raster_image, size = sample_size, method = "random", na.rm = TRUE, as.points = FALSE
  ),
  error = function(e)
    stop("Sampling error: ", e$message)
)

if (ncell(sampled_raster) == 0)
  stop("Sampling returned no data.")

sampled_matrix <- as.matrix(sampled_raster, wide = TRUE) # convert to matrix

cat("Sampled matrix dimensions:",
    nrow(sampled_matrix),
    "x",
    ncol(sampled_matrix),
    "\n")

# --- Optional downsampling for NbClust ---
nbclust_input <- if (nrow(sampled_matrix) > nbclust_sample_size) {
  sampled_matrix[sample(1:nrow(sampled_matrix), nbclust_sample_size),]
} else {
  sampled_matrix
}

cat(
  "NbClust input matrix dimensions:",
  nrow(nbclust_input),
  "x",
  ncol(nbclust_input),
  "\n"
)

# --- Run NbClust ---
nb_result <- tryCatch({
  NbClust(
    data = nbclust_input,
    distance = "euclidean",
    min.nc = min_clusters,
    max.nc = max_clusters,
    method = "kmeans",
    index = "all"
  )
  
}, error = function(e)
  stop("NbClust failed: ", e$message))

# --- Extract best number of clusters ---
best_k <- as.numeric(nb_result$Best.nc[1, ])

majority_vote_number <- mfv(best_k)

print(paste("Most frequent number:", majority_vote_number))


if (is.na(majority_vote_number)) {
  stop("Best number of clusters (majority_vote_number) is NA. Cannot proceed with k-means.")
}

# Extract file name without path and extension
base_name <- tools::file_path_sans_ext(basename(hyperspectral_path))
workspace_filename <- file.path('nbclust_analysis',paste0(base_name, "_clusteranalysis.RData"))
save.image(file = workspace_filename)

# Save most frequent number to a text file
txt_filename <- file.path('nbclust_analysis', paste0(base_name, "_most_frequent_number.txt"))
write(majority_vote_number, file = txt_filename)


# # --- KMeans clustering (on full sample, not just NbClust subset) ---
# kmeans_model <- kmeans(sampled_matrix, centers = majority_vote_number)
# clustered_data <-
#   cbind(sampled_matrix, cluster = kmeans_model$cluster)
# 
# # --- Visualization (optional) ---
# clustered_df <- as.data.frame(clustered_data)
# 
# if (ncol(sampled_matrix) >= 2) {
#   # Get the column names
#   column_names <- colnames(clustered_df)
#   
#   # Select the first two columns (excluding "cluster") as x and y
#   x_col <- column_names[1]
#   y_col <- column_names[2]
#   
#   ggplot(clustered_df, aes_string(
#     x = column_names[1],  # Use first column name
#     y = column_names[2],  # Use second column name
#     color = factor(cluster)
#   )) +
#     geom_point(alpha = 0.6) +
#     labs(
#       title = paste("KMeans Clustering (k =", majority_vote_number, ")"),
#       x = column_names[1],  # Use first column name
#       y = column_names[2],  # Use second column name
#       color = "Cluster"
#     ) +
#     theme_minimal()
# } else {
#   message("Not enough bands for 2D visualization.")
# }

