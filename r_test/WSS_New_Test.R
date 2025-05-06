# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Libraries
library(terra)
library(tools)

# Parameters
tif_folder <- "hs"
min_clusters <- 2
max_clusters <- 100
set.seed(123)

# Downsampling toggle
Downsample <- FALSE

# Function to compute optimal clusters via WSS second derivative
compute_optimal_k <- function(data_matrix, min_k = 2, max_k = 50) {
  if (nrow(data_matrix) < max_k) return(NA)
  
  # Scale data
  scaled_data <- scale(data_matrix)
  
  # cat(paste0("min_k: ",min_k," max_k: ",max_k))
  
  # Compute WSS
  wss_values <- sapply(min_k:max_k, function(k) {
    kmeans(scaled_data, centers = k, nstart = 10)$tot.withinss
  })
  
  # Calculate second derivative
  diff_wss <- diff(wss_values)
  diff2_wss <- diff(diff_wss)
  
  if (length(diff2_wss) > 0) {
    return(which.min(diff2_wss) + min_k + 1)
  } else {
    return(min_k)
  }
}

# List all .tif or .tiff files
tif_files <- list.files(tif_folder, pattern = "\\.tif(f)?$", full.names = TRUE)

# Loop over files
for (tif_path in tif_files) {
  cat("\nProcessing:", basename(tif_path), "\n")
  
  # Load raster
  r <- rast(tif_path)
  
  if (Downsample) {
    r <- terra::aggregate(r, fact = 2, fun = "sd", na.rm = TRUE)
  }
  
  # Extract values and clean
  mat <- as.matrix(terra::values(r))
  mat <- na.omit(mat)
  
  if (nrow(mat) < max_clusters) {
    cat("  ❌ Skipped - not enough valid pixels (", nrow(mat), " rows)\n")
    next
  }
  
  # Compute optimal clusters
  optimal_k <- compute_optimal_k(mat, min_clusters, max_clusters)
  cat("  ✅ Optimal clusters (WSS method):", optimal_k, "\n")
}
