# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load libraries
library(terra)
library(tools)
library(ggplot2)
library(dplyr)
library(dbscan)

# Parameters
tif_path <- "hs/AN_TJ_1_pc_selection.tif"  # your input .tif
output_path <- "hs/AN_TJ_1_hdbscan_clusters.tif"  # output .tif
minPts <- 2  # HDBSCAN parameter

# Load raster
r <- rast(tif_path)

# Extract raster values to matrix
mat <- as.matrix(values(r))  # dimension: (pixels) x (bands)

# Keep track of NA locations
na_rows <- apply(mat, 1, function(x) any(is.na(x)))

# Remove rows with NA for clustering
mat_clean <- mat[!na_rows, ]

# Run HDBSCAN
hdb_result <- hdbscan(mat_clean, minPts = minPts)

# Prepare full vector of cluster assignments (NA for original NA pixels)
full_clusters <- rep(NA_integer_, nrow(mat))
full_clusters[!na_rows] <- hdb_result$cluster  # Note: 0 = noise

# Optional: treat noise (cluster 0) as NA
full_clusters[full_clusters == 0] <- NA

# Create output raster
cluster_raster <- rast(r, nlyrs = 1)  # initialize with same geometry
values(cluster_raster) <- full_clusters

# Save result
writeRaster(cluster_raster, output_path, overwrite = TRUE)
cat("Cluster raster saved to:", output_path, "\n")
