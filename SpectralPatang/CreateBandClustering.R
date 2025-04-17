rm(list = ls(all = TRUE))
gc()
graphics.off()

devtools::load_all()

# Load necessary libraries
library(terra)
library(dplyr)
library(pracma)
library(NbClust)
library(cluster)
library(factoextra)
library(ClusterR)
library(doParallel)
library(SpectralPatang)
library(dbscan)
library(mclust)


# Load hyperspectral image
base_folder <- '/Users/patrickangst/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/final_hs_data_folder/ATQ_VK_1'
image_path <- file.path(base_folder,'image_rectified/ang20190712t231624_rfl_v2v2_img_rectified')
mask_path <- file.path(base_folder,'mask/ang20190712t231624_rfl_v2v2_img_rectified_savi_mask_02')
AUC_Converted_Image_Path <- file.path(base_folder,'image_rectified/AUC_Converted_Image.tif')
AUC_Clustered_Image_Path <- file.path(base_folder,'image_rectified/AUC_Clustered_Image.tif')
AUC_Shannon_Diversity_Map <- file.path(base_folder,'image_rectified/AUC_Shannon_Diversity_Map.tif')



DBSCAN_Clustered_Image_Path <- file.path(base_folder,'image_rectified/DBSCAN_Clustered_Image.tif')
Hierarchical_Clustered_Image_Path <- file.path(base_folder,'image_rectified/Hierarchical_Clustered_Image.tif')
GMM_Clustered_Image_Path <- file.path(base_folder,'image_rectified/GMM_Clustered_Image.tif')



raw_image <- terra::rast(image_path)
mask <- terra::rast(mask_path)

# Convert mask to logical: assuming 0 = masked, 1 = valid pixels
mask[mask == 0] <- NA

# Extract wavelengths
num_bands <- terra::nlyr(raw_image)
band_names <- names(raw_image)
wavelengths <- as.numeric(gsub(" Nanometers", "", band_names))

# Define excluded wavelength regions
excluded_regions <- list(
  unfit1 = c(0, 442),
  water_vapor1 = c(1368, 1499),
  water_vapor2 = c(1779, 2055),
  unfit2 = c(2400, 2501)
)

# Define spectral ranges
spectral_ranges <- list(
  Blue = c(400, 500),
  Green = c(500, 600),
  Red = c(600, 700),
  `Red Edge` = c(700, 750),
  NIR = c(750, 1300),
  `SWIR-1` = c(1300, 1800),
  `SWIR-2` = c(1800, 2500)
)

# Function to classify wavelengths
classify_band <- function(wavelength) {
  for (region in excluded_regions) {
    if (wavelength >= region[1] & wavelength <= region[2]) {
      return(NA)  # Mark as NA (invalid band)
    }
  }

  for (name in names(spectral_ranges)) {
    range <- spectral_ranges[[name]]
    if (wavelength > range[1] & wavelength <= range[2]) {
      return(name)
    }
  }

  return(NA)  # Mark as NA if it doesn't fit any category
}

# Apply classification
band_groups <- sapply(wavelengths, classify_band)

# Get valid bands
valid_indices <- which(!is.na(band_groups))
valid_wavelengths <- wavelengths[valid_indices]
valid_image <- raw_image[[valid_indices]]
valid_groups <- band_groups[valid_indices]

# Function to compute AUC for a single pixel
compute_auc_pixel <- function(pixel_values,
                              pixel_wavelengths,
                              pixel_groups) {
  auc_values <- c()

  for (group in names(spectral_ranges)) {
    group_indices <- which(pixel_groups == group)

    if (length(group_indices) > 1) {
      # Ensure multiple bands exist
      group_wavelengths <- pixel_wavelengths[group_indices]
      group_values <- pixel_values[group_indices]
      auc_values <- c(auc_values, trapz(group_wavelengths, group_values))
    } else {
      auc_values <- c(auc_values, NA)  # Assign NA if not enough data
    }
  }

  return(auc_values)
}

# Apply AUC calculation pixel by pixel
auc_raster <- app(
  valid_image,
  fun = function(pixel_values) {
    compute_auc_pixel(pixel_values, valid_wavelengths, valid_groups)
  }
)

# Rename raster bands
names(auc_raster) <- names(spectral_ranges)

# Apply mask AFTER computing AUC
auc_raster_masked <- auc_raster
auc_raster_masked[is.na(mask)] <- NA

# Save new raster
writeRaster(auc_raster_masked, AUC_Converted_Image_Path, overwrite = TRUE)

print("AUC per pixel raster created successfully!")


################################################################################
# Find optimal cluster
################################################################################
# Load AUC-transformed raster
# auc_raster <- rast(output_path)
#
# # Convert raster to dataframe (flattening pixels)
auc_values <- as.data.frame(auc_raster, xy = TRUE)  # Keep X, Y coordinates
pixel_values <- auc_values[, -c(1, 2)]  # Exclude spatial coordinates
#
# # Remove NA rows
pixel_values <- na.omit(pixel_values)
#
# # Standardize
pixel_values_scaled <- scale(pixel_values)
#
#
# nb <- NbClust(
#   pixel_values_scaled,
#   min.nc = 2,
#   max.nc = 10,
#   method = "kmeans"
# )
# optimal_clusters <- nb$Best.nc[1]


optimal_clusters <- SpectralPatang::get_optimal_cluster_number(
  AUC_Converted_Image_Path,
  Downsample = TRUE,
  Downsample_factor = 2,
  Downsample_function = "sd",
  Min_Cluster = 2,
  Max_Cluster = 50
)



################################################################################
# Perform k-means clustering
################################################################################
pixel_matrix <- as.matrix(pixel_values)  # Convert to matrix

# Identify valid pixels (non-masked)
valid_pixel_indices <- which(!is.na(rowSums(pixel_matrix)))  # Indices of valid pixels
pixel_matrix_valid <- pixel_matrix[valid_pixel_indices, , drop = FALSE]  # Only valid pixels

# Standardize (mean=0, sd=1)
pixel_matrix_scaled <- scale(pixel_matrix_valid)

# Parallelize k-means++
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run k-means++ clustering (ONLY on valid pixels)
kmeans_result <- KMeans_rcpp(pixel_matrix_scaled,
                             clusters = optimal_clusters,
                             num_init = 10)

stopCluster(cl)

# Initialize full cluster array with NA
full_cluster_array <- rep(NA, nrow(pixel_matrix))  # Default all to NA
full_cluster_array[valid_pixel_indices] <- kmeans_result$clusters  # Assign only to valid pixels

# Assign clusters back to the AUC values
auc_values$Cluster <- full_cluster_array

# Convert back to raster and save
cluster_raster <- rast(auc_values[, c(1, 2, ncol(auc_values))], type = "xyz")

# Copy CRS & extent from AUC raster
crs(cluster_raster) <- crs(auc_raster)
ext(cluster_raster) <- ext(auc_raster)

# Ensure masked pixels remain NA
cluster_raster[is.na(mask)] <- NA

# Save final clustered raster
writeRaster(cluster_raster, AUC_Clustered_Image_Path, overwrite = TRUE)

print("Clustering completed & raster saved! Masked pixels are preserved as NA.")

################################################################################
# DBSCAN (Density-Based Spatial Clustering)
################################################################################
# Standardize the pixel values
pixel_matrix_scaled <- scale(as.matrix(pixel_values))

# Run DBSCAN (eps = neighborhood size, minPts = min. points per cluster)
dbscan_result <- dbscan(pixel_matrix_scaled, eps = 0.5, minPts = 5)

# Assign clusters (noise points get assigned -1)
auc_values$Cluster <- dbscan_result$cluster

# Convert back to raster and save
cluster_raster <- rast(auc_values[, c(1, 2, ncol(auc_values))], type = "xyz")

crs(cluster_raster) <- crs(auc_raster)
ext(cluster_raster) <- ext(auc_raster)

writeRaster(cluster_raster, DBSCAN_Clustered_Image_Path, overwrite = TRUE)

print("DBSCAN clustering completed & raster saved!")

################################################################################
# Hierarchical Clustering (Agglomerative)
################################################################################
# Standardize data
pixel_matrix_scaled <- scale(as.matrix(pixel_values))

# Compute hierarchical clustering (Euclidean distance, Ward's method)
hc <- hclust(dist(pixel_matrix_scaled), method = "ward.D2")

# Automatically find optimal clusters using dynamic tree cut
library(dynamicTreeCut)
cluster_labels <- cutreeDynamic(hc, distM = as.matrix(dist(pixel_matrix_scaled)))

# Assign clusters
auc_values$Cluster <- cluster_labels

# Convert back to raster and save
cluster_raster <- rast(auc_values[, c(1, 2, ncol(auc_values))], type = "xyz")

crs(cluster_raster) <- crs(auc_raster)
ext(cluster_raster) <- ext(auc_raster)

writeRaster(cluster_raster, Hierarchical_Clustered_Image_Path, overwrite = TRUE)

print("Hierarchical clustering completed & raster saved!")


################################################################################
# Gaussian Mixture Models (GMM)
################################################################################
# Standardize data
pixel_matrix_scaled <- scale(as.matrix(pixel_values))

# Fit Gaussian Mixture Model (GMM) and determine clusters
gmm_result <- Mclust(pixel_matrix_scaled)

# Assign clusters
auc_values$Cluster <- gmm_result$classification

# Convert back to raster and save
cluster_raster <- rast(auc_values[, c(1, 2, ncol(auc_values))], type = "xyz")

crs(cluster_raster) <- crs(auc_raster)
ext(cluster_raster) <- ext(auc_raster)

writeRaster(cluster_raster, GMM_Clustered_Image_Path, overwrite = TRUE)

print("GMM clustering completed & raster saved!")


################################################################################
# Shannon for k-means cluster
################################################################################

pre_shannon_cluster <- AUC_Clustered_Image_Path

# Define moving window size (e.g., 5x5 pixels)
window_size <- 5

# Function to compute Shannon Diversity in a window
shannon_diversity <- function(values) {
  values <- na.omit(values)  # Remove NA values
  if (length(values) == 0) return(NA)  # Return NA if window is empty

  cluster_freq <- table(values) / length(values)  # Get proportions
  H <- -sum(cluster_freq * log(cluster_freq))  # Compute Shannon Index

  return(H)
}

# Apply moving window
diversity_raster <- focal(cluster_raster, w = matrix(1, window_size, window_size), fun = shannon_diversity, na.policy = "omit")

# Save output
writeRaster(diversity_raster, AUC_Shannon_Diversity_Map, overwrite = TRUE)

print("✅ Shannon Diversity Map created successfully!")
