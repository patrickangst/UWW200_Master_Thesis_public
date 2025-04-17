# rm(list = ls(all = TRUE))
# gc()
# graphics.off()
#
# devtools::load_all()
#
# # Load necessary libraries
# library(SpectralPatang)
# library(biodivMapR)
# library(doParallel)
#
# test_sites_folder_path <- 'D:/MasterThesis/final_hs_data_folder'
#
# # various variables
# num_cores_to_use <- detectCores() - 2
# Window_size <- 5
# TypePCA <- 'SPCA'
#
# part_one <- function(test_site_folder_path){
#
#   # define folder path variables
#   shapefile_plotlocation_folder_path <- file.path(test_site_folder_path, 'shapefile_plotlocation')
#   shapefile_cutline_folder_path <- file.path(test_site_folder_path, 'shapefile_cutline')
#   mask_folder_path <- file.path(test_site_folder_path, 'mask')
#   image_rgb_folder_path <- file.path(test_site_folder_path, 'image_rgb')
#   image_rectified_folder_path <- file.path(test_site_folder_path, 'image_rectified')
#   image_raw_folder_path <- file.path(test_site_folder_path, 'image_raw')
#   result_biodivMapR_folder_path <- file.path(test_site_folder_path, 'result_biodivMapR')
#   test_site_name <- basename(test_site_folder_path)
#
#   # create RGBs
#   SpectralPatang::create_RGB(image_raw_folder_path, "D:/MasterThesis/04_RGB",test_site_name)
#
#   cat(paste0('PCA done for: ', basename(test_site_folder_path), '\n'))
#
# }
#
#
# test_sites <- list.files(test_sites_folder_path)
#
# sites_done <- c("Hugo")
#
# for (site in test_sites) {
#
#   if (!(site %in% sites_done)) {
#     test_site_folder_path_loop <- file.path(test_sites_folder_path,site)
#     cat(paste0('Start process: ', site, '\n'))
#     part_one(test_site_folder_path_loop)
#     cat(paste0('End process: ', site, '\n'))
#     sites_done <- c(sites_done, site)
#   } else {
#     print(paste(site, " is already done."))
#   }
#
# }
#
#
# image_rectified_file_path <- 'D:/MasterThesis/final_hs_data_folder_test/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified/PCout'
# mask_file_path <- 'D:/MasterThesis/final_hs_data_folder_test/AN_TJ_1/mask/ang20220711t002111_rfl_v2aa2_img_rectified_savi_mask_02'
# result_biodivMapR_folder_path <- 'D:/MasterThesis/final_hs_data_folder_test/AN_TJ_1/result_biodivMapR'
#
# PCA_Output <- biodivMapR::perform_PCA(
#   Input_Image_File = image_rectified_file_path,
#   Input_Mask_File = mask_file_path,
#   Output_Dir = result_biodivMapR_folder_path,
#   Continuum_Removal = FALSE,
#   TypePCA = 'SPCA',
#   NbPCs_To_Keep = 30,
#   FilterPCA = FALSE,
#   nbCPU = 8,
#   MaxRAM = 8
# )
#
# dire <- "D:/MasterThesis/final_hs_data_folder/AN_TJ_1/result_biodivMapR"
#
#
# # List all files in the folder
# dire_files <- list.files(dire, full.names = TRUE)
#
# # Filter files without an extension
# dire_folder_without_ext <- dire_files[!grepl("\\.[a-zA-Z0-9]+$", basename(dire_files))]
#
# # Check if exactly one file without extension exists
# if (length(hs_file_without_ext) != 1) {
#   stop("Either no or multiple files without extensions found in the hs image folder.")
# }
# print(list.files())
#
#
#
#
#
#
#
#
#
#
# library(terra)
# library(NbClust)
# library(cluster)
#
#
# img <- rast('D:/MasterThesis/final_hs_data_folder/AN_TJ_1/result_biodivMapR/ang20220711t002111_rfl_v2aa2_img_rectified/SPCA/PCA/AN_TJ_1_pc_selection.tif')
#
# minmax(img)
#
# data_matrix <- as.matrix(img)
#
# sum(is.na(values(img)))  # Number of NA pixels
# sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels
#
# img[img == -9999] <- NA
#
#
# sum(is.na(values(img)))  # Number of NA pixels
# sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels
#
# data_matrix <- as.matrix(img)
#
# # Check if the matrix contains valid values
# dim(data_matrix)  # Should not be (0, X)
# summary(data_matrix)  # Ensure no Inf/-Inf
#
# # Remove NA rows
# data_matrix <- na.omit(data_matrix)
#
# # Check the dimensions again
# dim(data_matrix)  # Should still have data
#
# # Determine optimal clusters
# nb <- NbClust(data_matrix, min.nc=2, max.nc=10, method="kmeans")
# optimal_clusters <- nb$Best.nc[1]
#
# save.image(file = "D:/MasterThesis/08_nbclust_results/AN_TJ_1.RData")
#
# filename <- 'D:/MasterThesis/final_hs_data_folder/AN_TJ_1/result_biodivMapR/ang20220711t002111_rfl_v2aa2_img_rectified/SPCA/PCA/PCA_Info.RData'
# pca_shizzle <- load(filename)
#
# library(terra)
#
# spectral_species_tiff_file_path <- 'D:/MasterThesis/03_Spectral_Species/AN_TJ_1_SpectralSpecies.tiff'
#
# gdal_warp_command <- sprintf("gdalwarp -of GTiff -dstnodata %s %s %s",
#                              0,
#                              spectral_species_tiff_file_path,
#                              spectral_species_tiff_file_path)
#
# # Execute the GDAL edit command
# system(gdal_warp_command)
# rm(list = ls(all = TRUE))
# gc()
# graphics.off()
# #
# devtools::load_all()
#
# # Load necessary libraries
# library(biodivMapR)
# library(SpectralPatang)
#
#
# pca_selection_file_path <- SpectralPatang::analyse_biodiversity(rectified_image_file_path,
#                                                                 mask_image_file_path,
#                                                                 NBbclusters = 20,
#                                                                 Window_size = Window_size,
#                                                                 NbCPU = num_cores_to_use,
#                                                                 MaxRAM = 8,
#                                                                 Perform_PCA = FALSE,
#                                                                 Map_Species = FALSE,
#                                                                 Map_Alpha = FALSE,
#                                                                 MAP_Beta = FALSE,
#                                                                 PCA_Threshold = 99)


rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary libraries
library(terra)
library(dplyr)
library(pracma)
library(NbClust)
library(cluster)
library(factoextra)
library(ClusterR)
library(doParallel)

# Load hyperspectral image
image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified'
mask_path <- "/Users/patrickangst/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/final_hs_data_folder/AN_TJ_1/mask/ang20220711t002111_rfl_v2aa2_img_rectified_savi_mask_02"  # Replace with actual mask file path

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
compute_auc_pixel <- function(pixel_values, pixel_wavelengths, pixel_groups) {
  auc_values <- c()

  for (group in names(spectral_ranges)) {
    group_indices <- which(pixel_groups == group)

    if (length(group_indices) > 1) {  # Ensure multiple bands exist
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
auc_raster <- app(valid_image, fun = function(pixel_values) {
  compute_auc_pixel(pixel_values, valid_wavelengths, valid_groups)
})

# Rename raster bands
names(auc_raster) <- names(spectral_ranges)

# Apply mask AFTER computing AUC
auc_raster_masked <- auc_raster
auc_raster_masked[is.na(mask)] <- NA

# Save new raster
output_path <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/final_hs_data_folder/AN_TJ_1/image_rectified/AUC_Converted_Image.tif"
writeRaster(auc_raster_masked, output_path, overwrite = TRUE)

print("AUC per pixel raster created successfully!")


################################################################################
# Find optimal cluster
################################################################################
# Load AUC-transformed raster
auc_raster <- rast(output_path)

# Convert raster to dataframe (flattening pixels)
auc_values <- as.data.frame(auc_raster, xy = TRUE)  # Keep X, Y coordinates
pixel_values <- auc_values[, -c(1,2)]  # Exclude spatial coordinates

# Remove NA rows
pixel_values <- na.omit(pixel_values)

# Standardize
pixel_values_scaled <- scale(pixel_values)


nb <- NbClust(pixel_values_scaled, min.nc=2, max.nc=10, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

# # Use Parallel NbClust
# num_cores <- detectCores() - 1
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)
#
# clusterEvalQ(cl, library(NbClust))
#
#
# nb_results <- foreach(k = 2:10, .combine = cbind) %dopar% {
#   nb <- NbClust(pixel_values_scaled, distance = "euclidean", min.nc = k, max.nc = k, method = "kmeans", index = "silhouette")
#   return(nb$Best.nc[1,])
# }
#
# stopCluster(cl)
#
# # Find optimal clusters
# optimal_clusters <- as.numeric(names(which.max(table(nb_results))))
# print(paste("Optimal clusters:", optimal_clusters))



################################################################################
# Perform k-means clustering
################################################################################



pixel_matrix <- as.matrix(pixel_values)  # Convert to matrix

# Standardize (mean=0, sd=1)
pixel_matrix_scaled <- scale(pixel_matrix)

# Parallelize k-means++
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run k-means++ clustering
optimal_clusters <- 5  # Set manually or use NbClust
kmeans_result <- KMeans_rcpp(pixel_matrix_scaled, clusters = optimal_clusters, num_init = 10)

stopCluster(cl)

# Assign clusters back
auc_values$Cluster <- NA
auc_values$Cluster[!is.na(rowSums(pixel_values))] <- kmeans_result$clusters

# Convert back to raster and save
cluster_raster <- rast(auc_values[, c(1,2,ncol(auc_values))], type = "xyz")

crs(cluster_raster) <- crs(auc_raster)  # Copy CRS
ext(cluster_raster) <- ext(auc_raster)  # Copy extent

writeRaster(cluster_raster, "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/final_hs_data_folder/AN_TJ_1/image_rectified/AUC_Clustered_Image.tif", overwrite = TRUE)

print("Clustering completed & raster saved!")






rm(list = ls(all = TRUE))
gc()
graphics.off()

devtools::load_all()

library(SpectralPatang)

file_path <- '/Users/patrickangst/SynologyDrive/UZH/Quantitative Environmental Sciences/Studium/EEE311/Final_Project/biodivMapR-master/biodivMapR_Example/03_RESULTS/S2A_T33NUD_20180104_Subset/SPCA/PCA/OutputPCA_8_PCs'
SpectralPatang::get_optimal_cluster_number()
