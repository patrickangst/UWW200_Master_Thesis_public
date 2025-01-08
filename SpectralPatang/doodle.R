# directory_path <- "/path/to/your/tewe.txt"
#
# # Replace the last directory with "mine"
# new_directory_path <- file.path(dirname(directory_path), "mine")
#
# print(new_directory_path)
#
#
# rm(list = ls())
# graphics.off()
#
# devtools::load_all()
#
# # Load necessary libraries
# library(doParallel)
# library(foreach)
# library(SpectralPatang)
# library(terra)
#
# raw_image_file_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/hs_raw_image/ang20190706t235120_rfl_v2v2_img'
# rectified_image_file_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/rectified/ang20190706t235120_rfl_v2v2_img_rectified_cut'
# cut_shp <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/plotlocations/06_Utqiaqvik_IBP_boundingbox_buffered.shp'
# savi_file_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/mask'
# savi_file_path <- paste0(savi_file_path,'/ang20190706t235120_rfl_v2v2_img_rectified_savi_mask_02_cut')
#
# gdal_command_rectify <- sprintf(
#   "gdalwarp -cutline %s -crop_to_cutline -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
#   cut_shp,
#   raw_image_file_path,
#   rectified_image_file_path
# )
#
# # # Execute the command in R
# system(gdal_command_rectify)
#
#
#
# SpectralPatang::create_SAVI_mask(rectified_image_file_path,savi_file_path)
#
#
# num_cores <- parallel::detectCores()
#
# SpectralPatang::analyse_biodiversity(rectified_image_file_path,
#                                      savi_file_path,
#                                      NBbclusters = 5,
#                                      Window_size = 10,
#                                      NbCPU = num_cores,
#                                      MaxRAM = 8,
#                                      Perform_PCA = TRUE,
#                                      PCA_Threshold = 99)
#
#
#
# uesche <- read.csv("~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/species_analysis/tvexport.csv")
#
# dirpath <- "~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/species_analysis/tvexport.csv"
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
#
#
#
#
#
#
#
# rm(list = ls())
# graphics.off()
#
# devtools::load_all()
#
# # Load necessary libraries
# library(doParallel)
# library(foreach)
# library(SpectralPatang)
# library(terra)
#
#
#
# get_optimal_cluster_number_local <- function(SpatRaster,
#                                        Min_Cluster = 2,
#                                        Max_Cluster = 30) {
#   # Load the raster image
#   pca_data <- as.matrix(terra::values(SpatRaster))
#
#   # Define cores for parallel processing
#   num_cores <- parallel::detectCores() - 2
#
#   # Prepare the data
#   pca_data_na_omitted <- na.omit(pca_data)  # Clean the data (remove NAs)
#   pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data for clustering
#   kmeans_clustering_data <- pca_data_na_omitted_scaled
#
#   # Define Range of Clusters
#   k.values <- Min_Cluster:Max_Cluster
#
#   # Define Clustering Metrics Function
#   compute_wss <- function(data, k, seed = 123) {
#     set.seed(seed)
#     kmeans(data, centers = k, nstart = 10)$tot.withinss
#   }
#
#   # Setup Parallel Cluster
#   cl <- parallel::makeCluster(num_cores)
#
#   # Export required objects and functions to the worker nodes
#   parallel::clusterExport(
#     cl,
#     varlist = c("kmeans_clustering_data", "compute_wss"),
#     envir = environment()
#   )
#
#   # Compute Metrics in Parallel
#   wss_values <- parallel::parSapply(cl, k.values, function(k) {
#     compute_wss(kmeans_clustering_data, k)
#   })
#
#   # Stop Cluster
#   parallel::stopCluster(cl)
#
#   # Create Data Frame with Metrics
#   cluster_metrics <- data.frame(k = k.values, WSS = wss_values)
#
#   # Determine Optimal Clusters
#   # Elbow Method: Find the point where the second derivative is minimized
#   diff_wss <- diff(cluster_metrics$WSS)
#   diff2_wss <- diff(diff_wss)
#   optimal_clusters_elbow <- which.min(diff2_wss) + Min_Cluster  # Adjust by Min_Cluster
#
#   return(optimal_clusters_elbow)
#
# }
#
# Image_File_Path <- '/media/patang/T9/d_done/d_ang20190713t002123rfl/result/ang20190713t002123_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection.tif'
#
# r <- terra::rast(Image_File_Path)
#
# # Downsample by a factor, preserving variability (standard deviation)
# factor <- 2  # Adjust as needed
# r_downsampled <- terra::aggregate(r, fact = factor, fun = sd, na.rm = TRUE)
#
# # Check new raster properties
# print(res(r_downsampled))
# print(dim(r_downsampled))
#
# r_values <- as.matrix(terra::values(r))
# r_downsampled_values <- as.matrix(terra::values(r_downsampled))
#
# # Plot to verify
# plot(r_downsampled, main = "Downsampled Raster (Preserving Variability)")
#
# clusters <- get_optimal_cluster_number_local(r_downsampled)
#
# output_folder_path <- dirname(Image_File_Path)
# output_file_path <- file.path(output_folder_path,'optimal_number_of_clusters.txt')
# write(clusters, file = output_file_path)
#
# print(clusters)





rm(list=ls(all=TRUE));gc()
graphics.off()

library(stars)
library(terra)

Image_File_Path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/PCA_selected.tif'
Image_File_Path_rectified <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/PCA_selected_rectified'


raster_data <- terra::rast(Image_File_Path_rectified)

plot(raster_data)
