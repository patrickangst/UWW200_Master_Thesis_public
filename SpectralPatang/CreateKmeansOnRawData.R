# Clean workspace
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required package
library(terra)
library(scico)


estimate_optimal_clusters <- function(data_matrix,
                                       Downsample = FALSE,
                                       Downsample_factor = 2,
                                       Downsample_function = "sd",
                                       Min_Cluster = 2,
                                       Max_Cluster = 50) {

  nstart_values <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  set.seed(123)

  # Find best nstart
  best_nstart <- nstart_values[1]
  lowest_wss <- Inf

  center_nstart_evaluation <- 20

  cat("Evaluating nstart values...\n")
  for (nstart in nstart_values) {
    wss <- kmeans(data_matrix, centers = center_nstart_evaluation, nstart = nstart)$tot.withinss
    if (wss < lowest_wss) {
      lowest_wss <- wss
      best_nstart <- nstart
    }
  }
  # cat("Best nstart:", best_nstart, "\n")

  # Evaluate WSS across cluster range
  potential_k_values <- Min_Cluster:Max_Cluster
  wss_values <- sapply(potential_k_values, function(k) {
    kmeans(data_matrix, centers = k, nstart = best_nstart)$tot.withinss
  })

  # Compute second derivative for elbow
  diff_wss <- diff(wss_values)
  diff2_wss <- diff(diff_wss)

  optimal_clusters_elbow <- Min_Cluster
  if (length(diff2_wss) > 0) {
    elbow_index <- which.min(diff2_wss) + 1
    optimal_clusters_elbow <- potential_k_values[elbow_index]
  } else {
    cat("Not enough points to compute elbow. Using Min_Cluster.\n")
  }

  # cat("Optimal number of clusters (Elbow Method):", optimal_clusters_elbow, "\n")

  # Save to a text file
  # output_folder_path <- dirname(Image_File_Path)
  # output_file_path <- file.path(output_folder_path, 'optimal_number_of_clusters.txt')
  # write(optimal_clusters_elbow, file = output_file_path)

  # return(optimal_clusters_elbow)
  return(list(
    optimal_clusters_elbow = optimal_clusters_elbow,
    best_nstart = best_nstart
  ))

}


# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_2/image_rectified/ang20220711t003358_rfl_v2aa2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/ATQ_VK_1/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_PW_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_VS_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_1/image_rectified/ang20190712t211646_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_2/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_3/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_4/image_rectified/ang20190706t210739_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_2/image_rectified/ang20220709t233937_rfl_v2aa2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_3/image_rectified/ang20190713t024201_rfl_v2v2_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUAIR_DW_1/image_rectified/ang20170709t003728_corr_v2p9_img_rectified'
# image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUARC_DW_1/image_rectified/ang20170709t000442_corr_v2p9_img_rectified'


image_paths <- c(
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_2/image_rectified/ang20220711t003358_rfl_v2aa2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/ATQ_VK_1/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_PW_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_VS_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_1/image_rectified/ang20190712t211646_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_2/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_3/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_4/image_rectified/ang20190706t210739_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_2/image_rectified/ang20220709t233937_rfl_v2aa2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_3/image_rectified/ang20190713t024201_rfl_v2v2_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUAIR_DW_1/image_rectified/ang20170709t003728_corr_v2p9_img_rectified',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUARC_DW_1/image_rectified/ang20170709t000442_corr_v2p9_img_rectified'
)

# image_paths <- c(
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified'
# )

savi_threshold <- 0.2

# Masked workflow
for (image_path in image_paths) {
    # Load hyperspectral image
  hyperspectral <- rast(image_path)

  test_site_image_name <- basename(dirname(dirname(image_path)))

  print(paste0(test_site_image_name,' masked start'))

  # # Run the same clustering with a SAVI mask applied to the raw image
  masked <- TRUE

  if (masked) {
    # Select appropriate bands for Red (670nm) and NIR ((800nm)
    # General formula: (800nm - 670nm) / (800nm + 670nm + L) * (1 + L)
    # red_band <- hyperspectral[[60]]    # Update with actual index
    # nir_band <- hyperspectral[[86]]    # Update with actual index
    # Calculate red band averages (used for SAVI)
    red_average <- terra::app(hyperspectral[[56:65]], fun = mean, na.rm = TRUE)
    # Calculate the mean of a few values of the near infrared bands (used for NDWI and SAVI)
    NIR_average <- terra::app(hyperspectral[[86:105]], fun = mean, na.rm = TRUE)

    # Compute SAVI
    L <- 0.5
    savi <- ((NIR_average - red_average) / (NIR_average + red_average + L)) * (1 + L)

    # Mask threshold (e.g., SAVI > 0.2 indicates vegetation)
    veg_mask <- savi > savi_threshold

    masked_hyperspectral <- mask(hyperspectral, veg_mask, maskvalues = FALSE)

    hyperspectral <- masked_hyperspectral
  }
  # Check number of bands
  nr_bands <- nlyr(hyperspectral)
  if (nr_bands != 425) {
    stop("Expected 425 bands, but found a different number.")
  }

  bands <- nlyr(hyperspectral)
  nrows <- nrow(hyperspectral)
  ncols <- ncol(hyperspectral)

  message(paste0(
    "Loaded raster with ",
    bands,
    " bands, ",
    nrows,
    " rows, ",
    ncols,
    " columns."
  ))

  pixel_matrix <- as.matrix(hyperspectral)

  # Rename columns for clarity
  colnames(pixel_matrix) <- paste0("Band_", 1:425)

  # Set selected bands to NA
  pixel_matrix[, 1:15]     <- NA  # Note: R is 1-indexed, not 0
  pixel_matrix[, 191:211]  <- NA
  pixel_matrix[, 285:320]  <- NA
  pixel_matrix[, 418:425]  <- NA

  pixel_matrix <- pixel_matrix[, colSums(!is.na(pixel_matrix)) > 0]

  # Remove rows with NA values
  valid_rows <- complete.cases(pixel_matrix)
  raster_clean <- pixel_matrix[valid_rows, ]

  cluster_analysis_result <- estimate_optimal_clusters(data_matrix=raster_clean)
  optimal_k <- cluster_analysis_result$optimal_clusters_elbow
  best_nstart <- cluster_analysis_result$best_nstart


  print(paste("Estimated optimal number of clusters: ", optimal_k))
  print(paste("Estimated optimal nstart: ", best_nstart))

  # Perform k-means clustering
  set.seed(42)
  kmeans_result <- kmeans(raster_clean, centers = optimal_k, nstart = best_nstart)

  # Create an empty vector for cluster labels
  clusters <- rep(NA, nrow(pixel_matrix))
  clusters[valid_rows] <- kmeans_result$cluster

  # Convert clusters to raster shape
  r_clusters <- rast(hyperspectral[[1]])  # Use first band as template
  values(r_clusters) <- clusters

  # Save output raster

  ouput_file_name <- paste0(test_site_image_name, '_KmeansOnRawData.tiff')
  if (masked) {
    ouput_file_name <- paste0(test_site_image_name, '_KmeansOnRawDataMasked.tiff')
  }

  output_path <- file.path('data/MasterThesis/RawDataClusteringMasked',ouput_file_name)
  writeRaster(r_clusters, output_path, overwrite = TRUE)

  # Visualize result

  plot(
    r_clusters,
    main = paste("K-means Clusters (k =", optimal_k, ")"),
    col = scico(optimal_k, palette = "batlow")
  )

  print(paste0(test_site_image_name,' masked done'))

  # Perform a little housekeeping
  gc()

}









# # Clean workspace
# rm(list = ls(all = TRUE))
# gc()
# graphics.off()
#
# # Load required package
# library(terra)
# library(scico)
#
#
# estimate_optimal_clusters <- function(data,
#                                       max_k = 50,
#                                       seed = 42,
#                                       plot = TRUE) {
#   wcss <- numeric(max_k)
#
#   for (k in 1:max_k) {
#     set.seed(seed)
#     km <- kmeans(data, centers = k)
#     wcss[k] <- km$tot.withinss
#   }
#
#   # Optional plot
#   if (plot) {
#     plot(
#       1:max_k,
#       wcss,
#       type = "b",
#       pch = 19,
#       frame = FALSE,
#       xlab = "Number of Clusters (k)",
#       ylab = "Total Within-Cluster Sum of Squares (WCSS)",
#       main = "Elbow Method for Optimal k"
#     )
#   }
#
#   # Heuristic: Look for biggest drop in WCSS (second derivative)
#   delta_wcss <- diff(wcss)
#   accel <- diff(delta_wcss)
#   elbow_k <- which.max(-accel) + 1  # +1 shifts back to correct k
#
#   return(elbow_k)
# }
#
#
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_2/image_rectified/ang20220711t003358_rfl_v2aa2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/ATQ_VK_1/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_PW_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_VS_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_1/image_rectified/ang20190712t211646_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_2/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_3/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_4/image_rectified/ang20190706t210739_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_2/image_rectified/ang20220709t233937_rfl_v2aa2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_3/image_rectified/ang20190713t024201_rfl_v2v2_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUAIR_DW_1/image_rectified/ang20170709t003728_corr_v2p9_img_rectified'
# # image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUARC_DW_1/image_rectified/ang20170709t000442_corr_v2p9_img_rectified'
#
#
# image_paths <- c(
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_2/image_rectified/ang20220711t003358_rfl_v2aa2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/ATQ_VK_1/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_PW_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_VS_1/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_1/image_rectified/ang20190712t211646_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_2/image_rectified/ang20190712t212208_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_3/image_rectified/ang20190712t231624_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_4/image_rectified/ang20190706t210739_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_2/image_rectified/ang20220709t233937_rfl_v2aa2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_3/image_rectified/ang20190713t024201_rfl_v2v2_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUAIR_DW_1/image_rectified/ang20170709t003728_corr_v2p9_img_rectified',
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUARC_DW_1/image_rectified/ang20170709t000442_corr_v2p9_img_rectified'
# )
#
# savi_threshold <- 0.2
#
# for (image_path in image_paths) {
#   # Load hyperspectral image
#   hyperspectral <- rast(image_path)
#
#   test_site_image_name <- basename(dirname(dirname(image_path)))
#
#   print(paste0(test_site_image_name,' start'))
#
#   masked <- FALSE
#
#   if (masked) {
#     # Select appropriate bands for Red (670nm) and NIR ((800nm)
#     # General formula: (800nm - 670nm) / (800nm + 670nm + L) * (1 + L)
#     # red_band <- hyperspectral[[60]]    # Update with actual index
#     # nir_band <- hyperspectral[[86]]    # Update with actual index
#     # Calculate red band averages (used for SAVI)
#     red_average <- terra::app(tile[[56:65]], fun = mean, na.rm = TRUE)
#     # Calculate the mean of a few values of the near infrared bands (used for NDWI and SAVI)
#     NIR_average <- terra::app(tile[[86:105]], fun = mean, na.rm = TRUE)
#
#     # Compute SAVI
#     L <- 0.5
#     savi <- ((NIR_average - red_average) / (NIR_average + red_average + L)) * (1 + L)
#
#     # Mask threshold (e.g., SAVI > 0.2 indicates vegetation)
#     veg_mask <- savi > savi_threshold
#
#     masked_hyperspectral <- mask(hyperspectral, veg_mask, maskvalues = FALSE)
#
#     hyperspectral <- masked_hyperspectral
#   }
#   # Check number of bands
#   nr_bands <- nlyr(hyperspectral)
#   if (nr_bands != 425) {
#     stop("Expected 425 bands, but found a different number.")
#   }
#
#   bands <- nlyr(hyperspectral)
#   nrows <- nrow(hyperspectral)
#   ncols <- ncol(hyperspectral)
#
#   message(paste0(
#     "Loaded raster with ",
#     bands,
#     " bands, ",
#     nrows,
#     " rows, ",
#     ncols,
#     " columns."
#   ))
#
#   pixel_matrix <- as.matrix(hyperspectral)
#
#   # Rename columns for clarity
#   colnames(pixel_matrix) <- paste0("Band_", 1:425)
#
#   # Set selected bands to NA
#   pixel_matrix[, 1:15]     <- NA  # Note: R is 1-indexed, not 0
#   pixel_matrix[, 191:211]  <- NA
#   pixel_matrix[, 285:320]  <- NA
#   pixel_matrix[, 418:425]  <- NA
#
#   pixel_matrix <- pixel_matrix[, colSums(!is.na(pixel_matrix)) > 0]
#
#   # Remove rows with NA values
#   valid_rows <- complete.cases(pixel_matrix)
#   raster_clean <- pixel_matrix[valid_rows, ]
#
#   optimal_k <- estimate_optimal_clusters(raster_clean, max_k = 50)
#   print(paste("Estimated optimal number of clusters:", optimal_k))
#
#   # Perform k-means clustering
#   set.seed(42)
#   kmeans_result <- kmeans(raster_clean, centers = optimal_k, nstart = 50)
#
#   # Create an empty vector for cluster labels
#   clusters <- rep(NA, nrow(pixel_matrix))
#   clusters[valid_rows] <- kmeans_result$cluster
#
#   # Convert clusters to raster shape
#   r_clusters <- rast(hyperspectral[[1]])  # Use first band as template
#   values(r_clusters) <- clusters
#
#   # Save output raster
#
#   ouput_file_name <- paste0(test_site_image_name, '_KmeansOnRawData.tiff')
#   if (masked) {
#     ouput_file_name <- paste0(test_site_image_name, '_KmeansOnRawDataMasked.tiff')
#   }
#
#   output_path <- file.path('data/MasterThesis/RawDataClustering',ouput_file_name)
#   writeRaster(r_clusters, output_path, overwrite = TRUE)
#
#   # Visualize result
#
#   plot(
#     r_clusters,
#     main = paste("K-means Clusters (k =", optimal_k, ")"),
#     col = scico(optimal_k, palette = "batlow")
#   )
#
#   # Perform a little housekeeping
#   gc()
#
# }
#
#
# # Masked workflow
# for (image_path in image_paths) {
#   # Load hyperspectral image
#   hyperspectral <- rast(image_path)
#
#   test_site_image_name <- basename(dirname(dirname(image_path)))
#
#   print(paste0(test_site_image_name,' masked start'))
#
#   # # Run the same clustering with a SAVI mask applied to the raw image
#   masked <- TRUE
#
#   if (masked) {
#     # Select appropriate bands for Red (670nm) and NIR ((800nm)
#     # General formula: (800nm - 670nm) / (800nm + 670nm + L) * (1 + L)
#     # red_band <- hyperspectral[[60]]    # Update with actual index
#     # nir_band <- hyperspectral[[86]]    # Update with actual index
#     # Calculate red band averages (used for SAVI)
#     red_average <- terra::app(hyperspectral[[56:65]], fun = mean, na.rm = TRUE)
#     # Calculate the mean of a few values of the near infrared bands (used for NDWI and SAVI)
#     NIR_average <- terra::app(hyperspectral[[86:105]], fun = mean, na.rm = TRUE)
#
#     # Compute SAVI
#     L <- 0.5
#     savi <- ((NIR_average - red_average) / (NIR_average + red_average + L)) * (1 + L)
#
#     # Mask threshold (e.g., SAVI > 0.2 indicates vegetation)
#     veg_mask <- savi > savi_threshold
#
#     masked_hyperspectral <- mask(hyperspectral, veg_mask, maskvalues = FALSE)
#
#     hyperspectral <- masked_hyperspectral
#   }
#   # Check number of bands
#   nr_bands <- nlyr(hyperspectral)
#   if (nr_bands != 425) {
#     stop("Expected 425 bands, but found a different number.")
#   }
#
#   bands <- nlyr(hyperspectral)
#   nrows <- nrow(hyperspectral)
#   ncols <- ncol(hyperspectral)
#
#   message(paste0(
#     "Loaded raster with ",
#     bands,
#     " bands, ",
#     nrows,
#     " rows, ",
#     ncols,
#     " columns."
#   ))
#
#   pixel_matrix <- as.matrix(hyperspectral)
#
#   # Rename columns for clarity
#   colnames(pixel_matrix) <- paste0("Band_", 1:425)
#
#   # Set selected bands to NA
#   pixel_matrix[, 1:15]     <- NA  # Note: R is 1-indexed, not 0
#   pixel_matrix[, 191:211]  <- NA
#   pixel_matrix[, 285:320]  <- NA
#   pixel_matrix[, 418:425]  <- NA
#
#   pixel_matrix <- pixel_matrix[, colSums(!is.na(pixel_matrix)) > 0]
#
#   # Remove rows with NA values
#   valid_rows <- complete.cases(pixel_matrix)
#   raster_clean <- pixel_matrix[valid_rows, ]
#
#   optimal_k <- estimate_optimal_clusters(raster_clean, max_k = 50)
#   print(paste("Estimated optimal number of clusters:", optimal_k))
#
#   # Perform k-means clustering
#   set.seed(42)
#   kmeans_result <- kmeans(raster_clean, centers = optimal_k, nstart = 50)
#
#   # Create an empty vector for cluster labels
#   clusters <- rep(NA, nrow(pixel_matrix))
#   clusters[valid_rows] <- kmeans_result$cluster
#
#   # Convert clusters to raster shape
#   r_clusters <- rast(hyperspectral[[1]])  # Use first band as template
#   values(r_clusters) <- clusters
#
#   # Save output raster
#
#   ouput_file_name <- paste0(test_site_image_name, '_KmeansOnRawData.tiff')
#   if (masked) {
#     ouput_file_name <- paste0(test_site_image_name, '_KmeansOnRawDataMasked.tiff')
#   }
#
#   output_path <- file.path('data/MasterThesis/RawDataClusteringMasked',ouput_file_name)
#   writeRaster(r_clusters, output_path, overwrite = TRUE)
#
#   # Visualize result
#
#   plot(
#     r_clusters,
#     main = paste("K-means Clusters (k =", optimal_k, ")"),
#     col = scico(optimal_k, palette = "batlow")
#   )
#
#   print(paste0(test_site_image_name,' masked done'))
#
#   # Perform a little housekeeping
#   gc()
#
# }

