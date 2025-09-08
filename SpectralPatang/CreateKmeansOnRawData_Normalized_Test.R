# Clean workspace
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required package
library(terra)
library(scico)
library(SpectralPatang)

get_optimal_cluster_number <- function(Image_File_Path,
                                       Downsample = FALSE,
                                       Downsample_factor = 2,
                                       Downsample_function = "sd",
                                       Min_Cluster = 2,
                                       Max_Cluster = 50) {

  nstart_values <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  set.seed(123)

  cat("\nProcessing file:", Image_File_Path, "\n")

  # Load the PCA GeoTIFF
  pca_data <- rast(Image_File_Path)

  # Optional downsampling
  if (Downsample) {
    fun <- match.fun(Downsample_function)
    pca_data <- aggregate(pca_data, fact = Downsample_factor, fun = fun)
  }

  # Convert raster to matrix
  pca_matrix <- as.matrix(terra::values(pca_data))

  # Clean matrix: remove rows with NA/NaN/Inf
  pca_matrix <- pca_matrix[apply(pca_matrix, 1, function(x) all(is.finite(x))), , drop = FALSE]

  # Remove zero-variance columns (otherwise scale() gives NaN/Inf)
  nzv <- apply(pca_matrix, 2, sd, na.rm = TRUE) > 0
  pca_matrix <- pca_matrix[, nzv, drop = FALSE]

  if (nrow(pca_matrix) < Max_Cluster) {
    warning("Insufficient data points after cleaning. Skipping.")
    return(NULL)
  }

  # Find best nstart
  best_nstart <- nstart_values[1]
  lowest_wss <- Inf

  cat("Evaluating nstart values...\n")
  for (nstart in nstart_values) {
    wss <- kmeans(pca_matrix, centers = Min_Cluster, nstart = nstart)$tot.withinss
    if (wss < lowest_wss) {
      lowest_wss <- wss
      best_nstart <- nstart
    }
  }
  # cat("Best nstart:", best_nstart, "\n")

  # Evaluate WSS across cluster range
  potential_k_values <- Min_Cluster:Max_Cluster
  wss_values <- sapply(potential_k_values, function(k) {
    kmeans(pca_matrix, centers = k, nstart = best_nstart)$tot.withinss
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
  output_folder_path <- dirname(Image_File_Path)
  output_file_path <- file.path(output_folder_path, 'optimal_number_of_clusters.txt')
  write(optimal_clusters_elbow, file = output_file_path)

  # return(optimal_clusters_elbow)
  return(list(
    optimal_clusters_elbow = optimal_clusters_elbow,
    best_nstart = best_nstart
  ))

}


image_paths <- c(
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_normalized/AN_TJ_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_2/image_normalized/AN_TJ_2_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/ATQ_VK_1/image_normalized/ATQ_VK_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_PW_1/image_normalized/BRW_PW_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_VS_1/image_normalized/BRW_VS_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_1/image_normalized/FLXTWRZONA_SD_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_2/image_normalized/FLXTWRZONA_SD_2_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_3/image_normalized/FLXTWRZONA_SD_3_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_4/image_normalized/FLXTWRZONA_SD_4_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_2/image_normalized/FRST_AK_2_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_3/image_normalized/FRST_AK_3_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUAIR_DW_1/image_normalized/PRUAIR_DW_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUARC_DW_1/image_normalized/PRUARC_DW_1_NormalizedHyperspectral.tif'
)

# Masked workflow
for (image_path in image_paths) {
  # Load hyperspectral image
  hyperspectral <- rast(image_path)

  test_site_image_name <- basename(dirname(dirname(image_path)))

  print(paste0(test_site_image_name, ' masked start'))


  cluster_analysis_result <- get_optimal_cluster_number(
    image_path,
    Downsample = FALSE,
    Downsample_factor = 2,
    Downsample_function = "sd",
    Min_Cluster = 2,
    Max_Cluster = 50
  )

  optimal_k <- cluster_analysis_result$optimal_clusters_elbow
  best_nstart <- cluster_analysis_result$best_nstart

  pixel_matrix <- values(hyperspectral)

  # Remove rows with NA, NaN, or Inf
  pixel_matrix <- pixel_matrix[complete.cases(pixel_matrix), ]
  pixel_matrix <- pixel_matrix[apply(pixel_matrix, 1, function(x) all(is.finite(x))), ]

  # Optional: sample pixels for faster clustering
  print(paste("Estimated optimal number of clusters:", optimal_k))
  print(paste("Estimated optimal nstart:", best_nstart))

  # Perform k-means clustering
  set.seed(42)
  kmeans_result <- kmeans(raster_normalized, centers = optimal_k, nstart = best_nstart)

  # Create an empty vector for cluster labels
  clusters <- rep(NA, nrow(pixel_matrix))
  clusters[valid_rows] <- kmeans_result$cluster

  # Convert clusters to raster shape
  r_clusters <- rast(hyperspectral[[1]])  # Use first band as template
  values(r_clusters) <- clusters

  # Save output raster
  ouput_file_name <- paste0(test_site_image_name, '_KmeansOnRawDataMasked_Normalized.tiff')

  output_path <- file.path('data/MasterThesis/RawDataClusteringMaskedNormalized',
                           ouput_file_name)
  writeRaster(r_clusters, output_path, overwrite = TRUE)

  # Visualize result

  plot(
    r_clusters,
    main = paste("K-means Clusters (k =", optimal_k, ")"),
    col = scico(optimal_k, palette = "batlow")
  )

  print(paste0(test_site_image_name, ' done'))

  # Perform a little housekeeping
  gc()

}











# Clean workspace
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load required packages
library(terra)
library(scico)
# library(SpectralPatang) # This package is not used in the provided code

#' Performs k-means clustering on a raster image.
#'
#' This function determines the optimal number of clusters (k) using the
#' elbow method and the best nstart value, then performs k-means
#' clustering and returns the classified raster.
#'
#' @param Image_File_Path Path to the input GeoTIFF file.
#' @param Downsample Logical, whether to downsample the image for faster processing.
#' @param Downsample_factor Integer, factor by which to aggregate (downsample).
#' @param Downsample_function Character, function to use for aggregation (e.g., "mean", "sd").
#' @param Min_Cluster Integer, the minimum number of clusters to test.
#' @param Max_Cluster Integer, the maximum number of clusters to test.
#' @return A list containing the clustered raster (`clustered_raster`) and the
#'         optimal number of clusters used (`optimal_k`).
cluster_image_with_optimal_k <- function(Image_File_Path,
                                         Downsample = FALSE,
                                         Downsample_factor = 2,
                                         Downsample_function = "sd",
                                         Min_Cluster = 2,
                                         Max_Cluster = 50) {
  nstart_values <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  set.seed(123)

  cat("\nProcessing file:", basename(Image_File_Path), "\n")

  # Load the raster data
  raster_data <- rast(Image_File_Path)

  # Optional downsampling for parameter estimation
  if (Downsample) {
    fun <- match.fun(Downsample_function)
    raster_for_params <- aggregate(raster_data, fact = Downsample_factor, fun = fun)
  } else {
    raster_for_params <- raster_data
  }

  # Convert raster to a matrix for analysis, keeping only valid pixels
  full_matrix <- terra::values(raster_for_params)
  valid_rows_idx <- complete.cases(full_matrix)
  clean_matrix <- full_matrix[valid_rows_idx, , drop = FALSE]

  # Remove zero-variance columns to prevent errors in k-means
  sd_cols <- apply(clean_matrix, 2, sd, na.rm = TRUE)
  clean_matrix <- clean_matrix[, sd_cols > 0, drop = FALSE]

  if (nrow(clean_matrix) < Max_Cluster) {
    warning("Insufficient data points after cleaning. Skipping clustering.")
    return(NULL)
  }

  # ---- Step 1: Find the best nstart value ----
  cat("-> Finding optimal nstart value...\n")
  best_nstart <- nstart_values[1]
  lowest_wss <- Inf
  for (nstart in nstart_values) {
    # Using a small k for this test is efficient
    wss <- kmeans(clean_matrix, centers = Min_Cluster, nstart = nstart)$tot.withinss
    if (wss < lowest_wss) {
      lowest_wss <- wss
      best_nstart <- nstart
    }
  }
  cat("-> Best nstart found:", best_nstart, "\n")

  # ---- Step 2: Find the optimal number of clusters (k) ----
  cat("-> Finding optimal number of clusters (k)...\n")
  potential_k_values <- Min_Cluster:Max_Cluster
  wss_values <- sapply(potential_k_values, function(k) {
    kmeans(clean_matrix, centers = k, nstart = best_nstart)$tot.withinss
  })

  # Compute second derivative to find the "elbow"
  diff_wss <- diff(wss_values)
  diff2_wss <- diff(diff_wss)

  if (length(diff2_wss) > 0) {
    elbow_index <- which.min(diff2_wss) + 1 # +1 to adjust index
    optimal_k <- potential_k_values[elbow_index]
  } else {
    cat("-> Not enough points to compute elbow. Defaulting to Min_Cluster.\n")
    optimal_k <- Min_Cluster
  }
  cat("-> Optimal k found:", optimal_k, "\n")

  # ---- Step 3: Perform final clustering on the full, clean dataset ----
  cat("-> Performing final k-means clustering...\n")
  set.seed(42) # for reproducibility of the final clustering
  kmeans_result <- kmeans(clean_matrix, centers = optimal_k, nstart = best_nstart)

  # ---- Step 4: Create the output raster ----
  # Create a new vector to store cluster results, initialized with NA
  cluster_vector <- rep(NA, nrow(full_matrix))

  # Place the cluster assignments back into the correct positions for valid pixels
  cluster_vector[valid_rows_idx] <- kmeans_result$cluster

  # Create a new raster for the cluster results
  clustered_raster <- raster_for_params[[1]] # Use first band as a template
  values(clustered_raster) <- cluster_vector
  names(clustered_raster) <- "cluster_id"

  cat("-> Clustering complete.\n")

  # Return both the raster and the optimal k value
  return(list(
    clustered_raster = clustered_raster,
    optimal_k = optimal_k
  ))
}

# --- Main Workflow ---

image_paths <- c(
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_1/image_normalized/AN_TJ_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/AN_TJ_2/image_normalized/AN_TJ_2_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/ATQ_VK_1/image_normalized/ATQ_VK_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_PW_1/image_normalized/BRW_PW_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/BRW_VS_1/image_normalized/BRW_VS_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_1/image_normalized/FLXTWRZONA_SD_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_2/image_normalized/FLXTWRZONA_SD_2_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_3/image_normalized/FLXTWRZONA_SD_3_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FLXTWRZONA_SD_4/image_normalized/FLXTWRZONA_SD_4_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_2/image_normalized/FRST_AK_2_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/FRST_AK_3/image_normalized/FRST_AK_3_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUAIR_DW_1/image_normalized/PRUAIR_DW_1_NormalizedHyperspectral.tif',
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/final_hs_data_folder/PRUARC_DW_1/image_normalized/PRUARC_DW_1_NormalizedHyperspectral.tif'
)

# Create output directory if it doesn't exist
output_dir <- 'data/MasterThesis/RawDataClusteringMaskedNormalized'
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (image_path in image_paths) {
  test_site_image_name <- basename(dirname(dirname(image_path)))
  print(paste0("--- Starting ", test_site_image_name, " ---"))

  # Call the main function to get the clustered raster
  clustering_result <- cluster_image_with_optimal_k(
    Image_File_Path = image_path,
    Downsample = FALSE, # Set to TRUE for faster testing
    Min_Cluster = 2,
    Max_Cluster = 50
  )

  # Proceed if clustering was successful
  if (!is.null(clustering_result)) {
    # Unpack results from the list
    r_clusters <- clustering_result$clustered_raster
    optimal_k <- clustering_result$optimal_k

    # Save output raster
    output_file_name <- paste0(test_site_image_name, '_KmeansOnRawDataMasked_Normalized.tiff')
    output_path <- file.path(output_dir, output_file_name)
    writeRaster(r_clusters, output_path, overwrite = TRUE)

    # Visualize result
    plot(
      r_clusters,
      main = paste(test_site_image_name, "\nK-means Clusters (k =", optimal_k, ")"),
      col = scico(optimal_k, palette = "batlow")
    )

    print(paste0("--- Finished ", test_site_image_name, " ---"))
  }

  # Perform a little housekeeping
  gc()
}
