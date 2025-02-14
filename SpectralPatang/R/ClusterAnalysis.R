#' Perform cluster analysis with wss
#'
#' This uses wss to get the optimal kmeans cluster number.
#' @param Image_File_Path character. Path of the selected PC image
#' @param Downsample boolean. Should the dataset be down sampled
#' @param Downsample_factor numeric. Factor to down sample (eg. 2 means 2x2 pixel)
#' @param Downsample_function character. Dwon sample function (sd, mean, range)
#' @param Min_Cluster numeric. Minimal amount of clusters to test.
#' @param Max_Cluster numeric. Maximal amount of clusters to test.
#'
#' @return Returns the number of optimal clusters
#' @export
#'

# get_optimal_cluster_number <- function(Image_File_Path,
#                                        Downsample = TRUE,
#                                        Downsample_factor = 2,
#                                        Downsample_function = "sd",
#                                        Min_Cluster = 2,
#                                        Max_Cluster = 50) {
#   # Load the raster image
#   pca_hs_image <- terra::rast(Image_File_Path)
#
#   if (Downsample) {
#     pca_hs_image <- terra::aggregate(pca_hs_image,
#                                      fact = Downsample_factor,
#                                      fun = Downsample_function,
#                                      na.rm = TRUE)
#   }
#
#   pca_data <- as.matrix(terra::values(pca_hs_image))
#
#   # Define cores for parallel processing
#   num_cores <- parallel::detectCores() - 2
#
#   # Prepare the data
#   pca_data_na_omitted <- na.omit(pca_data)  # Remove NAs
#   pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data for clustering
#   kmeans_clustering_data <- pca_data_na_omitted_scaled
#
#   # Define Range of Clusters
#   k.values <- Min_Cluster:Max_Cluster
#
#   # Define function to compute silhouette score
#   compute_silhouette <- function(data, k, seed = 123) {
#     set.seed(seed)
#     km_res <- kmeans(data, centers = k, nstart = 10)
#     ss <- cluster::silhouette(km_res$cluster, dist(data))
#     mean(ss[, 3])  # Mean silhouette width
#   }
#
#   # Setup Parallel Cluster
#   cl <- parallel::makeCluster(num_cores)
#
#   # Export required objects and functions to the worker nodes
#   parallel::clusterExport(
#     cl,
#     varlist = c("kmeans_clustering_data", "compute_silhouette"),
#     envir = environment()
#   )
#
#   # Compute Silhouette scores in parallel
#   silhouette_scores <- parallel::parSapply(cl, k.values, function(k) {
#     compute_silhouette(kmeans_clustering_data, k)
#   })
#
#   # Stop Cluster
#   parallel::stopCluster(cl)
#
#   # Create Data Frame with Metrics
#   cluster_metrics <- data.frame(k = k.values, Silhouette = silhouette_scores)
#
#   # Determine Optimal Clusters using the Maximum Silhouette Score
#   optimal_clusters_silhouette <- cluster_metrics$k[which.max(cluster_metrics$Silhouette)]
#
#   cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_silhouette, "\n")
#
#   # Save to a text file
#   output_folder_path <- dirname(Image_File_Path)
#   output_file_path <- file.path(output_folder_path, 'optimal_number_of_clusters.txt')
#   write(optimal_clusters_silhouette, file = output_file_path)
#
#   return(optimal_clusters_silhouette)
# }




get_optimal_cluster_number <- function(Image_File_Path,
                                       Downsample = TRUE,
                                       Downsample_factor = 2,
                                       Downsample_function = "sd",
                                       Min_Cluster = 2,
                                       Max_Cluster = 50) {
  # Load the raster image
  pca_hs_image <- terra::rast(Image_File_Path)

  if(Downsample){
    pca_hs_image <- terra::aggregate(pca_hs_image,
                                                 fact = Downsample_factor,
                                                 fun = Downsample_function,
                                                 na.rm = TRUE)
  }

  pca_data <- as.matrix(terra::values(pca_hs_image))

  # Define cores for parallel processing
  num_cores <- parallel::detectCores() - 2

  # Prepare the data
  pca_data_na_omitted <- na.omit(pca_data)  # Clean the data (remove NAs)
  pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)  # Standardize data for clustering
  kmeans_clustering_data <- pca_data_na_omitted_scaled

  # Define Range of Clusters
  k.values <- Min_Cluster:Max_Cluster

  # Define Clustering Metrics Function
  compute_wss <- function(data, k, seed = 123) {
    set.seed(seed)
    kmeans(data, centers = k, nstart = 10)$tot.withinss
  }

  # Setup Parallel Cluster
  cl <- parallel::makeCluster(num_cores)

  # Export required objects and functions to the worker nodes
  parallel::clusterExport(
    cl,
    varlist = c("kmeans_clustering_data", "compute_wss"),
    envir = environment()
  )

  # Compute Metrics in Parallel
  wss_values <- parallel::parSapply(cl, k.values, function(k) {
    compute_wss(kmeans_clustering_data, k)
  })

  # Stop Cluster
  parallel::stopCluster(cl)

  # Create Data Frame with Metrics
  cluster_metrics <- data.frame(k = k.values, WSS = wss_values)

  # Determine Optimal Clusters
  # Elbow Method: Find the point where the second derivative is minimized
  diff_wss <- diff(cluster_metrics$WSS)
  diff2_wss <- diff(diff_wss)
  optimal_clusters_elbow <- which.min(diff2_wss) + Min_Cluster  # Adjust by Min_Cluster

  cat("Optimal number of clusters (Elbow Method):",
      optimal_clusters_elbow,
      "\n")

  # Save to a text file
  output_folder_path <- dirname(Image_File_Path)
  output_file_path <- file.path(output_folder_path,'optimal_number_of_clusters.txt')
  write(optimal_clusters_elbow, file = output_file_path)

  return(optimal_clusters_elbow)

}

#debug(get_optimal_cluster_number)
# path_name <- get_optimal_cluster_number(
#   '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/clustertest/result/ang20190706t235120_rfl_v2v2_img_rectified_clustertest/SPCA/PCA/OutputPCA_30_PCs_selection.tif'
# )
