#' Perform cluster analysis with wss
#'
#' This uses wss to get the optimal kmeans cluster number.
#' @param Image_File_Path character. Path of the selected PC image
#' @param Min_Cluster numeric. Minimal amount of clusters to test.
#' @param Max_Cluster numeric. Maximal amount of clusters to test.
#'
#' @return Returns the number of optimal clusters
#' @export
#'

get_optimal_cluster_number <- function(Image_File_Path,
                                       Min_Cluster = 2,
                                       Max_Cluster = 30) {
  # Load the raster image
  pca_hs_image <- terra::rast(Image_File_Path)
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

  return(optimal_clusters_elbow)

}
