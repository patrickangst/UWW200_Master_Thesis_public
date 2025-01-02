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

  # Visualize the Metrics
  # Elbow Method Plot
  # Create the WSS plot

  ggplot2::ggplot(cluster_metrics) +
    ggplot2::geom_point(mapping = aes(x = k, y = WSS), color = "blue", size = 2) +
    ggplot2::geom_line(mapping = aes(x = k, y = WSS), color = "blue", linewidth = 1) +
    ggplot2::geom_vline(xintercept = optimal_clusters_elbow, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = optimal_clusters_elbow, y = max(cluster_metrics$WSS),
             label = paste("Optimal k =", optimal_clusters_elbow),
             vjust = -1, color = "red", size = 4) +
    labs(
      title = "Elbow Method for Optimal Clusters",
      x = "Number of Clusters (k)",
      y = "Total Within-Cluster Sum of Squares (WSS)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )



  # ggplot2::ggplot(cluster_metrics, aes(x = k, y = WSS)) +
  #   geom_point(color = "blue", size = 2) +                      # Add points with styling
  #   geom_line(color = "blue", linewidth = 1) +                 # Add a line with styling
  #   geom_vline(xintercept = optimal_clusters_elbow,
  #              linetype = "dashed", color = "red", linewidth = 1) +  # Add dashed vertical line
  #   annotate("text", x = optimal_clusters_elbow,
  #            y = max(cluster_metrics$WSS),
  #            label = paste("Optimal k =", optimal_clusters_elbow),
  #            vjust = -1, color = "red", size = 4) +             # Add annotation
  #   labs(
  #     title = "Elbow Method for Optimal Clusters",             # Title
  #     x = "Number of Clusters (k)",                            # X-axis label
  #     y = "Total Within-Cluster Sum of Squares (WSS)"          # Y-axis label
  #   ) +
  #   theme_minimal(base_size = 14) +                            # Use minimal theme with larger text
  #   theme(
  #     plot.title = element_text(hjust = 0.5, face = "bold")    # Center and bold the title
  #   )



  directory_path <- dirname(Image_File_Path)
  directory_path <- sub('rectified$', 'species_analyses', directory_path)

  Plot_File_Path <- file.path(directory_path, 'plots','relative_abundance_plot.png')

  # Save the relative abundance plot
  ggplot2::ggsave(Plot_File_Path, relative_abundance_plot, dpi = 300, width = 10, height = 6)

  return(optimal_clusters_elbow)

}

#debug(get_optimal_cluster_number)
path_name <- get_optimal_cluster_number(
  '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/clustertest/result/ang20190706t235120_rfl_v2v2_img_rectified_clustertest/SPCA/PCA/OutputPCA_30_PCs_selection.tif'
)
