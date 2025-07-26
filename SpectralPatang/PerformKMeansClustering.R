rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(ggplot2)
library(scico)
scico(n = 20, palette = "batlow")

run_kmeans_on_pca <- function(pca_path, n_clusters, output_path = NULL, visualize = TRUE) {
  # Load the PCA raster
  r_pca <- rast(pca_path)
  bands <- nlyr(r_pca)
  nrows <- nrow(r_pca)
  ncols <- ncol(r_pca)

  # Convert raster to matrix: rows = pixels, columns = bands
  pca_matrix <- as.matrix(r_pca)

  # Remove rows with NA values
  valid_rows <- complete.cases(pca_matrix)
  pca_clean <- pca_matrix[valid_rows, ]

  # Perform k-means clustering
  set.seed(42)
  kmeans_result <- kmeans(pca_clean, centers = n_clusters)

  # Create an empty vector for cluster labels
  clusters <- rep(NA, nrow(pca_matrix))
  clusters[valid_rows] <- kmeans_result$cluster

  # Convert clusters to raster shape
  r_clusters <- rast(r_pca[[1]])  # Use first band as template
  values(r_clusters) <- clusters

  # Save output raster
  if (is.null(output_path)) {
    output_path <- sub(".tif$", "_clustered_kmeans.tif", pca_path)
  }
  writeRaster(r_clusters, output_path, overwrite = TRUE)

  # Visualize result
  if (visualize) {
    plot(
      r_clusters,
      main = paste("K-means Clusters (k =", n_clusters, ")"),
      col = scico(n_clusters, palette = "batlow")
    )
  }

  return(r_clusters)
}

# Define your inputs
pca_files <- c(
  "data/MasterThesis/08_principle_components_selection/AN_TJ_1_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/AN_TJ_2_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/ATQ_VK_1_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/BRW_PW_1_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/BRW_VS_1_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/FLXTWRZONA_SD_1_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/FLXTWRZONA_SD_2_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/FLXTWRZONA_SD_3_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/FLXTWRZONA_SD_4_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/FRST_AK_2_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/FRST_AK_3_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/PRUAIR_DW_1_pc_selection.tif",
  "data/MasterThesis/08_principle_components_selection/PRUARC_DW_1_pc_selection.tif"
)

cluster_values <- c(26, 12, 43, 40, 25, 27, 11, 27, 7, 48, 5, 7, 42)

# Run clustering on all files
results <- vector("list", length(pca_files))

for (i in seq_along(pca_files)) {
  cat("Processing:", pca_files[i], "\n")
  results[[i]] <- run_kmeans_on_pca(
    pca_path = pca_files[i],
    n_clusters = cluster_values[i],
    visualize = TRUE  # You can set to FALSE to speed things up
  )
}
