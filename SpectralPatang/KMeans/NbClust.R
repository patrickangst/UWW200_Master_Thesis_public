# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(NbClust)
library(factoextra)
library(terra)


## Patang test
pca_hs_image_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/result/ang20190706t235120_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection_cutline.tif'
pca_hs_image <- terra::rast(pca_hs_image_path)

# Downsample the raster (reduce spatial resolution)
pca_hs_image_subset <- terra::aggregate(pca_hs_image, fact = 2, fun = mean, cores = num_cores)

pca_data <- as.matrix(terra::values(pca_hs_image_subset))

# Prepare the data
pca_data_na_omitted <- na.omit(pca_data)   # Clean the data (remove NAs)
pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)     # Standardize data for clustering
kmeans_clustering_data <- pca_data_na_omitted_scaled


# Perform NbClust to determine the optimal number of clusters
set.seed(123)  # For reproducibility
nbclust_result <- NbClust(
  data = kmeans_clustering_data,         # The matrix with rows as observations and columns as variables
  distance = "euclidean",  # Distance metric for clustering
  min.nc = 2,              # Minimum number of clusters to evaluate
  max.nc = 30,             # Maximum number of clusters to evaluate
  method = "kmeans",       # Clustering method
  index = "all"            # Use all available indices to determine the optimal number of clusters
)

# Extract the number of clusters suggested by each index
optimal_clusters <- nbclust_result$Best.nc

# Create a frequency table for the suggested cluster numbers
cluster_votes <- table(optimal_clusters)

# Determine the optimal number of clusters based on the majority vote
best_number_of_clusters <- as.numeric(names(which.max(cluster_votes)))
cat("The optimal number of clusters is:", best_number_of_clusters, "\n")


# Visualize the number of clusters recommended by various indices
fviz_nbclust(nbclust_result) +
  labs(
    title = "Optimal Number of Clusters",
    subtitle = "Based on 30 clustering indices",
    x = "Number of Clusters",
    y = "Frequency of Indices Supporting Cluster Count"
  ) +
  theme_minimal()
