# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(NbClust)
library(factoextra)
library(parallel)


## Patang test
pca_hs_image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20180729t212542rfl/result/ang20180729t212542_rfl_v2r2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection.tif'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores()

pca_data <- as.matrix(terra::values(pca_hs_image))

# Prepare the data
pca_data_na_omitted <- na.omit(pca_data)   # Clean the data (remove NAs)
pca_data_na_omitted_scaled <- scale(pca_data_na_omitted)     # Standardize data for clustering

set.seed(123)  # Set the seed for reproducibility within each worker
nbclust_result <- NbClust(
  data = pca_data,
  distance = "euclidean",
  min.nc = 1,
  max.nc = 30,
  method = "kmeans",
  index = "all"
)


# Extract the number of clusters suggested by each index
optimal_clusters <- nbclust_result$Best.nc

# Create a frequency table for the suggested cluster numbers
cluster_votes <- table(optimal_clusters)

# Determine the optimal number of clusters based on the majority vote
best_number_of_clusters <- as.numeric(names(which.max(cluster_votes)))
cat("The optimal number of clusters is:", best_number_of_clusters, "\n")


# Barplot to visualize votes for the number of clusters
barplot(
  cluster_votes,
  main = "Number of Clusters Suggested by NbClust Indices",
  xlab = "Number of Clusters",
  ylab = "Frequency",
  col = "skyblue",
  border = "black"
)

fviz_nbclust(nbclust_result) +
  labs(title = "Optimal Number of Clusters Using NbClust",
       subtitle = paste("Optimal Clusters:", optimal_clusters)) +
  theme_minimal()




# Define Optimal Number of Clusters (Replace with your computed optimal value)
optimal_k <- 5

# Perform K-means Clustering
set.seed(123)  # Ensure reproducibility
kmeans_result <- kmeans(pca_data, centers = optimal_k, nstart = 10)

# View the clustering result
print(kmeans_result)


# Cluster Assignments
head(kmeans_result$cluster)
# Cluster Centers
kmeans_result$centers


fviz_cluster(kmeans_result, data = pca_data, geom = "point", ellipse.type = "convex") +
  labs(title = paste("K-means Clustering with k =", optimal_k))



































