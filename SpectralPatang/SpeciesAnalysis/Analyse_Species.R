rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(SpectralPatang)
library(parallel)
library(factoextra)
library(cluster)
library(terra)
library(stats)
library(NbClust)
library(ggplot2)

raw_image_file_path <- 'C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/data/hs_raw_image/ang20190712t231624_rfl_v2v2_img'
rectified_image_folder_path <- 'C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/data/rectified'
rectified_image_file_path <- file.path(rectified_image_folder_path,'ang20190712t231624_rfl_v2v2_img_rectified_cut')
cut_shp <- 'C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cut_shp/output_square_R/output_square_R.shp'

gdal_command_rectify <- sprintf(
  "gdalwarp -cutline %s -crop_to_cutline -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
  cut_shp,
  raw_image_file_path,
  rectified_image_file_path
)

# # Execute the command in R
system(gdal_command_rectify)

savi_file_path <- 'C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/mask'

SpectralPatang::create_SAVI_mask(rectified_image_folder_path,savi_file_path)

savi_file_path <- paste0(savi_file_path,'/ang20190712t231624_rfl_v2v2_img_rectified_cut_savi_mask_02')

num_cores <- parallel::detectCores()

SpectralPatang::analyse_biodiversity(rectified_image_file_path,
                                     savi_file_path,
                                     NBbclusters = 5,
                                     Window_size = 10,
                                     NbCPU = num_cores,
                                     MaxRAM = 8,
                                     Perform_PCA = TRUE,
                                     PCA_Threshold = 99)


################################################################################
################################################################################
# Analyse cluster performance
################################################################################
################################################################################

################################################################################
# Using fviz_cluster
################################################################################

pca_hs_image_path <- 'C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/result/ang20190712t231624_rfl_v2v2_img_rectified_cut/SPCA/PCA/OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores()

# select only relevant principle components
pca_hs_image_subset <- terra::subset(pca_hs_image, 1:17)
downsamle <- FALSE
# Downsample the raster (reduce spatial resolution)
if (downsamle){
  pca_hs_image_subset <- terra::aggregate(pca_hs_image_subset, fact = 1, fun = mean, cores = num_cores)
}


# read values as a matrix
pca_data_convex <- as.matrix(terra::values(pca_hs_image_subset))

# Omit NA values
pca_data_convex <- na.omit(pca_data_convex)

# Standardize the data for better clustering performance
pca_data_convex <- scale(pca_data_convex)


set.seed(123)
km.res <- kmeans(pca_data_convex, centers = 2, nstart = 50, algorithm = "Hartigan-Wong")

fviz_cluster(km.res, data = pca_data_convex, geom = "point",
             stand = FALSE, ellipse = TRUE, ellipse.type = "convex") +
  labs(title = "K-Means Clustering with Convex Hulls")



################################################################################
# Using WSS and Silhouette
################################################################################

# Define Range of Clusters
k.values <- 2:30

# Define Clustering Metrics Functions
compute_wss <- function(data, k, seed = 123) {
  set.seed(seed)
  kmeans(data, centers = k, nstart = 10)$tot.withinss
}

compute_silhouette <- function(data, k, seed = 123) {
  set.seed(seed)
  km <- kmeans(data, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(data))
  mean(ss[, 3])
}


# Setup Parallel Cluster
cl <- makeCluster(num_cores)
clusterExport(cl, varlist = c("pca_data_convex", "compute_wss", "compute_silhouette"))
clusterEvalQ(cl, library(cluster))  # Load `cluster` library on workers

# Compute Metrics in Parallel
wss_values <- parSapply(cl, k.values, function(k) compute_wss(pca_data_convex, k))
sil_values <- parSapply(cl, k.values, function(k) compute_silhouette(pca_data_convex, k))

# Stop Cluster
stopCluster(cl)

# Create Data Frame with Metrics
cluster_metrics <- data.frame(
  k = k.values,
  WSS = wss_values,
  Silhouette = sil_values
)

# Determine Optimal Clusters
# Elbow Method: Find the point where the second derivative is minimized
diff_wss <- diff(cluster_metrics$WSS)
diff2_wss <- diff(diff_wss)
optimal_clusters_elbow <- which.min(diff2_wss) + 2  # +2 to account for the two diffs

# Silhouette Method: Find the maximum silhouette width
optimal_clusters_sil <- which.max(cluster_metrics$Silhouette) + 1  # +1 because k starts at 2

cat("Optimal number of clusters (Elbow Method):", optimal_clusters_elbow, "\n")
cat("Optimal number of clusters (Silhouette Method):", optimal_clusters_sil, "\n")

# Visualize the Metrics
# Elbow Method Plot
ggplot(cluster_metrics, aes(x = k, y = WSS)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_elbow, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_clusters_elbow, y = max(cluster_metrics$WSS), label = paste("Optimal k =", optimal_clusters_elbow), vjust = -1, color = "red") +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

# Silhouette Method Plot
ggplot(cluster_metrics, aes(x = k, y = Silhouette)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters_sil, linetype = "dashed", color = "blue") +
  annotate("text", x = optimal_clusters_sil, y = max(cluster_metrics$Silhouette), label = paste("Optimal k =", optimal_clusters_sil), vjust = -1, color = "blue") +
  labs(title = "Silhouette Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()

################################################################################
# Using NbClust
################################################################################

# Perform NbClust to determine the optimal number of clusters
set.seed(123)  # For reproducibility
nbclust_result <- NbClust(
  data = pca_data_convex,         # The matrix with rows as observations and columns as variables
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


cluster_freq <- as.data.frame(table(optimal_clusters))
colnames(cluster_freq) <- c("Number_of_Clusters", "Frequency")


# Plot the frequency of recommended cluster numbers
ggplot(cluster_freq, aes(x = Number_of_Clusters, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Optimal Number of Clusters",
    subtitle = "Based on 30 clustering indices",
    x = "Number of Clusters",
    y = "Frequency of Indices Supporting Cluster Count"
  ) +
  theme_minimal()

# Visualize the number of clusters recommended by various indices
fviz_nbclust(nbclust_result) +
  labs(
    title = "Optimal Number of Clusters",
    subtitle = "Based on 30 clustering indices",
    x = "Number of Clusters",
    y = "Frequency of Indices Supporting Cluster Count"
  ) +
  theme_minimal()

save.image(file = "C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/result/ang20190712t231624_rfl_v2v2_img_rectified_cut/SPCA/PCA/kmeans_analytics.RData")

