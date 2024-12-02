# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(factoextra)
library(cluster)
library(terra)
library(parallel)
library(stats)

## Patang test
pca_hs_image_path <- 'D:\\MasterThesis\\test_data_elbow\\ang20180729t212542rfl\\result\\ang20180729t212542_rfl_v2r2_img_rectified\\SPCA\\PCA\\OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores()
set.seed(0)

pca_hs_image_subset <- terra::subset(pca_hs_image, 1:4)
# Downsample the raster (reduce spatial resolution)
pca_hs_image_downsampled <- terra::aggregate(pca_hs_image_subset, fact = 3, fun = mean, cores = num_cores)
pca_data <- as.matrix(terra::values(pca_hs_image_subset))
pca_data <- na.omit(pca_data)
#terra::nlyr(pca_hs_image_subset)

k.values <- 1:30

# Setup cluster
cl <- makeCluster(num_cores)

# Export data and required function to cluster
clusterExport(cl, varlist = c("pca_data"))

# Export required libraries in cluster
clusterEvalQ(cl, library(stats))

# Run parallel computation
wss_values <- parLapply(cl, k.values, function(k) {
  stats::kmeans(pca_data, k, nstart = 10)$tot.withinss
})

# Stop cluster after computation
stopCluster(cl)

wss_values_unlisted <- unlist(wss_values)
# Find the optimal number of clusters using the Elbow Method
diff_wss <- diff(wss_values_unlisted)  # First derivative
diff2_wss <- diff(diff_wss)  # Second derivative
optimal_clusters <- which.max(-diff2_wss) + 1 + min(k.values) - 1

cat("Optimal number of clusters (Elbow Method):", optimal_clusters, "\n")
#
# plot(k.values, wss_values,
#      type="b", pch = 19, frame = FALSE,
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
#
# fviz_nbclust_result <- fviz_nbclust(
#   pca_data,
#   kmeans,
#   method = "wss", # Elbow Method
#   k.max = 30,
#   verbose = interactive(),
#   barfill = "steelblue",
#   barcolor = "steelblue",
#   linecolor = "steelblue",
#   print.summary = TRUE
# )
#
# plot(fviz_nbclust_result)



# Create a data frame for plotting
wss_df <- data.frame(
  k = k.values,
  wss = wss_values_unlisted
)

# Plot using ggplot2 for better customization
ggplot(wss_df, aes(x = k, y = wss)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_clusters, y = max(wss_values_unlisted), label = paste("Optimal k =", optimal_clusters), vjust = -1, color = "red") +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

# Compute Total Sum of Squares (TSS)
TSS <- sum(apply(pca_data, 2, function(x) (x - mean(x))^2))

# Calculate explained variance for each k
explained_variance <- 1 - (wss_values_unlisted / TSS)

# Create a data frame for plotting
explained_variance_df <- data.frame(
  k = k.values,
  explained_variance = explained_variance
)

# Plot using ggplot2 for better customization
ggplot(explained_variance_df, aes(x = k, y = explained_variance)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = optimal_clusters, linetype = "dashed", color = "red") +
  annotate("text", x = optimal_clusters, y = max(explained_variance), label = paste("Optimal k =", optimal_clusters), vjust = -1, color = "red") +
  labs(title = "Explained Variance for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Explained Variance") +
  theme_minimal()
