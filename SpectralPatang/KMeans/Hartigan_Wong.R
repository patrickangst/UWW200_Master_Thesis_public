# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(factoextra)
library(cluster)
library(terra)
library(parallel)
library(stats)
library(NbClust)

## Patang test
pca_hs_image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20180729t212542rfl/result/ang20180729t212542_rfl_v2r2_img_rectified/SPCA/PCA/OutputPCA_30_PCs'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores()

# select only relevant principle components
pca_hs_image_subset <- terra::subset(pca_hs_image, 1:6)

# Downsample the raster (reduce spatial resolution)
pca_hs_image_subset <- terra::aggregate(pca_hs_image_subset, fact = 1, fun = mean, cores = num_cores)

# read values as a matrix
pca_data_convex <- as.matrix(terra::values(pca_hs_image_subset))

# Omit NA values
pca_data_convex <- na.omit(pca_data_convex)

# Standardize the data for better clustering performance
pca_data_convex <- scale(pca_data_convex)


set.seed(123)
km.res <- kmeans(pca_data_convex, centers = 3, nstart = 100, algorithm = "Hartigan-Wong")

fviz_cluster(km.res, data = pca_data_convex, geom = "point",
             stand = FALSE, ellipse = TRUE, ellipse.type = "convex") +
  labs(title = "K-Means Clustering with Convex Hulls")
