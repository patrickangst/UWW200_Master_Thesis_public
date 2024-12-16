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
pca_hs_image_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/result/ang20190706t235120_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection_cutline.tif'
pca_hs_image <- terra::rast(pca_hs_image_path)
num_cores <- parallel::detectCores() - 2

# select only relevant principle components
# pca_hs_image_subset <- terra::subset(pca_hs_image, 1:6)

# Downsample the raster (reduce spatial resolution)
#pca_hs_image_subset <- terra::aggregate(pca_hs_image_subset, fact = 1, fun = mean, cores = num_cores)

# read values as a matrix
pca_data_convex <- as.matrix(terra::values(pca_hs_image))

# Omit NA values
pca_data_convex <- na.omit(pca_data_convex)

# Standardize the data for better clustering performance
pca_data_convex <- scale(pca_data_convex)


set.seed(123)
km.res <- kmeans(pca_data_convex, centers = 24, nstart = 20, algorithm = "Hartigan-Wong")

fviz_cluster(km.res, data = pca_data_convex, geom = "point",
             stand = FALSE, ellipse = TRUE, ellipse.type = "convex") +
  labs(title = "K-Means Clustering with Convex Hulls")
