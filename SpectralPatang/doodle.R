rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)


testsite <- 'AN_TJ_2'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))


rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'ATQ_VK_1'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))


rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'BRW_PW_1'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))


rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'BRW_VS_1'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))



rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'FLXTWRZONA_SD_1'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))




rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'FLXTWRZONA_SD_2'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))




rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'FLXTWRZONA_SD_3'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))




rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'FLXTWRZONA_SD_4'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))




rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'FRST_AK_2'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))



rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'FRST_AK_3'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))




rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'PRUAIR_DW_1'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))



rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(NbClust)
library(cluster)

testsite <- 'PRUARC_DW_1'
image_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_principle_components_selection/',testsite,'_pc_selection.tif')
result_path <- paste0('C:/Users/patri/Documents/MasterThesis/08_nbclust_results/',testsite,'.RData')

img <- rast(image_path)

minmax(img)

data_matrix <- as.matrix(img)

sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

img[img == -9999] <- NA


sum(is.na(values(img)))  # Number of NA pixels
sum(values(img) != -9999, na.rm = TRUE)  # Number of valid pixels

data_matrix <- as.matrix(img)

# Check if the matrix contains valid values
dim(data_matrix)  # Should not be (0, X)
summary(data_matrix)  # Ensure no Inf/-Inf

# Remove NA rows
data_matrix <- na.omit(data_matrix)

# Check the dimensions again
dim(data_matrix)  # Should still have data

# Determine optimal clusters
nb <- NbClust(data_matrix, min.nc=2, max.nc=50, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = result_path)

cat(paste0('NbClust done for: ', testsite, ' Optimal number of clusters: ', optimal_clusters, '\n'))
