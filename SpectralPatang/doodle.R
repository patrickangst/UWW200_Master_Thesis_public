rm(list = ls(all = TRUE))
gc()
graphics.off()

devtools::load_all()

# Load necessary libraries
library(SpectralPatang)
library(biodivMapR)
library(doParallel)

test_sites_folder_path <- 'D:/MasterThesis/final_hs_data_folder'

# various variables
num_cores_to_use <- detectCores() - 2
Window_size <- 5
TypePCA <- 'SPCA'

part_one <- function(test_site_folder_path){

  # define folder path variables
  shapefile_plotlocation_folder_path <- file.path(test_site_folder_path, 'shapefile_plotlocation')
  shapefile_cutline_folder_path <- file.path(test_site_folder_path, 'shapefile_cutline')
  mask_folder_path <- file.path(test_site_folder_path, 'mask')
  image_rgb_folder_path <- file.path(test_site_folder_path, 'image_rgb')
  image_rectified_folder_path <- file.path(test_site_folder_path, 'image_rectified')
  image_raw_folder_path <- file.path(test_site_folder_path, 'image_raw')
  result_biodivMapR_folder_path <- file.path(test_site_folder_path, 'result_biodivMapR')
  test_site_name <- basename(test_site_folder_path)

  # create RGBs
  SpectralPatang::create_RGB(image_raw_folder_path, "D:/MasterThesis/04_RGB",test_site_name)

  cat(paste0('PCA done for: ', basename(test_site_folder_path), '\n'))

}


test_sites <- list.files(test_sites_folder_path)

sites_done <- c("Hugo")

for (site in test_sites) {

  if (!(site %in% sites_done)) {
    test_site_folder_path_loop <- file.path(test_sites_folder_path,site)
    cat(paste0('Start process: ', site, '\n'))
    part_one(test_site_folder_path_loop)
    cat(paste0('End process: ', site, '\n'))
    sites_done <- c(sites_done, site)
  } else {
    print(paste(site, " is already done."))
  }

}


image_rectified_file_path <- 'D:/MasterThesis/final_hs_data_folder_test/AN_TJ_1/image_rectified/ang20220711t002111_rfl_v2aa2_img_rectified/PCout'
mask_file_path <- 'D:/MasterThesis/final_hs_data_folder_test/AN_TJ_1/mask/ang20220711t002111_rfl_v2aa2_img_rectified_savi_mask_02'
result_biodivMapR_folder_path <- 'D:/MasterThesis/final_hs_data_folder_test/AN_TJ_1/result_biodivMapR'

PCA_Output <- biodivMapR::perform_PCA(
  Input_Image_File = image_rectified_file_path,
  Input_Mask_File = mask_file_path,
  Output_Dir = result_biodivMapR_folder_path,
  Continuum_Removal = FALSE,
  TypePCA = 'SPCA',
  NbPCs_To_Keep = 30,
  FilterPCA = FALSE,
  nbCPU = 8,
  MaxRAM = 8
)

dire <- "D:/MasterThesis/final_hs_data_folder/AN_TJ_1/result_biodivMapR"


# List all files in the folder
dire_files <- list.files(dire, full.names = TRUE)

# Filter files without an extension
dire_folder_without_ext <- dire_files[!grepl("\\.[a-zA-Z0-9]+$", basename(dire_files))]

# Check if exactly one file without extension exists
if (length(hs_file_without_ext) != 1) {
  stop("Either no or multiple files without extensions found in the hs image folder.")
}
print(list.files())










library(terra)
library(NbClust)
library(cluster)


img <- rast('D:/MasterThesis/final_hs_data_folder/AN_TJ_1/result_biodivMapR/ang20220711t002111_rfl_v2aa2_img_rectified/SPCA/PCA/AN_TJ_1_pc_selection.tif')

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
nb <- NbClust(data_matrix, min.nc=2, max.nc=10, method="kmeans")
optimal_clusters <- nb$Best.nc[1]

save.image(file = "D:/MasterThesis/08_nbclust_results/AN_TJ_1.RData")

filename <- 'D:/MasterThesis/final_hs_data_folder/AN_TJ_1/result_biodivMapR/ang20220711t002111_rfl_v2aa2_img_rectified/SPCA/PCA/PCA_Info.RData'
pca_shizzle <- load(filename)
