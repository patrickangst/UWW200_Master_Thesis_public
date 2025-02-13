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
