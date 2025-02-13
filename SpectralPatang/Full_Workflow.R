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

  # create cut shape for gdal_warp
  # SpectralPatang::create_rectangular_shapefile(
  #   shapefile_plotlocation_folder_path,
  #   shapefile_cutline_folder_path,
  #   Buffer = 100,
  #   Square = TRUE
  # )

  # rectify image
  #debug(rectify_Image)
  image_rectified_file_path <- SpectralPatang::rectify_Image(image_raw_folder_path, image_rectified_folder_path, Cutfile = TRUE, shapefile_cutline_folder_path)


  # create RGB for entire picture
  #SpectralPatang::create_RGB(image_raw_folder_path, image_rgb_folder_path,Site_Name = test_site_name)
  #SpectralPatang::create_RGB(image_raw_folder_path, "D:/MasterThesis/04_RGB",Site_Name = test_site_name)

  # create RGB for rectified picture
  SpectralPatang::create_RGB(image_rectified_folder_path, image_rgb_folder_path,Site_Name = test_site_name)
  SpectralPatang::create_RGB(image_rectified_folder_path, "D:/MasterThesis/04_RGB",Site_Name = test_site_name)

  #create MASK
  mask_file_path <- SpectralPatang::create_SAVI_mask(image_rectified_folder_path, mask_folder_path)

  # peform PCA
  cat(paste0('Start PCA for: ', test_site_name, '\n'))

  # perform biodivMapR PCA
  PCA_Output <- biodivMapR::perform_PCA(
    Input_Image_File = image_rectified_file_path,
    Input_Mask_File = mask_file_path,
    Output_Dir = result_biodivMapR_folder_path,
    Continuum_Removal = FALSE,
    TypePCA = TypePCA,
    NbPCs_To_Keep = 30,
    FilterPCA = FALSE,
    nbCPU = num_cores_to_use,
    MaxRAM = 8
  )

  # create a txt file for the pc selection
  selected_components_file_path <- file.path(result_biodivMapR_folder_path,
                                             basename(image_rectified_file_path),
                                             TypePCA,
                                             'PCA',
                                             'Selected_Components.txt')
  file.create(selected_components_file_path)

  cat(paste0('PCA done for: ', test_site_name, '\n'))

}


test_sites <- list.files(test_sites_folder_path)

sites_done <- c("hugo")

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
