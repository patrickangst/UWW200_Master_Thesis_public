rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(doParallel)
library(foreach)
library(SpectralPatang)

raw_image_file_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20180729t212542rfl/data/hs_raw_image/ang20190712t231624_rfl_v2v2_img'
rectified_image_file_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20180729t212542rfl/data/rectified/ang20180729t212542_rfl_v2r2_img_rectified'
cut_shp <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20190712t231624cut/data/cut_shp/output_square_R.shp'

gdal_command_rectify <- sprintf(
  "gdalwarp -cutline %s -crop_to_cutline -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
  cut_shp,
  raw_image_file_path,
  rectified_image_file_path
)

# # Execute the command in R
system(gdal_command_rectify)

savi_file_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_elbow/ang20180729t212542rfl/mask'

SpectralPatang::create_SAVI_mask(rectified_image_file_path,savi_file_path)

savi_file_path <- paste0(savi_file_path,'/ang20180729t212542_rfl_v2r2_img_rectified_savi_mask_02')

num_cores <- parallel::detectCores()
debug(analyse_biodiversity)
SpectralPatang::analyse_biodiversity(rectified_image_file_path,
                                     savi_file_path,
                                     NBbclusters = 5,
                                     Window_size = 10,
                                     NbCPU = num_cores,
                                     MaxRAM = 8,
                                     Perform_PCA = FALSE,
                                     PCA_Threshold = 99)

