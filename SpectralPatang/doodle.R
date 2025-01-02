directory_path <- "/path/to/your/tewe.txt"

# Replace the last directory with "mine"
new_directory_path <- file.path(dirname(directory_path), "mine")

print(new_directory_path)


rm(list = ls())
graphics.off()

devtools::load_all()

# Load necessary libraries
library(doParallel)
library(foreach)
library(SpectralPatang)

raw_image_file_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/hs_raw_image/ang20190706t235120_rfl_v2v2_img'
rectified_image_file_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/rectified/ang20190706t235120_rfl_v2v2_img_rectified_cut'
cut_shp <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/plotlocations/06_Utqiaqvik_IBP_boundingbox_buffered.shp'
savi_file_path <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/mask'
savi_file_path <- paste0(savi_file_path,'/ang20190706t235120_rfl_v2v2_img_rectified_savi_mask_02_cut')

gdal_command_rectify <- sprintf(
  "gdalwarp -cutline %s -crop_to_cutline -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
  cut_shp,
  raw_image_file_path,
  rectified_image_file_path
)

# # Execute the command in R
system(gdal_command_rectify)



SpectralPatang::create_SAVI_mask(rectified_image_file_path,savi_file_path)


num_cores <- parallel::detectCores()

SpectralPatang::analyse_biodiversity(rectified_image_file_path,
                                     savi_file_path,
                                     NBbclusters = 5,
                                     Window_size = 10,
                                     NbCPU = num_cores,
                                     MaxRAM = 8,
                                     Perform_PCA = TRUE,
                                     PCA_Threshold = 99)



uesche <- read.csv("~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/species_analysis/tvexport.csv")
