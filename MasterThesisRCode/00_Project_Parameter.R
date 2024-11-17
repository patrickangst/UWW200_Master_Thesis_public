
# filename of the raw hyperspectral image
file_name <- 'ang20190712t231624_rfl_v2v2_img'

# definition of the subzone (c, d or e)
subzone <- 'c'
file_name_rectified <- paste0(file_name,'_rectified')

# definition of the thresholdes for the mask creation
ndwi_threshold <- 0.1
ndvi_threshold <- 0.3
savi_threshold <- 0.2
savi_L <- 0.5

#create additional variables for the masks
mask_name_suffix <- gsub("\\.", "", savi_threshold)
mask_name <- paste0(file_name_rectified,'_savi_mask_',mask_name_suffix)

# get the base path of the project
base_path <- getwd()

