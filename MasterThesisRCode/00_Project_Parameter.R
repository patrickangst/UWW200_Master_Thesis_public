
# filename of the raw hyperspectral image
# folder_name <- 'ang20180812t231551rfl'
# file_name <- 'ang20180812t231551_rfl_v2r2_img'
# csv_file_name <- 'subzone_c.csv'

folder_name <- 'ang20180729t212542rfl'
file_name <- 'ang20180729t212542_rfl_v2r2_img_rect_9999'
csv_file_name <- '14_Flux_Towers_Zona_Species_List.csv'

# definition of the subzone (c, d or e)
subzone <- 'c'
file_name_rectified <- paste0(file_name,'_rectified')
file_name_rectified <- 'ang20180729t212542_rfl_v2r2_img_rect_9999'

# definition of the thresholdes for the mask creation
ndwi_threshold <- 0.1
ndvi_threshold <- 0.3
savi_threshold <- 0.2
savi_L <- 0.5

#create additional variables for the masks
mask_name_suffix <- gsub("\\.", "", savi_threshold)
mask_name <- paste0(file_name_rectified,'_savi_mask_',mask_name_suffix)

# number of clusters (spectral species)
nbclusters_calculated <- NA

# get the base path of the project
base_path <- paste0(getwd(),'/',folder_name)

