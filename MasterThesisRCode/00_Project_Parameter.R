folder_name <- 'ang20190713t002123_rfl_v2v2_img'
file_name <- 'ang20190713t002123_rfl_v2v2_img'
subzone <- 'c'
file_name_rectified <- paste0(file_name,'_rectified')

ndwi_threshold <- 0.1
ndvi_threshold <- 0.3
savi_threshold <- 0.2
savi_L <- 0.5

mask_name_suffix <- gsub("\\.", "", savi_threshold)
mask_name <- paste0(file_name_rectified,'_savi_mask_',mask_name_suffix)
#base_path <- getwd()


# base_path <- "/Volumes/Desk SSD/MasterThesisFinalDataFolder/subzone_c/ang20180812t231551rfl/rectified/ang20180812t231551_rfl_v2r2_img_rectified"
# base_path <- paste0("/Volumes/Desk SSD/MasterThesisFinalDataFolder/subzone_",subzone,"/",folder_name)
base_path <- getwd()
