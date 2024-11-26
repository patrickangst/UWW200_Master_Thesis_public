#' Create SAVI mask
#'
#' This Library is used to create a SAVI (soil adjusted vegetation index)
#' mask for a hyperspectral image
#'
#' @param Hyperspectral_Image_File_Path character. Path of the image to be processed
#' @param SAVI_Mask_Folder_Path character. Path of the rectified image
#' @param SAVI_threshold number. SAVI threshold. Default: 0.2
#' @param SAVI_L number. Canopy background adjustment factor. Default: 0.5
#'
#' @return Returns the full mask file path
#' @export

create_SAVI_mask <- function(Hyperspectral_Image_File_Path,
                             SAVI_Mask_Folder_Path,
                             SAVI_threshold = 0.2,
                             SAVI_L = 0.5){

  tile <- terra::rast(Hyperspectral_Image_File_Path)

  # Calculate the mean of a few values of the near infrared bands (used for NDWI and SAVI)
  NIR_average <- terra::app(tile[[86:105]], fun = mean, na.rm = TRUE)

  # Calculate green band averages (used for NDWI)
  green_average <- terra::app(tile[[26:45]], fun = mean, na.rm = TRUE)

  # Calculate red band averages (used for SAVI)
  red_average <- terra::app(tile[[56:65]], fun = mean, na.rm = TRUE)

  ################################################################################
  # Create SAVI Mask SAVI = ((NIR - Red) / (NIR + Red + L)) * (1 + L)
  ################################################################################

  # Calculate the SAVI
  SAVI <- ((NIR_average - red_average) * (1 + SAVI_L)) / (NIR_average + red_average + SAVI_L)

  # Create a SAVI mask (e.g., thresholding SAVI to identify vegetation)
  # This threshold can be adjusted based on your analysis needs
  savi_mask <- terra::ifel(SAVI > SAVI_threshold, 1, 0)  # Here, 0.2 is an example threshold

  # Set value 0 to NA to exclude the unwanted pixels
  savi_mask <- terra::ifel(savi_mask==0, NA, 1)


  # If desired, save the SAVI mask to a file
  savi_threshold_modified <- gsub("\\.", "", SAVI_threshold)

  # Extract the file name
  savi_image_file_name <- paste0(basename(Hyperspectral_Image_File_Path),'_savi_mask_',savi_threshold_modified)
  savi_image_file_path <- file.path(SAVI_Mask_Folder_Path,savi_image_file_name)

  terra::writeRaster(savi_mask, filename = savi_image_file_path,
              filetype = "ENVI",
              gdal = "INTERLEAVE=BSQ",
              overwrite = TRUE,
              datatype = "INT1U")


  return(savi_image_file_path)
  }


# debug(create_SAVI_mask)
#create_SAVI_mask('ang20180729t212542rfl/data/rectified/ang20180729t212542_rfl_v2r2_img_rectified','ang20180729t212542rfl/mask')
