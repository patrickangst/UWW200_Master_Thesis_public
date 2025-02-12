#' Create SAVI mask
#'
#' This Library is used to create a SAVI (soil adjusted vegetation index)
#' mask for a hyperspectral image
#'
#' @param Rectified_Image_Folder_Path character. Folderpath of the image to be processed
#' @param SAVI_Mask_Folder_Path character. Path of the rectified image
#' @param SAVI_threshold number. SAVI threshold. Default: 0.2
#' @param SAVI_L number. Canopy background adjustment factor. Default: 0.5
#'
#' @return Returns the full mask file path
#' @export

create_SAVI_mask <- function(Rectified_Image_Folder_Path,
                             SAVI_Mask_Folder_Path,
                             SAVI_threshold = 0.2,
                             SAVI_L = 0.5) {
  # List all files in the folder
  files <- list.files(Rectified_Image_Folder_Path, full.names = TRUE)

  # Filter files without an extension
  file_without_ext <- files[!grepl("\\.[a-zA-Z0-9]+$", basename(files))]

  # Check if exactly one file without extension exists
  if (length(file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the folder.")
  }

  # Extract the file name
  rectified_image_file_name <- basename(file_without_ext)

  # Construct the full raw file path
  rectified_image_file_path <- file.path(Rectified_Image_Folder_Path, rectified_image_file_name)

  # Read raster file with terra
  tile <- terra::rast(rectified_image_file_path)

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
  savi_mask <- terra::ifel(savi_mask == 0, NA, 1)


  # If desired, save the SAVI mask to a file
  savi_threshold_modified <- gsub("\\.", "", SAVI_threshold)

  # Extract the file name
  savi_image_file_name <- paste0(rectified_image_file_name,
                                 '_savi_mask_',
                                 savi_threshold_modified)
  savi_image_file_path <- file.path(SAVI_Mask_Folder_Path, savi_image_file_name)

  terra::writeRaster(
    savi_mask,
    filename = savi_image_file_path,
    filetype = "ENVI",
    gdal = "INTERLEAVE=BSQ",
    overwrite = TRUE,
    datatype = "INT1U"
  )

  return(savi_image_file_path)
}

#debug(create_SAVI_mask)
create_SAVI_mask("D:/MasterThesis/final_hs_data_folder/AN_TJ_Plot2_ang20220711t003358rfl/data/rectified","D:/MasterThesis/final_hs_data_folder/AN_TJ_Plot2_ang20220711t003358rfl/data" )
