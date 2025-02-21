#' Rectify hyperspectral image
#'
#' This function is used to rectify hyperspectral images using gdalwarp
#'
#' @param Hyperspectral_Raw_Image_Folder_Path character. Path of the image to be processed
#' @param Rectified_Image_Folder_Path character. Path of the rectified image
#'
#' @return Returns the full rectified image file path
#' @export

get_shapefile_path <- function(base_folder) {
  # Construct the path to the cutfiles directory

  cutfiles_dir <- sub("hs_raw_image", "cut_shapefile", base_folder)

  # Ensure the directory exists
  if (!dir.exists(cutfiles_dir)) {
    stop("Error: 'cutfiles' directory does not exist.")
  }

  # List all .shp files in the directory
  shapefiles <- list.files(cutfiles_dir, pattern = "\\.shp$", full.names = TRUE)

  # Ensure only one .shp file is found
  if (length(shapefiles) == 0) {
    stop("Error: No .shp file found in 'cutfiles' directory.")
  } else if (length(shapefiles) > 1) {
    stop("Error: Multiple .shp files found in 'cutfiles' directory.")
  }

  return(shapefiles)
}

rectify_Image  <- function(Hyperspectral_Raw_Image_Folder_Path,
                           Rectified_Image_Folder_Path, Cutfile=FALSE) {


  # List all files in the folder
  files <- list.files(Hyperspectral_Raw_Image_Folder_Path, full.names = TRUE)

  # Filter files without an extension
  file_without_ext <- files[!grepl("\\.[a-zA-Z0-9]+$", basename(files))]

  # Check if exactly one file without extension exists
  if (length(file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the folder.")
  }

  # Extract the file name
  raw_image_file_name <- basename(file_without_ext)

  # Construct the full raw file path
  raw_image_file_path <- file.path(Hyperspectral_Raw_Image_Folder_Path, raw_image_file_name)

  # Construct the full output file path
  rectified_image_file_path <- file.path(Rectified_Image_Folder_Path, paste0(raw_image_file_name,'_rectified'))
  rectified_hdr_file_path <- paste0(rectified_image_file_path, '.hdr')


  if(Cutfile){
    cutline_shapefile_path <- get_shapefile_path(Hyperspectral_Raw_Image_Folder_Path)
    gdal_command_rectify <- sprintf(
      "gdalwarp -cutline %s -crop_to_cutline -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
      cutline_shapefile_path,
      raw_image_file_path,
      rectified_image_file_path
    )

  } else {
    gdal_command_rectify <- sprintf(
      "gdalwarp -of ENVI -co INTERLEAVE=BIL -dstnodata -9999 %s %s",
      raw_image_file_path,
      rectified_image_file_path
    )
  }


  # # Execute the command in R
  system(gdal_command_rectify)

  # ===============================================================================
  # Define wavelength information to add to .hdr file
  # Read the .hdr file
  hdr_file_path <- paste0(raw_image_file_path, '.hdr')
  hdr_content <- readLines(hdr_file_path)

  # Extract lines with 'wavelength' and 'wavelength units'
  wavelength_line <- grep("^wavelength\\s*=", hdr_content, value = TRUE)
  wavelength_units_line <- grep("^wavelength units\\s*=", hdr_content, value = TRUE)

  # ===============================================================================
  # Append wavelength information to the .hdr file if not already present
  if (file.exists(rectified_hdr_file_path)) {
    # Read the existing content of the .hdr file
    rectified_hdr_content <- readLines(rectified_hdr_file_path)

    # Append the wavelength information at the end of the file

    # Extract lines with 'wavelength' and 'wavelength units'
    wavelength_line_rectified <- grep("^wavelength\\s*=", rectified_hdr_content, value = TRUE)
    wavelength_units_line_rectified <- grep("^wavelength units\\s*=", rectified_hdr_content, value = TRUE)

    if(length(wavelength_line_rectified) == 0){
      rectified_hdr_content <- c(rectified_hdr_content, wavelength_line)
    }

    if(length(wavelength_units_line_rectified) == 0){
      rectified_hdr_content <- c(rectified_hdr_content, wavelength_units_line)
    }

    # Ensure the last line has a newline character
    if (substring(rectified_hdr_content[length(rectified_hdr_content)], nchar(rectified_hdr_content[length(rectified_hdr_content)]), nchar(rectified_hdr_content[length(rectified_hdr_content)])) != "\n") {
      hdr_content[length(rectified_hdr_content)] <- paste0(hdr_content[length(rectified_hdr_content)])
    }

    # Write the updated content back to the .hdr file
    writeLines(rectified_hdr_content, rectified_hdr_file_path)

    cat("Wavelength information successfully added to the .hdr file.\n")
  } else {
    cat("Error: The .hdr file does not exist. Check the file path.\n")
  }

  return(rectified_image_file_path)

}

#debug(rectify_Image)
#path_name <- rectify_Image('ang20180729t212542rfl/data/hs_raw_image','ang20180729t212542rfl/data/rectified')
