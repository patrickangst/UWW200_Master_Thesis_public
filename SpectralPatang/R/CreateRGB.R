#' Create RGB image
#'
#' This function is used to create a RGB image out of a hyperspectral image
#'
#' @param Raw_Image_Folder_Path character. Path of the image to be processed
#' @param RGB_Image_Folder_Path character. Path of the RGB image
#' @param Band_Selection character. Bandselection in the format "-b 59 -b 34 -b 20"
#' @param Output_Image_Format Format ot the RGB image (default: GTiff)
#'
#' @return Returns the full hyperspectral image path
#' @export

create_RGB  <- function(Raw_Image_Folder_Path,
                        RGB_Image_Folder_Path,
                        Band_Selection = "-b 59 -b 34 -b 20",
                        Output_Image_Format='GTiff') {

  # List all files in the folder
  files <- list.files(Raw_Image_Folder_Path, full.names = TRUE)

  # Filter files without an extension
  file_without_ext <- files[!grepl("\\.[a-zA-Z0-9]+$", basename(files))]

  # Check if exactly one file without extension exists
  if (length(file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the folder.")
  }

  # Extract the file name
  raw_image_file_name <- basename(file_without_ext)

  # Construct the full raw file path
  raw_image_file_path <- file.path(Raw_Image_Folder_Path, raw_image_file_name)

  # Determine file extension
  file_extension <- ifelse(Output_Image_Format == 'GTiff', '.tif', '')

  # Construct the full output file path
  output_image_file_path <- file.path(RGB_Image_Folder_Path, paste0(raw_image_file_name,"_rgb",file_extension))


  # Define the bands you want to select
  bandselection <- Band_Selection

  # GDAL translate command to extract the specified bands and create a new image
  gdal_translate_command <- sprintf("gdal_translate %s -of GTiff %s %s", bandselection, raw_image_file_path, output_image_file_path)

  # Execute the GDAL translate command
  system(gdal_translate_command)

  # Check if the output file was created successfully
  if (file.exists(output_image_file_path)) {
    cat("Output file created successfully:", output_image_file_path, "\n")

    # GDAL edit command to set color interpretation for each band
    gdal_edit_command <- sprintf("gdal_edit.py -colorinterp_1 Red -colorinterp_2 Green -colorinterp_3 Blue %s", output_image_file_path)

    # Execute the GDAL edit command
    system(gdal_edit_command)

    # Confirm that color interpretation was set successfully
    cat("Color interpretation set to RGB for each band in", output_image_file_path, "\n")

  } else {
    cat("Failed to create output file.\n")
  }

  return(raw_image_file_path)

}
