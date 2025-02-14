# Clear workspace
rm(list = ls())
graphics.off()

# Load necessary libraries
library(terra)

# Set base path
base_path <- "D:/MasterThesis/final_hs_data_folder"

# List all subfolders in the base directory
subfolders <- list.dirs(base_path, recursive = FALSE)

# Iterate over each subfolder
for (subfolder in subfolders) {

  # Define the rectified data folder path
  rectified_folder <- file.path(subfolder, "data", "rectified")

  # Check if the rectified folder exists
  if (!dir.exists(rectified_folder)) {
    next  # Skip if the folder does not exist
  }

  # List files inside the rectified folder
  files <- list.files(rectified_folder, full.names = TRUE)

  # Filter files without an extension (ENVI file)
  envi_files <- files[!grepl("\\.[a-zA-Z0-9]+$", basename(files))]

  # Check if exactly one ENVI file exists
  if (length(envi_files) != 1) {
    warning(paste("Skipping folder:", subfolder, "- No unique ENVI file found."))
    next
  }

  # Extract folder name
  folder_name <- basename(subfolder)

  # Extract part before "_ang"
  extracted_name <- sub("(_ang.*)$", "", folder_name)

  # Print results
  print(paste("Processing:", extracted_name))
  print(paste("ENVI File:", envi_files))

  # TODO: Add further processing (e.g., reading the raster)
  # raster_data <- rast(envi_files)

  cut_shapefile_path <- file.path(subfolder,'data','cut_shapefile',paste0(extracted_name,'_cutshape_rotated.shp'))
  basepathfolder <- 'D:/MasterThesis/04_RGB'
  output_image_file_path <- file.path(basepathfolder,paste0(folder_name,'_rgb.tif'))

  # Define the bands you want to select
  bandselection <- '-b 59 -b 34 -b 20'

  # GDAL translate command to extract the specified bands and create a new image
  gdal_translate_command <- sprintf(
    "gdal_translate %s -of GTiff %s %s",
    bandselection,
    envi_files,
    output_image_file_path
  )

  # Execute the GDAL translate command
  system(gdal_translate_command)

  # Check if the output file was created successfully
  if (file.exists(output_image_file_path)) {
    cat("Output file created successfully:",
        output_image_file_path,
        "\n")

    # GDAL edit command to set color interpretation for each band
    gdal_edit_command <- sprintf(
      "gdal_edit.py -colorinterp_1 Red -colorinterp_2 Green -colorinterp_3 Blue %s",
      output_image_file_path
    )

    # Execute the GDAL edit command
    system(gdal_edit_command)

    # Confirm that color interpretation was set successfully
    cat("Color interpretation set to RGB for each band in",
        output_image_file_path,
        "\n")

  } else {
    cat("Failed to create output file.\n")
  }

}

