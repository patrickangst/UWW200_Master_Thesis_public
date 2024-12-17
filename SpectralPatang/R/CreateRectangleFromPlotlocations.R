#' Create Rectangular shapefile from plot locations
#'
#' This function is used to create a rectangular box around plot locations
#'
#' @param Plotlocation_Shp_File_Path character. Path of the plot locations shp
#' @param Output_Folder_Path character. Folder path of the shp of the cut file image
#' @param Buffer character. How much buffer is to be set around the plot locations
#'
#' @return Returns the full file path of the cut file
#' @export

create_rectangular_shapefile  <- function(Plotlocation_Shp_File_Path,
                                          Output_Folder_Path,
                                          Buffer = 10) {
  # Step 1: Read your shapefile
  input_file_name <- basename(Plotlocation_Shp_File_Path)
  input_file_name_without_ext <- sub("\\.shp$", "", input_file_name)
  plot_locations <- sf::st_read(Plotlocation_Shp_File_Path)

  # create output file name and path
  output_file_name <- paste0(input_file_name_without_ext, '_cutfile.shp')
  output_file_path <- file.path(Output_Folder_Path, output_file_name)

  # Step 2: Calculate the bounding box of all points
  bounding_box <- sf::st_as_sfc(sf::st_bbox(plot_locations$geometry))

  # Step 3: Assign the CRS of the input shapefile to the bounding box
  sf::st_crs(bounding_box) <- sf::st_crs(plot_locations)

  # Step 4: Apply a 10-meter buffer to the bounding box
  buffer_distance <- 10
  bounding_box_buffered <- sf::st_buffer(bounding_box, dist = buffer_distance)

  # Step 5: Reproject both bounding box and plot locations to geographic CRS (EPSG:4326)
  bounding_box_buffered <- sf::st_transform(bounding_box_buffered, crs = 4326)
  plot_locations <- sf::st_transform(plot_locations, crs = 4326)

  # Step 7: Save the buffered bounding box as a new shapefile

  # Ensure the output directory exists
  output_dir <- dirname(output_file_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }


  # Shorten field names to <= 10 characters
  bounding_box_sf <- sf::st_as_sf(bounding_box_buffered)
  bounding_box_sf$ID <- 1
  bounding_box_sf$Desc <- "Buffered box"

  # Write the shapefile
  sf::st_write(bounding_box_sf, output_file_path, driver = "ESRI Shapefile", delete_dsn = TRUE)

  return(output_file_path)
}
