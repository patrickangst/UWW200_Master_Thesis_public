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

# create_rectangular_shapefile  <- function(Plotlocation_Shp_File_Path,
#                                           Output_Folder_Path,
#                                           Buffer = 10) {
#   # Read your shapefile
#   input_file_name <- basename(Plotlocation_Shp_File_Path)
#   input_file_name_without_ext <- sub("\\.shp$", "", input_file_name)
#   plot_locations <- sf::st_read(Plotlocation_Shp_File_Path)
#
#   # create output file name and path
#   output_file_name <- paste0(input_file_name_without_ext, '_cutfile.shp')
#   output_file_path <- file.path(Output_Folder_Path, output_file_name)
#
#   # Calculate the bounding box of all points
#   bounding_box <- sf::st_as_sfc(sf::st_bbox(plot_locations$geometry))
#
#   # Assign the CRS of the input shapefile to the bounding box
#   sf::st_crs(bounding_box) <- sf::st_crs(plot_locations)
#
#   # Apply a Buffer buffer to the bounding box
#   buffer_distance <- Buffer
#   bounding_box_buffered <- sf::st_buffer(bounding_box, dist = buffer_distance)
#
#   # Reproject both bounding box and plot locations to geographic CRS (EPSG:4326)
#   # bounding_box_buffered <- sf::st_transform(bounding_box_buffered, crs = 4326)
#   # plot_locations <- sf::st_transform(plot_locations, crs = 4326)
#
#   # Save the buffered bounding box as a new shapefile
#   # Ensure the output directory exists
#   output_dir <- dirname(output_file_path)
#   if (!dir.exists(output_dir)) {
#     dir.create(output_dir, recursive = TRUE)
#   }
#
#
#   # Shorten field names to <= 10 characters
#   bounding_box_sf <- sf::st_as_sf(bounding_box_buffered)
#   bounding_box_sf$ID <- 1
#   bounding_box_sf$Desc <- "Buffered box"
#
#   # Write the shapefile
#   sf::st_write(bounding_box_sf, output_file_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
#
#   return(output_file_path)
# }

create_rectangular_shapefile <- function(Plotlocation_Shp_File_Path,
                                         Output_Folder_Path,
                                         Buffer = 10,
                                         Square = FALSE) {
  # Read the plot locations shapefile
  input_file_name <- basename(Plotlocation_Shp_File_Path)
  input_file_name_without_ext <- sub("\\.shp$", "", input_file_name)
  plot_locations <- sf::st_read(Plotlocation_Shp_File_Path, quiet = TRUE)

  # Extract the bounding box (xmin, ymin, xmax, ymax)
  bbox <- sf::st_bbox(plot_locations$geometry)

  # Expand bounding box by buffer distance
  xmin <- bbox$xmin - Buffer
  ymin <- bbox$ymin - Buffer
  xmax <- bbox$xmax + Buffer
  ymax <- bbox$ymax + Buffer

  # Check if we need a square instead of a rectangle
  if (Square) {
    width <- xmax - xmin
    height <- ymax - ymin
    side_length <- max(width, height)  # Use the longer side as the square length

    # Center the square around the original bounding box
    x_center <- (xmin + xmax) / 2
    y_center <- (ymin + ymax) / 2

    # Adjust to create a square
    xmin <- x_center - side_length / 2
    xmax <- x_center + side_length / 2
    ymin <- y_center - side_length / 2
    ymax <- y_center + side_length / 2
  }

  # Create the rectangular polygon
  rectangle_coords <- matrix(
    c(xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin),  # Closing the polygon
    ncol = 2, byrow = TRUE
  )

  # Convert to an sf polygon object
  rectangle <- sf::st_polygon(list(rectangle_coords)) |> sf::st_sfc(crs = sf::st_crs(plot_locations))

  # Convert to sf object and add attributes
  bounding_box_sf <- sf::st_sf(geometry = rectangle)
  bounding_box_sf$ID <- 1
  bounding_box_sf$Desc <- ifelse(Square, "Buffered square", "Buffered rectangle")

  # Create output file path
  output_file_name <- paste0(input_file_name_without_ext, "_cutfile.shp")
  output_file_path <- file.path(Output_Folder_Path, output_file_name)

  # Ensure output directory exists
  if (!dir.exists(Output_Folder_Path)) {
    dir.create(Output_Folder_Path, recursive = TRUE)
  }

  # Save as Shapefile
  sf::st_write(bounding_box_sf, output_file_path, driver = "ESRI Shapefile", delete_dsn = TRUE)

  return(output_file_path)
}
