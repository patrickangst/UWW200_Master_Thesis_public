# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()


library(sf)
library(tidyverse)
library(leaflet)

plot_locations <- sf::st_read('~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cut_shp/14_Flux_Towers_Zona.shp')

summary(plot_locations)

plot_locations %>%
  ggplot() +
  geom_sf()


# Create a leaflet map for point data
leaflet(plot_locations) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    radius = 3,          # Circle size
    color = "blue",      # Circle border color
    fillColor = "red",   # Circle fill color
    fillOpacity = 1,   # Transparency of the circle
    popup = ~paste("Field_rele:", Field_rele)
  )













create_square_from_points <- function(input_file, output_file, scaling_factor = 2.5) {
  # Read the input file (e.g., GeoJSON or Shapefile)
  gdf <- st_read(input_file)

  # Ensure that the input contains only point geometries
  if (!all(st_geometry_type(gdf) == "POINT")) {
    stop("The input file does not contain only Point geometries.")
  }

  # Compute the centroid of all points
  centroid <- st_union(gdf) %>% st_centroid()

  # Calculate the maximum distance from the centroid to any point
  max_distance <- max(st_distance(gdf, centroid))

  # Define the side length of the square
  side_length <- scaling_factor * max_distance

  # Ensure the coordinates of the centroid and side_length are compatible
  center_coords <- st_coordinates(centroid)
  center_x <- units::set_units(center_coords[1], units(side_length))  # Ensure units match
  center_y <- units::set_units(center_coords[2], units(side_length))

  # Create the square polygon
  square <- st_as_sfc(st_bbox(c(
    xmin = center_x - side_length / 2,
    ymin = center_y - side_length / 2,
    xmax = center_x + side_length / 2,
    ymax = center_y + side_length / 2
  ), crs = st_crs(gdf)))

  # Create a new sf object for the square
  square_gdf <- st_sf(geometry = square, crs = st_crs(gdf))

  # Save the square to the output file
  st_write(square_gdf, output_file, driver = "ESRI Shapefile")

  cat("Square shapefile created and saved to", output_file, "\n")
}

# Example usage
input_file <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cut_shp/14_Flux_Towers_Zona.shp'
output_file <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cut_shp/output_square.shp'

create_square_from_points(input_file, output_file)
