# # # clean environment
# rm(list=ls(all=TRUE));gc()
# graphics.off()
#
# library(sf)
# library(leaflet)
#
# # Step 1: Read your shapefile
# input_file <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/plotlocations/06_Utqiaqvik_IBP.shp'
# output_file <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/plotlocations/06_Utqiaqvik_IBP_cutshape.shp'
#
# plot_locations <- st_read(input_file)
#
# # Step 2: Calculate the centroid of all points
# all_points <- st_union(plot_locations$geometry)  # Combine all points
# centroid <- st_centroid(all_points)  # Calculate centroid
#
# # Step 3: Reproject to a projected CRS (e.g., UTM Zone 33N, EPSG:32633)
# plot_location_crs <- st_crs(plot_locations)
# utm_crs <- 32633  # Replace with the appropriate UTM zone for your data
# plot_locations <- st_transform(plot_locations, crs = utm_crs)
# centroid <- st_transform(centroid, crs = utm_crs)
#
# # Step 4: Create a square around the centroid (500m side length)
# side_length <- 1000  # Side length in meters
# centroid_coords <- st_coordinates(centroid)  # Extract X and Y coordinates
#
# # Calculate the square coordinates
# x_min <- centroid_coords[1] - side_length / 2
# x_max <- centroid_coords[1] + side_length / 2
# y_min <- centroid_coords[2] - side_length / 2
# y_max <- centroid_coords[2] + side_length / 2
#
# # Create a square polygon
# square <- st_polygon(list(rbind(
#   c(x_min, y_min),
#   c(x_max, y_min),
#   c(x_max, y_max),
#   c(x_min, y_max),
#   c(x_min, y_min)
# )))
# square <- st_sfc(square, crs = utm_crs)  # Convert to an sf object
#
# # Step 5: Reproject everything back to geographic CRS (EPSG:4326) for Leaflet
# plot_locations <- st_transform(plot_locations, crs = 4326)
# square <- st_transform(square, crs = 4326)
# centroid <- st_transform(centroid, crs = 4326)
#
# # Step 6: Convert centroid to coordinates for Leaflet
# centroid_coords <- st_coordinates(centroid)
# centroid_lat <- centroid_coords[2]
# centroid_lon <- centroid_coords[1]
#
# # Step 7: Plot the points, centroid, and square on a Leaflet map
# leaflet(plot_locations) %>%
#   addTiles() %>%  # Add default OpenStreetMap tiles
#   addCircleMarkers(
#     radius = 3,          # Circle size
#     color = "blue",      # Circle border color
#     fillColor = "red",   # Circle fill color
#     fillOpacity = 1,     # Transparency of the circle
#     popup = ~paste("Field_rele:", Field_rele)  # Tooltip/popup
#   ) %>%
#   addMarkers(
#     lng = centroid_lon,
#     lat = centroid_lat,
#     popup = "Centroid of All Points",
#     label = "Centroid"
#   ) %>%
#   addPolygons(
#     data = square,
#     color = "green",
#     weight = 2,
#     fillColor = "transparent",
#     popup = "Square (500m x 500m)"
#   )
#
# square_sf <- st_as_sf(square)
# square_sf$ID <- 1  # Add an ID column
# square_sf$Description <- "500m square around centroid"  # Add a description
#
# # Save the shapefile with attributes
# st_write(square_sf, output_file, delete_dsn = TRUE)



















# Clean the environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load libraries
library(sf)
library(leaflet)

# Step 1: Read your shapefile
input_file <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/plotlocations/06_Utqiaqvik_IBP.shp'
output_file <- '~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/data/plotlocations/06_Utqiaqvik_IBP_boundingbox_buffered.shp'

plot_locations <- st_read(input_file)

# Step 2: Calculate the bounding box of all points
bounding_box <- st_as_sfc(st_bbox(plot_locations$geometry))

# Step 3: Assign the CRS of the input shapefile to the bounding box
st_crs(bounding_box) <- st_crs(plot_locations)

# Step 4: Apply a 10-meter buffer to the bounding box
buffer_distance <- 10
bounding_box_buffered <- st_buffer(bounding_box, dist = buffer_distance)

# Step 5: Reproject both bounding box and plot locations to geographic CRS (EPSG:4326)
bounding_box_buffered <- st_transform(bounding_box_buffered, crs = 4326)
plot_locations <- st_transform(plot_locations, crs = 4326)

# Step 6: Add the buffered bounding box to a Leaflet map for visualization
leaflet(plot_locations) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 3,
    color = "blue",
    fillColor = "red",
    fillOpacity = 1,
    popup = ~paste("Field_rele:", Field_rele)
  ) %>%
  addPolygons(
    data = bounding_box_buffered,
    color = "green",
    weight = 2,
    fillColor = "transparent",
    popup = "Buffered Bounding Box (10m)"
  )

# Step 7: Save the buffered bounding box as a new shapefile

# Ensure the output directory exists
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Shorten field names to <= 10 characters
bounding_box_sf <- st_as_sf(bounding_box_buffered)
bounding_box_sf$ID <- 1
bounding_box_sf$Desc <- "Buffered box"

# Write the shapefile
st_write(bounding_box_sf, output_file, delete_dsn = TRUE)
