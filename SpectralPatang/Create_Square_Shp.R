# # clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(sf)
library(leaflet)

# Step 1: Read your shapefile
input_file <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cut_shp/14_Flux_Towers_Zona.shp'
output_file <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cut_shp/output_square_R.shp'

plot_locations <- st_read(input_file)

# Step 2: Calculate the centroid of all points
all_points <- st_union(plot_locations$geometry)  # Combine all points
centroid <- st_centroid(all_points)  # Calculate centroid

# Step 3: Reproject to a projected CRS (e.g., UTM Zone 33N, EPSG:32633)
plot_location_crs <- st_crs(plot_locations)
utm_crs <- 32633  # Replace with the appropriate UTM zone for your data
plot_locations <- st_transform(plot_locations, crs = utm_crs)
centroid <- st_transform(centroid, crs = utm_crs)

# Step 4: Create a square around the centroid (500m side length)
side_length <- 500  # Side length in meters
centroid_coords <- st_coordinates(centroid)  # Extract X and Y coordinates

# Calculate the square coordinates
x_min <- centroid_coords[1] - side_length / 2
x_max <- centroid_coords[1] + side_length / 2
y_min <- centroid_coords[2] - side_length / 2
y_max <- centroid_coords[2] + side_length / 2

# Create a square polygon
square <- st_polygon(list(rbind(
  c(x_min, y_min),
  c(x_max, y_min),
  c(x_max, y_max),
  c(x_min, y_max),
  c(x_min, y_min)
)))
square <- st_sfc(square, crs = utm_crs)  # Convert to an sf object

# Step 5: Reproject everything back to geographic CRS (EPSG:4326) for Leaflet
plot_locations <- st_transform(plot_locations, crs = 4326)
square <- st_transform(square, crs = 4326)
centroid <- st_transform(centroid, crs = 4326)

# Step 6: Convert centroid to coordinates for Leaflet
centroid_coords <- st_coordinates(centroid)
centroid_lat <- centroid_coords[2]
centroid_lon <- centroid_coords[1]

# Step 7: Plot the points, centroid, and square on a Leaflet map
leaflet(plot_locations) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    radius = 3,          # Circle size
    color = "blue",      # Circle border color
    fillColor = "red",   # Circle fill color
    fillOpacity = 1,     # Transparency of the circle
    popup = ~paste("Field_rele:", Field_rele)  # Tooltip/popup
  ) %>%
  addMarkers(
    lng = centroid_lon,
    lat = centroid_lat,
    popup = "Centroid of All Points",
    label = "Centroid"
  ) %>%
  addPolygons(
    data = square,
    color = "green",
    weight = 2,
    fillColor = "transparent",
    popup = "Square (500m x 500m)"
  )

square_sf <- st_as_sf(square)
square_sf$ID <- 1  # Add an ID column
square_sf$Description <- "500m square around centroid"  # Add a description

# Save the shapefile with attributes
st_write(square_sf, output_file, delete_dsn = TRUE)
