# Clear workspace and graphics
rm(list = ls())
graphics.off()

# Load required libraries
if (!require(sf)) install.packages("sf", dependencies = TRUE)
if (!require(terra)) install.packages("terra", dependencies = TRUE)
if (!require(zip)) install.packages("zip", dependencies = TRUE)
library(sf)
library(terra)
library(zip)

# Define parameters
centroid_coords <- c(-17514270,11193817)  # Coordinates for the centroid
x_dim_m <- 2700  # Width in meters
y_dim_m <- 35000  # Height in meters
crs <- 3857  # Geographic CRS
crs_meters <- 32606  # Projected CRS for UTM Zone 6N
subzone <- "d"

output_folder <- paste0("~/Documents/GitHub/UWW200_Master_Thesis_public/MasterThesisRCode/cutline/output_rectangle_subzone_", subzone)
output_shapefile <- file.path(output_folder, "output_rectangle.shp")
zip_file <- paste0(output_folder, '.zip')

# Ensure output folder exists
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}



# Create the centroid point in geographic coordinates
centroid_point <- st_point(centroid_coords) |> st_sfc(crs = crs)

# Transform the centroid to the projected CRS (meters)
centroid_m <- st_transform(centroid_point, crs_meters)

# Extract the transformed coordinates in meters
x_center <- st_coordinates(centroid_m)[1]
y_center <- st_coordinates(centroid_m)[2]

# Define half-dimensions in meters
half_x <- x_dim_m / 2
half_y <- y_dim_m / 2

# Define rectangle corners based on the centroid and dimensions (no rotation)
rect_coords <- matrix(
  c(x_center - half_x, y_center - half_y,  # Bottom-left
    x_center + half_x, y_center - half_y,  # Bottom-right
    x_center + half_x, y_center + half_y,  # Top-right
    x_center - half_x, y_center + half_y,  # Top-left
    x_center - half_x, y_center - half_y), # Close the polygon
  ncol = 2,
  byrow = TRUE
)

# Create a polygon from the rectangle coordinates
rectangle <- st_polygon(list(rect_coords)) |> st_sfc(crs = crs_meters)

# Transform back to geographic CRS for export if needed
rectangle_geo <- st_transform(rectangle, crs)

# Create an sf object with the transformed polygon
rectangle_sf <- st_sf(geometry = rectangle_geo)

# Save the rectangle as a shapefile
st_write(rectangle_sf, output_shapefile, delete_layer = TRUE)

# Zip the output folder into a .zip file
zip::zip(zip_file, files = list.files(output_folder, full.names = TRUE))

message("Rectangle shapefile created, saved to ", output_folder, " and zipped as ", zip_file)
