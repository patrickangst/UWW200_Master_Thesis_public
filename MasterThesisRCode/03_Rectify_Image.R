# Clear workspace and graphics
rm(list = ls())
graphics.off()

library(sf)
library(terra)

boundary_file_path <- "cutline/output_rectangle_subzone_d/output_rectangle.shp"
raw_image_file_path <- "data/hs_raw_image/ang20190712t231624_rfl_v2v2_img"
rectified_image_file_path <- "data/rectified/ang20190712t231624_rfl_v2v2_img_rectified"

target_srs <- "EPSG:32613"  # Define target CRS

if (file.exists(boundary_file_path)) {
  message("Boundary File found.")
} else {
  message("Boundary File not found. Check the path and working directory.")
}

if (file.exists(raw_image_file_path)) {
  message("Raw File found.")
} else {
  message("Raw File not found. Check the path and working directory.")
}

# Rectification of the flight strip
sf::gdal_utils("warp",
               source = raw_image_file_path,
               destination = rectified_image_file_path,
               options = c(
                 "-of", "ENVI",               # Output format (ENVI)
                 "-t_srs", target_srs,      # Target CRS
                 "-cutline", boundary_file_path, # Path to shapefile for cutline
                 "-crop_to_cutline",          # Crop to the cutline shape
                 "-co", "INTERLEAVE=BIL"      # Add BIL interleave option
               )
)


boundary_file_path <- "cutline/output_rectangle_subzone_d/output_rectangle.shp"
raw_image_file_path <- "data/hs_raw_image/ang20190712t231624_rfl_v2v2_img"
rectified_image_file_path <- "data/rectified/ang20190712t231624_rfl_v2v2_img_rectified_V2"

target_srs <- "EPSG:32613"  # Define target CRS

# Rectification of the flight strip
sf::gdal_utils("warp",
               source = raw_image_file_path,
               destination = rectified_image_file_path,
               options = c(
                 "-of", "ENVI",               # Output format (ENVI)
                 "-t_srs", target_srs,      # Target CRS
                 "-co", "INTERLEAVE=BIL"      # Add BIL interleave option
               )
)
