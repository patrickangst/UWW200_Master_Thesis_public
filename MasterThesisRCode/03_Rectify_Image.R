# Clear workspace and graphics
rm(list = ls())
graphics.off()

library(sf)
library(terra)

# ===============================================================================
# set important variables
base_path <- getwd()

file_name <- 'ang20190712t231624_rfl_v2v2_img'

boundary_file_path <- paste0(base_path,'/cutline/crop_large/crop_large.shp')
raw_image_file_path <- paste0(base_path,'/data/hs_raw_image/',file_name)
rectified_image_file_path <- paste0(base_path,'/data/rectified/',file_name,'_rectified')

target_srs <- "EPSG:32604"  # Define target CRS

# Construct the gdalwarp command for rectification and reprojection
gdal_command_cutline <- sprintf(
  #"gdalwarp -of ENVI -t_srs %s -co INTERLEAVE=BIL %s %s",
  "gdalwarp -of ENVI -co INTERLEAVE=BIL -srcnodata -9999 -dstnodata 0 -cutline %s -crop_to_cutline %s %s",
  boundary_file_path,
  raw_image_file_path,
  rectified_image_file_path
)

gdal_command_rectify <- sprintf(
  "gdalwarp -of ENVI -co INTERLEAVE=BIL -srcnodata -9999 -dstnodata 0 %s %s",
  raw_image_file_path,
  rectified_image_file_path
)

# Print the command to check if it's correctly formed
# print(gdal_command)

# Execute the command in R
system(gdal_command_rectify)


# # Rectification of the flight strip
# sf::gdal_utils("warp",
#                source = raw_image_file_path,
#                destination = rectified_image_file_path,
#                options = c(
#                  "-of", "ENVI",               # Output format (ENVI)
#                  "-t_srs", target_srs,      # Target CRS
#                  "-cutline", boundary_file_path, # Path to shapefile for cutline
#                  "-crop_to_cutline",          # Crop to the cutline shape
#                  "-co", "INTERLEAVE=BIL"      # Add BIL interleave option
#                )
# )
#
#
# boundary_file_path <- "cutline/output_rectangle_subzone_d/output_rectangle.shp"
# raw_image_file_path <- "data/hs_raw_image/ang20190712t231624_rfl_v2v2_img"
# rectified_image_file_path <- "data/rectified/ang20190712t231624_rfl_v2v2_img_rectified_V2"
#
# target_srs <- "EPSG:32613"  # Define target CRS
#
# # Rectification of the flight strip
# sf::gdal_utils("warp",
#                source = raw_image_file_path,
#                destination = rectified_image_file_path,
#                options = c(
#                  "-of", "ENVI",               # Output format (ENVI)
#                  "-t_srs", target_srs,      # Target CRS
#                  "-co", "INTERLEAVE=BIL"      # Add BIL interleave option
#                )
# )
