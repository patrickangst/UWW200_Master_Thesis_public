---
title: "Species Abundance Analysis"
author: "Patrick Angst"
date: "`r Sys.Date()`"
output: html_document
---

# Introduction

This document outlines the progress of my masters thesis. The objective of this project is to assess biodiversity in the Arctic Tundra using spectral species analysis.The key tasks include:

- Data collection and pre-processing
- Calculating relative abundance and variance for each species.
- Identifying significant species based on defined thresholds.
- Use the R-package BiodivmapR package to calculate $\alpha$ and $\beta$- diversity

---

# Data Preparation and Code

The following steps were taken to prepare the data and perform the analysis:

## Define project parameters
All of the scripts access the information set in the parameter document [00_Project_Parameter.R](https://github.com/patrickangst/UWW200_Master_Thesis_public/blob/main/MasterThesisRCode/00_Project_Parameter.R)
```r

# filename of the raw hyperspectral image
file_name <- 'ang20190712t231624_rfl_v2v2_img'

# definition of the subzone (c, d or e)
subzone <- 'c'
file_name_rectified <- paste0(file_name,'_rectified')

# definition of the thresholdes for the mask creation
ndwi_threshold <- 0.1
ndvi_threshold <- 0.3
savi_threshold <- 0.2
savi_L <- 0.5

#create additional variables for the masks
mask_name_suffix <- gsub("\\.", "", savi_threshold)
mask_name <- paste0(file_name_rectified,'_savi_mask_',mask_name_suffix)

# get the base path of the project
base_path <- getwd()
```
## Create an RGB image of the flightstrip
For quick analysis the RGB bands are extracted from the hyperspectal image and saved as a GeoTiff file [01_Create_RGB.R](https://github.com/patrickangst/UWW200_Master_Thesis_public/blob/main/MasterThesisRCode/01_Create_RGB.R)
```r
# Clear workspace and graphics
rm(list = ls())
graphics.off()

# Define parameter script
source('00_Project_Parameter.R')


raw_image_file_path <- paste0(base_path,'/data/hs_raw_image/',file_name)
rgb_image_file_path <- paste0(base_path,'/data/rgb/',file_name,'_rgb.tif')

# Define the bands you want to select
bandselection <- "-b 59 -b 34 -b 20"

# GDAL translate command to extract the specified bands and create a new image
gdal_translate_command <- sprintf("gdal_translate %s -of GTiff %s %s", bandselection, raw_image_file_path, rgb_image_file_path)

# Execute the GDAL translate command
system(gdal_translate_command)

# Check if the output file was created successfully
if (file.exists(rgb_image_file_path)) {
  cat("Output file created successfully:", rgb_image_file_path, "\n")

  # GDAL edit command to set color interpretation for each band
  gdal_edit_command <- sprintf("gdal_edit.py -colorinterp_1 Red -colorinterp_2 Green -colorinterp_3 Blue %s", rgb_image_file_path)

  # Execute the GDAL edit command
  system(gdal_edit_command)

  # Confirm that color interpretation was set successfully
  cat("Color interpretation set to RGB for each band in", rgb_image_file_path, "\n")

} else {
  cat("Failed to create output file.\n")
}
```

## Image reactification
To follow the format set by the BiodivmapR package, the raw hyperspectral image has to be transformed. Using [gdalwarp](https://gdal.org/en/latest/programs/gdalwarp.html), the no-data values -9999 have to be replaced with 0 and the image organization has to be set to [BIL](https://desktop.arcgis.com/en/arcmap/latest/manage-data/raster-and-images/bil-bip-and-bsq-raster-files.htm) (Band interleaved by line). [02_Rectify_Image.R](https://github.com/patrickangst/UWW200_Master_Thesis_public/blob/main/MasterThesisRCode/02_Rectify_Image.R)
```r
# Clear workspace and graphics
rm(list = ls())
graphics.off()

library(sf)
library(terra)

# Define parameter script
source('00_Project_Parameter.R')

#boundary_file_path <- paste0(base_path, '/cutline/crop_large/crop_large.shp')
raw_image_file_path <- paste0(base_path, '/data/hs_raw_image/', file_name)
rectified_image_file_path <- paste0(base_path, '/data/rectified/', file_name_rectified)
rectified_hdr_file_path <- paste0(rectified_image_file_path, '.hdr')

target_srs <- "EPSG:32604"  # Define target CRS

gdal_command_rectify <- sprintf(
  "gdalwarp -of ENVI -co INTERLEAVE=BIL -srcnodata -9999 -dstnodata 0 %s %s",
  raw_image_file_path,
  rectified_image_file_path
)

# Print the command to check if it's correctly formed
print(gdal_command_rectify)

# Execute the command in R
system(gdal_command_rectify)

# ===============================================================================
# Define wavelength information to add to .hdr file
wavelength_values <- c(
  376.719576 ,
  381.729576 ,
  ...
)

wavelength_units <- "Nanometers"
wavelength_line <- paste("wavelength = {", paste(wavelength_values, collapse = " , "), "}")
units_line <- paste("wavelength units =", wavelength_units)

# ===============================================================================
# Append wavelength information to the .hdr file
if (file.exists(rectified_hdr_file_path)) {
  # Read the existing content of the .hdr file
  hdr_content <- readLines(rectified_hdr_file_path)

  # Append the wavelength information at the end of the file
  hdr_content <- c(hdr_content, units_line, wavelength_line)

  # Write the updated content back to the .hdr file
  writeLines(hdr_content, rectified_hdr_file_path)

  cat("Wavelength information successfully added to the .hdr file.\n")
} else {
  cat("Error: The .hdr file does not exist. Check the file path.\n")
}

print(paste0('Rectification done for ', file_name))
```

## Mask creation
Since not all pixels have to be processed, only the "valid" have to be selected. For this purpose, a [Soil Adjusted Vegetation Index (SAVI)](https://www.usgs.gov/landsat-missions/landsat-soil-adjusted-vegetation-index) mask is created. [03_Create_MASK.R](https://github.com/patrickangst/UWW200_Master_Thesis_public/blob/main/MasterThesisRCode/03_Create_MASK.R)
```r
# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(terra)

# Define parameter script
source('00_Project_Parameter.R')

file_name <- file_name_rectified
cell <- paste0(base_path,'/data/rectified/',file_name)
tile <- rast(file.path(cell))

# Plot the RGB image for a quick check
plot(ext(tile))
plotRGB(tile, add=T, r=54, g=36, b=20, stretch="lin")


# Calculate the mean of a few values of the near infrared bands (used for NDWI and SAVI)
NIR_average <- mean(tile[[c(86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105)]])
# Calculate green band averages (used for NDWI)
green_average <- mean(tile[[c(26, 27, 28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)]])
# Calculate red band averages (used for SAVI)
red_average <- mean(tile[[c(56, 57, 58, 59, 60, 61, 62, 63, 64, 65)]])

# Plot histogram of the distribution of the values
hist(NIR_average, breaks = seq(terra::minmax(NIR_average)[1], terra::minmax(NIR_average)[2] + 0.05, by = 0.01),
     main = "Histogram of NIR average", xlab = "NIR_average")


################################################################################
# Create NDWI Mask NDWI = (Red - NIR) / (Red + NIR)
################################################################################

# Calculate the NDWI
NDWI <- (green_average-NIR_average)/(green_average+NIR_average)

# Plot histogram of the value distribution
hist(NDWI, breaks = seq(terra::minmax(NDWI)[1], terra::minmax(NDWI)[2] + 0.05, by = 0.01),
     main = "Histogram of NDWI", xlab = "NDWI")

# Create the NDWI mask (binary values) with a threshold of 0.1
#ndwi_threshold <- ndwi_threshold
ndwi_mask <- ifel(NDWI>ndwi_threshold, 0, 1)

# Plot the NDWI mask
plot(ndwi_mask, main = "NDWI Mask")

# Set value 0 to NA to exclude the unwanted pixels
ndwi_mask <- ifel(ndwi_mask==0, NA, 1)

# If desired, save the NDWI mask to a file
ndwi_threshold_modified <- gsub("\\.", "", ndwi_threshold)

ndwi_filename <- paste0(base_path,"/mask/",file_name_rectified,"_ndwi_mask_",ndwi_threshold_modified)
writeRaster(ndwi_mask, filename = file.path(ndwi_filename),
            filetype = "ENVI",
            gdal = "INTERLEAVE=BSQ",
            overwrite = TRUE,
            datatype = "INT1U")

################################################################################
# Create NDVI Mask NDVI = (NIR - Red) / (NIR + Red)
################################################################################

# Calculate the NDVI
NDVI <- (NIR_average-red_average)/(NIR_average+red_average)

# Plot histogram of the value distribution
hist(NDVI, breaks = seq(terra::minmax(NDVI)[1], terra::minmax(NDVI)[2] + 0.05, by = 0.01),
     main = "Histogram of NDVI", xlab = "NDVI")

# Create the NDVI mask (binary values) with a threshold of 0.1
#ndvi_threshold <- 0.3
ndvi_mask <- ifel(NDVI>ndvi_threshold, 1, 0)

# Plot the NDVI mask
plot(ndvi_mask, main = "NDVI Mask")

# Set value 0 to NA to exclude the unwanted pixels
ndvi_mask <- ifel(ndvi_mask==0, NA, 1)

# If desired, save the NDVI mask to a file
ndvi_threshold_modified <- gsub("\\.", "", ndvi_threshold)

ndvi_filename <- paste0(base_path,"/mask/",file_name_rectified,"_ndvi_mask_",ndvi_threshold_modified)
writeRaster(ndvi_mask, filename = file.path(ndvi_filename),
            filetype = "ENVI",
            gdal = "INTERLEAVE=BSQ",
            overwrite = TRUE,
            datatype = "INT1U")

################################################################################
# Create SAVI Mask SAVI = ((NIR - Red) / (NIR + Red + L)) * (1 + L)
################################################################################

# Set the L parameter for SAVI
L <- savi_L

# Calculate the SAVI
SAVI <- ((NIR_average - red_average) * (1 + L)) / (NIR_average + red_average + L)

# Plot a histogram of the SAVI values to inspect the distribution
hist(SAVI, breaks = seq(terra::minmax(SAVI)[1], terra::minmax(SAVI)[2] + 0.05, by = 0.01),
     main = "Histogram of SAVI", xlab = "SAVI")

# Create a SAVI mask (e.g., thresholding SAVI to identify vegetation)
# This threshold can be adjusted based on your analysis needs
#savi_threshold <- 0.2

savi_mask <- ifel(SAVI > savi_threshold, 1, 0)  # Here, 0.2 is an example threshold

# Plot the SAVI mask
plot(savi_mask, main = "SAVI Mask")

# Set value 0 to NA to exclude the unwanted pixels
savi_mask <- ifel(savi_mask==0, NA, 1)

# If desired, save the SAVI mask to a file
savi_threshold_modified <- gsub("\\.", "", savi_threshold)

savi_filename <- paste0(base_path,"/mask/",file_name_rectified,"_savi_mask_",savi_threshold_modified)
writeRaster(savi_mask, filename = file.path(savi_filename),
            filetype = "ENVI",
            gdal = "INTERLEAVE=BSQ",
            overwrite = TRUE,
            datatype = "INT1U")

################################################################################
# Create stacked Mask
################################################################################

mask <- mosaic(savi_mask, ndwi_mask, ndvi_mask, fun="min")

plot(mask)
mask <- ifel(mask==0, NA, 1)

# Now write the raster file
stack_filename <- paste0(base_path,"/mask/",file_name_rectified,"_stacked_mask")
writeRaster(mask, filename = file.path(stack_filename),
            filetype = "ENVI",
            gdal = "INTERLEAVE=BSQ",
            overwrite = TRUE,
            datatype = "INT1U")
```