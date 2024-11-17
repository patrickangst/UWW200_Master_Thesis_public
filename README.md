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
Since not all pixels have to be processed, only the "valid" have to be selected. For this purpose, a [Soil Adjusted Vegetation Index (SAVI)](https://www.usgs.gov/landsat-missions/landsat-soil-adjusted-vegetation-index) mask is created 