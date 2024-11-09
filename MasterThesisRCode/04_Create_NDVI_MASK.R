# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(terra)

# Read in hyperspectral image
cell <- "data/rectified/ang20190712t231624_rfl_v2v2_img_rectified_v2"
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
ndwi_threshold <- 0.1
ndwi_mask <- ifel(NDWI>ndwi_threshold, 0, 1)

# Plot the NDWI mask
plot(ndwi_mask, main = "NDWI Mask")

# Set value 0 to NA to exclude the unwanted pixels
ndwi_mask <- ifel(ndwi_mask==0, NA, 1)

# If desired, save the NDWI mask to a file
ndwi_threshold_modified <- gsub("\\.", "", ndwi_threshold)

ndwi_filename <- paste0("mask/ndwi_mask_",ndwi_threshold_modified)
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
ndvi_threshold <- 0.3
ndvi_mask <- ifel(NDVI>ndvi_threshold, 1, 0)

# Plot the NDVI mask
plot(ndvi_mask, main = "NDVI Mask")

# Set value 0 to NA to exclude the unwanted pixels
ndvi_mask <- ifel(ndvi_mask==0, NA, 1)

# If desired, save the NDVI mask to a file
ndvi_threshold_modified <- gsub("\\.", "", ndvi_threshold)

ndvi_filename <- paste0("mask/ndvi_mask_",ndvi_threshold_modified)
writeRaster(ndvi_mask, filename = file.path(ndvi_filename),
            filetype = "ENVI",
            gdal = "INTERLEAVE=BSQ",
            overwrite = TRUE,
            datatype = "INT1U")



################################################################################
# Create SAVI Mask SAVI = ((NIR - Red) / (NIR + Red + L)) * (1 + L)
################################################################################

# Set the L parameter for SAVI
L <- 0.5

# Calculate the SAVI
SAVI <- ((NIR_average - red_average) * (1 + L)) / (NIR_average + red_average + L)

# Plot a histogram of the SAVI values to inspect the distribution
hist(SAVI, breaks = seq(terra::minmax(SAVI)[1], terra::minmax(SAVI)[2] + 0.05, by = 0.01),
     main = "Histogram of SAVI", xlab = "SAVI")

# Create a SAVI mask (e.g., thresholding SAVI to identify vegetation)
# This threshold can be adjusted based on your analysis needs
savi_threshold <- 0.2

savi_mask <- ifel(SAVI > savi_threshold, 1, 0)  # Here, 0.2 is an example threshold

# Plot the SAVI mask
plot(savi_mask, main = "SAVI Mask")

# Set value 0 to NA to exclude the unwanted pixels
savi_mask <- ifel(savi_mask==0, NA, 1)

# If desired, save the SAVI mask to a file
savi_threshold_modified <- gsub("\\.", "", savi_threshold)

savi_filename <- paste0("mask/savi_mask_",savi_threshold_modified)
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
writeRaster(mask, filename = file.path("mask/mask_stack"),
            filetype = "ENVI",
            gdal = "INTERLEAVE=BSQ",
            overwrite = TRUE,
            datatype = "INT1U")

