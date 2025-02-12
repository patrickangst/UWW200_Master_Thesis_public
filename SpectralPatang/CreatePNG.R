






rm(list = ls())
graphics.off()

library(terra)
library(RColorBrewer)
library(png)
library(viridis)


# Set file paths
input_file <- "D:/MasterThesis/test_data/Shannon_20_Fullres"  # ENVI file (without .hdr)
output_png <- "D:/MasterThesis/test_data/Shannon_20_Fullres.jpg"

# Load the ENVI raster
raster_envi <- rast(input_file)

# Check raster size
print(dim(raster_envi))  # [rows, cols, bands]

# Read first band (assuming single-band raster)
raster_data <- values(raster_envi, mat=TRUE)  # Convert to matrix

# Handle NoData values (-9999), setting them to NA
raster_data[raster_data == -9999] <- NA

# Normalize raster values to 0-255
min_val <- min(raster_data, na.rm=TRUE)
max_val <- max(raster_data, na.rm=TRUE)
raster_data <- (raster_data - min_val) / (max_val - min_val) * 255

# Convert NA values to 0 (black) or 255 (white, optional)
raster_data[is.na(raster_data)] <- 0  # Set NoData to black

# Convert matrix to raster object
raster_final <- rast(raster_data)

# Define color palette (similar to matplotlib's "turbo")
# R has 'viridis' from 'viridis' package, but for turbo, we need to define a custom palette
color_palette <- colorRampPalette(c("blue", "green", "yellow", "red"))(256)  # Turbo-like palette

# Set the proper dimensions for saving PNG
jpeg(output_png, width=ncol(raster_final), height=nrow(raster_final), res=300)

# Plot raster with the color palette
#image(raster_data, col=color_palette, axes=FALSE, ann=FALSE)

# Close PNG device
dev.off()

print(paste("Saved PNG:", output_png))
