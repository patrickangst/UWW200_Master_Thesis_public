# Clear workspace and graphics
rm(list = ls())
graphics.off()


# Install necessary packages if not installed
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

# Load necessary libraries
library(terra)
library(ggplot2)

# Define directories
file_dir <- file.path(getwd(), "file")
input_dir <- "C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/code/input/ang20180812t232708rfl"
file <- file.path(input_dir, "ang20180812t232708_rfl_v2r2_img")
hdr_file <- file.path(input_dir, "ang20180812t232708_rfl_v2r2_img.hdr")
output_dir <- file.path(getwd(), "output")

# Open the hyperspectral image file with terra
img_open <- rast(file)

# Read a few key properties of the image
nbands <- nlyr(img_open)
ncols <- ncol(img_open)
nrows <- nrow(img_open)

cat("Bands: ", nbands, "\nCols (x-axis): ", ncols, "\nRows (y-axis): ", nrows, "\n")

# Read and display band 57 just to have a quick look at the image
img_band57 <- img_open[[57]]

# Plot the image with ggplot2
plot(img_band57, main = "Band 57", col = terrain.colors(100))
