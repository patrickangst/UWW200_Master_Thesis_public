rm(list = ls())
graphics.off()

library(terra)
library(future)
library(future.apply)
library(ggplot2)

# Load the hyperspectral image (e.g., GeoTIFF file)
input_image <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data/ang20180729t212542rfl/data/hs_raw_image/ang20180729t212542_rfl_v2r2_img"  # Update with the actual file path
input_image_rec <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data/ang20180729t212542rfl/data/rectified/ang20180729t212542_rfl_v2r2_img_rectified"  # Update with the actual file path
hyperspectral <- terra::rast(input_image_rec)

# Check basic properties of the image
print(hyperspectral)
#plot(hyperspectral[[1]])  # Plot the first band as an example

# Specify the three bands to stack (adjust numbers as needed)
red_band <- 20    # Band for the Red channel
green_band <- 34  # Band for the Green channel
blue_band <- 59   # Band for the Blue channel

# Plot the RGB image
plotRGB(hyperspectral, r = red_band, g = green_band, b = blue_band,
        scale = TRUE,  # Scales values between 0 and 255 for visualization
        stretch = "lin",  # Apply linear contrast stretching
        main = "RGB Composite of Hyperspectral Image")

# Convert hyperspectral image to a matrix
data_matrix <- as.matrix(hyperspectral)

# Remove any NA values (if present) to avoid issues during PCA
data_matrix <- na.omit(data_matrix)

# Check the dimensions of the matrix (rows: pixels, columns: bands)
dim(data_matrix)

# Set up parallel backend (use all available cores)
plan(multisession)  # Use multicore for parallelization

# Perform PCA in parallel
pca_result <- future({
  prcomp(data_matrix, scale. = TRUE)  # PCA with centering and scaling
})

# Collect results
pca_result <- value(pca_result)

# Print summary of PCA
summary(pca_result)
