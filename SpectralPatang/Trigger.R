devtools::load_all()
library(parallel)
library(SpectralPatang)
library(terra)

nb_cores <- detectCores(all.tests = FALSE, logical = TRUE)

#debug(analyse_biodiversity)
analyse_biodiversity('~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data/ang20180729t212542rfl/data/rectified/ang20180729t212542_rfl_v2r2_img_rectified',
                     '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data/ang20180729t212542rfl/mask/ang20180729t212542_rfl_v2r2_img_rectified_savi_mask_02',
                     NbCPU = nb_cores,
                     Window_size = 5,
                     Perform_PCA = FALSE,
                     PCA_Threshold = 99)


# # Clear workspace and graphics
# rm(list = ls())
# graphics.off()
# library(terra)
#
# # Load the hyperspectral image (replace 'path_to_image' with your file path)
# hyperspectral_image <- rast('~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/ang20180729t212542rfl/data/rectified/ang20180729t212542_rfl_v2r2_img_rectified')
#
#
# plot(hyperspectral_image)
#
# # Select a pixel location (row and column or x, y coordinates)
# # Option 1: Using row and column indices
# pixel_coords <- c(row = 1000, col = 200)
#
# # Option 2: Using geographic coordinates (x, y)
# # pixel_coords <- cbind(x = 200, y = 300)
#
# # Extract reflectance values for the selected pixel
# reflectance <- extract(hyperspectral_image, pixel_coords)
#
# # Assume band names are wavelengths (or assign wavelengths)
# wavelengths <- as.numeric(names(hyperspectral_image)) # If bands are named by wavelength
# # If not, provide wavelengths manually:
# # wavelengths <- c(400, 450, 500, ...)  # Replace with actual wavelengths
#
# # Create a data frame for plotting
# spectral_data <- data.frame(Wavelength = wavelengths, Reflectance = reflectance)
#
# # Plot the spectral signature
# plot(spectral_data$Wavelength, spectral_data$Reflectance, type = "l",
#      xlab = "Wavelength (nm)", ylab = "Reflectance",
#      main = "Spectral Signature of Selected Pixel")
