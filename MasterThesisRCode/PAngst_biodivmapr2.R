library(biodivMapR2)
library(terra)

input_file_path <- 'DATA/ang20190712t231624_rfl_v2v2_img_cutline_V3_BIL.tif'
mask_file_path <- 'DATA/savi_above_008.tif'
result_directory <- 'RESULT/'


hs_image <- rast(input_file_path)
metadata <- meta(hs_image)


# Assuming 'metadata' contains the matrix from meta(hs_image)
wavelength_info <- metadata[[1]][, 2]  # Extract the second column

# Extract numeric values from the wavelength information
wavelengths <- as.numeric(gsub(" Nanometers", "", wavelength_info))
wavelengths <- wavelengths[!is.na(wavelengths)]

# Convert to character vector (since `names()` requires character labels)
input_rast_wl <- as.character(wavelengths)


Excluded_WL <- c(0, 442)
Excluded_WL <- rbind(Excluded_WL, c(1368, 1499))
Excluded_WL <- rbind(Excluded_WL, c(1779, 2055))
Excluded_WL <- rbind(Excluded_WL, c(2400, 2501))

min_values <- c(0, 1368, 1779, 2400)  # Minimum wavelength values
max_values <- c(442, 1499, 2055, 2501)  # Maximum wavelength values

# Create the data frame
Excluded_WL <- data.frame(min = min_values, max = max_values)


pca_result <- biodivMapR2::perform_PCA(input_raster_path = input_file_path,
                                       output_dir = result_directory,
                                       input_rast_wl = input_rast_wl,
                                       input_mask_path = NULL,
                                       Continuum_Removal = FALSE,
                                       TypePCA = "SPCA",
                                       NbPCs_To_Keep = 30,
                                       Excluded_WL = Excluded_WL,
                                       nbPix_PCA = 1e+06,
                                       nbIter = 20,
                                       maxRows = 100,
                                       filetype = "GTiff")
