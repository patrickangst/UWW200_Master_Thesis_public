rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary libraries
library(vegan)
library(biodivMapR)
library(doParallel)
library(terra)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(readxl)
library(writexl)
library(dplyr)


image_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/new/final_hs_data_folder/AN_TJ_1/result_biodivMapR/ang20220711t002111_rfl_v2aa2_img_rectified/SPCA/SpectralSpecies/SpectralSpecies'

image <- rast(image_path)

hist(image)

# Extract raster values as a vector
pixel_values <- values(image)

# Count occurrences of each unique value
pixel_counts <- table(pixel_values)

# Print results
print(pixel_counts)


# Convert to a data frame
histogram_df <- as.data.frame(pixel_counts)

# Remove rows where Pixel_Value = 0
histogram_df <- histogram_df[histogram_df$pixel_values != 0, ]

colnames(histogram_df) <- c("Spectral_Species", "Count")

# Calculate total number of pixels (excluding 0 values)
total_pixels <- sum(histogram_df$Count)

# Convert counts to proportions (percentages)
histogram_df$Proportion <- histogram_df$Count / total_pixels

# Use the diversity() function from vegan
shannon_index <- diversity(histogram_df$Count, index = "shannon")

# Print the result
print(shannon_index)
