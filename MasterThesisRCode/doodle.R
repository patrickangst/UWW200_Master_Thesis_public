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

main_folder <- 'C:/Users/Patrick/Documents/GitHub/UWW200_Master_Thesis_public/MasterThesisRCode/data'
spectral_species_files_path <- file.path(main_folder,'03_Spectral_Species')
metrics_file_path <- file.path(main_folder,'07_Testsite_Metrics','Shannon_Diversity_Plotlevel.xlsx')
spectral_species_images <- list.files(spectral_species_files_path, full.names = TRUE)

#read excel with pca info
metrics_data <- read_excel(metrics_file_path, sheet = "Sheet1")

for (ss_image in spectral_species_images) {
  image_name <- basename(ss_image)
  test_site_name <- sub("_SpectralSpecies\\.tiff$", "", image_name)

  image <- rast(ss_image)

  #hist(image)

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

  histogram_df <- histogram_df[order(histogram_df$Count), ]  # Sort by count


  barplot(histogram_df$Count, names.arg = histogram_df$Spectral_Species,
          main = paste("Spectral Species Distribution -", test_site_name),
          xlab = "Spectral Species", ylab = "Count", col = "steelblue", las = 2)


  # Calculate total number of pixels (excluding 0 values)
  total_pixels <- sum(histogram_df$Count)

  # Convert counts to proportions (percentages)
  histogram_df$Proportion <- histogram_df$Count / total_pixels

  # Use the diversity() function from vegan
  shannon_index <- diversity(histogram_df$Count, index = "shannon")

  # Print the result
  print(shannon_index)


  # Update the 'SpectralSpecies' column
  metrics_data$Shannon_Diversity_Spectral[metrics_data$Testsite == test_site_name] <- shannon_index
  write_xlsx(metrics_data, path = metrics_file_path)

}
