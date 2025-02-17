rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary libraries
library(vegan)
library(terra)
library(readxl)
library(writexl)
library(dplyr)

# Define paths
base_folder <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis'
spectral_species_files_path <- file.path(base_folder, '03_Spectral_Species')
shannon_diversity_plotlevel_file_path <- file.path(base_folder, '07_Testsite_Metrics', 'Shannon_Diversity_Plotlevel.xlsx')
metrics_file_path <- file.path(base_folder, '07_Testsite_Metrics', 'Metrics.xlsx')

# Read Excel file
shannon_diversity_plotlevel_file_path_data <- read_excel(shannon_diversity_plotlevel_file_path, sheet = "Sheet1")

# Get list of spectral species images
spectral_species_images <- list.files(
  spectral_species_files_path,
  full.names = TRUE,
  pattern = "\\.tiff$"
)

for (ss_image in spectral_species_images) {
  image_name <- basename(ss_image)
  test_site_name <- sub("_SpectralSpecies\\.tiff$", "", image_name)
  image <- rast(ss_image)

  # Get the number of bands
  num_bands <- nlyr(image)

  # Store Shannon diversity and unique species count per band
  shannon_values <- numeric(num_bands)
  unique_species_counts <- numeric(num_bands)

  for (band in 1:num_bands) {
    # Extract pixel values for the current band
    pixel_values <- values(image[[band]])

    # Count occurrences of each unique value
    pixel_counts <- table(pixel_values)

    # Convert to data frame
    histogram_df <- as.data.frame(pixel_counts)

    # Remove rows where Spectral_Species = 0
    histogram_df <- histogram_df[histogram_df$pixel_values != 0, ]

    # Rename columns
    colnames(histogram_df) <- c("Spectral_Species", "Count")

    # Calculate Shannon diversity for the band
    shannon_values[band] <- diversity(histogram_df$Count, index = "shannon")

    # Count the number of unique spectral species (clusters)
    unique_species_counts[band] <- length(unique(histogram_df$Spectral_Species))
  }

  # Compute the average Shannon diversity across all bands
  average_shannon <- mean(shannon_values, na.rm = TRUE)

  # Compute the average unique species count across all bands
  average_unique_species <- mean(unique_species_counts, na.rm = TRUE)

  # Print results
  print(paste("Testsite:", test_site_name))
  print(paste("Shannon diversity per band:", paste(round(shannon_values, 3), collapse = ", ")))
  print(paste("Average Shannon diversity:", round(average_shannon, 3)))
  print(paste("Unique species count per band:", paste(unique_species_counts, collapse = ", ")))
  print(paste("Average unique species count:", round(average_unique_species, 3)))

  # Update shannon_diversity_plotlevel_file_path_data with the average Shannon diversity and species count
  shannon_diversity_plotlevel_file_path_data$Shannon_Diversity_Spectral[shannon_diversity_plotlevel_file_path_data$Testsite == test_site_name] <- average_shannon
  shannon_diversity_plotlevel_file_path_data$SpectralSpecies[shannon_diversity_plotlevel_file_path_data$Testsite == test_site_name] <- average_unique_species
}

# Save updated data to Excel
write_xlsx(shannon_diversity_plotlevel_file_path_data, path = shannon_diversity_plotlevel_file_path)


