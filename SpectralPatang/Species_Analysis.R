# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(vegan)     # For Shannon diversity calculation
library(ggplot2)   # For visualization
library(sf)        # For spatial data manipulation
library(raster)    # For rasterizing the map
library(dplyr)     # For data manipulation
library(terra)
library(readr)
library(reshape2)


# Load data
csv_path <- "~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/data/species_analysis/Species_List.csv"
shannon_div_idx_file_path <- "~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/result/ang20190712t231624_rfl_v2v2_img_rectified_cut/SPCA/ALPHA/Shannon_10"

# Read the CSV file with encoding
plot_data <- read_csv(csv_path)

image_data <- terra::rast(shannon_div_idx_file_path)
image_values <- values(image_data)
image_values <- na.omit(image_values)
# Extract coordinates
#coordinates <- plot_data[, c("Longitude", "Latitude")]

# Extract species composition
species_data <- plot_data[, -(0:1)]  # Assuming first 3 columns are ID, Lat, Long
# Remove rows with all zeros


species_data[is.na(species_data)] <- 0

rowSums(species_data)

# Normalize data so that each row sums to 1
species_data_normalized <- species_data / rowSums(species_data)
rowSums(species_data_normalized)

# Calculate Shannon Diversity Index for each plot
plot_data$Shannon_Index <- diversity(species_data_normalized, index = "shannon")


# Extract 40 evenly spaced indices
indices <- seq(1, length(image_values), length.out = 40)

# Use the indices to subset the vector
image_values <- sort(image_values)
selected_values <- image_values[indices]


# Combine into a data frame
comparison_df <- data.frame(
  ENVI = selected_values, # Match lengths
  Ground = sort(plot_data$Shannon_Index)
)


ggplot(comparison_df, aes(x = Ground, y = ENVI)) +
  geom_point(alpha = 0.7, color = "blue", size = 2) +  # Scatter points in blue
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +  # Red dashed line of equality (y = x)
  labs(
    title = "Comparison of Shannon Index Values",
    x = "Calculated Shannon Index",
    y = "ENVI File Shannon Index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Reshape data for density comparison
melted_df <- melt(comparison_df, variable.name = "Source", value.name = "Shannon_Index")

ggplot(melted_df, aes(x = Shannon_Index, fill = Source)) +
  geom_density(alpha = 0.5) +  # Overlapping density curves
  labs(
    title = "Density Comparison of Shannon Index Values",
    x = "Shannon Index",
    y = "Density",
    fill = "Source"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green"))  # Custom colors
