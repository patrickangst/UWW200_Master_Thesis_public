# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(vegan)     # For Shannon diversity calculation
library(ggplot2)   # For visualization
library(sf)        # For spatial data manipulation
library(raster)    # For rasterizing the map
library(dplyr)     # For data manipulation

# Load data
csv_path <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_data_species/species.csv"
plot_data <- read.csv(csv_path)

# Extract coordinates
coordinates <- plot_data[, c("Longitude", "Latitude")]

# Extract species composition
species_data <- plot_data[, -(1:3)]  # Assuming first 3 columns are ID, Lat, Long
# Remove rows with all zeros


species_data[is.na(species_data)] <- 0


# Calculate Shannon Diversity Index for each plot
plot_data$Shannon_Index <- diversity(species_data, index = "shannon")

summary(plot_data$Shannon_Index)

