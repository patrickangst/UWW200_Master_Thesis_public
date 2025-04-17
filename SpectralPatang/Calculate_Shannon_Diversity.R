rm(list = ls(all = TRUE))
gc()
graphics.off()

library(readxl)
library(tidyverse)
library(vegan)
library(terra)
library(sf)
library(stringr)


# Specify the path to your Excel file
excel_file_path <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/gound_data/datasets/All_plots.xlsx"

# Read the Excel file into a data frame (first sheet by default)
df <- read_excel(excel_file_path)

# If your Excel file has multiple sheets, specify the sheet name or index
df <- read_excel(excel_file_path, sheet = "Turboveg export selection trans") # By sheet name
# df <- read_excel(excel_file_path, sheet = 1)        # By sheet index

frst_ak_3_df <- df %>%
  filter(Testsite == "FRST_AK_3" & `System for plant community name` == "B-B") %>%
  select(Testsite, `System for plant community name`, `Plant community name`)
# %>%
#   filter('System for plant community name' == "B-B") %>%

plant_community_counts <- frst_ak_3_df %>%
  group_by(`Plant community name`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count)) # Proportion for each plant community

# Calculate the Shannon diversity index
shannon_index_ground <- -sum(plant_community_counts$percentage * log(plant_community_counts$percentage))

# # Print the Shannon diversity index
# print(paste("Shannon Diversity Index for plot location FRST_AK_3:", round(shannon_index, 3)))



#
# use vegan package
#

# Create a community data matrix with counts of Plant community names by Testsite
community_matrix <- frst_ak_3_df %>%
  group_by(Testsite, `Plant community name`) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = `Plant community name`, values_from = count, values_fill = 0)

# View the matrix
print(community_matrix)

# Compute Shannon diversity for each Testsite
shannon_diversity <- diversity(community_matrix[,-1], index = "shannon") # Exclude the Testsite column

# Combine results with Testsite names
shannon_results <- data.frame(
  Testsite = community_matrix$Testsite,
  Shannon_Diversity = shannon_diversity
)

# View the results
print(shannon_results)

# Print the Shannon diversity index
print(paste("Shannon Diversity Index for plot location FRST_AK_3 (manual calculation):", round(shannon_index_ground, 3)))
print(paste("Shannon Diversity Index for plot location FRST_AK_3 (vegan package):", shannon_results$Shannon_Diversity))



#
# Shannon diversity on clusters
#

spectral_species_image <- rast("~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/03_Spectral_Species/FRST_AK_3_SpectralSpecies.tiff")

last_band <- spectral_species_image[[nlyr(spectral_species_image)]]

cluster_counts <- freq(last_band) # Exclude NA values if present
colnames(cluster_counts) <- c("layer","SpectralSpecies", "Count")

cluster_counts <- cluster_counts %>%
  select(-layer) %>%
  mutate(Percentage = Count / sum(Count))

# Calculate Shannon diversity index
shannon_index <- -sum(cluster_counts$Percentage * log(cluster_counts$Percentage))

# Print the Shannon diversity index
# print(paste("Shannon Diversity Index:", round(shannon_index, 3)))

# Create a vector of counts for each cluster
cluster_counts_vector <- cluster_counts$Count

# Use vegan::diversity to calculate Shannon diversity
shannon_index_vegan <- diversity(cluster_counts_vector, index = "shannon")

# Print the Shannon diversity index
print(paste("Spectral Shannon Diversity Index for FRST_AK_3 (manual calculation):", round(shannon_index, 3)))
print(paste("Spectral Shannon Diversity Index for FRST_AK_3 (vegan package):", shannon_index_vegan))


#
# Extract Spectral Shannon Diversity
#

# Step 1: Read the GeoTIFF file
shannon_image <- rast("~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/05_alpha_diversity/FRST_AK_3_Shannon_5.tiff")

shannon_matrix <- as.matrix(terra::values(shannon_image))

# Compute min, max, and average values
min_value <- min(shannon_matrix, na.rm = TRUE) # Minimum value
max_value <- max(shannon_matrix, na.rm = TRUE) # Maximum value
mean_value <- mean(shannon_matrix, na.rm = TRUE) # Average value (excluding NAs)

# Print the results
print(paste("Minimum Spectral Shannon Diversity Index (from alpha div. Image):", min_value))
print(paste("Maximum Spectral Shannon Diversity Index (from alpha div. Image):", max_value))
print(paste("Average Spectral Shannon Diversity Index (from alpha div. Image):", mean_value))







# Print the Shannon diversity index for Plant Community Name
print(paste("Shannon Diversity Index Plant Community Name for plot location FRST_AK_3 (manual calculation):", round(shannon_index_ground, 3)))
print(paste("Shannon Diversity Index Plant Community Name for plot location FRST_AK_3 (vegan package):", shannon_results$Shannon_Diversity))


# Print the Shannon diversity index manually counted for spectral species
print(paste("Spectral Shannon Diversity Index for FRST_AK_3 (manual calculation):", round(shannon_index, 3)))
print(paste("Spectral Shannon Diversity Index for FRST_AK_3 (vegan package):", shannon_index_vegan))

# Print the Shannon diversity index from the alpha diversity image
print(paste("Minimum Spectral Shannon Diversity Index (from alpha div. Image):", min_value))
print(paste("Maximum Spectral Shannon Diversity Index (from alpha div. Image):", max_value))
print(paste("Average Spectral Shannon Diversity Index (from alpha div. Image):", mean_value))



#
# Spectral Curves
#
img_file_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/final_hs_data_folder/FRST_AK_3/image_rectified/ang20190713t024201_rfl_v2v2_img_rectified'
hyperspectral <- rast(img_file_path)

#
# High Diversity
#
Forest_HighDiversity_file_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/test/gound_data/plot_location_shp/FRST_AK_Plot3/FRST_AK_Plot3.shp'
Forest_HighDiversity <- st_read(Forest_HighDiversity_file_path)
Forest_HighDiversity <- Forest_HighDiversity %>%
  # filter(System_for == 'Braun-Blanquet') %>%
  mutate(ID = row_number())

Forest_HighDiversity <- st_transform(Forest_HighDiversity, crs(hyperspectral))
Forest_HighDiversity_pixel_values <- terra::extract(hyperspectral, vect(Forest_HighDiversity))

Forest_HighDiversity_info <- Forest_HighDiversity %>%
  select(ID, 'Habitat_Ty')  # Adjust column name as needed

# Merge plant community name into extracted pixel values
Forest_HighDiversity_pixel_values_df <- Forest_HighDiversity_pixel_values %>%
  left_join(Forest_HighDiversity_info, by = "ID") %>%
  mutate(TestSite = 'FRST_AK3') %>%
  mutate(PlotID = paste0(ID, '_FRST_AK3')) %>%
  select(-ID)  # Remove ID if no longer needed


# str(Forest_HighDiversity_pixel_values_df)


colnames(Forest_HighDiversity_pixel_values_df) <- str_replace(
  colnames(Forest_HighDiversity_pixel_values_df),
  " Nanometers",
  ""
)




Forest_HighDiversity_pixel_values_long <- Forest_HighDiversity_pixel_values_df %>%
  pivot_longer(
    cols = -c(geometry, Habitat_Ty, TestSite, PlotID),  # Keep these columns as-is
    names_to = "Wavelength",  # New column for wavelength names
    values_to = "Reflectance"  # New column for reflectance values
  ) %>%
  mutate(Wavelength = as.numeric(Wavelength)) %>%
  mutate(Reflectance = ifelse(Reflectance < 0, NA, Reflectance)) %>%
  mutate(Reflectance = case_when(
    (Wavelength > 1353 & Wavelength < 1354) ~ NA,
    (Wavelength > 1413 & Wavelength < 1414) ~ NA,
    (Wavelength > 2400 & Wavelength < 2500) ~ NA,
    TRUE ~ Reflectance  # Keep the rest unchanged
  ))


unique_habitat_types <- sort(unique(Forest_HighDiversity_pixel_values_long$Habitat_Ty))

unique_wavelengths_signature_2 <- sort(unique(Forest_HighDiversity_pixel_values_long$Wavelength))

signature_2 <- ggplot(
  Forest_HighDiversity_pixel_values_long,
  aes(
    x = Wavelength,
    y = Reflectance,
    color = Habitat_Ty,
    group = PlotID
  )
) +
  geom_line() +
  labs(x = "Wavelength (nm)", y = "Reflectance", title = "Spectral Signatures") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = unique_wavelengths_signature_2) + # Use the unique wavelengths
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("green", "red", "blue", "purple")) # Specify your colors

plot(signature_2)
