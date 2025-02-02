rm(list = ls())
graphics.off()

library(terra)
library(sf)
library(ggplot2)

plotsite_name <- 'AN_TJ_Plot1_ang20220711t002111rfl'
result_folder_name <- 'ang20220711t002111_rfl_v2aa2_img_rectified'
csv_file_name <- 'AN_TJ_Plot1'
csv_file <- paste0(csv_file_name,'.csv')

shannon_img_path <- paste0('D:/MasterThesis/final_hs_data_folder/',plotsite_name,'/result/',result_folder_name,'/SPCA/ALPHA/Shannon_20')
pcoa_image_path <- paste0('D:/MasterThesis/final_hs_data_folder/',plotsite_name,'/result/',result_folder_name,'/SPCA/BETA/BetaDiversity_BCdiss_PCO_20')
spectral_species_img_path <- paste0('D:/MasterThesis/final_hs_data_folder/',plotsite_name,'/result/',result_folder_name,'/SPCA/SpectralSpecies/SpectralSpecies')
diversity_ground_data_path <- paste0('D:/MasterThesis/final_hs_data_folder/',plotsite_name,'/data/species_analysis')
csv_file_path <- file.path(diversity_ground_data_path, csv_file)
output_shapefile <- file.path(diversity_ground_data_path, paste0(csv_file_name,'_Shannon_Diversity_Ground.shp'))
spectral_species_count_plot <- paste0('D:/MasterThesis/final_hs_data_folder/',plotsite_name,'/data/species_analysis/',csv_file_name,'_cluster_count.png')

# Load the raster map
raster_map <- rast(shannon_img_path)

# Load the shapefile
shapefile <- st_read(output_shapefile)

# Extract pixel values at point locations
extracted_data <- extract(raster_map, shapefile)

# Extract pixel values correctly (assuming first column is ID)
shapefile$pixel_diversity <- extracted_data[, 2]  # Adjust index if needed

# Create a data frame for plotting
plot_data <- data.frame(
  pixel_diversity = shapefile$pixel_diversity,
  shapefile_diversity = shapefile$ShnnnId  # Ensure column name is correct
)

# Check for NA values and remove them (optional)
plot_data <- na.omit(plot_data)

# Create the scatter plot using ggplot2
ggplot(plot_data, aes(x = pixel_diversity, y = shapefile_diversity)) +
  geom_point(aes(color = factor(shapefile_diversity)), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Shannon Diversity (Pixel)",
    y = "Shannon Diversity (Shapefile)",
    title = "Scatter Plot of Shannon Diversity",
    color = "Shannon Diversity\n(Shapefile)"
  ) +
  theme_bw()

