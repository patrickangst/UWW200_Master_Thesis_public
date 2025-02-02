rm(list = ls())
graphics.off()

library(terra)
library(sf)
library(ggplot2)

base_folder <- 'D:/MasterThesis/final_hs_data_folder'

plot_list <- list.dirs(base_folder, recursive = FALSE, full.names = FALSE)


create_scatterplot <- function(plt_site_name) {
  plotsite_name <- plt_site_name
  result_folder <- file.path(base_folder, plotsite_name, 'result')
  result_subfolders <- list.dirs(result_folder, recursive = FALSE, full.names = TRUE)
  result_folder_name <- basename(result_subfolders)

  path_to_csv_folder <- file.path(base_folder, plotsite_name, 'data', 'species_analysis')

  csv_files <- list.files(
    path_to_csv_folder,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  # If you expect only one CSV file and want to extract the name:
  if (length(csv_files) == 1) {
    csv_file <- basename(csv_files)
    print(csv_file)
  } else if (length(csv_files) > 1) {
    print("Multiple CSV files found:")
    print(csv_files)
    # Add logic here to handle multiple CSV files based on your needs
  } else {
    print("No CSV files found in the folder.")
  }


  csv_file_name <- tools::file_path_sans_ext(csv_file)


  shannon_img_path <- paste0(
    'D:/MasterThesis/final_hs_data_folder/',
    plotsite_name,
    '/result/',
    result_folder_name,
    '/SPCA/ALPHA/Shannon_20'
  )
  pcoa_image_path <- paste0(
    'D:/MasterThesis/final_hs_data_folder/',
    plotsite_name,
    '/result/',
    result_folder_name,
    '/SPCA/BETA/BetaDiversity_BCdiss_PCO_20'
  )
  spectral_species_img_path <- paste0(
    'D:/MasterThesis/final_hs_data_folder/',
    plotsite_name,
    '/result/',
    result_folder_name,
    '/SPCA/SpectralSpecies/SpectralSpecies'
  )
  diversity_ground_data_path <- paste0(
    'D:/MasterThesis/final_hs_data_folder/',
    plotsite_name,
    '/data/species_analysis'
  )
  csv_file_path <- file.path(diversity_ground_data_path, csv_file)
  output_shapefile <- file.path(
    diversity_ground_data_path,
    paste0(csv_file_name, '_Shannon_Diversity_Ground.shp')
  )
  shannon_scatter_plot <- paste0(
    'D:/MasterThesis/final_hs_data_folder/',
    plotsite_name,
    '/data/species_analysis/',
    csv_file_name,
    '_shannon_scatter_plot.png'
  )

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
  plot_data[is.na(plot_data)] <- 0
  #plot_data <- na.omit(plot_data)

  # Calculate the Pearson correlation coefficient
  cor_value <- cor(plot_data$pixel_diversity,
                   plot_data$shapefile_diversity,
                   method = "pearson")

  # Ensure the number of colors does not exceed the max supported by Dark2
  num_colors <- length(unique(plot_data$shapefile_diversity))
  palette_choice <- if (num_colors > 8)
    "Set3"
  else
    "Dark2"  # Switch to "Set3" if > 8 groups

  # Create the scatter plot with regression line
  scatter_plot <- ggplot(plot_data, aes(x = pixel_diversity, y = shapefile_diversity)) +
    geom_point(aes(color = factor(shapefile_diversity)), size = 3) +
    scale_color_brewer(palette = palette_choice) +
    geom_smooth(method = "lm",
                color = "red",
                se = TRUE) +  # Add regression line
    annotate(
      "text",
      x = max(plot_data$pixel_diversity) * 0.7,
      y = max(plot_data$shapefile_diversity) * 0.9,
      label = paste("Correlation: ", round(cor_value, 3)),
      size = 5,
      color = "black"
    ) +  # Show correlation value
    labs(
      x = "Shannon Diversity (Hyperspectral image)",
      y = "Shannon Diversity (Ground data)",
      title = paste0("Shannon Diversity with Regression Line ", csv_file_name),
      color = "Shannon Diversity\n(Shapefile)"
    ) +
    theme_bw()

  print(scatter_plot)

  ggsave(
    shannon_scatter_plot,
    scatter_plot,
    dpi = 300,
    width = 10,
    height = 6
  )

}


for (plot in plot_list) {
  result <- create_scatterplot(plot)

  cat(sprintf("Done with %s!", plot))
}

