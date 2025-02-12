# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(vegan)     # For Shannon diversity calculation
library(ggplot2)   # For visualization
library(sf)        # For spatial data manipulation
library(raster)    # For rasterizing the map
library(dplyr)     # For data manipulation
library(terra)
library(readr)
library(reshape2)
library(leaflet)

input_folder <- 'final_hs_data_folder_test'
base_folder <- file.path('D:/MasterThesis',input_folder)
plot_list <- list.dirs(base_folder, recursive = FALSE, full.names = FALSE)


create_species_analysis <- function(plt_site_name) {
  # Define file paths
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
    'D:/MasterThesis/',input_folder,
    '/',plotsite_name,
    '/result/',
    result_folder_name,
    '/SPCA/ALPHA/Shannon_20'
  )
  pcoa_image_path <- paste0(
    'D:/MasterThesis/',input_folder,
    '/',plotsite_name,
    '/result/',
    result_folder_name,
    '/SPCA/BETA/BetaDiversity_BCdiss_PCO_20'
  )
  spectral_species_img_path <- paste0(
    'D:/MasterThesis/',input_folder,
    '/',plotsite_name,
    '/result/',
    result_folder_name,
    '/SPCA/SpectralSpecies/SpectralSpecies'
  )
  diversity_ground_data_path <- paste0(
    'D:/MasterThesis/',input_folder,
    '/',plotsite_name,
    '/data/species_analysis'
  )
  csv_file_path <- file.path(diversity_ground_data_path, csv_file)
  output_shapefile <- file.path(
    diversity_ground_data_path,
    paste0(csv_file_name, '_Shannon_Diversity_Ground.shp')
  )
  spectral_species_count_plot <- paste0(
    'D:/MasterThesis/',input_folder,
    '/',plotsite_name,
    '/data/species_analysis/',
    csv_file_name,
    '_cluster_count.png'
  )

  shannon_diversity_ground_plot <- paste0(
    'D:/MasterThesis/',input_folder,
    '/',plotsite_name,
    '/data/species_analysis/',
    csv_file_name,
    '_shannon_diversity_ground_plot.png'
  )

  shannon_img <- terra::rast(shannon_img_path)

  # Replace -9 with NA
  plot_data <- read_csv(csv_file_path)

  # Remove columns where all values are NA
  plot_data_filtered <- plot_data[, colSums(!is.na(plot_data)) > 0]

  # Find the column position of "Number of species"
  number_species_pos <- which(names(plot_data_filtered) == 'Number of species')

  # Dynamically select "Releve number", "Number of species", and all columns to the right of "Number of species"
  selected_data <- plot_data_filtered %>%
    dplyr::select('Releve number', 'Number of species', all_of(names(plot_data_filtered)[(number_species_pos + 1):ncol(plot_data_filtered)]))

  # select only species list
  species_table <- selected_data %>%
    dplyr::select(-c('Releve number', 'Number of species'))


  species_table[is.na(species_table)] <- 0

  rowSums(species_table)

  # Normalize data so that each row sums to 1
  species_table_normalized <- species_table / rowSums(species_table)
  rowSums(species_table_normalized)

  # Calculate Shannon Diversity Index for each plot
  plot_data_filtered$Shannon_Index <- diversity(species_table_normalized, index = "shannon")

  print(plot_data_filtered$Shannon_Index)

  plot_prefix <- sub("(_ang.*)$", "", plot)


  # Create a map with ggplot2
  shannon_plot <- ggplot(
    plot_data_filtered,
    aes(x = `Longitude (decimal degrees)`, y = `Latitude (decimal degrees)`, color = Shannon_Index)
  ) +
    geom_point(size = 3) +  # Use points to represent plot locations
    scale_color_viridis_c(option = "plasma", name = "Shannon Index") +  # Use a color scale
    theme_minimal() +  # Minimal theme for clean visuals
    labs(title = paste0("Shannon Diversity Index Map ", plot_prefix), x = "Longitude", y = "Latitude")

  print(shannon_plot)

  # Save the Pareto chart
  ggsave(
    shannon_diversity_ground_plot,
    shannon_plot,
    dpi = 300,
    width = 10,
    height = 6
  )

  # csv_file_path
  #
  # ggplot2::ggsave(Plot_File_Path, relative_abundance_plot, dpi = 300, width = 10, height = 6)

  # Create an interactive map with Leaflet
  leaflet(plot_data_filtered) %>%
    addTiles() %>%  # Add a base map
    addCircleMarkers(
      lng = ~ `Longitude (decimal degrees)`,
      lat = ~ `Latitude (decimal degrees)`,
      color = ~ colorNumeric(palette = "viridis", domain = plot_data_filtered$Shannon_Index)(Shannon_Index),
      radius = 5,
      fillOpacity = 0.8,
      popup = ~ paste0(
        "<b>Releve number:</b> ",
        `Releve number`,
        "<br>",
        "<b>Shannon Index:</b> ",
        round(Shannon_Index, 2)
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = colorNumeric(palette = "viridis", domain = plot_data_filtered$Shannon_Index),
      values = ~ Shannon_Index,
      title = "Shannon Index",
      opacity = 0.8
    )

  ##############################################################################
  #
  # Export Shannon diversity index as shapefile
  #
  ##############################################################################


  # Export Shapefile
  plot_data_export <- plot_data_filtered %>%
    dplyr::select(
      PlotLocation,
      `Releve number`,
      Year,
      Month,
      `Date (yyyymmdd)`,
      `Longitude (decimal degrees)`,
      `Latitude (decimal degrees)`,
      `Number of species`,
      Shannon_Index
    ) %>%
    dplyr::rename(
      ReleveNr = `Releve number`,
      Longitude = `Longitude (decimal degrees)`,
      Latitude = `Latitude (decimal degrees)`,
      SpeciesCnt = `Number of species`,
      ShannonIdx = Shannon_Index
    )

  # Convert to spatial data (sf)
  plot_sf <- st_as_sf(plot_data_export,
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)

  # Write shapefile
  st_write(plot_sf, output_shapefile, delete_dsn = TRUE)

  cat("Shapefile successfully saved at:", output_shapefile, "\n")




  ##############################################################################
  #
  # Plot PCoA
  #
  ##############################################################################

  # # Load required libraries
  # library(terra)     # For raster handling
  # library(ggplot2)   # For plotting
  # library(dplyr)     # For data manipulation
  #
  # # Load ENVI images as raster stacks
  # pcoa_raster <- rast(pcoa_image_path)        # Raster with PCoA bands
  # shannon_raster <- rast(shannon_img_path)  # Raster with Shannon index
  #
  # # Extract pixel values into a dataframe
  # pcoa_values <- as.data.frame(values(pcoa_raster))      # Convert PCoA bands to dataframe
  # shannon_values <- as.data.frame(values(shannon_raster)) # Convert Shannon index to dataframe
  #
  # # Ensure column names are meaningful
  # colnames(pcoa_values) <- c("PCoA1", "PCoA2", "PCoA3")
  # colnames(shannon_values) <- c("Shannon_Index")
  #
  # # Merge the dataframes
  # combined_data <- cbind(pcoa_values, shannon_values)
  #
  # # Remove NA values (optional but recommended)
  # combined_data <- combined_data %>% na.omit()
  #
  # # ==========================
  # # Classify Shannon Index into 4 categories
  # # ==========================
  # combined_data$Diversity_Category <- cut(
  #   combined_data$Shannon_Index,
  #   breaks = c(-Inf, 0.5, 1.5, 2.5, Inf),
  #   # Define category boundaries
  #   labels = c(
  #     "Low Vegetation",
  #     "Monospecific",
  #     "Medium Diversity",
  #     "High Diversity"
  #   )
  # )
  #
  # # Define matching sizes and colors for each category
  # diversity_styles <- data.frame(
  #   Diversity_Category = factor(
  #     c(
  #       "Low Vegetation",
  #       "Monospecific",
  #       "Medium Diversity",
  #       "High Diversity"
  #     ),
  #     levels = c(
  #       "Low Vegetation",
  #       "Monospecific",
  #       "Medium Diversity",
  #       "High Diversity"
  #     )
  #   ),
  #   Size = c(0.5, 1, 1.5, 2),
  #   # Smaller circle sizes
  #   Color = c("red", "orange", "green", "blue")  # Matching colors
  # )
  #
  # # Merge styles into main dataframe
  # combined_data <- left_join(combined_data, diversity_styles, by = "Diversity_Category")
  #
  # # Scatter Plot Function
  # scatter_plot <- function(x_col, y_col) {
  #   ggplot(
  #     combined_data,
  #     aes_string(
  #       x = x_col,
  #       y = y_col,
  #       size = "Size",
  #       color = "Diversity_Category"
  #     )
  #   ) +
  #     geom_point(alpha = 0.6) +  # Increased transparency for better visibility
  #     scale_size_identity() +  # Keep fixed sizes from dataframe
  #     scale_color_manual(values = setNames(
  #       diversity_styles$Color,
  #       diversity_styles$Diversity_Category
  #     )) +
  #     theme_minimal() +
  #     labs(
  #       title = paste(x_col, "vs", y_col),
  #       x = x_col,
  #       y = y_col,
  #       size = "Diversity Category",
  #       # Legend for size
  #       color = "Diversity Category"  # Legend for color
  #     )
  # }
  #
  # # Generate scatter plots
  # plot1 <- scatter_plot("PCoA1", "PCoA2")
  # plot2 <- scatter_plot("PCoA1", "PCoA3")
  # plot3 <- scatter_plot("PCoA2", "PCoA3")
  #
  # # Display plots
  # print(plot1)
  # print(plot2)
  # print(plot3)
  #
  # # Save plots
  # # Define file paths for saving
  # plot1_path <- file.path(diversity_ground_data_path,
  #                         paste0(csv_file_name, '_PCoA1_vs_PCoA2.png'))
  # plot2_path <- file.path(diversity_ground_data_path,
  #                         paste0(csv_file_name, '_PCoA1_vs_PCoA3.png'))
  # plot3_path <- file.path(diversity_ground_data_path,
  #                         paste0(csv_file_name, '_PCoA2_vs_PCoA3.png'))
  #
  # # Save the plots with high resolution
  # ggsave(
  #   plot1_path,
  #   plot = plot1,
  #   dpi = 300,
  #   width = 8,
  #   height = 6
  # )
  # ggsave(
  #   plot2_path,
  #   plot = plot2,
  #   dpi = 300,
  #   width = 8,
  #   height = 6
  # )
  # ggsave(
  #   plot3_path,
  #   plot = plot3,
  #   dpi = 300,
  #   width = 8,
  #   height = 6
  # )
  #
  # # Confirmation message
  # cat("Plots saved successfully!\n")


  ##############################################################################
  #
  # Relative abundance plot
  #
  ##############################################################################


  # Calculate total abundance for each species
  species_sums <- sort(colSums(species_table, na.rm = TRUE), decreasing = TRUE)

  # Calculate relative abundance
  total_abundance <- sum(species_sums)  # Total abundance across all species
  relative_abundance <- species_sums / total_abundance * 100  # Convert to percentages

  # Variance of each species across samples
  species_variance <- apply(species_table, 2, var, na.rm = TRUE)

  # Set threshold for significance (e.g., minimum relative abundance of 1%)
  threshold <- 1
  significant_species <- names(relative_abundance[relative_abundance >= threshold])
  insignificant_species <- names(relative_abundance[relative_abundance < threshold])

  # Calculate relative abundance
  relative_abundance_df <- data.frame(Species = names(relative_abundance),
                                      RelativeAbundance = relative_abundance)

  # Calculate the number of species above the threshold
  num_significant_species <- sum(relative_abundance >= threshold)

  # Function to round up to the nearest multiple of n
  round_up <- function(x, multiple) {
    ceiling(x / multiple) * multiple
  }

  nbclusters_calculated <- round_up(num_significant_species, 5)

  # Create the plot with the additional annotation
  relative_abundance_plot <- ggplot(relative_abundance_df,
                                    aes(x = reorder(Species, -RelativeAbundance), y = RelativeAbundance)) +
    geom_bar(
      stat = "identity",
      aes(fill = RelativeAbundance >= threshold),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "lightgray")) +
    geom_hline(
      yintercept = threshold,
      linetype = "dashed",
      color = "red",
      size = 1
    ) +
    annotate(
      "text",
      x = nrow(relative_abundance_df) / 2,
      y = max(relative_abundance) * 0.9,
      label = paste(
        num_significant_species,
        "species above",
        threshold,
        "% threshold"
      ),
      color = "blue",
      size = 5,
      angle = 0,
      hjust = 0.5
    ) +  # Annotation for number of significant species
    annotate(
      "text",
      x = nrow(relative_abundance_df) / 2,
      y = threshold + 0.5,
      label = paste("Threshold =", threshold, "%"),
      color = "red",
      size = 4,
      angle = 0,
      hjust = 0.5
    ) +
    labs(title = "Relative Abundance of Species", x = "Species", y = "Relative Abundance (%)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line.x = element_line(color = "black")
    )

  # Create the directory
  #if (!dir.exists(diversity_ground_data_path)) dir.create("species_analysis/plots", recursive = TRUE)


  print(relative_abundance_plot)

  rel_abundanca_plot_path <- paste0(diversity_ground_data_path,'/',csv_file_name,
                                    "_relative_abundance_plot.png")
  # Save the relative abundance plot
  ggsave(
    rel_abundanca_plot_path,
    relative_abundance_plot,
    dpi = 300,
    width = 10,
    height = 6
  )



  # Print summary
  cat("Total species:", length(species_sums), "\n")
  cat("Significant species (>= 1%):",
      length(significant_species),
      "\n")
  cat("Insignificant species (< 1%):",
      length(insignificant_species),
      "\n\n")

  cat("Significant Species:\n")
  print(significant_species)



  # Define threshold for cumulative percentage
  cumulative_threshold <- 90  # You can change this value to any percentage

  # Prepare the data for Pareto chart
  species_df <- data.frame(Species = names(species_sums), Abundance = species_sums)

  # Sort species by abundance in descending order and calculate cumulative percentage
  species_df <- species_df[order(-species_df$Abundance), ]
  species_df$Cumulative <- cumsum(species_df$Abundance) / sum(species_df$Abundance) * 100

  # Find the number of species required for the given cumulative threshold
  n_species_threshold <- which(species_df$Cumulative >= cumulative_threshold)[1]  # First species to exceed the threshold

  # Create Pareto chart
  pareto_plot <- ggplot(species_df, aes(x = reorder(Species, -Abundance), y = Abundance)) +
    # Bar chart
    geom_bar(stat = "identity", fill = "steelblue") +

    # Cumulative line
    geom_line(aes(
      y = (Cumulative / 100) * max(Abundance),
      group = 1
    ),
    color = "red",
    size = 1) +
    geom_point(aes(y = (Cumulative / 100) * max(Abundance)), color = "red", size = 2) +

    # Threshold line
    geom_hline(
      yintercept = (cumulative_threshold / 100) * max(species_df$Abundance),
      linetype = "dashed",
      color = "darkgreen",
      size = 1
    ) +

    # Annotate the number of species required for the threshold
    annotate(
      "text",
      x = n_species_threshold,
      y = (cumulative_threshold / 100) * max(species_df$Abundance) * 0.9,
      label = paste(
        n_species_threshold,
        "species for",
        cumulative_threshold,
        "%"
      ),
      color = "darkgreen",
      angle = 45,
      hjust = 1
    ) +

    # Customize y-axis with dual axes
    scale_y_continuous(name = "Abundance",
                       sec.axis = sec_axis(~ . / max(species_df$Abundance) * 100, name = "Cumulative Percentage")) +

    # Titles and labels
    labs(
      title = paste(
        "Pareto Chart of Species Abundance (",
        cumulative_threshold,
        "% Threshold)",
        sep = ""
      ),
      x = "Species",
      y = "Abundance"
    ) +

    # Rotate x-axis labels and add axis line
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line.x = element_line(color = "black")  # Add x-axis line
    )

  print(pareto_plot)

  pareto_plot_path <- paste0(diversity_ground_data_path,'/',csv_file_name,
                                    "_pareto_chart.png")

  # Save the Pareto chart
  ggsave(
    pareto_plot_path,
    pareto_plot,
    dpi = 300,
    width = 10,
    height = 6
  )


  ##############################################################################
  #
  # Spectral species count
  #
  ##############################################################################

  # Step 1: Load the hyperspectral image (replace with your actual file path)
  raster_image <- rast(spectral_species_img_path)
  dim(raster_image)  # Output will be (rows, cols, bands)

  # Check if there are any NoData values
  no_data_value <- 0
  raster_values <- values(raster_image)

  # Remove NoData values
  valid_values <- raster_values[raster_values != no_data_value]

  # Step 2: Extract the cluster values (assuming the cluster values are in the first band)
  # If your clusters are in a different band, change the band index (e.g., rast(image_path)[[1]])
  cluster_values <- values(raster_image)

  # Step 3: Omit pixels with a value of 0 (assumed to be background or no-data)
  cluster_values <- cluster_values[cluster_values != 0]

  # Step 4: Create a dataframe for plotting
  data <- data.frame(cluster_values)
  # cluster_values_sorted <- sort(cluster_values, decreasing = TRUE)
  # data_sorted <- data.frame(cluster_values = cluster_values_sorted)

  # Step 5: Plot the histogram using ggplot2
  cluster_values <- ggplot(data, aes(x = cluster_values)) +
    geom_histogram(
      binwidth = 1,
      fill = "blue",
      color = "black",
      alpha = 0.7
    ) +
    labs(
      title = paste0('Cluster count ', csv_file_name),
      x = "Cluster Number",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(text = element_text(size = 12))

  print(cluster_values)

  ggsave(
    spectral_species_count_plot,
    cluster_values,
    dpi = 300,
    width = 10,
    height = 6
  )


}


for (plot in plot_list) {
  result <- create_species_analysis(plot)
  cat(sprintf("Done with %s!", plot))
}
