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
  library(leaflet)


  # Load data
  shannon_img_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/result/ang20190706t235120_rfl_v2v2_img_rectified/SPCA/PCA/OutputPCA_30_PCs_selection_cut.tif'
  csv_path <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/ang20190706t235120rfl/clustertest/data/species_analysis/06_Utqiagvik_IBP_R_Input.csv"

  shannon_img <- terra::rast(shannon_img_path)

  # Replace -9 with NA
  plot_data <- read_csv(csv_path)
  plot_data_cleaned <- plot_data %>%
    mutate(across(everything(), ~ ifelse(. == -9, NA, .)))


  # Find the column position of "Number of species"
  number_species_pos <- which(names(plot_data_cleaned) == 'Number of species')

  # Dynamically select "Releve number", "Number of species", and all columns to the right of "Number of species"
  selected_data <- plot_data_cleaned %>%
    dplyr::select(
      'Releve number',
      'Number of species',
      all_of(names(plot_data_cleaned)[(number_species_pos + 1):ncol(plot_data_cleaned)])
    )

  # select only species list
  species_table <- selected_data %>%
    dplyr::select(-c('Releve number','Number of species'))

  shannon_div_idx_file_path <- "~/GitHub/UWW200_Master_Thesis_public/SpectralPatang/test_cropped_flightstrip/ang20190712t231624rfl/result/ang20190712t231624_rfl_v2v2_img_rectified_cut/SPCA/ALPHA/Shannon_10"

  # Read the CSV file with encoding


  # image_data <- terra::rast(shannon_div_idx_file_path)
  # image_values <- values(image_data)
  # image_values <- na.omit(image_values)
  # Extract coordinates
  #coordinates <- plot_data[, c("Longitude", "Latitude")]


  # Remove rows with all zeros


  species_table[is.na(species_table)] <- 0

  rowSums(species_table)

  # Normalize data so that each row sums to 1
  species_table_normalized <- species_table / rowSums(species_table)
  rowSums(species_table_normalized)

  # Calculate Shannon Diversity Index for each plot
  plot_data$Shannon_Index <- diversity(species_table_normalized, index = "shannon")


  # Create a map with ggplot2
  shannon_plot <- ggplot(plot_data, aes(x = Longitude, y = Latitude, color = Shannon_Index)) +
    geom_point(size = 3) +  # Use points to represent plot locations
    scale_color_viridis_c(option = "plasma", name = "Shannon Index") +  # Use a color scale
    theme_minimal() +  # Minimal theme for clean visuals
    labs(
      title = "Shannon Diversity Index Map",
      x = "Longitude",
      y = "Latitude"
    )

  print(shannon_plot)

  # csv_path
  #
  # ggplot2::ggsave(Plot_File_Path, relative_abundance_plot, dpi = 300, width = 10, height = 6)

  # Create an interactive map with Leaflet
  leaflet(plot_data) %>%
    addTiles() %>%  # Add a base map
    addCircleMarkers(
      lng = ~Longitude,
      lat = ~Latitude,
      color = ~colorNumeric(palette = "viridis", domain = plot_data$Shannon_Index)(Shannon_Index),
      radius = 5,
      fillOpacity = 0.8,
      popup = ~paste0(
        "<b>Releve number:</b> ", `Releve number`, "<br>",
        "<b>Shannon Index:</b> ", round(Shannon_Index, 2)
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = colorNumeric(palette = "viridis", domain = plot_data$Shannon_Index),
      values = ~Shannon_Index,
      title = "Shannon Index",
      opacity = 0.8
    )























library(leaflet)
library(terra)
library(dplyr)

# Load the raster data (ensure the path is correct)
shannon_img <- terra::rast(shannon_img_path)

plot(shannon_img, main = "Hyperspectral Image")

# Convert rasters to data frames
shannon_df <- as.data.frame(shannon_img,na.rm = TRUE)


# Define the color palette for Shannon Index, ensuring the domain is set correctly
shannon_palette <- colorNumeric(
  palette = "viridis",
  domain = range(plot_data$Shannon_Index, na.rm = TRUE),
  na.color = "gray"  # Optionally handle NA values
)

# Create the interactive map
leaflet() %>%
  addTiles() %>%  # Add a base map
  # Add the raster image (Shannon Index) as a background
  addRasterImage(shannon_img, colors = shannon_palette, opacity = 0.6) %>%
  # Add the plot locations (points) with Shannon Index color
  addCircleMarkers(
    data = plot_data,
    lng = ~Longitude,
    lat = ~Latitude,
    color = ~shannon_palette(Shannon_Index),  # Map color based on Shannon_Index
    radius = 5,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Releve number:</b> ", 'Releve number', "<br>",
      "<b>Shannon Index:</b> ", round(Shannon_Index, 2)
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = shannon_palette,
    values = ~Shannon_Index,
    title = "Shannon Index",
    opacity = 0.8
  )






























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
