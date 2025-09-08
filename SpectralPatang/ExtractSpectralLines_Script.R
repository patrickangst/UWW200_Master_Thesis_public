## Patrick Angst
## Master thesis
## 11.07.2025
## This script extracts the spectral / reflectance values for every plot location
## and plots it grouped by plant community and habitat type.

# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(viridis)
library(lme4)
library(lmerTest)
library(writexl)
library(readxl)
library(spdep)
library(units)
library(ape)
library(performance)
library(DHARMa)
library(see)

# testsite_name <- 'AN_TJ_1'
# x_axis_intervall <- 50
# pixel_buffer <- 5 # Buffer in meters around the coordinates to calculate the mean reflectance value


extract_spectral_signature <- function(testsite_name,
                                       x_axis_intervall = 50,
                                       pixel_buffer = 5) {
  working_folder_base_path <- file.path('data',
                                        'MasterThesis',
                                        'final_hs_data_folder',
                                        testsite_name)

  img_file_folder_path <- file.path(working_folder_base_path, 'image_rectified')
  spectral_signature_folder_path <- file.path(working_folder_base_path, 'spectral_signature')

  # Create necessary folders if they do not exist
  if (!dir.exists(spectral_signature_folder_path))
    dir.create(spectral_signature_folder_path)

  ground_data_file_path <- file.path('data',
                                     'MasterThesis',
                                     'ground_data',
                                     'All_plots_Desktop.xlsx')

  cluster_assignement_file_path <- 'data/MasterThesis/07_Testsite_Metrics/Cluster_Assignement.xlsx'

  # Load cluster assignments from Excel
  cluster_assignments <- readxl::read_excel(path = cluster_assignement_file_path)

  cluster_assignments <- cluster_assignments %>%
    select(PlotID, Cluster)

  # List all files in the folder
  files <- list.files(img_file_folder_path, full.names = TRUE)

  # Filter files without an extension
  file_without_ext <- files[!grepl("\\.[a-zA-Z0-9]+$", basename(files))]

  # Check if exactly one file without extension exists
  if (length(file_without_ext) != 1) {
    stop("Either no or multiple files without extensions found in the folder.")
  }

  hyperspectral <- rast(file_without_ext)

  # Load ground data
  ground_data_df <- read_excel(ground_data_file_path, sheet = 'Turboveg export selection trans') %>%
    as.data.frame() %>%
    filter(Testsite == testsite_name)


  ground_data_subset_df <- ground_data_df %>%
    #mutate(Testsite = paste0(`Table number`, '_', Testsite)) %>%
    mutate(PlotID = paste0(`Table number`, '_', Testsite)) %>%
    select(PlotID, Subzone, Longitude, Latitude, `Habitat Type`)


  # Convert ground data to an sf object
  ground_data_sf <- st_as_sf(
    ground_data_subset_df,
    coords = c("Longitude", "Latitude"),
    crs = 4326  # WGS84: most common CRS for GPS coordinates
  )

  # Reproject to match raster CRS
  ground_data_proj <- st_transform(ground_data_sf, crs(hyperspectral))

  # Extract mean of pixel values within a 5-meter buffer around each point/polygon
  pixel_values <- terra::extract(
    hyperspectral,
    vect(ground_data_proj),
    # Convert to SpatVector
    buffer = pixel_buffer,
    # buffer size in map units (e.g., meters)
    fun = mean,
    # function to apply (mean, median, etc.)
    na.rm = TRUE,
    # ignore NA values
    df = TRUE             # return as data frame
  )

  # Combine data with original df
  ground_data_pixel_values_combined <- bind_cols(ground_data_proj, pixel_values)

  # Select relevant columns and transform to df
  ground_data_pixel_values_cleaned <- ground_data_pixel_values_combined %>%
    select(-c(ID)) %>%
    st_drop_geometry() %>%  # Completely remove spatial attributes
    as.data.frame()         # Convert to a standard data frame


  # Identify spectral band columns (exclude metadata)
  band_columns <- setdiff(names(ground_data_pixel_values_cleaned),
                          c('PlotID', 'Subzone', 'Habitat Type'))

  rounded_wavelength <- str_extract(band_columns, "\\d+\\.\\d+") %>%
    as.numeric() %>%
    round()

  band_info <- data.frame(BandNr = seq_along(rounded_wavelength),
                          Wavelength = rounded_wavelength)

  # Rename band columns sequentially
  names(ground_data_pixel_values_cleaned)[match(band_columns, names(ground_data_pixel_values_cleaned))] <- paste0("Band ", seq_along(band_columns))


  # Convert spectral data into long format for plotting
  ground_data_pixel_values_cleaned_long <- ground_data_pixel_values_cleaned %>%
    pivot_longer(-c(PlotID, Subzone, `Habitat Type`),
                 names_to = "Band",
                 values_to = "Reflectance") %>%
    mutate(BandNr = as.numeric(str_extract(Band, "\\d+"))) %>%
    mutate(Reflectance = Reflectance) %>%
    left_join(band_info, by = "BandNr")  # Adds Wavelength column

  # save as csv
  csv_file_path <- file.path(spectral_signature_folder_path,
                             paste0(testsite_name, '_banddata.csv'))
  write.csv(ground_data_pixel_values_cleaned_long,
            file = csv_file_path,
            row.names = FALSE)
  message(paste("Saved CSV: ", testsite_name, '.csv'))

  ground_data_pixel_values_cleaned_long <- ground_data_pixel_values_cleaned_long %>%
    left_join(cluster_assignments, by = "PlotID")

  # Filter out certain wavelength bands
  ground_data_pixel_values_cleaned_long <- ground_data_pixel_values_cleaned_long %>%
    mutate(
      Reflectance = case_when(
        Wavelength >= 0 & Wavelength <= 442 ~ NA_real_,
        Wavelength >= 1329 & Wavelength <= 1429 ~ NA_real_,
        Wavelength >= 1800 & Wavelength <= 1980 ~ NA_real_,
        Wavelength >= 2456 ~ NA_real_,
        TRUE ~ Reflectance
      )
    )


  # Ensure wavelengths are sorted correctly
  unique_wavelengths_signature <- sort(unique(ground_data_pixel_values_cleaned_long$Wavelength))

  # Generate spectral signature plot

  # Define spectral bands
  band_data <- data.frame(
    Lower = c(375, 450, 485, 500, 565, 590, 625, 740, 1100),
    Upper = c(450, 485, 500, 565, 590, 625, 740, 1100, 2500),
    FillCategory = c(
      "Violet",
      "Blue",
      "Cyan",
      "Green",
      "Yellow",
      "Orange",
      "Red",
      "Near-Infrared",
      "Shortwave-Infrared"
    )
  )

  # Generate spectral signature plot with bands
  signature_plot_habitat <- ggplot() +
    # 1. Background spectral regions
    geom_rect(
      data = band_data,
      aes(
        xmin = Lower,
        xmax = Upper,
        ymin = -Inf,
        ymax = Inf,
        fill = FillCategory
      ),
      alpha = 0.2,
      inherit.aes = FALSE
    ) +

    # 2. Spectral reflectance lines
    geom_line(
      data = ground_data_pixel_values_cleaned_long,
      aes(
        x = Wavelength,
        y = Reflectance,
        color = `Habitat Type`,
        group = PlotID
      ),
      na.rm = FALSE
    ) +

    # 3. Plot styling
    labs(
      x = "Wavelength [nm]",
      y = "Reflectance",
      title = paste0("Spectral signatures per habitat type ", testsite_name)
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(
      min(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
      max(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
      by = x_axis_intervall
    )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

    # 4. Color scales
    scale_color_viridis(discrete = TRUE, name = "Habitat Type",
                        guide = guide_legend(order = 1)) +
    scale_fill_manual(
      name = "Spectral Region",
      values = c(
        "Violet" = "violet",
        "Blue" = "blue",
        "Cyan" = "cyan",
        "Green" = "green",
        "Yellow" = "yellow",
        "Orange" = "darkorange",
        "Red" = "red",
        "Near-Infrared" = "darkgrey",
        "Shortwave-Infrared" = "indianred",
        guide = guide_legend(order = 2)
      )
    )

  # Display the plot
  print(signature_plot_habitat)

  habitat_type_spectral_signature_png_path <- file.path(
    spectral_signature_folder_path,
    paste0(testsite_name, '_signature_plot_habitat.png')
  )

  # Save plot
  ggsave(
    habitat_type_spectral_signature_png_path,
    signature_plot_habitat,
    width = 10,
    height = 8,
    dpi = 150
  )
  message('Saved plot signature_plot_habitat')


  signature_plot_plant_community <- ggplot() +
    # 1. Background spectral regions
    geom_rect(
      data = band_data,
      aes(
        xmin = Lower,
        xmax = Upper,
        ymin = -Inf,
        ymax = Inf,
        fill = FillCategory
      ),
      alpha = 0.2,
      inherit.aes = FALSE
    ) +

    # 2. Spectral reflectance lines
    geom_line(
      data = ground_data_pixel_values_cleaned_long,
      aes(
        x = Wavelength,
        y = Reflectance,
        color = Cluster,
        group = PlotID
      ),
      na.rm = FALSE
    ) +

    # 3. Plot styling
    labs(
      x = "Wavelength [nm]",
      y = "Reflectance",
      title = paste0("Spectral signatures per plant community ", testsite_name)
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(
      min(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
      max(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
      by = x_axis_intervall
    )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

    # 4. Color scales
    scale_color_viridis(discrete = TRUE, name = "Plant Community Cluster",
                        guide = guide_legend(order = 1)) +
    scale_fill_manual(
      name = "Spectral Region",
      values = c(
        "Violet" = "violet",
        "Blue" = "blue",
        "Cyan" = "cyan",
        "Green" = "green",
        "Yellow" = "yellow",
        "Orange" = "darkorange",
        "Red" = "red",
        "Near-Infrared" = "darkgrey",
        "Shortwave-Infrared" = "indianred",
        guide = guide_legend(order = 2)
      )
    )

  # Display the plot
  print(signature_plot_plant_community)

  plant_community_spectral_signature_png_path <- file.path(
    spectral_signature_folder_path,
    paste0(testsite_name, '_signature_plot_plant_community.png')
  )

  # Save plot
  ggsave(
    plant_community_spectral_signature_png_path,
    signature_plot_plant_community,
    width = 10,
    height = 8,
    dpi = 150
  )
  message('Saved plot signature_plot_plant_community')


  # Generate mean spectral signature plot
  ground_data_pixel_values_cleaned_long_mean_HT <- ground_data_pixel_values_cleaned_long %>%
    group_by(`Habitat Type`, Wavelength) %>%
    summarize(
      MeanReflectance = mean(Reflectance, na.rm = TRUE),
      MinReflectance = min(Reflectance, na.rm = TRUE),
      MaxReflectance = max(Reflectance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      MeanReflectance = ifelse(is.infinite(MeanReflectance), NaN, MeanReflectance),
      MinReflectance = ifelse(is.infinite(MinReflectance), NaN, MinReflectance),
      MaxReflectance = ifelse(is.infinite(MaxReflectance), NaN, MaxReflectance)
    )


  # Generate mean spectral signature plot with bands
  mean_signature_plot_habitat <- ggplot() +
    # 1. Background spectral regions
    geom_rect(
      data = band_data,
      aes(
        xmin = Lower,
        xmax = Upper,
        ymin = -Inf,
        ymax = Inf,
        fill = FillCategory
      ),
      alpha = 0.2,
      inherit.aes = FALSE
    ) +

    # 2. Ribbon and line for mean reflectance
    geom_ribbon(
      data = ground_data_pixel_values_cleaned_long_mean_HT,
      aes(
        x = Wavelength,
        ymin = MinReflectance,
        ymax = MaxReflectance,
        fill = `Habitat Type`,
        group = `Habitat Type`
      ),
      alpha = 0.2,
      color = NA  # This removes the outline around the ribbon
    ) +
    geom_line(
      data = ground_data_pixel_values_cleaned_long_mean_HT,
      aes(
        x = Wavelength,
        y = MeanReflectance,
        color = `Habitat Type`,
        group = `Habitat Type`
      ),
      linewidth = 0.45
    ) +

    # 3. Labels and theme
    labs(
      x = "Wavelength [nm]",
      y = "Mean Reflectance",
      title = paste0("Mean spectral signatures per habitat type ", testsite_name)
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(
      min(
        ground_data_pixel_values_cleaned_long_mean_HT$Wavelength,
        na.rm = TRUE
      ),
      max(
        ground_data_pixel_values_cleaned_long_mean_HT$Wavelength,
        na.rm = TRUE
      ),
      by = x_axis_intervall
    )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

    # 4. Color scales
    scale_color_viridis(discrete = TRUE, name = "Habitat Type",
                        guide = guide_legend(order = 1)) +
    scale_fill_manual(
      name = "Spectral Region",
      values = c(
        "Violet" = "violet",
        "Blue" = "blue",
        "Cyan" = "cyan",
        "Green" = "green",
        "Yellow" = "yellow",
        "Orange" = "darkorange",
        "Red" = "red",
        "Near-Infrared" = "darkgrey",
        "Shortwave-Infrared" = "indianred"
      ),
      guide = guide_legend(order = 2)
    )

  # Display the plot
  print(mean_signature_plot_habitat)

  mean_habitat_type_spectral_signature_png_path <- file.path(
    spectral_signature_folder_path,
    paste0(testsite_name, '_mean_signature_plot_habitat.png')
  )

  # Save plot --> todo
  ggsave(
    mean_habitat_type_spectral_signature_png_path,
    mean_signature_plot_habitat,
    width = 10,
    height = 8,
    dpi = 150
  )
  message('Saved plot mean_signature_plot_habitat')


  # Generate mean spectral signature plot
  ground_data_pixel_values_cleaned_long_mean_PC <- ground_data_pixel_values_cleaned_long %>%
    group_by(Cluster, Wavelength) %>%
    summarize(
      MeanReflectance = mean(Reflectance, na.rm = TRUE),
      MinReflectance = min(Reflectance, na.rm = TRUE),
      MaxReflectance = max(Reflectance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      MeanReflectance = ifelse(is.infinite(MeanReflectance), NaN, MeanReflectance),
      MinReflectance = ifelse(is.infinite(MinReflectance), NaN, MinReflectance),
      MaxReflectance = ifelse(is.infinite(MaxReflectance), NaN, MaxReflectance)
    )

  # Generate mean spectral signature plot with bands
  mean_signature_plot_plant_community <- ggplot() +
    # 1. Background spectral regions
    geom_rect(
      data = band_data,
      aes(
        xmin = Lower,
        xmax = Upper,
        ymin = -Inf,
        ymax = Inf,
        fill = FillCategory
      ),
      alpha = 0.2,
      inherit.aes = FALSE
    ) +

    # 2. Ribbon and line for mean reflectance
    geom_ribbon(
      data = ground_data_pixel_values_cleaned_long_mean_PC,
      aes(
        x = Wavelength,
        ymin = MinReflectance,
        ymax = MaxReflectance,
        fill = Cluster,
        group = Cluster
      ),
      alpha = 0.2,
      color = NA  # This removes the outline around the ribbon
    ) +
    geom_line(
      data = ground_data_pixel_values_cleaned_long_mean_PC,
      aes(
        x = Wavelength,
        y = MeanReflectance,
        color = Cluster,
        group = Cluster
      ),
      linewidth = 0.45
    ) +

    # 3. Labels and themes
    labs(
      x = "Wavelength [nm]",
      y = "Mean Reflectance",
      title = paste0("Mean spectral signatures per plant community ", testsite_name)
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(
      min(
        ground_data_pixel_values_cleaned_long_mean_PC$Wavelength,
        na.rm = TRUE
      ),
      max(
        ground_data_pixel_values_cleaned_long_mean_PC$Wavelength,
        na.rm = TRUE
      ),
      by = x_axis_intervall
    )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

    # 4. Color scales
    scale_color_viridis(discrete = TRUE,
                        name = "Plant Community Cluster",
                        guide = guide_legend(order = 1)) +
    scale_fill_manual(
      name = "Spectral Region",
      values = c(
        "Violet" = "violet",
        "Blue" = "blue",
        "Cyan" = "cyan",
        "Green" = "green",
        "Yellow" = "yellow",
        "Orange" = "darkorange",
        "Red" = "red",
        "Near-Infrared" = "darkgrey",
        "Shortwave-Infrared" = "indianred"
      ),
      guide = guide_legend(order = 2)
    )

  # Display the plot
  print(mean_signature_plot_plant_community)

  mean_plant_community_spectral_signature_png_path <- file.path(
    spectral_signature_folder_path,
    paste0(testsite_name, '_mean_signature_plot_plant_community.png')
  )

  # Save plot
  ggsave(
    mean_plant_community_spectral_signature_png_path,
    mean_signature_plot_plant_community,
    width = 10,
    height = 8,
    dpi = 150
  )
  message('Saved plot mean_signature_plot_plant_community')

}
