# Clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(tidyverse)
library(viridis)
library(Hmisc)

folder_names <- list.dirs(
  "data/MasterThesis/final_hs_data_folder",
  full.names = TRUE,
  recursive = FALSE
)

x_axis_intervall <- 50

combined_df <- data.frame()  # Initialize empty data frame

for (name in folder_names) {
  testsite_name <- basename(name)
  csv_folder_path <- file.path(name, "spectral_signature")
  csv_file_path <- file.path(csv_folder_path, paste0(testsite_name, "_banddata.csv"))

  if (file.exists(csv_file_path)) {
    temp_df <- read.csv(csv_file_path)
    temp_df$TestSite <- testsite_name  # Add site identifier for traceability
    combined_df <- dplyr::bind_rows(combined_df, temp_df)
  } else {
    message(paste("Missing file:", csv_file_path))
  }
}

ground_data_pixel_values_cleaned_long <- combined_df

# Filter out certain wavelength bands
ground_data_pixel_values_cleaned_long <- ground_data_pixel_values_cleaned_long %>%
  mutate(
    Reflectance = case_when(
      Wavelength >= 0 & Wavelength <= 442 ~ NA_real_,
      Wavelength >= 1310 & Wavelength <= 1429 ~ NA_real_,
      Wavelength >= 1800 & Wavelength <= 1980 ~ NA_real_,
      Wavelength >= 2456 ~ NA_real_,
      TRUE ~ Reflectance
    )
  )

# Reduce Habitat.Type to numeric codes
ground_data_pixel_values_cleaned_long_modified <- ground_data_pixel_values_cleaned_long %>%
  mutate(Habitat.Type = str_extract(Habitat.Type, "^\\d+\\.\\d+"))

# Step 1: Get unique Band-Wavelength mapping
band_wavelength_lookup <- ground_data_pixel_values_cleaned_long_modified %>%
  select(Band, Wavelength) %>%
  distinct()

# Step 2: Group by Band and Habitat.Type, compute mean/min/max
filtered_data_grouped <- ground_data_pixel_values_cleaned_long_modified %>%
  group_by(Band, Habitat.Type) %>%
  summarise(
    mean_reflectance = mean(Reflectance, na.rm = TRUE),
    min_reflectance = min(Reflectance, na.rm = TRUE),
    max_reflectance = max(Reflectance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_reflectance = ifelse(is.infinite(mean_reflectance), NaN, mean_reflectance),
    min_reflectance = ifelse(is.infinite(min_reflectance), NaN, min_reflectance),
    max_reflectance = ifelse(is.infinite(max_reflectance), NaN, max_reflectance)
  ) %>%
  left_join(band_wavelength_lookup, by = "Band") %>%
  arrange(Wavelength)

# Define spectral bands
band_data <- data.frame(
  Lower = c(375, 450, 485, 500, 565, 590, 625, 740, 1100),
  Upper = c(450, 485, 500, 565, 590, 625, 740, 1100, 2500),
  FillCategory = c(
    "Violet", "Blue", "Cyan", "Green", "Yellow", "Orange",
    "Red", "Near-Infrared", "Shortwave-Infrared"
  )
)

# Generate spectral signature plot with min-max ribbon
filtered_data_plot <- ggplot() +
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

  # 2. Min-max ribbon
  geom_ribbon(
    data = filtered_data_grouped,
    aes(
      x = Wavelength,
      ymin = min_reflectance,
      ymax = max_reflectance,
      fill = Habitat.Type,
      group = Habitat.Type
    ),
    alpha = 0.2,
    show.legend = FALSE
  ) +

  # 3. Mean reflectance line
  geom_line(
    data = filtered_data_grouped,
    aes(
      x = Wavelength,
      y = mean_reflectance,
      color = Habitat.Type,
      # linetype = Habitat.Type
    ),
    # linewidth = 0.45
    linewidth = 0.75
  ) +
  # 4. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = "Mean spectral signatures per habitat type"
  ) +
  theme_minimal() +
  # theme(
  #   axis.text.x = element_text(angle = 45, hjust = 1),
  #   legend.position = "bottom", # Move legend to the bottom
  #   legend.box = "vertical"     # Stack the two legends vertically
  # ) +
  scale_x_continuous(breaks = seq(
    min(filtered_data_grouped$Wavelength, na.rm = TRUE),
    max(filtered_data_grouped$Wavelength, na.rm = TRUE),
    by = x_axis_intervall + 30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 5. Color scales
  # scale_color_viridis_d(
  #   name = "Habitat Type",
  #   guide = guide_legend(order = 1)
  # ) +

  scale_color_manual(
    name = "Habitat Type",
    values = c(
      "1.02" = "#E41A1C",  # red
      "1.03" = "#377EB8",  # blue
      "4.01" = "#4DAF4A",  # green
      "5.01" = "#984EA3",  # purple
      "5.05" = "#FF7F00",  # orange
      "5.06" = "#A65628",  # brown
      "5.07" = "#FFFF33",  # brown
      "5.09" = "#F781BF"   # pink
    ),
    labels = c(
      "1.02" = "Dry dwarf shrubs and graminoids",
      "1.03" = "Acidic dwarf-shrub-heath",
      "4.01" = "Boreal forest enclaves",
      "5.01" = "Costal habitat",
      "5.05" = "Fresh water",
      "5.06" = "Mires",
      "5.07" = "Riparian shrublands and forests",
      "5.09" = "Ruderal vegetation"
    ),
    # labels = c(
    #   "1.02" = "1.02", "1.03" = "1.03", "4.01" = "4.01",
    #   "5.01" = "5.01", "5.05" = "5.05", "5.06" = "5.06", "5.07" = "5.07", "5.09" = "5.09"
    # ),
    guide = guide_legend(order = 1)
  ) +

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
  ) +
  guides(fill = guide_legend(order = 1))

# Display the plot
print(filtered_data_plot)

# Save plot
signature_plot_by_habitat_type_mean_png_path <- file.path(
  'data/MasterThesis/all_testsite_signatures',
  'All_testsites_signature_plot_by_habitat_type_mean.png'
)

ggsave(
  signature_plot_by_habitat_type_mean_png_path,
  filtered_data_plot,
  width = 14,
  height = 8,
  dpi = 300
)

# ggsave(
#   signature_plot_by_habitat_type_mean_png_path,
#   filtered_data_plot,
#   width = 12,
#   height = 9,
#   dpi = 300
# )




# Generate spectral signature plot with min-max ribbon
filtered_data_plot_numbered <- ggplot() +
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

  # 2. Min-max ribbon
  geom_ribbon(
    data = filtered_data_grouped,
    aes(
      x = Wavelength,
      ymin = min_reflectance,
      ymax = max_reflectance,
      fill = Habitat.Type,
      group = Habitat.Type
    ),
    alpha = 0.2,
    show.legend = FALSE
  ) +

  # 3. Mean reflectance line
  geom_line(
    data = filtered_data_grouped,
    aes(
      x = Wavelength,
      y = mean_reflectance,
      color = Habitat.Type,
      # linetype = Habitat.Type
    ),
    # linewidth = 0.45
    linewidth = 0.75
  ) +
  # 4. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = "Mean spectral signatures per habitat type"
  ) +
  theme_minimal() +
  # theme(
  #   axis.text.x = element_text(angle = 45, hjust = 1),
  #   legend.position = "bottom", # Move legend to the bottom
  #   legend.box = "vertical"     # Stack the two legends vertically
  # ) +
  scale_x_continuous(breaks = seq(
    min(filtered_data_grouped$Wavelength, na.rm = TRUE),
    max(filtered_data_grouped$Wavelength, na.rm = TRUE),
    by = x_axis_intervall + 30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 5. Color scales
  # scale_color_viridis_d(
  #   name = "Habitat Type",
  #   guide = guide_legend(order = 1)
  # ) +

  scale_color_manual(
    name = "Habitat Type",
    values = c(
      "1.02" = "#E41A1C",  # red
      "1.03" = "#377EB8",  # blue
      "4.01" = "#4DAF4A",  # green
      "5.01" = "#984EA3",  # purple
      "5.05" = "#FF7F00",  # orange
      "5.06" = "#A65628",  # brown
      "5.07" = "#FFFF33",  # brown
      "5.09" = "#F781BF"   # pink
    ),
    labels = c(
      "1.02" = "1.02", "1.03" = "1.03", "4.01" = "4.01",
      "5.01" = "5.01", "5.05" = "5.05", "5.06" = "5.06", "5.07" = "5.07", "5.09" = "5.09"
    ),
    guide = guide_legend(order = 1)
  ) +

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
  ) +
  guides(fill = guide_legend(order = 1))

# Display the plot
print(filtered_data_plot_numbered)

# Save plot
signature_plot_by_habitat_type_mean_numbered_png_path <- file.path(
  'data/MasterThesis/all_testsite_signatures',
  'All_testsites_signature_plot_by_habitat_type_mean_numbered.png'
)

ggsave(
  signature_plot_by_habitat_type_mean_numbered_png_path,
  filtered_data_plot_numbered,
  width = 14,
  height = 8,
  dpi = 300
)

################################################################################
################################################################################
# Normalized plot
################################################################################
################################################################################

# Normalize reflectance per PlotID using L2 norm
reflectance_norm <- ground_data_pixel_values_cleaned_long_modified %>%
  group_by(PlotID, Habitat.Type, Band) %>%
  summarise(MeanReflectance = mean(Reflectance, na.rm = TRUE), .groups = "drop") %>%
  group_by(PlotID, Habitat.Type) %>%
  mutate(
    l2_norm = sqrt(sum(MeanReflectance^2, na.rm = TRUE)),
    NormReflectance = ifelse(l2_norm == 0, 0, MeanReflectance / l2_norm)
  ) %>%
  ungroup()

# Get Band-Wavelength mapping
band_wavelength_lookup <- ground_data_pixel_values_cleaned_long_modified %>%
  select(Band, Wavelength) %>%
  distinct()

# Compute mean normalized reflectance per Band and Habitat.Type
normalized_summary <- reflectance_norm %>%
  group_by(Band, Habitat.Type) %>%
  summarise(
    mean_norm_reflectance = mean(NormReflectance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(band_wavelength_lookup, by = "Band") %>%
  arrange(Wavelength)

normalized_plot <- ggplot() +
  # Background spectral regions
  geom_rect(
    data = band_data,
    aes(xmin = Lower, xmax = Upper, ymin = -Inf, ymax = Inf, fill = FillCategory),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +

  # Normalized reflectance lines
  geom_line(
    data = normalized_summary,
    aes(x = Wavelength, y = mean_norm_reflectance, color = Habitat.Type),
    linewidth = 0.75
  ) +

  # Styling
  labs(
    x = "Wavelength [nm]",
    y = "Normalized Reflectance",
    title = "Normalized spectral signatures per habitat type"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(normalized_summary$Wavelength, na.rm = TRUE),
    max(normalized_summary$Wavelength, na.rm = TRUE),
    by = x_axis_intervall + 30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # Color scale (same as before)
  scale_color_manual(
    name = "Habitat Type",
    values = c(
      "1.02" = "#E41A1C", "1.03" = "#377EB8", "4.01" = "#4DAF4A",
      "5.01" = "#984EA3", "5.05" = "#FF7F00", "5.06" = "#A65628","5.07" = "#FFFF33","5.09" = "#F781BF"
    ),
    # labels = c(
    #   "1.02" = "1.02", "1.03" = "1.03", "4.01" = "4.01",
    #   "5.01" = "5.01", "5.05" = "5.05", "5.06" = "5.06", "5.07" = "5.07", "5.09" = "5.09"
    # ),
    labels = c(
      "1.02" = "Dry dwarf shrubs and graminoids",
      "1.03" = "Acidic dwarf-shrub-heath",
      "4.01" = "Boreal forest enclaves",
      "5.01" = "Costal habitat",
      "5.05" = "Fresh water",
      "5.06" = "Mires",
      "5.07" = "Riparian shrublands and forests",
      "5.09" = "Ruderal vegetation"
    ),
    guide = guide_legend(order = 1)
  ) +
  scale_fill_manual(
    name = "Spectral Region",
    values = c(
      "Violet" = "violet", "Blue" = "blue", "Cyan" = "cyan", "Green" = "green",
      "Yellow" = "yellow", "Orange" = "darkorange", "Red" = "red",
      "Near-Infrared" = "darkgrey", "Shortwave-Infrared" = "indianred"
    ),
    guide = guide_legend(order = 2)
  ) +
  guides(fill = guide_legend(order = 1))

# Display normalized plot
print(normalized_plot)

# Save normalized plot
normalized_plot_path <- file.path(
  'data/MasterThesis/all_testsite_signatures',
  'All_testsites_signature_plot_by_habitat_type_normalized.png'
)

ggsave(
  normalized_plot_path,
  normalized_plot,
  width = 12,
  height = 9,
  dpi = 300
)



#########
#########
#########

habitat_labels <- c(
  "1.02" = "Dry dwarf shrubs and graminoids",
  "1.03" = "Acidic dwarf-shrub-heath",
  "4.01" = "Boreal forest enclaves",
  "5.01" = "Coastal habitat",
  "5.05" = "Fresh water",
  "5.06" = "Mires",
  "5.07" = "Riparian shrublands and forests",
  "5.09" = "Ruderal vegetation"
)


ground_data_pixel_values_cleaned_long_modified_selection <- ground_data_pixel_values_cleaned_long_modified

# Generate spectral signature plot with bands, faceted by Habitat.Type
signature_plot_by_habitat_type <- ggplot() +
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
    alpha = 0.2
    # inherit.aes = FALSE
  ) +

  # 2. Spectral reflectance lines
  geom_line(
    data = ground_data_pixel_values_cleaned_long_modified_selection,
    aes(
      x = Wavelength,
      y = Reflectance,
      color = Habitat.Type,
      # Still color by Habitat.Type if you want lines to be different colors within each facet
      group = PlotID # Group by PlotID for individual lines
    ),
    na.rm = FALSE,
    linewidth = 0.45
  ) +

  # *** Add this line to create separate plots for each Habitat.Type ***
  facet_wrap(
    ~ Habitat.Type,
    scales = "fixed",
    ncol = 2,
    labeller = labeller(Habitat.Type = habitat_labels)
  ) +

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type") # Title might be simpler now
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(
      ground_data_pixel_values_cleaned_long_modified_selection$Wavelength,
      na.rm = TRUE
    ),
    max(
      ground_data_pixel_values_cleaned_long_modified_selection$Wavelength,
      na.rm = TRUE
    ),
    by = x_axis_intervall + 30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  # Color scale (same as before)
  scale_color_manual(
    name = "Habitat Type",
    values = c(
      "1.02" = "#E41A1C", "1.03" = "#377EB8", "4.01" = "#4DAF4A",
      "5.01" = "#984EA3", "5.05" = "#FF7F00", "5.06" = "#A65628","5.07" = "#FFFF33","5.09" = "#F781BF"
    ),
    # labels = c(
    #   "1.02" = "1.02", "1.03" = "1.03", "4.01" = "4.01",
    #   "5.01" = "5.01", "5.05" = "5.05", "5.06" = "5.06", "5.07" = "5.07", "5.09" = "5.09"
    # ),
    labels = c(
      "1.02" = "Dry dwarf shrubs and graminoids",
      "1.03" = "Acidic dwarf-shrub-heath",
      "4.01" = "Boreal forest enclaves",
      "5.01" = "Costal habitat",
      "5.05" = "Fresh water",
      "5.06" = "Mires",
      "5.07" = "Riparian shrublands and forests",
      "5.09" = "Ruderal vegetation"
    ),
    guide = guide_legend(order = 1)
    # guide = guide_legend(order = 2)
  ) +
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
  guide = guide_legend(order = 2))
  # Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
  # guides(fill = guide_legend(order = 1))


# Display the plot
print(signature_plot_by_habitat_type)

signature_plot_by_habitat_type_png_path <- file.path(
  'data/MasterThesis/all_testsite_signatures',
  'All_testsites_signature_plot_by_habitat_type.png'
)

# Save plot
ggsave(
  signature_plot_by_habitat_type_png_path,
  signature_plot_by_habitat_type,
  width = 12,
  height = 9,
  dpi = 300
)


similarity <- ground_data_pixel_values_cleaned_long_modified_selection %>%
  group_by(Habitat.Type) %>%
  group_split() %>%   # one df per habitat
  map_df(function(df) {
    wide <- df %>%
      select(Wavelength, PlotID, Reflectance) %>%
      pivot_wider(names_from = PlotID, values_from = Reflectance) %>%
      select(-Wavelength)

    # correlation matrix (only numeric)
    corr_mat <- cor(wide, use = "pairwise.complete.obs")

    # convert to long format (pairs)
    corr_df <- as.data.frame(corr_mat) %>%
      rownames_to_column("PlotID") %>%
      pivot_longer(-PlotID, names_to = "OtherPlot", values_to = "correlation") %>%
      filter(PlotID < OtherPlot)

    tibble(
      Habitat.Type = unique(df$Habitat.Type),
      mean_correlation = mean(corr_df$correlation, na.rm = TRUE)
    )
  })

print(similarity)


mean_variation_data <- ground_data_pixel_values_cleaned_long_modified %>%
  group_by(Habitat.Type, Wavelength) %>%
  dplyr::summarize(
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

################################################################################
################################################################################
################################################################################
################################################################################


# --- Aggregate statistics (mean, min, max reflectance) per plot ---
reflectance_summary <- ground_data_pixel_values_cleaned_long_modified_selection %>%
  group_by(Habitat.Type, PlotID, Wavelength) %>%
  dplyr::summarize(
    MeanReflectance = mean(Reflectance, na.rm = TRUE),
    MinReflectance  = min(Reflectance, na.rm = TRUE),
    MaxReflectance  = max(Reflectance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MeanReflectance = ifelse(is.infinite(MeanReflectance), NaN, MeanReflectance),
    MinReflectance = ifelse(is.infinite(MinReflectance), NaN, MinReflectance),
    MaxReflectance = ifelse(is.infinite(MaxReflectance), NaN, MaxReflectance)
  )


reflectance_norm <- reflectance_summary %>%
  # Group by each individual signature
  group_by(PlotID, Habitat.Type) %>%
  # Apply the vector normalization within each group
  mutate(# 1. Calculate the L2 norm (Euclidean norm) for the entire signature
    l2_norm = sqrt(sum(MeanReflectance ^ 2, na.rm = TRUE)),

    # 2. Divide each reflectance value by its signature's L2 norm
    NormReflectance = MeanReflectance / l2_norm) %>%
  ungroup() %>%
  # 3. Handle cases where the norm might be 0 (if all values are 0), which would result in NaN
  mutate(NormReflectance = ifelse(l2_norm == 0, 0, NormReflectance))

# Generate spectral signature plot with bands, faceted by Habitat.Type
signature_plot_by_habitat_type_norm <- ggplot() +
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
    alpha = 0.2
    # inherit.aes = FALSE
  ) +

  # 2. Spectral reflectance lines
  geom_line(
    data = reflectance_norm,
    aes(
      x = Wavelength,
      y = NormReflectance,
      color = Habitat.Type,
      # Still color by Habitat.Type if you want lines to be different colors within each facet
      group = PlotID # Group by PlotID for individual lines
    ),
    na.rm = FALSE,
    linewidth = 0.45
  ) +

  # *** Add this line to create separate plots for each Habitat.Type ***
  facet_wrap(
    ~ Habitat.Type,
    scales = "fixed",
    ncol = 2,
    labeller = labeller(Habitat.Type = habitat_labels)
  ) +

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type") # Title might be simpler now
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(reflectance_norm$Wavelength, na.rm = TRUE),
    max(reflectance_norm$Wavelength, na.rm = TRUE),
    by = x_axis_intervall + 30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_manual(
    name = "Habitat Type",
    values = c(
      "1.02" = "#E41A1C", "1.03" = "#377EB8", "4.01" = "#4DAF4A",
      "5.01" = "#984EA3", "5.05" = "#FF7F00", "5.06" = "#A65628","5.07" = "#FFFF33","5.09" = "#F781BF"
    ),
    # labels = c(
    #   "1.02" = "1.02", "1.03" = "1.03", "4.01" = "4.01",
    #   "5.01" = "5.01", "5.05" = "5.05", "5.06" = "5.06", "5.07" = "5.07", "5.09" = "5.09"
    # ),
    labels = c(
      "1.02" = "Dry dwarf shrubs and graminoids",
      "1.03" = "Acidic dwarf-shrub-heath",
      "4.01" = "Boreal forest enclaves",
      "5.01" = "Costal habitat",
      "5.05" = "Fresh water",
      "5.06" = "Mires",
      "5.07" = "Riparian shrublands and forests",
      "5.09" = "Ruderal vegetation"
    ),
    guide = guide_legend(order = 1)
    # guide = guide_legend(order = 2)
  ) +
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
    guide = guide_legend(order = 2))
# Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
# guides(fill = guide_legend(order = 1))


# Display the plot
print(signature_plot_by_habitat_type_norm)

signature_plot_by_habitat_type_norm_png_path <- file.path(
  'data/MasterThesis/all_testsite_signatures',
  'All_testsites_signature_plot_by_habitat_type_norm.png'
)

# Save plot
ggsave(
  signature_plot_by_habitat_type_norm_png_path,
  signature_plot_by_habitat_type_norm,
  width = 12,
  height = 9,
  dpi = 300
)


