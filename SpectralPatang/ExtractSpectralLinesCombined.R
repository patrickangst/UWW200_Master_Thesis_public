# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(tidyverse)
library(viridis)
library(Hmisc)

folder_names <- folder_names <- list.dirs(
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

# Generate mean spectral signature plot
ground_data_pixel_values_cleaned_long_mean <- ground_data_pixel_values_cleaned_long %>%
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


# Generate mean spectral signature plot with bands with reduced habitat type
ground_data_pixel_values_cleaned_long_modified <- ground_data_pixel_values_cleaned_long %>%
  mutate(Habitat.Type = str_extract(Habitat.Type, "^\\d+\\.\\d+"))

ground_data_pixel_values_cleaned_long_modified_mean <- ground_data_pixel_values_cleaned_long_modified %>%
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
    alpha = 0.2,
    inherit.aes = FALSE
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
  facet_wrap( ~ Habitat.Type, scales = "fixed", ncol = 2) + # Adjust ncol for desired number of columns

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
  scale_color_viridis_d(# Use _d for discrete scales with viridis
    name = "Habitat Type", guide = guide_legend(order = 1)) +
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
  # Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
  guides(fill = guide_legend(order = 1))


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
    alpha = 0.2,
    inherit.aes = FALSE
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
  facet_wrap( ~ Habitat.Type, scales = "fixed", ncol = 2) + # Adjust ncol for desired number of columns

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
  scale_color_viridis_d(# Use _d for discrete scales with viridis
    name = "Habitat Type", guide = guide_legend(order = 1)) +
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
  # Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
  guides(fill = guide_legend(order = 1))


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


# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(tidyverse)
library(viridis)
library(Hmisc)

folder_names <- folder_names <- list.dirs(
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

# Generate mean spectral signature plot
ground_data_pixel_values_cleaned_long_mean <- ground_data_pixel_values_cleaned_long %>%
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


# Generate mean spectral signature plot with bands with reduced habitat type
ground_data_pixel_values_cleaned_long_modified <- ground_data_pixel_values_cleaned_long %>%
  mutate(Habitat.Type = str_extract(Habitat.Type, "^\\d+\\.\\d+"))

ground_data_pixel_values_cleaned_long_modified_mean <- ground_data_pixel_values_cleaned_long_modified %>%
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
    alpha = 0.2,
    inherit.aes = FALSE
  ) +

  # 2. Spectral reflectance lines
  geom_line(
    data = ground_data_pixel_values_cleaned_long_modified_selection,
    aes(
      x = Wavelength,
      y = Reflectance,
      color = Habitat.Type, # Still color by Habitat.Type if you want lines to be different colors within each facet
      group = PlotID # Group by PlotID for individual lines
    ),
    na.rm = FALSE,
    linewidth = 0.45
  ) +

  # *** Add this line to create separate plots for each Habitat.Type ***
  facet_wrap(~ Habitat.Type, scales = "free_y", ncol = 2) + # Adjust ncol for desired number of columns

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type") # Title might be simpler now
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
    max(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
    by = x_axis_intervall+30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis_d( # Use _d for discrete scales with viridis
    name = "Habitat Type",
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
  # Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
  guides(fill = guide_legend(order = 1))


# Display the plot
print(signature_plot_by_habitat_type)

signature_plot_by_habitat_type_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_signature_plot_by_habitat_type.png')

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
reflectance_summary_minmax <- ground_data_pixel_values_cleaned_long_modified %>%
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


reflectance_norm_minmax <- reflectance_summary_minmax %>%
  group_by(PlotID, Habitat.Type) %>%
  mutate(NormReflectance = (MeanReflectance - min(MeanReflectance, na.rm = TRUE)) /
           (
             max(MeanReflectance, na.rm = TRUE) - min(MeanReflectance, na.rm = TRUE)
           ))

# Generate spectral signature plot with bands, faceted by Habitat.Type
signature_plot_by_habitat_type_norm_minmax <- ggplot() +
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
    data = reflectance_norm_minmax,
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
  # facet_wrap(~ Habitat.Type, scales = "free_y", ncol = 2) + # Adjust ncol for desired number of columns
  facet_wrap( ~ Habitat.Type, scales = "fixed", ncol = 2) + # Adjust ncol for desired number of columns

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type") # Title might be simpler now
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(reflectance_norm_minmax$Wavelength, na.rm = TRUE),
    max(reflectance_norm_minmax$Wavelength, na.rm = TRUE),
    by = x_axis_intervall + 30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis_d(# Use _d for discrete scales with viridis
    name = "Habitat Type", guide = guide_legend(order = 1)) +
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
  # Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
  guides(fill = guide_legend(order = 1))


# Display the plot
print(signature_plot_by_habitat_type_norm_minmax)

signature_plot_by_habitat_type_norm_minmax_png_path <- file.path(
  'data/MasterThesis/all_testsite_signatures',
  'All_testsites_signature_plot_by_habitat_type_norm_minmax.png'
)

# Save plot
ggsave(
  signature_plot_by_habitat_type_norm_minmax_png_path,
  signature_plot_by_habitat_type_norm_minmax,
  width = 12,
  height = 9,
  dpi = 300
)


similarity_with_pvalues <- ground_data_pixel_values_cleaned_long_modified_selection %>%
  group_by(Habitat.Type) %>%
  group_split() %>%   # one df per habitat
  map_df(function(df) {
    # Pivot to wide format, same as your original code
    wide <- df %>%
      select(Wavelength, PlotID, Reflectance) %>%
      pivot_wider(names_from = PlotID, values_from = Reflectance) %>%
      select(-Wavelength)

    # Convert to a matrix for the correlation function
    wide_matrix <- as.matrix(wide)

    # Use rcorr() to get correlations AND p-values
    # It returns a list containing the correlation matrix (r) and the p-value matrix (P)
    corr_results <- rcorr(wide_matrix, type = "pearson") # Or "spearman"

    # Extract the correlation matrix
    corr_mat <- corr_results$r
    # Extract the p-value matrix
    pval_mat <- corr_results$P

    # Convert correlation matrix to a long format (as you did before)
    corr_df <- as.data.frame(corr_mat) %>%
      rownames_to_column("PlotID") %>%
      pivot_longer(-PlotID, names_to = "OtherPlot", values_to = "correlation") %>%
      filter(PlotID < OtherPlot) # Avoid duplicates and self-correlation

    # Convert the p-value matrix to a long format so it can be joined
    pval_df <- as.data.frame(pval_mat) %>%
      rownames_to_column("PlotID") %>%
      pivot_longer(-PlotID, names_to = "OtherPlot", values_to = "p_value") %>%
      filter(PlotID < OtherPlot)

    # Join the correlation and p-value dataframes
    combined_results <- full_join(corr_df, pval_df, by = c("PlotID", "OtherPlot"))

    # Summarize the results for the habitat type
    tibble(
      Habitat.Type = unique(df$Habitat.Type),
      mean_correlation = mean(combined_results$correlation, na.rm = TRUE),
      # You can also add other metrics, like the proportion of significant correlations
      proportion_significant_05 = mean(combined_results$p_value < 0.05, na.rm = TRUE)
    )
  })

# Print the final results
print(similarity_with_pvalues)


# Generate spectral signature plot with bands, faceted by Habitat.Type
signature_plot_by_habitat_type2 <- ggplot() +
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
    data = ground_data_pixel_values_cleaned_long_modified_selection,
    aes(
      x = Wavelength,
      y = Reflectance,
      color = Habitat.Type, # Still color by Habitat.Type if you want lines to be different colors within each facet
      group = PlotID # Group by PlotID for individual lines
    ),
    na.rm = FALSE,
    linewidth = 0.45
  ) +

  # *** Add this line to create separate plots for each Habitat.Type ***
  facet_wrap(~ Habitat.Type, scales = "fixed", ncol = 2) + # Adjust ncol for desired number of columns

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type") # Title might be simpler now
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
    max(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
    by = x_axis_intervall+30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis_d( # Use _d for discrete scales with viridis
    name = "Habitat Type",
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
  # Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
  guides(fill = guide_legend(order = 1))


# Display the plot
print(signature_plot_by_habitat_type2)



#
# filtered_data <- ground_data_pixel_values_cleaned_long_modified %>%
#   filter(Habitat.Type == "5.07")


# Step 1: Get unique Band-Wavelength mapping
band_wavelength_lookup <- ground_data_pixel_values_cleaned_long_modified %>%
  select(Band, Wavelength) %>%
  distinct()


# Step 2: Group by Band and Habitat.Type
filtered_data_grouped <- ground_data_pixel_values_cleaned_long_modified %>%
  group_by(Band, Habitat.Type) %>%
  summarise(mean_reflectance = mean(Reflectance, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(band_wavelength_lookup, by = "Band") %>%
  arrange(Wavelength)



# Generate spectral signature plot with bands, faceted by Habitat.Type
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

  # 2. Spectral reflectance lines
  geom_line(
    data = filtered_data_grouped,
    aes(x = Wavelength, y = mean_reflectance, color = Habitat.Type),
    linewidth = 0.45
  ) +

  # *** Add this line to create separate plots for each Habitat.Type ***
  # facet_wrap( ~ Habitat.Type, scales = "fixed", ncol = 2) + # Adjust ncol for desired number of columns

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type") # Title might be simpler now
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(filtered_data_grouped$Wavelength, na.rm = TRUE),
    max(filtered_data_grouped$Wavelength, na.rm = TRUE),
    by = x_axis_intervall + 30
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis_d(# Use _d for discrete scales with viridis
    name = "Habitat Type", guide = guide_legend(order = 1)) +
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
  # Remove the fill guide from the scale_fill_manual if you want a shared legend at the top level
  guides(fill = guide_legend(order = 1))


# Display the plot
print(filtered_data_plot)



signature_plot_by_habitat_type_mean_png_path <- file.path(
  'data/MasterThesis/all_testsite_signatures',
  'All_testsites_signature_plot_by_habitat_type_mean.png'
)

# Save plot
ggsave(
  signature_plot_by_habitat_type_mean_png_path,
  filtered_data_plot,
  width = 12,
  height = 9,
  dpi = 300
)









