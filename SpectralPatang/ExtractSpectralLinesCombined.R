# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

library(tidyverse)
library(viridis)

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

# # Generate spectral signature plot with bands
# signature_plot <- ggplot() +
#   # 1. Background spectral regions
#   geom_rect(
#     data = band_data,
#     aes(
#       xmin = Lower,
#       xmax = Upper,
#       ymin = -Inf,
#       ymax = Inf,
#       fill = FillCategory
#     ),
#     alpha = 0.2,
#     inherit.aes = FALSE
#   ) +
#
#   # 2. Spectral reflectance lines
#   geom_line(
#     data = ground_data_pixel_values_cleaned_long,
#     aes(
#       x = Wavelength,
#       y = Reflectance,
#       color = Habitat.Type,
#       group = PlotID
#     ),
#     na.rm = FALSE,
#     linewidth = 0.45
#   ) +
#
#   # 3. Plot styling
#   labs(
#     x = "Wavelength [nm]",
#     y = "Reflectance",
#     title = paste0("Spectral signatures per habitat type over all test sites")
#   ) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(
#     min(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
#     max(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
#     by = x_axis_intervall
#   )) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#
#   # 4. Color scales
#   scale_color_viridis(discrete = TRUE,
#                       name = "Habitat Type",
#                       guide = guide_legend(order = 1)) +
#   scale_fill_manual(
#     name = "Spectral Region",
#     values = c(
#       "Violet" = "violet",
#       "Blue" = "blue",
#       "Cyan" = "cyan",
#       "Green" = "green",
#       "Yellow" = "yellow",
#       "Orange" = "darkorange",
#       "Red" = "red",
#       "Near-Infrared" = "darkgrey",
#       "Shortwave-Infrared" = "indianred",
#       guide = guide_legend(order = 2)
#     )
#   )
#
# # Display the plot
# print(signature_plot)
#
# spectral_signature_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_signature_plot.png')
#
# # Save plot
# ggsave(
#   spectral_signature_png_path,
#   signature_plot,
#   width = 12,
#   height = 9,
#   dpi = 300
# )


# Generate mean spectral signature plot
ground_data_pixel_values_cleaned_long_mean <- ground_data_pixel_values_cleaned_long %>%
  group_by(Habitat.Type, Wavelength) %>%
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


# # Generate mean spectral signature plot with bands
# mean_signature_plot <- ggplot() +
#   # 1. Background spectral regions
#   geom_rect(
#     data = band_data,
#     aes(
#       xmin = Lower,
#       xmax = Upper,
#       ymin = -Inf,
#       ymax = Inf,
#       fill = FillCategory
#     ),
#     alpha = 0.2,
#     inherit.aes = FALSE
#   ) +
#
#   # 2. Ribbon and line for mean reflectance
#   geom_ribbon(
#     data = ground_data_pixel_values_cleaned_long_mean,
#     aes(
#       x = Wavelength,
#       ymin = MinReflectance,
#       ymax = MaxReflectance,
#       fill = Habitat.Type,
#       group = Habitat.Type
#     ),
#     alpha = 0.2,
#     color = NA  # This removes the outline around the ribbon
#   ) +
#   geom_line(
#     data = ground_data_pixel_values_cleaned_long_mean,
#     aes(
#       x = Wavelength,
#       y = MeanReflectance,
#       color = Habitat.Type,
#       group = Habitat.Type
#     ),
#     linewidth = 0.45
#   ) +
#
#   # 3. Labels and theme
#   labs(
#     x = "Wavelength [nm]",
#     y = "Mean Reflectance",
#     title = paste0("Mean spectral signatures per habitat type over all test sites")
#   ) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(
#     min(
#       ground_data_pixel_values_cleaned_long_mean$Wavelength,
#       na.rm = TRUE
#     ),
#     max(
#       ground_data_pixel_values_cleaned_long_mean$Wavelength,
#       na.rm = TRUE
#     ),
#     by = x_axis_intervall
#   )) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#
#   # 4. Color scales
#   scale_color_viridis(discrete = TRUE,
#                       name = "Habitat Type",
#                       guide = guide_legend(order = 1)) +
#   scale_fill_manual(
#     name = "Spectral Region",
#     values = c(
#       "Violet" = "violet",
#       "Blue" = "blue",
#       "Cyan" = "cyan",
#       "Green" = "green",
#       "Yellow" = "yellow",
#       "Orange" = "darkorange",
#       "Red" = "red",
#       "Near-Infrared" = "darkgrey",
#       "Shortwave-Infrared" = "indianred"
#     ),
#     guide = guide_legend(order = 2)
#   )
#
# # Display the plot
# print(mean_signature_plot)
#
# mean_spectral_signature_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_mean_signature_plot.png')
#
# # Save plot
# ggsave(
#   mean_spectral_signature_png_path,
#   mean_signature_plot,
#   width = 12,
#   height = 9,
#   dpi = 300
# )




# Generate mean spectral signature plot with bands with reduced habitat type
ground_data_pixel_values_cleaned_long_modified <- ground_data_pixel_values_cleaned_long %>%
  mutate(Habitat.Type = str_extract(Habitat.Type, "^\\d+\\.\\d+"))

ground_data_pixel_values_cleaned_long_modified_mean <- ground_data_pixel_values_cleaned_long_modified %>%
  group_by(Habitat.Type, Wavelength) %>%
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

# ground_data_pixel_values_cleaned_long_modified_mean_filter <- ground_data_pixel_values_cleaned_long_modified_mean %>%
#   filter(Habitat.Type == '4.01')
#
# mean_signature_plot_modified <- ggplot() +
#   # 1. Background spectral regions
#   geom_rect(
#     data = band_data,
#     aes(
#       xmin = Lower,
#       xmax = Upper,
#       ymin = -Inf,
#       ymax = Inf,
#       fill = FillCategory
#     ),
#     alpha = 0.2,
#     inherit.aes = FALSE
#   ) +
#
#   # 2. Ribbon and line for mean reflectance
#   # geom_ribbon(
#   #   data = ground_data_pixel_values_cleaned_long_modified_mean_filter,
#   #   aes(
#   #     x = Wavelength,
#   #     ymin = MinReflectance,
#   #     ymax = MaxReflectance,
#   #     fill = Habitat.Type,
#   #     group = Habitat.Type
#   #   ),
#   #   alpha = 0.2,
#   #   color = NA  # This removes the outline around the ribbon
#   # ) +
#   geom_line(
#     data = ground_data_pixel_values_cleaned_long_modified_mean_filter,
#     aes(
#       x = Wavelength,
#       y = MeanReflectance,
#       color = Habitat.Type,
#       group = Habitat.Type
#     ),
#     linewidth = 0.45
#   ) +
#
#   # 3. Labels and theme
#   labs(
#     x = "Wavelength [nm]",
#     y = "Mean Reflectance",
#     title = paste0("Mean spectral signatures per habitat type over all test sites")
#   ) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(
#     min(
#       ground_data_pixel_values_cleaned_long_modified_mean_filter$Wavelength,
#       na.rm = TRUE
#     ),
#     max(
#       ground_data_pixel_values_cleaned_long_modified_mean_filter$Wavelength,
#       na.rm = TRUE
#     ),
#     by = x_axis_intervall
#   )) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#
#   # 4. Color scales
#   scale_color_viridis(discrete = TRUE,
#                       name = "Habitat Type",
#                       guide = guide_legend(order = 1)) +
#   scale_fill_manual(
#     name = "Spectral Region",
#     values = c(
#       "Violet" = "violet",
#       "Blue" = "blue",
#       "Cyan" = "cyan",
#       "Green" = "green",
#       "Yellow" = "yellow",
#       "Orange" = "darkorange",
#       "Red" = "red",
#       "Near-Infrared" = "darkgrey",
#       "Shortwave-Infrared" = "indianred"
#     ),
#     guide = guide_legend(order = 2)
#   )
#
# # Display the plot
# print(mean_signature_plot_modified)
#
# mean_spectral_signature_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_mean_signature_plot.png')
#
# # Save plot
# ggsave(
#   mean_spectral_signature_png_path,
#   mean_signature_plot,
#   width = 12,
#   height = 9,
#   dpi = 300
# )





# Generate mean spectral signature plot with bands with reduced habitat type
ground_data_pixel_values_cleaned_long_modified_selection <- ground_data_pixel_values_cleaned_long_modified %>%
  filter(Habitat.Type == '5.07')

ground_data_pixel_values_cleaned_long_modified_selection <- ground_data_pixel_values_cleaned_long_modified

# # Generate spectral signature plot with bands
# signature_plot_2 <- ggplot() +
#   # 1. Background spectral regions
#   geom_rect(
#     data = band_data,
#     aes(
#       xmin = Lower,
#       xmax = Upper,
#       ymin = -Inf,
#       ymax = Inf,
#       fill = FillCategory
#     ),
#     alpha = 0.2,
#     inherit.aes = FALSE
#   ) +
#
#   # 2. Spectral reflectance lines
#   geom_line(
#     data = ground_data_pixel_values_cleaned_long_modified_selection,
#     aes(
#       x = Wavelength,
#       y = Reflectance,
#       color = Habitat.Type,
#       group = PlotID
#     ),
#     na.rm = FALSE,
#     linewidth = 0.45
#   ) +
#
#   # 3. Plot styling
#   labs(
#     x = "Wavelength [nm]",
#     y = "Reflectance",
#     title = paste0("Spectral signatures per habitat type over all test sites")
#   ) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(
#     min(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
#     max(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
#     by = x_axis_intervall
#   )) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#
#   # 4. Color scales
#   scale_color_viridis(discrete = TRUE,
#                       name = "Habitat Type",
#                       guide = guide_legend(order = 1)) +
#   scale_fill_manual(
#     name = "Spectral Region",
#     values = c(
#       "Violet" = "violet",
#       "Blue" = "blue",
#       "Cyan" = "cyan",
#       "Green" = "green",
#       "Yellow" = "yellow",
#       "Orange" = "darkorange",
#       "Red" = "red",
#       "Near-Infrared" = "darkgrey",
#       "Shortwave-Infrared" = "indianred",
#       guide = guide_legend(order = 2)
#     )
#   )
#
# # Display the plot
# print(signature_plot_2)



































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




# # Aggregate mean reflectance
# mean_signature_data <- ground_data_pixel_values_cleaned_long_modified_selection %>%
#   group_by(Habitat.Type, Wavelength) %>%
#   summarise(MeanReflectance = mean(Reflectance, na.rm = TRUE), .groups = "drop")
#
# mean_variation_data <- ground_data_pixel_values_cleaned_long_modified_selection %>%
#   group_by(Habitat.Type, Wavelength) %>%
#   summarise(
#     MeanReflectance = mean(Reflectance, na.rm = TRUE),
#     SD = sd(Reflectance, na.rm = TRUE),  # Variation
#     N = n(),  # Sample size
#     .groups = "drop"
#   ) %>%
#   mutate(
#     LowerCI = MeanReflectance - SD,
#     UpperCI = MeanReflectance + SD
#     # Or use CI: qt(0.975, df = N-1) * (SD / sqrt(N))
#   )



mean_variation_data <- ground_data_pixel_values_cleaned_long_modified %>%
  group_by(Habitat.Type, Wavelength) %>%
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


# # Generate mean spectral signature plot with bands
# mean_signature_plot_by_habitat_type <- ggplot() +
#   # 1. Background spectral regions
#   geom_rect(
#     data = band_data,
#     aes(
#       xmin = Lower,
#       xmax = Upper,
#       ymin = -Inf,
#       ymax = Inf,
#       fill = FillCategory
#     ),
#     alpha = 0.2,
#     inherit.aes = FALSE
#   ) +
#
#   # 2. Ribbon and line for mean reflectance
#   geom_ribbon(
#     data = mean_variation_data,
#     aes(
#       x = Wavelength,
#       ymin = MinReflectance,
#       ymax = MaxReflectance,
#       fill = Habitat.Type,
#       group = Habitat.Type
#     ),
#     alpha = 0.2,
#     color = NA  # This removes the outline around the ribbon
#   ) +
#   geom_line(
#     data = mean_variation_data,
#     aes(
#       x = Wavelength,
#       y = MeanReflectance,
#       color = Habitat.Type,
#       group = Habitat.Type
#     ),
#     linewidth = 0.45
#   ) +
#
#   # 3. Labels and theme
#   labs(
#     x = "Wavelength [nm]",
#     y = "Mean Reflectance",
#     title = paste0("Mean spectral signatures per habitat type over all test sites")
#   ) +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(
#     min(
#       mean_variation_data$Wavelength,
#       na.rm = TRUE
#     ),
#     max(
#       mean_variation_data$Wavelength,
#       na.rm = TRUE
#     ),
#     by = x_axis_intervall
#   )) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#
#   # 4. Color scales
#   scale_color_viridis(discrete = TRUE,
#                       name = "Habitat Type",
#                       guide = guide_legend(order = 1)) +
#   scale_fill_manual(
#     name = "Spectral Region",
#     values = c(
#       "Violet" = "violet",
#       "Blue" = "blue",
#       "Cyan" = "cyan",
#       "Green" = "green",
#       "Yellow" = "yellow",
#       "Orange" = "darkorange",
#       "Red" = "red",
#       "Near-Infrared" = "darkgrey",
#       "Shortwave-Infrared" = "indianred"
#     ),
#     guide = guide_legend(order = 2)
#   )
#
# # Display the plot
# print(mean_signature_plot_by_habitat_type)
#
# mean_spectral_signature_by_habitat_type_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_mean_signature_plot_by_habitat_type.png')
#
# # Save plot
# ggsave(
#   mean_spectral_signature_by_habitat_type_png_path,
#   mean_signature_plot_by_habitat_type,
#   width = 16,
#   height = 9,
#   dpi = 300
# )


################################################################################
################################################################################
################################################################################
################################################################################


# --- Aggregate statistics (mean, min, max reflectance) per plot ---
reflectance_summary <- ground_data_pixel_values_cleaned_long_modified_selection %>%
  group_by(Habitat.Type, PlotID, Wavelength) %>%
  summarize(
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
  group_by(PlotID, Habitat.Type) %>%
  mutate(
    NormReflectance = (MeanReflectance - min(MeanReflectance, na.rm = TRUE)) /
      (max(MeanReflectance, na.rm = TRUE) - min(MeanReflectance, na.rm = TRUE))
  )

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
    min(reflectance_norm$Wavelength, na.rm = TRUE),
    max(reflectance_norm$Wavelength, na.rm = TRUE),
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
print(signature_plot_by_habitat_type_norm)



similarity_norm <- reflectance_norm %>%
  group_by(Habitat.Type) %>%
  group_split() %>%   # one df per habitat
  map_df(function(df) {
    wide <- df %>%
      select(Wavelength, PlotID, NormReflectance) %>%
      pivot_wider(names_from = PlotID, values_from = NormReflectance) %>%
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

print(similarity_norm)







library(tidyverse)

# --- 1. Prepare the Data ---
# First, get the names of the two plots you want to compare
plot_ids_to_compare <- ground_data_pixel_values_cleaned_long_modified %>%
  filter(Habitat.Type == '4.01') %>%
  distinct(PlotID) %>%
  slice(1:2) %>% # Select the first two plots for this example
  pull(PlotID)

# Now, filter the data and pivot it to a wide format
# This creates two columns, one for each plot's reflectance, making comparison easy
wide_data <- ground_data_pixel_values_cleaned_long_modified %>%
  filter(PlotID %in% plot_ids_to_compare) %>%
  select(Wavelength, PlotID, Reflectance) %>%
  pivot_wider(names_from = PlotID, values_from = Reflectance)

# Let's name the columns for clarity (replace with your actual plot IDs)
plot1_col_name <- plot_ids_to_compare[1]
plot2_col_name <- plot_ids_to_compare[2]

# Extract the reflectance vectors, removing any NAs from either curve
# `na.omit` is important to ensure the vectors are perfectly aligned
reflectance_vectors <- wide_data %>%
  select(all_of(plot_ids_to_compare)) %>%
  na.omit()

vec1 <- reflectance_vectors[[plot1_col_name]]
vec2 <- reflectance_vectors[[plot2_col_name]]


# --- 2. Calculate Similarity Metrics ---

# a) Pearson Correlation
correlation <- cor(vec1, vec2, method = "pearson")

# b) Spectral Angle Mapper (SAM)
# R doesn't have a built-in function, but the formula is simple
spectral_angle <- acos( sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2))) )

# c) Euclidean Distance
euclidean_dist <- sqrt(sum((vec1 - vec2)^2))


# --- 3. Print the Results ---
print(paste("Comparing Plot", plot1_col_name, "and", plot2_col_name))
print(paste("Pearson Correlation:", round(correlation, 4)))
print(paste("Spectral Angle (in radians):", round(spectral_angle, 4)))
print(paste("Euclidean Distance:", round(euclidean_dist, 4)))




library(tidyverse)

# --- 1. Get All PlotIDs in Habitat.Type 4.01 ---
plot_ids <- ground_data_pixel_values_cleaned_long_modified %>%
  filter(Habitat.Type == '5.06') %>%
  distinct(PlotID) %>%
  pull(PlotID)

# --- 2. Prepare Wide Data for All Plots ---
wide_data <- ground_data_pixel_values_cleaned_long_modified %>%
  filter(PlotID %in% plot_ids) %>%
  select(Wavelength, PlotID, Reflectance) %>%
  pivot_wider(names_from = PlotID, values_from = Reflectance)

# --- 3. Generate All Unique Plot Pairs ---
plot_pairs <- combn(plot_ids, 2, simplify = FALSE)

# --- 4. Define a Function to Compute Similarity Metrics ---
compute_similarity <- function(plot1, plot2, data) {
  reflectance_vectors <- data %>%
    select(all_of(c(plot1, plot2))) %>%
    na.omit()

  vec1 <- reflectance_vectors[[plot1]]
  vec2 <- reflectance_vectors[[plot2]]

  correlation <- cor(vec1, vec2, method = "pearson")
  spectral_angle <- acos(sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2))))
  euclidean_dist <- sqrt(sum((vec1 - vec2)^2))

  tibble(
    Plot1 = plot1,
    Plot2 = plot2,
    Pearson_Correlation = round(correlation, 4),
    Spectral_Angle_Radians = round(spectral_angle, 4),
    Euclidean_Distance = round(euclidean_dist, 4)
  )
}

# --- 5. Apply the Function to All Pairs ---
similarity_results <- map_dfr(plot_pairs, ~compute_similarity(.x[1], .x[2], wide_data))

# --- 6. View the Results ---
print(similarity_results)

mean_metrics <- similarity_results %>%
  summarise(
    Mean_Pearson = mean(Pearson_Correlation, na.rm = TRUE),
    Mean_Angle = mean(Spectral_Angle_Radians, na.rm = TRUE),
    Mean_Euclidean = mean(Euclidean_Distance, na.rm = TRUE)
  )

print(mean_metrics)


print(mean_correlation)

