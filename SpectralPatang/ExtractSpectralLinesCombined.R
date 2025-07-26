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

# Generate spectral signature plot with bands
signature_plot <- ggplot() +
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
      color = Habitat.Type,
      group = PlotID
    ),
    na.rm = FALSE,
    linewidth = 0.45
  ) +

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type over all test sites")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
    max(ground_data_pixel_values_cleaned_long$Wavelength, na.rm = TRUE),
    by = x_axis_intervall
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis(discrete = TRUE,
                      name = "Habitat Type",
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
print(signature_plot)

spectral_signature_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_signature_plot.png')

# Save plot
ggsave(
  spectral_signature_png_path,
  signature_plot,
  width = 12,
  height = 9,
  dpi = 300
)


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


# Generate mean spectral signature plot with bands
mean_signature_plot <- ggplot() +
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
    data = ground_data_pixel_values_cleaned_long_mean,
    aes(
      x = Wavelength,
      ymin = MinReflectance,
      ymax = MaxReflectance,
      fill = Habitat.Type,
      group = Habitat.Type
    ),
    alpha = 0.2,
    color = NA  # This removes the outline around the ribbon
  ) +
  geom_line(
    data = ground_data_pixel_values_cleaned_long_mean,
    aes(
      x = Wavelength,
      y = MeanReflectance,
      color = Habitat.Type,
      group = Habitat.Type
    ),
    linewidth = 0.45
  ) +

  # 3. Labels and theme
  labs(
    x = "Wavelength [nm]",
    y = "Mean Reflectance",
    title = paste0("Mean spectral signatures per habitat type over all test sites")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(
      ground_data_pixel_values_cleaned_long_mean$Wavelength,
      na.rm = TRUE
    ),
    max(
      ground_data_pixel_values_cleaned_long_mean$Wavelength,
      na.rm = TRUE
    ),
    by = x_axis_intervall
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis(discrete = TRUE,
                      name = "Habitat Type",
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
print(mean_signature_plot)

mean_spectral_signature_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_mean_signature_plot.png')

# Save plot
ggsave(
  mean_spectral_signature_png_path,
  mean_signature_plot,
  width = 12,
  height = 9,
  dpi = 300
)




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

ground_data_pixel_values_cleaned_long_modified_mean_filter <- ground_data_pixel_values_cleaned_long_modified_mean %>%
  filter(Habitat.Type == '4.01')

mean_signature_plot_modified <- ggplot() +
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
  # geom_ribbon(
  #   data = ground_data_pixel_values_cleaned_long_modified_mean_filter,
  #   aes(
  #     x = Wavelength,
  #     ymin = MinReflectance,
  #     ymax = MaxReflectance,
  #     fill = Habitat.Type,
  #     group = Habitat.Type
  #   ),
  #   alpha = 0.2,
  #   color = NA  # This removes the outline around the ribbon
  # ) +
  geom_line(
    data = ground_data_pixel_values_cleaned_long_modified_mean_filter,
    aes(
      x = Wavelength,
      y = MeanReflectance,
      color = Habitat.Type,
      group = Habitat.Type
    ),
    linewidth = 0.45
  ) +

  # 3. Labels and theme
  labs(
    x = "Wavelength [nm]",
    y = "Mean Reflectance",
    title = paste0("Mean spectral signatures per habitat type over all test sites")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(
      ground_data_pixel_values_cleaned_long_modified_mean_filter$Wavelength,
      na.rm = TRUE
    ),
    max(
      ground_data_pixel_values_cleaned_long_modified_mean_filter$Wavelength,
      na.rm = TRUE
    ),
    by = x_axis_intervall
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis(discrete = TRUE,
                      name = "Habitat Type",
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
print(mean_signature_plot_modified)

mean_spectral_signature_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_mean_signature_plot.png')

# Save plot
ggsave(
  mean_spectral_signature_png_path,
  mean_signature_plot,
  width = 12,
  height = 9,
  dpi = 300
)












# Generate mean spectral signature plot with bands with reduced habitat type
ground_data_pixel_values_cleaned_long_modified_selection <- ground_data_pixel_values_cleaned_long_modified %>%
  filter(Habitat.Type == '5.07')

ground_data_pixel_values_cleaned_long_modified_selection <- ground_data_pixel_values_cleaned_long_modified

# Generate spectral signature plot with bands
signature_plot_2 <- ggplot() +
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
      group = PlotID
    ),
    na.rm = FALSE,
    linewidth = 0.45
  ) +

  # 3. Plot styling
  labs(
    x = "Wavelength [nm]",
    y = "Reflectance",
    title = paste0("Spectral signatures per habitat type over all test sites")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
    max(ground_data_pixel_values_cleaned_long_modified_selection$Wavelength, na.rm = TRUE),
    by = x_axis_intervall
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis(discrete = TRUE,
                      name = "Habitat Type",
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
print(signature_plot_2)



































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


# Generate mean spectral signature plot with bands
mean_signature_plot_by_habitat_type <- ggplot() +
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
    data = mean_variation_data,
    aes(
      x = Wavelength,
      ymin = MinReflectance,
      ymax = MaxReflectance,
      fill = Habitat.Type,
      group = Habitat.Type
    ),
    alpha = 0.2,
    color = NA  # This removes the outline around the ribbon
  ) +
  geom_line(
    data = mean_variation_data,
    aes(
      x = Wavelength,
      y = MeanReflectance,
      color = Habitat.Type,
      group = Habitat.Type
    ),
    linewidth = 0.45
  ) +

  # 3. Labels and theme
  labs(
    x = "Wavelength [nm]",
    y = "Mean Reflectance",
    title = paste0("Mean spectral signatures per habitat type over all test sites")
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(
    min(
      mean_variation_data$Wavelength,
      na.rm = TRUE
    ),
    max(
      mean_variation_data$Wavelength,
      na.rm = TRUE
    ),
    by = x_axis_intervall
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # 4. Color scales
  scale_color_viridis(discrete = TRUE,
                      name = "Habitat Type",
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
print(mean_signature_plot_by_habitat_type)

mean_spectral_signature_by_habitat_type_png_path <- file.path('data/MasterThesis/all_testsite_signatures', 'All_testsites_mean_signature_plot_by_habitat_type.png')

# Save plot
ggsave(
  mean_spectral_signature_by_habitat_type_png_path,
  mean_signature_plot_by_habitat_type,
  width = 16,
  height = 9,
  dpi = 300
)


# Create an output folder for the plots
# output_folder <- "data/MasterThesis/plotid_signatures"
# dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
#
# # Loop over unique PlotID values
# unique_ids <- unique(ground_data_pixel_values_cleaned_long$PlotID)
#
# for (id in unique_ids) {
#   temp_df <- ground_data_pixel_values_cleaned_long %>%
#     filter(PlotID == id)
#
#   temp_plot <- ggplot() +
#     geom_rect(
#       data = band_data,
#       aes(
#         xmin = Lower,
#         xmax = Upper,
#         ymin = -Inf,
#         ymax = Inf,
#         fill = FillCategory
#       ),
#       alpha = 0.2,
#       inherit.aes = FALSE
#     ) +
#     geom_line(
#       data = temp_df,
#       aes(x = Wavelength, y = Reflectance, color = Habitat.Type),
#       linewidth = 0.7
#     ) +
#     labs(
#       title = paste("Spectral Signature for PlotID:", id),
#       x = "Wavelength [nm]",
#       y = "Reflectance"
#     ) +
#     theme_minimal() +
#     scale_x_continuous(breaks = seq(
#       min(temp_df$Wavelength, na.rm = TRUE),
#       max(temp_df$Wavelength, na.rm = TRUE),
#       by = x_axis_intervall
#     )) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     scale_color_viridis(discrete = TRUE,
#                         name = "Habitat Type",
#                         guide = guide_legend(order = 1)) +
#     scale_fill_manual(
#       name = "Spectral Region",
#       values = c(
#         "Violet" = "violet",
#         "Blue" = "blue",
#         "Cyan" = "cyan",
#         "Green" = "green",
#         "Yellow" = "yellow",
#         "Orange" = "darkorange",
#         "Red" = "red",
#         "Near-Infrared" = "darkgrey",
#         "Shortwave-Infrared" = "indianred",
#         guide = guide_legend(order = 2)
#       )
#     )
#
#   # Save the plot with PlotID as part of filename
#   output_path <- file.path(output_folder, paste0("signature_", id, ".png"))
#   ggsave(
#     output_path,
#     temp_plot,
#     width = 10,
#     height = 5,
#     dpi = 300
#   )
#   message(paste("Saved plot for PlotID:", id))
# }




#
# ground_data_pixel_values_cleaned_long_stat <- ground_data_pixel_values_cleaned_long %>%
#   dplyr::select(-c(Band, Subzone))  %>%
#   mutate(Wavelength = as.factor(Wavelength)) %>%
#   mutate(`Habitat Type` = as.factor(Habitat.Type)) %>%
#   mutate(PlotID = as.factor(PlotID)) %>%
#   mutate(BandNr = as.factor(BandNr))
#
# str(ground_data_pixel_values_cleaned_long_stat)
#
# # model_lm <- lm(Reflectance ~ Habitat.Type  + PlotID + Wavelength + Habitat.Type:Wavelength ,
# #                data = ground_data_pixel_values_cleaned_long_stat)
# # anova(model_lm)
# # par(mfrow = c(2, 2))
# # plot(model_lm)
#
# library(lme4)
# library(lmerTest)
#
# model_lmer <- lmer(Reflectance ~ Habitat.Type * Wavelength + (1 |
#                                                                 PlotID), data = ground_data_pixel_values_cleaned_long_stat)
# # summary(model_2)
# anova(model_lmer, type = "I")
# par(mfrow = c(2, 2))
# plot(model_lmer)
#
#
