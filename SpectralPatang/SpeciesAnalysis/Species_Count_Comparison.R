# Clear workspace
rm(list = ls(all = TRUE))
gc()
graphics.off()


# Load necessary packages
library("readxl")
library("dplyr")
library("vegan")
library("writexl")
library("tidyr")
library("ggplot2")

# Specify file paths
# base_folder <- 'D:/MasterThesis'
base_folder <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis'
output_folder_path <- file.path(base_folder, '07_Testsite_Metrics', 'plots')

metrics_file_path <- file.path(base_folder, '07_Testsite_Metrics', 'Metrics.xlsx')

# Read the input data (specify the sheet name)
my_data <- read_excel(metrics_file_path, sheet = "Sheet1")

my_data_filterd <- my_data %>%
  select(
    Plot_Location_Shp_Subset_Name,
    SpectralSpecies,
    Species_Ground_AVG,
    Species_Ground_MIN,
    Species_Ground_MAX
  ) %>%
  rename(Testsite = Plot_Location_Shp_Subset_Name)

my_data_filterd$SpectralSpecies <- as.numeric(my_data_filterd$SpectralSpecies)
my_data_filterd$Species_Ground_AVG <- as.numeric(my_data_filterd$Species_Ground_AVG)
my_data_filterd$Species_Ground_MIN <- as.numeric(my_data_filterd$Species_Ground_MIN)
my_data_filterd$Species_Ground_MAX <- as.numeric(my_data_filterd$Species_Ground_MAX)

# # Reshape data to long format
df_long <- my_data_filterd %>%
  pivot_longer(
    cols = c(
      SpectralSpecies,
      Species_Ground_MIN,
      Species_Ground_AVG,
      Species_Ground_MAX
    ),
    names_to = "Species_Count_Type",
    values_to = "Value"
  )

# Set factor levels in correct order
df_long$Species_Count_Type <- factor(df_long$Species_Count_Type,
                                     levels = c("Species_Ground_MIN", "Species_Ground_AVG", "Species_Ground_MAX", "SpectralSpecies"))

# Create the bar plot
barplot <- ggplot(df_long, aes(x = Testsite, y = Value, fill = Species_Count_Type)) +
  # Plot the bars
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.7) +
  # Add labels
  labs(title = "Comparison of Ground and Spectral Species",
       x = "Testsite",
       y = "Species Count",
       fill = "Species Count") +
  # Theme adjustments
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Ensure correct colors and labels
  scale_fill_manual(
    values = c(
      "Species_Ground_MIN" = "green",
      "Species_Ground_AVG" = "blue",
      "Species_Ground_MAX" = "orange",
      "SpectralSpecies" = "red"
    ),
    labels = c("Minimum species count ground",
               "Average species count ground",
               "Maximum species count ground",
               "Spectral species count")
  )

plot
print(barplot)

# Save the plot as a PNG
barplot_species_comparison_file_path <- file.path(output_folder_path, 'barplot_species_comparison.png')
ggsave(
  filename = barplot_species_comparison_file_path,
  plot = barplot,
  width = 9,
  height = 6,
  dpi = 400
)
