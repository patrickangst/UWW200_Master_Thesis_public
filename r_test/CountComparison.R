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

# Set the path to the directory where the Excel file is located
plot_metrics_path <- 'plot_metrics' # Make sure this path is correct
output_folder_path <- 'correlation_plots'

# Read the Excel data
my_data <- read_excel(file.path(plot_metrics_path, "Plot_Metrics_Combined.xlsx"), sheet = 'Sheet1')

# Filter the columns
my_data_filterd <- my_data %>%
  select(
    Testsite,
    Unique_Plant_Cummunities,
    Unique_Habitat_Types,
    Unique_Plant_Types,
    Unique_Spectral_Species_WCSS
  )

# Convert columns to numeric
my_data_filterd$Unique_Spectral_Species_WCSS <- as.numeric(my_data_filterd$Unique_Spectral_Species_WCSS)
my_data_filterd$Unique_Plant_Types <- as.numeric(my_data_filterd$Unique_Plant_Types)
my_data_filterd$Unique_Habitat_Types <- as.numeric(my_data_filterd$Unique_Habitat_Types)
my_data_filterd$Unique_Plant_Cummunities <- as.numeric(my_data_filterd$Unique_Plant_Cummunities)

# Reshape data to long format
df_long <- my_data_filterd %>%
  pivot_longer(
    cols = c(
      Unique_Spectral_Species_WCSS,
      Unique_Plant_Types,
      Unique_Habitat_Types,
      Unique_Plant_Cummunities
    ),
    names_to = "Species_Count_Type",
    values_to = "Value"
  )

# Set factor levels in the correct order
df_long$Species_Count_Type <- factor(df_long$Species_Count_Type,
                                     levels = c("Unique_Spectral_Species_WCSS", "Unique_Plant_Types", "Unique_Habitat_Types", "Unique_Plant_Cummunities"))

# Create the bar plot
barplot <- ggplot(df_long, aes(x = Testsite, y = Value, fill = Species_Count_Type)) +
  # Plot the bars
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  # Add labels
  labs(
    title = "Comparison of Ground and Spectral Species",
    x = "Testsite",
    y = "Species Count",
    fill = "Species Count Type"  # Changed legend title to match
  ) +
  # Theme adjustments
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Ensure correct colors and labels.  Use the *actual* values from the data.
  scale_fill_manual(
    values = c(
      "Unique_Spectral_Species_WCSS" = "#b85042",
      "Unique_Plant_Types" = "#e2975d",
      "Unique_Habitat_Types" = "#4f6d7a",
      "Unique_Plant_Cummunities" = "#66a182"
    ),
    labels = c(
      "Spectral species count",
      "Unique Plant Species",
      "Unique Habitat Types",
      "Unique Plant Communities"
    )
  )

print(barplot)

# Save the plot as a PNG
if (!dir.exists(output_folder_path)) {
  dir.create(output_folder_path, recursive = TRUE) # recursive = TRUE for nested directories
}

barplot_species_comparison_file_path <- file.path(output_folder_path, 'barplot_species_comparison.png')
ggsave(
  filename = barplot_species_comparison_file_path,
  plot = barplot,
  width = 9,
  height = 6,
  dpi = 400
)
