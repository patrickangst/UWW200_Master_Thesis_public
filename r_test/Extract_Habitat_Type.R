rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary packages
library(vegan)
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(NbClust)
library(dbscan)
library(ggplot2)
library(tidyr)
library(pracma)

# Load species abundance data
plot_metrics_path <- 'plot_metrics'
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'InputHabitatType')
summary_data <- read_excel(file.path(plot_metrics_path,"Cluster_Summary.xlsx"), sheet = 'Sheet1')
output_dir <- plot_metrics_path

# Create folder for plots if not exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

colnames(species_data) <- trimws(colnames(species_data))
# colnames(species_data)

df <- species_data %>%
  as.data.frame() %>%
  select('Table number',Dataset, Testsite, Subzone, 'Habitat Type') %>%
  mutate(PlotIdentifier = paste0(`Table number`,'_',Testsite))

df_selection <- df %>%
  select(Testsite, `Habitat Type`)


df_summary <- df %>%
  select(Testsite, `Habitat Type`) %>%
  group_by(Testsite) %>%
  summarise(Unique_Habitat_Types = n_distinct(`Habitat Type`)) %>%
  arrange(desc(Unique_Habitat_Types))  # Optional: sort by most diverse

print(df_summary)

df_combined <- summary_data %>%
  left_join(df_summary, by = "Testsite")


df_simpson <- df %>%
  select(Testsite, `Habitat Type`) %>%
  count(Testsite, `Habitat Type`) %>%
  pivot_wider(
    names_from = `Habitat Type`,
    values_from = n,
    values_fill = 0
  )

# Calculate Simpson diversity index for each Testsite (row)
df_simpson$simpson_index_HT <- diversity(df_simpson[, -1], index = "simpson")

df_simpson_selection <- df_simpson %>%
  select(Testsite, simpson_index_HT)

df_combined <- df_combined %>%
  left_join(df_simpson_selection, by = "Testsite") %>%
  arrange(Testsite)

print(df_combined)

write_xlsx(df_combined, path = file.path(output_dir, "Plot_Metrics_Summary.xlsx"))