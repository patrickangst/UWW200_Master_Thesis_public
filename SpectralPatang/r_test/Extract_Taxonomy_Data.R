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
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'InputTaxonomyType')
summary_data <- read_excel(file.path(plot_metrics_path,"Plot_Metrics_Summary.xlsx"), sheet = 'Sheet1')

colnames(species_data) <- trimws(colnames(species_data))
# colnames(species_data)

df <- species_data %>%
  as.data.frame() %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(PlotIdentifier = paste0(`Table number`,'_',Testsite))

df_selection <- df %>%
  select(-c(`Table number`,Dataset))


df_summary <- df_selection %>%
  group_by(Testsite) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

df_summary_normalized <- df_summary %>%
  mutate(across(-Testsite, ~ . / rowSums(across(-Testsite))))



# Calculate Simpson diversity index for each Testsite (row)
df_summary_normalized$simpson_index_TAX <- diversity(df_summary_normalized[, -1], index = "simpson")

df_summary_normalized$Unique_Plant_Types <- df_summary_normalized %>%
  select(-c(Testsite,simpson_index_TAX)) %>%
  apply(1, function(x) sum(x != 0))

df_summary_normalized <- df_summary_normalized %>%
  select(Testsite,simpson_index_TAX,Unique_Plant_Types,everything())


df_simpson_selection <- df_summary_normalized %>%
  select(Testsite, simpson_index_TAX,Unique_Plant_Types)

df_combined <- summary_data %>%
  left_join(df_simpson_selection, by = "Testsite") %>%
  arrange(Testsite) %>%
  select(Testsite,simpson_index_PC,Unique_Plant_Cummunities,simpson_index_HT,Unique_Habitat_Types,simpson_index_TAX,Unique_Plant_Types)
  
print(df_combined)

write_xlsx(df_combined, path = file.path(plot_metrics_path, "Plot_Metrics_Summary.xlsx"))