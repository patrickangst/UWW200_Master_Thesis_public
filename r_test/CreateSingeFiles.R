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
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'InputPlantCommunity')
output_site_folder <- 'site_data'
colnames(species_data) <- trimws(colnames(species_data))
# colnames(species_data)

# df <- species_data %>%
#   as.data.frame() %>%
#   filter(Testsite == "PRUARC_DW_1")

df <- species_data %>%
  as.data.frame()

df_backup <- df

test_sites <- unique(df$Testsite)

if (!dir.exists(output_site_folder)) dir.create(output_site_folder)

for (site in test_sites) {
  
  print(paste('Start with site', site))
  
  # Use a fresh copy instead of modifying df directly
  df_site <- df %>%
    filter(Testsite == site) %>%
    as.data.frame()
  
  rownames(df_site) <- df_site$PlotIdentifier
  
  # Select only numeric species data
  df_species_list <- df_site %>%
    select(where(is.numeric)) %>%
    filter(rowSums(.) != 0) %>%  # Remove empty rows
    select(where(~ sum(.) != 0)) # Remove empty columns
  
  # Skip iteration if dataframe is empty
  if (nrow(df_species_list) == 0 || ncol(df_species_list) == 0) {
    print(paste("Skipping site", site, "due to lack of valid data"))
    next  # Skip to next iteration
  }
  
  # Create a dataframe with column sums (1 row)
  df_species_list_gamma <- as.data.frame(t(colSums(df_species_list)))
  
  # Add a row name for clarity (optional)
  rownames(df_species_list_gamma) <- "Total_Sum"
  
  # View the dataframe
  # print(df_species_list_gamma)
  
  # Normalize species data (Avoid division by zero)
  df_species_list <- df_species_list %>%
    mutate(across(everything(), ~ ifelse(rowSums(df_species_list) == 0, 0, . / rowSums(df_species_list))))
  
  # Calculate diversity indices
  shannon_idx <- vegan::diversity(df_species_list, index = "shannon")
  simpson_idx <- vegan::diversity(df_species_list, index = "simpson")
  richness <- rowSums(df_species_list > 0)
  evenness <- ifelse(richness > 1, shannon_idx / log(richness), 0)
  
  # Create final dataframe
  df_species_list_diversity_idx <- df_species_list %>%
    mutate(Shannon = shannon_idx,
           Simpson = simpson_idx,
           Richness = richness,
           Evenness = evenness) %>%
    relocate(Shannon, Simpson, Richness, Evenness)  # Move new columns to the front
  
  # Add PlotIdentifier as last column
  df_species_list_diversity_idx$PlotIdentifier <- rownames(df_species_list_diversity_idx)
  
  df_species_list_diversity_idx <- df_species_list_diversity_idx %>%
    select(PlotIdentifier, everything())
  
  
  # Save each site’s data as an Excel file
  xlsx_name <- file.path(output_site_folder, paste0(site, '.xlsx'))
  write.xlsx(df_species_list_diversity_idx, xlsx_name, rowNames = FALSE)
  
  print(paste("Saved:", xlsx_name))
}