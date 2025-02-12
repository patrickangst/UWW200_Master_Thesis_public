# Clear workspace
rm(list = ls())
graphics.off()

library("readxl")
library("dplyr")
library(vegan)

# Specify sheet by its name
my_data <- read_excel("D:/MasterThesis/gound_data/datasets/All_plots.xlsx", sheet = "input")

calculate_shannon <- function(df) {

  # get the position of the 'Number of species' column
  num_spec_pos <- which(names(df) == 'Number of species')

  # select only the species from the dataframe
  shannon_df <- df %>%
    dplyr::select(all_of(names(df)[(num_spec_pos + 1):ncol(df)]))

  # Replace NA with 0
  shannon_df[is.na(shannon_df)] <- 0

  # Keep columns that have at least one non-zero value
  shannon_df_cleaned <- shannon_df %>%
    select(where(~ any(. != 0)))

  # Only keep numeric columns
  shannon_df_cleaned <- select(shannon_df_cleaned, where(is.numeric))

  # Sum species abundance across all plots (rows) for the site
  combined_abundance <- colSums(shannon_df_cleaned, na.rm = TRUE)

  # Normalize data
  total_abundance <- sum(combined_abundance)
  combined_abundance <- (combined_abundance / total_abundance)

  return(diversity(combined_abundance, index = "shannon"))  # Excluding the first column (assumed non-species)
}

# get the position of the 'Number of species' column
number_species_pos <- which(names(my_data) == 'Number of species')

# Select relevant columns
plot_data_filtered <- my_data %>%
  dplyr::select('Dataset','Testsite','Releve number','Year','Month','Day','Elevation  (m)', 'Releve area (m2)','Cover total vegetation (%)','Cover trees (%)','Cover total shrubs (%)','Cover herb layer (%)','Cover moss layer (%)','Cover lichen (%)','Cover algae (%)','Cover litter (%)','Cover water (%)','Cover rock (%)','Longitude','Latitude','Plant community name','Shannon','Evenness','Simpson', 'Number of species', all_of(names(my_data)[(number_species_pos + 1):ncol(my_data)]))

# replace -9 with NA
plot_data_filtered <- plot_data_filtered %>% mutate(across(everything(), ~ replace(.x, .x == -9, NA)))

# plot_data_filtered <- plot_data_filtered %>%
#   mutate(Shannon_Diversity_Calculated = NA) %>%
#   mutate(Beta_Diversity_Calculated = NA)

# split list into test sites
df_list <- split(plot_data_filtered, plot_data_filtered$Testsite)

# Calculate shannon for all test sites
shannon_results <- lapply(df_list, calculate_shannon)

# Convert to a data frame
diversity_table <- data.frame(
  Testsite = names(df_list),  # Use list element names as Testsite
  Shannon_Diversity_Calculated = unlist(shannon_results)  # Convert list to numeric vector
)

# View the new dataframe
print(diversity_table)

write.csv(diversity_table, "D:/MasterThesis/gound_data/datasets/Shannon_Diversity.csv", row.names=FALSE)
