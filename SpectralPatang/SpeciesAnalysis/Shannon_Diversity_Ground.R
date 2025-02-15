# Clear workspace
rm(list = ls())
graphics.off()

# Load necessary packages
library("readxl")
library("dplyr")
library("vegan")
library("writexl")  # New package for writing Excel

# Specify file paths
base_folder <- 'D:/MasterThesis'
input_excel_file <- file.path(base_folder, 'gound_data/datasets/All_plots.xlsx')
output_excel_file <- file.path(base_folder, '07_Testsite_Metrics', 'Shannon_Diversity_Plotlevel.xlsx')

# Read the input data (specify the sheet name)
my_data <- read_excel(input_excel_file, sheet = "input")

# Function to calculate Shannon Diversity
calculate_shannon <- function(df) {

  # Get the position of the 'Number of species' column
  num_spec_pos <- which(names(df) == 'Number of species')

  # Select only the species abundance columns
  shannon_df <- df %>%
    dplyr::select(all_of(names(df)[(num_spec_pos + 1):ncol(df)]))

  # Replace NA with 0
  shannon_df[is.na(shannon_df)] <- 0

  # Keep only columns with at least one non-zero value
  shannon_df_cleaned <- shannon_df %>%
    select(where(~ any(. != 0)))

  # Ensure only numeric columns are kept
  shannon_df_cleaned <- select(shannon_df_cleaned, where(is.numeric))

  # Sum species abundance across all plots (rows) for the site
  combined_abundance <- colSums(shannon_df_cleaned, na.rm = TRUE)

  # Normalize data
  total_abundance <- sum(combined_abundance)
  combined_abundance <- combined_abundance / total_abundance

  # Calculate and return Shannon diversity index
  return(diversity(combined_abundance, index = "shannon"))
}

# Get the position of 'Number of species' column
number_species_pos <- which(names(my_data) == 'Number of species')

# Select relevant columns
plot_data_filtered <- my_data %>%
  dplyr::select('Dataset','Testsite','Releve number','Year','Month','Day','Elevation  (m)', 'Releve area (m2)',
                'Cover total vegetation (%)','Cover trees (%)','Cover total shrubs (%)','Cover herb layer (%)',
                'Cover moss layer (%)','Cover lichen (%)','Cover algae (%)','Cover litter (%)','Cover water (%)',
                'Cover rock (%)','Longitude','Latitude','Plant community name','Shannon','Evenness','Simpson',
                'Number of species', all_of(names(my_data)[(number_species_pos + 1):ncol(my_data)]))

# Replace -9 with NA
plot_data_filtered <- plot_data_filtered %>%
  mutate(across(everything(), ~ replace(.x, .x == -9, NA)))

# Split data by 'Testsite'
df_list <- split(plot_data_filtered, plot_data_filtered$Testsite)

# Calculate Shannon Diversity for all test sites
shannon_results <- lapply(df_list, calculate_shannon)

# Convert results to a data frame
diversity_table <- data.frame(
  Testsite = names(df_list),  # Use list element names as Testsite
  Shannon_Diversity_Ground = unlist(shannon_results)  # Convert list to numeric vector
)

# View the result
print(diversity_table)

# Write to Excel file
write_xlsx(diversity_table, output_excel_file)

# Confirmation message
cat("Shannon diversity results successfully saved to:", output_excel_file, "\n")
