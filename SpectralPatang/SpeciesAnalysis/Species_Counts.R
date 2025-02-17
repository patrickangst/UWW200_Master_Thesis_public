# Clear workspace
rm(list = ls(all = TRUE))
gc()
graphics.off()


# Load necessary packages
library("readxl")
library("dplyr")
library("vegan")
library("writexl")

# Specify file paths
# base_folder <- 'D:/MasterThesis'
base_folder <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis'
input_excel_file <- file.path(base_folder, 'gound_data/datasets/All_plots.xlsx')
metrics_file_path <- file.path(base_folder, '07_Testsite_Metrics', 'Metrics.xlsx')
shannon_diversity_plotlevel_file_path <- file.path(base_folder, '07_Testsite_Metrics', 'Shannon_Diversity_Plotlevel.xlsx')

# Read the input data (specify the sheet name)
my_data <- read_excel(input_excel_file, sheet = "input")

# Function to calculate Shannon Diversity
calculate_species_number <- function(df) {

  # Get the position of the 'Number of species' column
  num_spec_pos <- which(names(df) == 'Number of species')

  # Calculate the average of the "Richness" column
  avg_species_count <- mean(df$'Number of species', na.rm = TRUE)  # na.rm=TRUE ensures NAs are ignored

  # Calculate the maximum of the "Richness" column
  max_species_count <- max(df$'Number of species', na.rm = TRUE)  # na.rm=TRUE ensures NAs are ignored

  # Calculate the minimum of the "Richness" column
  min_species_count <- min(df$'Number of species', na.rm = TRUE)  # na.rm=TRUE ensures NAs are ignored

  # Print the results
  return(list(avg=avg_species_count, min=min_species_count, max=max_species_count))
}

# Get the position of 'Number of species' column
number_species_pos <- which(names(my_data) == 'Number of species')

# Select relevant columns
plot_data_filtered <- my_data %>%
  dplyr::select('Testsite','Shannon','Evenness','Simpson','Number of species')

# Split data by 'Testsite'
df_list <- split(plot_data_filtered, plot_data_filtered$Testsite)

# Calculate Shannon Diversity for all test sites
species_count_results <- lapply(df_list, calculate_species_number)

result_list <- list()

# Loop through each element of the species_count_results
for (name in names(species_count_results)) {
  # Extract the values (avg, min, max) for each list
  avg_value <- species_count_results[[name]]$avg
  min_value <- species_count_results[[name]]$min
  max_value <- species_count_results[[name]]$max

  # Store the result as a list (to add later to the dataframe)
  result_list[[name]] <- c(Testsite = name, Species_Ground_AVG = avg_value, Species_Ground_MIN = min_value, Species_Ground_MAX = max_value)
}

# Convert the result list to a dataframe
df <- do.call(rbind, result_list)

# Convert row names to a separate column "Testsite"
df <- as.data.frame(df)
df$Testsite <- rownames(df)

# Reset rownames
rownames(df) <- NULL

# Reorder the columns to match the desired format
df <- df[, c("Testsite", "Species_Ground_AVG", "Species_Ground_MIN", "Species_Ground_MAX")]

# Print the dataframe
print(df)

#read excel with pca info
metrics_data <- read_excel(metrics_file_path, sheet = "Sheet1")
shannon_diversity_plotlevel <- read_excel(shannon_diversity_plotlevel_file_path, sheet = "Sheet1")

for( i in rownames(df) ){
  test_site_name <- df[i, "Testsite"]
  Species_Ground_AVG <- as.numeric(df[i, "Species_Ground_AVG"])
  Species_Ground_MIN <- as.numeric(df[i, "Species_Ground_MIN"])
  Species_Ground_MAX <- as.numeric(df[i, "Species_Ground_MAX"])

  print(paste0(test_site_name, ' ',Species_Ground_AVG,' ',Species_Ground_MIN,' ',Species_Ground_MAX))

  # Update the 'SpectralSpecies' column
  metrics_data$Species_Ground_AVG[metrics_data$Plot_Location_Shp_Subset_Name == test_site_name] <- Species_Ground_AVG
  metrics_data$Species_Ground_MIN[metrics_data$Plot_Location_Shp_Subset_Name == test_site_name] <- Species_Ground_MIN
  metrics_data$Species_Ground_MAX[metrics_data$Plot_Location_Shp_Subset_Name == test_site_name] <- Species_Ground_MAX

  shannon_diversity_plotlevel$Species_Ground_AVG[shannon_diversity_plotlevel$Testsite == test_site_name] <- Species_Ground_AVG
  shannon_diversity_plotlevel$Species_Ground_MIN[shannon_diversity_plotlevel$Testsite == test_site_name] <- Species_Ground_MIN
  shannon_diversity_plotlevel$Species_Ground_MAX[shannon_diversity_plotlevel$Testsite == test_site_name] <- Species_Ground_MAX

}

write_xlsx(metrics_data, path = metrics_file_path)
write_xlsx(shannon_diversity_plotlevel, path = shannon_diversity_plotlevel)
