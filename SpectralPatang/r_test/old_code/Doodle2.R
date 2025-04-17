rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary packages
library(vegan)
library(readxl)
library(dplyr)
library(NbClust)
library(dbscan)
library(ggplot2)
library(tidyr)

# Load species abundance data
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'Sheet2')
colnames(species_data) <- trimws(colnames(species_data))
# colnames(species_data)

df <- species_data %>%
  as.data.frame()

rownames(df) <- df$PlotIdentifier

df_species_list <- df %>%
  select(where(is.numeric)) %>% # Select only numeric columns
  select(where(~ sum(.) != 0)) %>%
  filter(rowSums(.) != 0)

df_species_list <- df_species_list %>%
  mutate(across(everything(), ~ . / rowSums(across(everything()))))



# Filter out columns with a sum less than 5
# df_species_list_filtered <- df_species_list %>%
#   select(where(~ sum(.) >= 5))
# 

# Calculate the column sums
column_sums <- colSums(df_species_list)

# Convert to data frame with column names
df_sums <- data.frame(species = names(column_sums),
                      sum = column_sums)

# Calculate the total sum of all columns
total_sum <- sum(df_sums$sum)

# Calculate the proportion (as percentage) and add it to the data frame
df_sums$proportion <- (df_sums$sum / total_sum) * 100

# Reorder factor levels by value
df_sums$species <- factor(df_sums$species, levels = df_sums$species[order(df_sums$sum)])

# Bar plot sorted by sum
ggplot(df_sums, aes(x = species, y = sum)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Column Sums (Sorted)",
       x = "Species",
       y = "Sum") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


















# Add Testsite column to df_species_list
df_species_list$Testsite <- df[rownames(df_species_list), "Testsite"]


df_species_list <- df_species_list %>%
  dplyr::select("Testsite", everything())


df_species_split_list <- split(df_species_list,df_species_list$Testsite)

for (testsite_name in names(df_species_split_list)) {
  df_species_split_list[[testsite_name]] <- df_species_split_list[[testsite_name]] %>%
    dplyr::select(-Testsite)
}


for (testsite in df_species_split_list) {
  
  df_species_list <- df %>%
    select(where(is.numeric)) %>% # Select only numeric columns
    select(where(~ sum(.) != 0)) %>%
    filter(rowSums(.) != 0)
  
  
}


shannon_alpha_div <- vegan::diversity(df_species_list, index = "shannon")

simpson_alpha_div <- vegan::diversity(df_species_list, index = "simpson")

df_species_list_diversity_idx <- df_species_list

df_species_list_diversity_idx$Shannon <- shannon_alpha_div
df_species_list_diversity_idx$Simpson <- simpson_alpha_div

# Add rownames as a new column for coloring
df_species_list_diversity_idx$Testsite <- sub("^[^_]+_", "", rownames(df_species_list))
df_species_list_diversity_idx$Plotsite <- rownames(df_species_list)

# Reshape the data for plotting (long format)
df_long <- pivot_longer(df_species_list_diversity_idx, 
                        cols = c("Shannon", "Simpson"),
                        names_to = "Index", 
                        values_to = "Diversity")

df_long <- df_long %>%
  select(Testsite, Plotsite, Index, Diversity, everything())


# # Plot: all Testsites in one figure, faceted
# ggplot(df_long, aes(x = Plotsite, y = Diversity, color = Plotsite)) +
#   geom_point(size = 3) +
#   facet_wrap(~ Testsite, scales = "free_x") +
#   labs(x = "Plotsite", y = "Diversity", title = "Diversity per Plotsite by Testsite") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     legend.position = "none",                # 🔥 Hides the huge legend
#     plot.title = element_text(hjust = 0.5)
#   )




# Loop over each Testsite and generate a plot
unique_testsites <- unique(df_long$Testsite)

for (site in unique_testsites) {
  df_subset <- df_long %>% filter(Testsite == site) %>%
    filter(Index == "Simpson")
  
  p <- ggplot(df_subset, aes(x = Plotsite, y = Diversity, color = Plotsite)) +
    geom_point(size = 3) +
    facet_wrap(~ Index, scales = "free_y") +  # if you want both Shannon & Simpson
    # Or just this if only Simpson: filter above and remove this line
    labs(title = paste("Diversity Index for Testsite:", site),
         x = "Plotsite",
         y = "Diversity") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  print(p)  # Display the plot
}







library(NbClust)

# Determine optimal clusters
nb <- NbClust(data = df_species_list, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")

# Get the suggested number of clusters
optimal_clusters <- nb$Best.nc[1]
print(paste("Optimal number of clusters:", optimal_clusters))




highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.9)
df_species_list <- df_species_list[, -highly_correlated]














# Plot
ggplot(df_long, aes(x = SampleGroup, y = Diversity, fill = SampleGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Index, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter only Simpson index
df_simpson <- df_long %>% filter(Index == "Simpson")

# Plot: point plot with coloring by SampleGroup
ggplot(df_simpson, aes(x = rownames(df_species_list_diversity_idx), 
                       y = Diversity, 
                       color = SampleGroup)) +
  geom_point(size = 3) +
  labs(x = "Sample", y = "Simpson Diversity", title = "Simpson Diversity Index per Sample") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Hide x-axis text if too many samples
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5))





















# Check for non numeric culumns
# char_cols <- names(df_species_list)[sapply(df_species_list, is.character)]
# factor_cols <- names(df_species_list)[sapply(df_species_list, is.factor)]
# logical_cols <- names(df_species_list)[sapply(df_species_list, is.logical)]
# numeric_cols <- names(df_species_list)[sapply(df_species_list, is.numeric)]
# 
# print(char_cols)   # Character columns
# print(factor_cols) # Factor columns
# print(logical_cols) # Factor columns
# print(length(numeric_cols)) # Factor columns

# alpha_div <- vegan::diversity(df_species_list, index = "shannon")
# 
# print(alpha_div)



# Function to calculate Whittaker's Beta diversity for each Testsite
calculate_whittaker_beta <- function(df) {
  testsite_list <- split(df, df$Testsite)  # Split dataset by Testsite
  
  beta_results <- lapply(testsite_list, function(sub_df) {
    sub_df <- select(sub_df, -Testsite)  # Remove Testsite column
    
    # Convert percentage data into presence/absence (1 if species present, 0 if absent)
    sub_df_pa <- as.data.frame((sub_df > 0) * 1)
    
    # Skip if all values are zero (no species present in the test site)
    if (all(colSums(sub_df_pa) == 0)) {
      return(NULL)
    }
    
    gamma <- specnumber(colSums(sub_df_pa))  # Total species richness in Testsite
    alpha <- mean(apply(sub_df_pa, 1, specnumber))  # Avg species richness per plot
    beta_whittaker <- (gamma / alpha) - 1  # Whittaker's Beta diversity
    
    return(data.frame(Testsite = unique(sub_df$Testsite)[1], gamma, alpha, beta_whittaker))
  })
  
  beta_diversity_df <- bind_rows(beta_results)  # Combine results into one dataframe
  return(beta_diversity_df)
}

# Compute beta diversity
whittaker_beta <- calculate_whittaker_beta(df_species_list)
print(whittaker_beta)




















df_only_species_normalized <- df_species_list %>%
  mutate(across(everything(), ~ . / rowSums(across(everything()))))


# Step 1: Calculate column sums
column_sums <- colSums(df_only_species_normalized)

# Step 2: Sort column sums in descending order
sorted_column_sums <- sort(column_sums, decreasing = TRUE)

# Step 3: Prepare the data for ggplot2
sorted_data <- data.frame(Species = names(sorted_column_sums), ColumnSum = sorted_column_sums)

# Step 4: Create the boxplot
ggplot(sorted_data, aes(x = reorder(Species, -ColumnSum), y = ColumnSum)) +
  geom_boxplot() +  # Create a boxplot
  coord_flip() +    # Flip coordinates for better readability
  theme_minimal() +
  labs(title = "Sorted Boxplot of Column Sums",
       x = "Species",
       y = "Column Sum")

df_thresholded <- df[, colSums(df) >= 1]
