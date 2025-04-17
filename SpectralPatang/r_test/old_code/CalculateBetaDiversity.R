rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary packages
library(vegan)
library(readxl)
library(dplyr)
library(NbClust)
library(dbscan)

# Load species abundance data
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'input')
colnames(species_data) <- trimws(colnames(species_data))
colnames(species_data)

index <- which(names(species_data) == 'Number of species')

# Extract the required columns
selected_columns <- species_data %>%
  dplyr::select('Table number', 'Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness',index:ncol(species_data)) %>%
  dplyr::mutate(PlotIdentifier = paste0(`Table number`,'_',Testsite)) %>%
  dplyr::select(PlotIdentifier, everything()) %>%
  dplyr::filter(Testsite == "FRST_AK_2")



# dataset_all_subset <- selected_columns %>%
#   dplyr::select(-c('Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness','Number of species', 'PlotIdentifier','Table number'))

dataset_all_subset <- selected_columns %>%
  dplyr::select(-c('Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness','Number of species', 'Table number'))

dataset_all_subset[is.na(dataset_all_subset)] <- 0
# 
# df <- dataset_all_subset %>%
#   rowwise() %>%
#   mutate(TotalSpecies = sum(c_across(-PlotIdentifier)))

df <- dataset_all_subset %>%
  mutate(across(-PlotIdentifier, ~ . / rowSums(across(-PlotIdentifier)))) %>%
  as.data.frame()
  


# df3 <- df2 %>%
#   rowwise() %>%
#   mutate(TotalSpecies = sum(c_across(-PlotIdentifier)))

rownames(df) <- df$PlotIdentifier

df <- df %>%
  dplyr::select(-PlotIdentifier)


# Convert to long format
df_long <- df %>%
  tibble::rownames_to_column(var = "PlotIdentifier") %>%
  pivot_longer(-PlotIdentifier, names_to = "Species", values_to = "Abundance") %>%
  drop_na()
  
df_filtered <- df_long %>% filter(Abundance > 0.06)

# Plot the species abundance curve
ggplot(df_filtered, aes(x = reorder(Species, -Abundance), y = Abundance, group = PlotIdentifier, color = PlotIdentifier)) +
  # geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Species", y = "Abundance", title = "Species Abundance Curve") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove legend

df <- dataset_all_subset

# Assume df is your original dataframe with plot locations as row names
df_clean <- df %>% na.omit()  # Remove missing values



df_clean <- df_clean %>%
  as.data.frame()

rownames(df_clean) <- df_clean$PlotIdentifier

df_clean <- df_clean %>%
  dplyr::select(-PlotIdentifier)

rownames(df_clean) <- df_clean$PlotIdentifier

df_scaled <- scale(df_clean)  # Normalize data to make comparisons fair










# dataset_all_subset[is.na(dataset_all_subset)] <- 0

# any(is.na(df))   # Check for NA values
# 
# which(is.na(df), arr.ind = TRUE)   # Locate NA values
# 
# df[is.na(df)] <- 0

# empty_rows <- rowSums(df) == 0
# which(empty_rows)  # Identifies the indices of empty rows

# df_clean <- df[rowSums(df) > 0, ]  # Keep rows with a row sum greater than 0


# Calculate Bray-Curtis beta diversity
bray_curtis_matrix <- vegdist(df_clean, method = "bray", na.rm = TRUE)

bray_curtis_matrix


################################################################################
#
################################################################################
bray_curtis_matrix[is.na(bray_curtis_matrix)] <- 0
bray_curtis_matrix[is.nan(bray_curtis_matrix)] <- 0

bray_curtis_matrix

# Perform hierarchical clustering
hc <- hclust(bray_curtis_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Hierarchical Clustering of Plots", sub = "", xlab = "Plots", ylab = "Dissimilarity")



################################################################################
# plot DBSCAN communities
################################################################################

# Convert distance matrix to a suitable format
bray_coords <- cmdscale(bray_curtis_matrix, k = 2)

# Run DBSCAN with an automatically chosen epsilon
db <- dbscan(bray_coords, eps = 0.1, minPts = 5)

# Add clusters to the dataset
df_clean$Community <- as.factor(db$cluster)

df_clean <- df_clean %>%
  dplyr::select(Community, everything())

# Visualize
library(ggplot2)
ggplot(as.data.frame(bray_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering of Plant Communities", color = "Community")



################################################################################
# NbClust on bray_coords
################################################################################

# Run NbClust to determine the best number of clusters
nb <- NbClust(data = bray_coords, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# Get the suggested number of clusters
best_k <- nb$Best.nc[1]  
print(paste("Optimal number of clusters:", best_k))





library(dbscan)
kNNdistplot(bray_coords, k = 2)  # k = minPts - 1
abline(h = 0.2, col = "red", lty = 2)  # Adjust 0.2 based on the elbow point









# Example using Euclidean
euclidean_matrix <- dist(dataset_all_subset, method = "euclidean")
euclidean_coords <- cmdscale(euclidean_matrix, k = 2)

db <- dbscan(euclidean_coords, eps = 0.2, minPts = 3)
ggplot(as.data.frame(euclidean_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "DBSCAN with Euclidean", color = "Community")


dataset_all_subset <- dataset_all_subset %>%
  dplyr::select(-Community)
  
# Example using Jaccard
# Remove rows with all zero values
dataset_all_subset_clean <- dataset_all_subset[rowSums(dataset_all_subset) > 0, ]

# Recompute the Jaccard dissimilarity matrix
jaccard_matrix <- vegdist(dataset_all_subset_clean, method = "jaccard", binary = TRUE)

hc <- hclust(jaccard_matrix, method = "ward.D2")
plot(hc, main = "Hierarchical Clustering with Jaccard", xlab = "Plots", ylab = "Dissimilarity")

























library(vegan)

# Example community matrix
community_data <- matrix(
  c(2, 3, 0,
    1, 0, 4,
    0, 2, 5),
  nrow = 3, byrow = TRUE
)
rownames(community_data) <- c("Site1", "Site2", "Site3")
colnames(community_data) <- c("Species1", "Species2", "Species3")

# Using vegdist
dissimilarity <- vegdist(community_data, method = "bray")
print(dissimilarity)






dataset_FRST_AK_3 <- selected_columns %>%
  dplyr::filter(Testsite == "FRST_AK_3")

dataset_FRST_AK_3_beta_subset <- dataset_FRST_AK_3 %>%
  dplyr::select(-c('Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness','Number of species', 'PlotIdentifier','Table number'))

dataset_FRST_AK_3_beta_subset[is.na(dataset_FRST_AK_3_beta_subset)] <- 0

# Calculate Bray-Curtis beta diversity
bray_curtis_matrix <- vegdist(dataset_FRST_AK_3_beta_subset, method = "bray")

bray_curtis_matrix


################################################################################
#
################################################################################

# Perform hierarchical clustering
hc <- hclust(bray_curtis_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Hierarchical Clustering of Plots", sub = "", xlab = "Plots", ylab = "Dissimilarity")



################################################################################
# plot DBSCAN communities
################################################################################

# Convert distance matrix to a suitable format
bray_coords <- cmdscale(bray_curtis_matrix, k = 2)

# Run DBSCAN with an automatically chosen epsilon
db <- dbscan(bray_coords, eps = 0.2, minPts = 3)

# Add clusters to the dataset
dataset_FRST_AK_3_beta_subset$Community <- as.factor(db$cluster)

# Visualize
library(ggplot2)
ggplot(as.data.frame(bray_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering of Plant Communities", color = "Community")



################################################################################
# NbClust on bray_coords
################################################################################

# Run NbClust to determine the best number of clusters
nb <- NbClust(data = bray_coords, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# Get the suggested number of clusters
best_k <- nb$Best.nc[1]  
print(paste("Optimal number of clusters:", best_k))






# # Perform PCoA
# pcoa_result <- cmdscale(bray_curtis_matrix, eig = TRUE, k = 2)
# 
# # Convert to dataframe
# pcoa_df <- as.data.frame(pcoa_result$points)
# pcoa_df$Plot <- rownames(pcoa_df)
# 
# # Plot
# ggplot(pcoa_df, aes(x = V1, y = V2, label = Plot)) +
#   geom_point(color = "blue", size = 3) +
#   geom_text(vjust = -1) +
#   theme_minimal() +
#   labs(title = "PCoA of Plant Communities", x = "Axis 1", y = "Axis 2")
# 
# 
# # Perform NMDS
# nmds_result <- metaMDS(dataset_FRST_AK_3_beta_subset, distance = "bray", k = 2, trymax = 100)
# 
# # Plot NMDS
# plot(nmds_result, type = "t", main = "NMDS of Plant Communities")


# Run PERMANOVA
# adonis2(bray_curtis_matrix ~ dataset_FRST_AK_3_beta_subset$PlotGroup, data = dataset_FRST_AK_3_beta_subset)






#
##
#
##
#
dataset_ATQ_VK_1 <- selected_columns %>%
  dplyr::filter(Testsite == "ATQ_VK_1")

dataset_ATQ_VK_1_beta_subset <- dataset_ATQ_VK_1 %>%
  dplyr::select(-c('Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness','Number of species'))

dataset_ATQ_VK_1_beta_subset[is.na(dataset_ATQ_VK_1_beta_subset)] <- 0

# Calculate Bray-Curtis beta diversity
bray_curtis_matrix <- vegdist(dataset_ATQ_VK_1_beta_subset, method = "bray", na.rm = )

bray_curtis_matrix


################################################################################
#
################################################################################

# Perform hierarchical clustering
hc <- hclust(bray_curtis_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Hierarchical Clustering of Plots", sub = "", xlab = "Plots", ylab = "Dissimilarity")



################################################################################
# plot DBSCAN communities
################################################################################

# Convert distance matrix to a suitable format
bray_coords <- cmdscale(bray_curtis_matrix, k = 2)

# Run DBSCAN with an automatically chosen epsilon
db <- dbscan(bray_coords, eps = 0.2, minPts = 3)

# Add clusters to the dataset
dataset_ATQ_VK_1_beta_subset$Community <- as.factor(db$cluster)

# Visualize
library(ggplot2)
ggplot(as.data.frame(bray_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering of Plant Communities", color = "Community")



################################################################################
# NbClust on bray_coords
################################################################################

# Run NbClust to determine the best number of clusters
nb <- NbClust(data = bray_coords, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# Get the suggested number of clusters
best_k <- nb$Best.nc[1]  
print(paste("Optimal number of clusters:", best_k))





#
##
#
##
#
dataset_FLXTWRZONA_SD_2 <- selected_columns %>%
  dplyr::filter(Testsite == "FLXTWRZONA_SD_2")

dataset_FLXTWRZONA_SD_2_beta_subset <- dataset_FLXTWRZONA_SD_2 %>%
  dplyr::select(-c('Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness','Number of species'))

dataset_FLXTWRZONA_SD_2_beta_subset[is.na(dataset_FLXTWRZONA_SD_2_beta_subset)] <- 0

# Calculate Bray-Curtis beta diversity
bray_curtis_matrix <- vegdist(dataset_FLXTWRZONA_SD_2_beta_subset, method = "bray", na.rm = )

bray_curtis_matrix


################################################################################
#
################################################################################

# Perform hierarchical clustering
hc <- hclust(bray_curtis_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Hierarchical Clustering of Plots", sub = "", xlab = "Plots", ylab = "Dissimilarity")



################################################################################
# plot DBSCAN communities
################################################################################

# Convert distance matrix to a suitable format
bray_coords <- cmdscale(bray_curtis_matrix, k = 2)

# Run DBSCAN with an automatically chosen epsilon
db <- dbscan(bray_coords, eps = 0.2, minPts = 3)

# Add clusters to the dataset
dataset_FLXTWRZONA_SD_2_beta_subset$Community <- as.factor(db$cluster)

# Visualize
library(ggplot2)
ggplot(as.data.frame(bray_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering of Plant Communities", color = "Community")



################################################################################
# NbClust on bray_coords
################################################################################

# Run NbClust to determine the best number of clusters
nb <- NbClust(data = bray_coords, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

# Get the suggested number of clusters
best_k <- nb$Best.nc[1]  
print(paste("Optimal number of clusters:", best_k))










#
##
#
##
#
# dataset_all_sites <- selected_columns %>%
#   dplyr::filter(Testsite != "")




