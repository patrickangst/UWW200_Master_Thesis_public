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
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'input')
colnames(species_data) <- trimws(colnames(species_data))
colnames(species_data)

index <- which(names(species_data) == 'Number of species')

# Extract the required columns
selected_columns <- species_data %>%
  dplyr::select('Table number', 'Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness',index:ncol(species_data)) %>%
  dplyr::mutate(PlotIdentifier = paste0(`Table number`,'_',Testsite)) %>%
  dplyr::select(PlotIdentifier, everything())

index <- which(names(selected_columns) == 'Number of species')

selected_columns[is.na(selected_columns)] <- 0

selected_columns <- selected_columns %>%
  mutate(across((index + 1):ncol(.), as.numeric))


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


rownames(df) <- df$PlotIdentifier

df <- df %>%
  dplyr::select(-PlotIdentifier)

df[is.na(df)] <- 0

print(data.frame(Column = names(df), Sum = colSums(df)))

# hist(df)

FRST_AK_3_rows <- df[grep("FRST_AK_3$", rownames(df)), ]

# Identify columns with zero variance
zero_variance_columns <- sapply(FRST_AK_3_rows, function(col) var(col) == 0)

# Remove constant/zero-variance columns
FRST_AK_3_rows_clean <- FRST_AK_3_rows[, !zero_variance_columns]

df_standardized <- scale(FRST_AK_3_rows_clean)


pca_result <- prcomp(df_standardized, scale. = TRUE)



# Variance explained
explained_variance <- summary(pca_result)$importance[2, ]
cumulative_variance <- summary(pca_result)$importance[3, ]

# Identify the number of PCs
num_pcs <- which(cumulative_variance >= 0.99)[1]
print(paste("Number of PCs explaining 99% variance:", num_pcs))

# Extract these PCs
selected_pcs <- pca_result$x[, 1:num_pcs]


# Convert the matrix to a data frame and add row names
long_data <- as.data.frame(selected_pcs) %>%
  mutate(RowName = rownames(selected_pcs)) %>%  # Add rownames as a column
  pivot_longer(-RowName, names_to = "Variable", values_to = "Value")  # Convert to long format

# Plot each row separately
ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_line(aes(group = RowName), color = "blue") +  # Line plot
  facet_wrap(~RowName, ncol = 1, scales = "free_y") +  # One plot per row
  theme_minimal() +
  labs(title = "Plots for Each Row", x = "Principal Component", y = "Score")



# Create a bar plot for each row
ggplot(long_data, aes(x = Variable, y = Value, fill = RowName)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar plot for each row
  facet_wrap(~RowName, ncol = 1, scales = "free_y") +  # One plot per row
  theme_minimal() +
  labs(title = "Bar Plots for Each Row", x = "Principal Component", y = "Score")

# Create a bar plot for each row
# Convert the matrix to a data frame and add row names
long_data_FRST_AK_3 <- as.data.frame(FRST_AK_3_rows_clean) %>%
  mutate(RowName = rownames(selected_pcs)) %>%  # Add rownames as a column
  pivot_longer(-RowName, names_to = "Variable", values_to = "Value")  # Convert to long format

# Create a bar plot for each row
ggplot(long_data_FRST_AK_3, aes(x = Variable, y = Value, fill = RowName)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar plot for each row
  facet_wrap(~RowName, ncol = 1, scales = "free_y") +  # One plot per row
  theme_minimal() +
  labs(title = "Bar Plots for Each Row", x = "Principal Component", y = "Score")


library(dbscan)

# Prepare data (standardized or reduced to principal components)
bray_coords <- cmdscale(dist(selected_pcs), k = 2)  # Example using PCA or distance matrix

# Perform DBSCAN
db <- dbscan(bray_coords, eps = 0.1, minPts = 3)  # Adjust eps and minPts as needed

# Add clusters to the dataset
selected_pcs$Cluster <- as.factor(db$cluster)

# Visualize clusters
ggplot(as.data.frame(bray_coords), aes(x = V1, y = V2, color = as.factor(db$cluster))) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering", color = "Cluster")


library(dbscan)
kNNdistplot(bray_coords, k = 4)  # k = minPts - 1
abline(h = 0.2, col = "red", lty = 2)  # Elbow method to determine `eps`




# Perform hierarchical clustering
dist_matrix <- dist(selected_pcs)  # Distance matrix
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "Plots", sub = "", ylab = "Height")

# Cut tree to create clusters (optional)
clusters <- cutree(hc, h = 5)  # Choose a cut height based on dendrogram
selected_pcs$Cluster <- as.factor(clusters)



library(NbClust)

# Determine optimal clusters
nb <- NbClust(data = selected_pcs, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")

# Get the suggested number of clusters
optimal_clusters <- nb$Best.nc[1]
print(paste("Optimal number of clusters:", optimal_clusters))






rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary packages
library(vegan)
library(readxl)
library(dplyr)
library(NbClust)
library(dbscan)
library(tibble)

# Load species abundance data
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'input')
colnames(species_data) <- trimws(colnames(species_data))
colnames(species_data)

index <- which(names(species_data) == 'Number of species')

# Extract the required columns
selected_columns <- species_data %>%
  dplyr::select('Table number', 'Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness',index:ncol(species_data)) %>%
  dplyr::mutate(PlotIdentifier = paste0(`Table number`,'_',Testsite)) %>%
  dplyr::select(PlotIdentifier, everything())


dataset_all_subset <- selected_columns %>%
  dplyr::select(-c('Dataset', 'Testsite', 'Subzone','Richness','Shannon','Evenness','Number of species', 'Table number'))

dataset_all_subset[is.na(dataset_all_subset)] <- 0


df <- dataset_all_subset %>%
  as.data.frame()
# 
# # df[is.na(df)] <- 0
# 
# # Assume df is your original dataframe with plot locations as row names
# df_clean <- df %>%
#   mutate(across(everything(), ~replace_na(., 0)))
# 
rownames(df) <- df$PlotIdentifier

df <- df %>%
  dplyr::select(-PlotIdentifier)


write.table(sapply(df, class), "column_classes.txt", sep = "\t", quote = FALSE)

char_cols <- names(df)[sapply(df, is.character)]
factor_cols <- names(df)[sapply(df, is.factor)]
logical_cols <- names(df)[sapply(df, is.logical)]

print(char_cols)   # Character columns
print(factor_cols) # Factor columns
print(logical_cols) # Factor columns

df <- df %>%
  mutate(across(everything(), as.numeric))

hist(df[[1]])

df_filtered <- df[, colSums(df != 0) > 0]  # Keep only columns with nonzero values
df_scaled <- scale(df_filtered)  # Scale only valid columns

# 
# dist_matrix <- vegdist(df_scaled, method = "bray", na.rm = FALSE)  # Bray-Curtis is good for ecological data
# 
# # Perform hierarchical clustering
# hc <- hclust(dist_matrix, method = "ward.D2")
# 
# # Plot the dendrogram
# plot(hc, labels = rownames(df_scaled), main = "Hierarchical Clustering of Vegetation Plots")
# 
# # Cut the tree into k clusters (choose k based on the dendrogram)
# k <- 3  # Example: Assign 3 clusters
# clusters <- cutree(hc, k = k)
# 
# # Add cluster labels to original dataframe
# df_clustered <- df_clean %>% mutate(Cluster = as.factor(clusters))
# dataset_all_subset[is.na(dataset_all_subset)] <- 0
# 
