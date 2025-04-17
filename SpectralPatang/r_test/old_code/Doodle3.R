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
species_data <- read_excel("All_plots_Desktop.xlsx", sheet = 'Sheet2')
colnames(species_data) <- trimws(colnames(species_data))
# colnames(species_data)

# df <- species_data %>%
#   as.data.frame() %>%
#   filter(Testsite == "PRUARC_DW_1")

df <- species_data %>%
  as.data.frame()

df_backup <- df

test_sites <- unique(df$Testsite)

for (site in test_sites) {
  print(site)
}

df <- df %>%
  filter(Testsite == "FRST_AK_2")

rownames(df) <- df$PlotIdentifier

# df_species_list <- df %>%
#   select(where(is.numeric)) %>% # Select only numeric columns
#   select(where(~ sum(.) != 0)) %>%
#   filter(rowSums(.) != 0)


df_species_list <- df %>%
  select(where(is.numeric)) %>%
  # mutate(across(everything(), ~ ifelse(. <= 5, 0, .))) %>%
  filter(rowSums(.) != 0) %>%
  select(where(~ sum(.) != 0))



# filter out species with only 1 % of coverage in a plot

df_species_list <- df_species_list %>%
  mutate(across(everything(), ~ . / rowSums(across(everything()))))

df_species_list_backup <- df_species_list

shannon_idx <- vegan::diversity(df_species_list, index = "shannon")
simpson_idx <- vegan::diversity(df_species_list, index = "simpson")

df_species_list_diversity_idx <- df_species_list

df_species_list_diversity_idx$Shannon <- shannon_idx
df_species_list_diversity_idx$Simpson <- simpson_idx

df_species_list_diversity_idx$Richness <- rowSums(df_species_list > 0)

# Calculate evenness (Shannon index divided by log of richness)
df_species_list_diversity_idx$Evenness <- ifelse(
  df_species_list_diversity_idx$Richness > 1, 
  df_species_list_diversity_idx$Shannon / log(df_species_list_diversity_idx$Richness),
  0 # Assign 0 if Richness is 1 or 0 (to avoid division by zero)
)

df_species_list_diversity_idx$PlotIdentifier <- rownames(df_species_list_diversity_idx)


df_species_list_diversity_idx <- df_species_list_diversity_idx %>%
  select(PlotIdentifier, Richness,Evenness, Shannon, Simpson, everything())


# Write to Excel file
write.xlsx(df_species_list_diversity_idx, "FRST_AK_2.xlsx", rowNames = FALSE)














beta_dist <- vegdist(df_species_list, method = "bray")  # Bray-Curtis dissimilarity
beta_matrix <- as.matrix(beta_dist)

hc <- hclust(beta_dist, method = "ward.D2")  # Ward’s method (minimizes variance)
# hc <- hclust(beta_dist, method = "average")
plot(hc, labels = rownames(df_species_list), main = "Hierarchical Clustering of Plot Sites")



# DBSCAN
k <- floor(sqrt(nrow(beta_matrix)))
knn_distances <- kNNdist(beta_matrix, k = k)
sorted_distances <- sort(knn_distances)

elbow_index <- which.max(diff(diff(sorted_distances)))

optimal_eps <- sorted_distances[elbow_index]

# optimal_eps <- 1

abline(h = optimal_eps, col = "red", lty = 2)  # Add detected elbow point
print(optimal_eps)

minPts_calculated <- (dim(beta_matrix)[1] / 10)
# minPts_calculated <- 5

db <- dbscan(beta_matrix, eps = optimal_eps, minPts = minPts_calculated)  # Adjust eps and minPts as needed

# Add clusters to the dataset
df_species_list$Cluster <- as.factor(db$cluster)

df_species_list <- df_species_list %>%
  select(Cluster, everything())

# Perform Principal Coordinates Analysis (PCoA)
pcoa_result <- cmdscale(beta_dist, k = 2, eig = TRUE)  # k=2 for 2D plotting

# Convert to dataframe
pcoa_df <- as.data.frame(pcoa_result$points)
colnames(pcoa_df) <- c("PCoA1", "PCoA2")

# Add clusters from DBSCAN
pcoa_df$Cluster <- as.factor(db$cluster)
pcoa_df$PlotID <- rownames(df_species_list)  # Keep track of plots

# Plot DBSCAN clusters in PCoA space
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = Cluster)) +
  geom_point(size = 4, alpha = 0.8) +
  theme_minimal() +
  labs(title = "DBSCAN Clustering (PCoA on Bray-Curtis)", color = "Cluster") +
  theme(legend.position = "right")








library(kohonen)

som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
som_model <- som(as.matrix(df_species_list_backup), grid = som_grid)
plot(som_model, type = "codes")

library(mclust)

gmm_result <- Mclust(as.matrix(df_species_list_backup))
summary(gmm_result)



library(stats)

pca_result <- prcomp(df_species_list_backup, center = TRUE, scale. = TRUE)

summary(pca_result)


dbscan_result <- dbscan(pca_result$x[, 1:5], eps = 0.5, minPts = 5)









# # Filter out columns with a sum less than 5
# # df_species_list_filtered <- df_species_list %>%
# #   select(where(~ sum(.) >= 5))
# # 
# 
# # Calculate the column sums
# column_sums <- colSums(df_species_list)
# 
# # Convert to data frame with column names
# df_sums <- data.frame(species = names(column_sums),
#                       sum = column_sums)
# 
# # Calculate the total sum of all columns
# total_sum <- sum(df_sums$sum)
# 
# # Calculate the proportion (as percentage) and add it to the data frame
# df_sums$proportion <- (df_sums$sum / total_sum) * 100
# 
# # Reorder factor levels by value
# df_sums$species <- factor(df_sums$species, levels = df_sums$species[order(df_sums$sum)])
# 
# # Bar plot sorted by sum
# ggplot(df_sums, aes(x = species, y = sum)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   labs(title = "Bar Plot of Column Sums (Sorted)",
#        x = "Species",
#        y = "Sum") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# 
# df_sums_filtered <- df_sums %>%
#   filter(proportion >= 0.5)
# 
# # Bar plot sorted by sum
# ggplot(df_sums_filtered, aes(x = species, y = sum)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   labs(title = "Bar Plot of Column Sums filtered (Sorted)",
#        x = "Species",
#        y = "Sum") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# df_nbclust <- df_species_list %>%
#   mutate(across(everything(), ~ ifelse(. <= 1, 0, .))) %>%
#   filter(rowSums(.) != 0) %>%
#   select(where(~ sum(.) != 0))
# 
# # Determine optimal clusters
# nb <- NbClust(data = df_species_list, distance = "euclidean", min.nc = 2, max.nc = 15, method = "kmeans")
# 
# # Get the suggested number of clusters
# optimal_clusters <- nb$Best.nc[1]
# print(paste("Optimal number of clusters:", optimal_clusters))
# 
# 
