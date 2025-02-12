# Clear workspace
rm(list = ls())
graphics.off()

library(vegan)
library(ggplot2)
library(dplyr)

# Example: 10 plots (Releve_number) with 5 species
set.seed(123)
df <- data.frame(
  Releve_number = 1:10,  # Unique plot IDs
  Species_A = runif(10, 0, 100),
  Species_B = runif(10, 0, 100),
  Species_C = runif(10, 0, 100),
  Species_D = runif(10, 0, 100),
  Species_E = runif(10, 0, 100)
)

# View data
head(df)

# Remove Releve_number for calculation
species_data <- df %>% select(-Releve_number)

# Calculate Bray-Curtis Dissimilarity
beta_div <- vegdist(species_data, method = "bray")

# View the distance matrix
as.matrix(beta_div)


# Run NMDS
nmds_result <- metaMDS(beta_div, k = 2, trymax = 100)

# Convert NMDS results to a dataframe
nmds_df <- data.frame(
  NMDS1 = nmds_result$points[,1],
  NMDS2 = nmds_result$points[,2],
  Releve_number = df$Releve_number
)

# Plot NMDS
ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, label = Releve_number)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1) +
  theme_minimal() +
  labs(title = "NMDS of Beta Diversity (Bray-Curtis)",
       x = "NMDS1", y = "NMDS2")

# Run PCoA
pcoa_result <- cmdscale(beta_div, k = 2, eig = TRUE)

# Convert to dataframe
pcoa_df <- data.frame(
  PCoA1 = pcoa_result$points[,1],
  PCoA2 = pcoa_result$points[,2],
  Releve_number = df$Releve_number
)

# Plot PCoA
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, label = Releve_number)) +
  geom_point(color = "red", size = 3) +
  geom_text(vjust = -1) +
  theme_minimal() +
  labs(title = "PCoA of Beta Diversity (Bray-Curtis)",
       x = "PCoA1", y = "PCoA2")
