# To evaluate the significance of species and determine if some are too insignificant to include in further analysis, you can perform the following steps in R:
#
# 1. Statistical Approach
# Relative Abundance: Calculate the proportion of each species' total abundance relative to the total abundance of all species. Species with low proportions may be considered insignificant.
#
# Variance Analysis: Evaluate the variability of each species across samples. Species with very low variance might contribute little information.
#
# Plot Significance: Create visualizations such as a cumulative proportion curve or bar plots highlighting significant vs. insignificant species.



# clean environment
rm(list=ls(all=TRUE));gc()
graphics.off()

library(ggplot2)

data <- read.csv("species_csv/14_Flux_Towers_Zona_Species_List.csv", header = TRUE, fileEncoding = "latin1")

species_data <- data[, -c(1, ncol(data))]  # Remove the first and last columns

# Calculate total abundance for each species
species_sums <- sort(colSums(species_data, na.rm = TRUE), decreasing = TRUE)

# Calculate relative abundance
total_abundance <- sum(species_sums)  # Total abundance across all species
relative_abundance <- species_sums / total_abundance * 100  # Convert to percentages

# Variance of each species across samples
species_variance <- apply(species_data, 2, var, na.rm = TRUE)

# Set threshold for significance (e.g., minimum relative abundance of 1%)
threshold <- 1
significant_species <- names(relative_abundance[relative_abundance >= threshold])
insignificant_species <- names(relative_abundance[relative_abundance < threshold])

# Calculate relative abundance
relative_abundance_df <- data.frame(
  Species = names(relative_abundance),
  RelativeAbundance = relative_abundance
)

# Create the plot
ggplot(relative_abundance_df, aes(x = reorder(Species, -RelativeAbundance), y = RelativeAbundance)) +
  geom_bar(stat = "identity", aes(fill = RelativeAbundance >= threshold), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "lightgray")) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = nrow(relative_abundance_df) / 2, y = threshold + 0.5,
           label = paste("Threshold =", threshold, "%"), color = "red", size = 4, angle = 0, hjust = 0.5) +
  labs(
    title = "Relative Abundance of Species",
    x = "Species",
    y = "Relative Abundance (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.x = element_line(color = "black")
  )


# Print summary
cat("Total species:", length(species_sums), "\n")
cat("Significant species (>= 1%):", length(significant_species), "\n")
cat("Insignificant species (< 1%):", length(insignificant_species), "\n\n")

cat("Significant Species:\n")
print(significant_species)



# Define threshold for cumulative percentage
cumulative_threshold <- 99  # You can change this value to any percentage

# Prepare the data for Pareto chart
species_df <- data.frame(
  Species = names(species_sums),
  Abundance = species_sums
)

# Sort species by abundance in descending order and calculate cumulative percentage
species_df <- species_df[order(-species_df$Abundance), ]
species_df$Cumulative <- cumsum(species_df$Abundance) / sum(species_df$Abundance) * 100

# Find the number of species required for the given cumulative threshold
n_species_threshold <- which(species_df$Cumulative >= cumulative_threshold)[1]  # First species to exceed the threshold

# Create Pareto chart
ggplot(species_df, aes(x = reorder(Species, -Abundance), y = Abundance)) +
  # Bar chart
  geom_bar(stat = "identity", fill = "steelblue") +

  # Cumulative line
  geom_line(aes(y = (Cumulative / 100) * max(Abundance), group = 1), color = "red", size = 1) +
  geom_point(aes(y = (Cumulative / 100) * max(Abundance)), color = "red", size = 2) +

  # Threshold line
  geom_hline(yintercept = (cumulative_threshold / 100) * max(species_df$Abundance),
             linetype = "dashed", color = "darkgreen", size = 1) +

  # Annotate the number of species required for the threshold
  annotate("text", x = n_species_threshold,
           y = (cumulative_threshold / 100) * max(species_df$Abundance) * 0.9,
           label = paste(n_species_threshold, "species for", cumulative_threshold, "%"),
           color = "darkgreen", angle = 45, hjust = 1) +

  # Customize y-axis with dual axes
  scale_y_continuous(
    name = "Abundance",
    sec.axis = sec_axis(~ . / max(species_df$Abundance) * 100, name = "Cumulative Percentage")
  ) +

  # Titles and labels
  labs(title = paste("Pareto Chart of Species Abundance (", cumulative_threshold, "% Threshold)", sep = ""),
       x = "Species", y = "Abundance") +

  # Rotate x-axis labels and add axis line
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.x = element_line(color = "black")  # Add x-axis line
  )
