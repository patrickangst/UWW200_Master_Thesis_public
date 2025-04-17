rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary libraries
library(readxl)
library(ggplot2)
library(tidyr)
library(ggrepel)


# Load your data
# base_folder <- 'D:/MasterThesis'
base_folder <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis'
file_path <- file.path(base_folder,'07_Testsite_Metrics', 'Shannon_Diversity_Plotlevel.xlsx')
output_folder_path <- file.path(base_folder,'07_Testsite_Metrics','plots')

diversity_data <- read_excel(file_path, sheet = "Sheet1")

#
# Pearson Correlation
#

# Compute  correlation test
correlation_pearson <- cor.test(diversity_data$Shannon_Diversity_Ground,
                        diversity_data$Shannon_Diversity_Spectral,
                        method = "pearson")

r_value_pearson <- correlation_pearson$estimate  # Correlation coefficient (r)
r_squared_pearson <- round(r_value_pearson^2, 3) # R² value
p_value_pearson <- correlation_pearson$p.value    # P-value from correlation test

# Print results
print(paste("Correlation coefficient (r):", round(r_value_pearson, 3)))
print(paste("R² value:", r_squared_pearson))
print(paste("P-value:", round(p_value_pearson, 5)))

# Interpretation of significance
if (p_value_pearson < 0.05) {
  print("The correlation is statistically significant (p < 0.05).")
} else {
  print("The correlation is NOT statistically significant (p >= 0.05).")
}

# Scatter plot with regression line and R² value
plot_pearson <- ggplot(diversity_data, aes(x = Shannon_Diversity_Ground, y = Shannon_Diversity_Spectral)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
  labs(title = "Pearson Correlation between Shannon Indices",
       x = "Shannon Diversity (Ground)",
       y = "Shannon Diversity (Spectral)") +
  theme_minimal() +
  annotate("text", x = min(diversity_data$Shannon_Diversity_Ground),
           y = max(diversity_data$Shannon_Diversity_Spectral),
           label = paste("R² =", r_squared_pearson, "\n p =", round(p_value_pearson, 5)),
           hjust = 0, size = 5, color = "black")

# Display the plot
print(plot_pearson)

plot_pearson_file_path <- file.path(output_folder_path, 'pearson_correlation_shannon_diversity_plotlevel.png')
ggsave(
  filename = plot_pearson_file_path,
  plot = plot_pearson,
  width = 6,
  height = 6,
  dpi = 400
)

# Scatter plot with colors for each location
plot_pearson2 <- ggplot(diversity_data, aes(x = Shannon_Diversity_Ground,
                                           y = Shannon_Diversity_Spectral,
                                           color = Testsite)) +  # Color points by location
  geom_point(size = 4, alpha = 0.8) +  # Scatter points
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Linear regression line
  geom_text_repel(aes(label = Testsite), size = 4) +  # Label points with location names
  labs(title = "Pearson Correlation between Shannon Indices",
       x = "Shannon Diversity (Ground)",
       y = "Shannon Diversity (Spectral)",
       color = "Testsite",
       caption = paste("R² =", r_squared_pearson, "| p =", p_value_pearson)) +  # Add R² and p-value below legend
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",  # Stack legend elements
    legend.margin = margin(t = 5, b = 5),  # Add margin around legend
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 12)  # Style caption
  )

# Display the plot
print(plot_pearson2)

# Save the plot
plot2_pearson_file_path <- file.path(output_folder_path, 'pearson_correlation_shannon_diversity_plotlevel_2.png')
ggsave(
  filename = plot2_pearson_file_path,
  plot = plot_pearson2,
  width = 7,
  height = 6,
  dpi = 400
)


#
# Spearman Correlation
#

# Compute Spearman correlation test
correlation_spearman <- cor.test(diversity_data$Shannon_Diversity_Ground,
                                diversity_data$Shannon_Diversity_Spectral,
                                method = "spearman")

r_value_spearman <- correlation_spearman$estimate  # Correlation coefficient (r)
r_squared_spearman <- round(r_value_spearman^2, 3) # R² value
p_value_spearman <- correlation_spearman$p.value    # P-value from correlation test

# Print results
print(paste("Correlation coefficient (r):", round(r_value_spearman, 3)))
print(paste("R² value:", r_squared_spearman))
print(paste("P-value:", round(p_value_spearman, 5)))

# Interpretation of significance
if (p_value_spearman < 0.05) {
  print("The correlation is statistically significant (p < 0.05).")
} else {
  print("The correlation is NOT statistically significant (p >= 0.05).")
}

# Scatter plot with regression line and R² value
plot_spearman <- ggplot(diversity_data, aes(x = Shannon_Diversity_Ground, y = Shannon_Diversity_Spectral)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
  labs(title = "Spearman Correlation between Shannon Indices",
       x = "Shannon Diversity (Ground)",
       y = "Shannon Diversity (Spectral)") +
  theme_minimal() +
  annotate("text", x = min(diversity_data$Shannon_Diversity_Ground),
           y = max(diversity_data$Shannon_Diversity_Spectral),
           label = paste("R² =", r_squared_spearman, "\n p =", round(p_value_spearman, 5)),
           hjust = 0, size = 5, color = "black")

# Display the plot
print(plot_spearman)

plot_spearman_file_path <- file.path(output_folder_path, 'spearman_correlation_shannon_diversity_plotlevel.png')
ggsave(
  filename = plot_spearman_file_path,
  plot = plot_spearman,
  width = 6,
  height = 6,
  dpi = 400
)


# Barplot

# Reshape the data from wide to long format
diversity_data_long <- diversity_data %>%
  pivot_longer(cols = c(Shannon_Diversity_Ground, Shannon_Diversity_Spectral),
               names_to = "Diversity_Type",
               values_to = "Diversity_Value")

# Create the bar plot with renamed legend labels and title
barplot_diversity_grouped <- ggplot(diversity_data_long, aes(x = Testsite, y = Diversity_Value, fill = Diversity_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Shannon Diversity: Ground vs Spectral",
    y = "Shannon Diversity Index",
    x = "Testsite",
    fill = "Diversity"
  ) +
  scale_fill_manual(
    values = c("Shannon_Diversity_Ground" = "#66a182", "Shannon_Diversity_Spectral" = "#b85042"),  # Optional: set specific colors
    labels = c("Ground Diversity", "Spectral Diversity")  # Rename the legend labels
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(barplot_diversity_grouped)

# Save the plot as a PNG
barplot_diversity_grouped_file_path <- file.path(output_folder_path, 'barplot_diversity_grouped.png')
ggsave(
  filename = barplot_diversity_grouped_file_path,
  plot = barplot_diversity_grouped,
  width = 9,
  height = 6,
  dpi = 400
)


# # Load necessary libraries
# library(readxl)
# library(ggplot2)
#
# # Load your data
# base_folder <- 'D:/MasterThesis'
# file_path <- file.path(base_folder,'07_Testsite_Metrics', 'Shannon_Diversity_Plotlevel.xlsx')
#
# diversity_data <- read_excel(file_path)
#
# # Compute Pearson correlation
# correlation_pearson <- cor.test(diversity_data$Shannon_Diversity_Ground,
#                                 diversity_data$Shannon_Diversity_Spectral,
#                                 method = "pearson")
#
# print(correlation_pearson)
#
# r_value_pearson <- correlation_pearson$estimate  # Correlation coefficient (r)
# r_squared_pearson <- round(r_value_pearson^2, 3) # R² value
#
# # Create scatter plot with regression line
# plot_pearson <- ggplot(diversity_data, aes(x = Shannon_Diversity_Ground, y = Shannon_Diversity_Spectral)) +
#   geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter points
#   geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
#   labs(title = "Correlation between Shannon Indices Pearson Correlation",
#        x = "Shannon Diversity (Ground)",
#        y = "Shannon Diversity (Spectral)") +
#   theme_minimal() +
#   annotate("text", x = min(diversity_data$Shannon_Diversity_Ground),
#            y = max(diversity_data$Shannon_Diversity_Spectral),
#            label = paste("R² =", r_squared_pearson),
#            hjust = 0, size = 5, color = "black")
#
# # Display the plot
# print(plot_pearson)
#
#
# # Compute Spearman correlation
# correlation_spearman <- cor.test(diversity_data$Shannon_Diversity_Ground,
#                                  diversity_data$Shannon_Diversity_Spectral,
#                                  method = "spearman")
#
# print(correlation_spearman)
#
# r_value_spearman <- correlation_spearman$estimate  # Correlation coefficient (r)
# r_squared_spearman <- round(r_value_spearman^2, 3) # R² value
#
# # Create scatter plot with regression line
# plot_pearson <- ggplot(diversity_data, aes(x = Shannon_Diversity_Ground, y = Shannon_Diversity_Spectral)) +
#   geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter points
#   geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
#   labs(title = "Correlation between Shannon Indices Spearman Correlation",
#        x = "Shannon Diversity (Ground)",
#        y = "Shannon Diversity (Spectral)") +
#   theme_minimal() +
#   annotate("text", x = min(diversity_data$Shannon_Diversity_Ground),
#            y = max(diversity_data$Shannon_Diversity_Spectral),
#            label = paste("R² =", r_squared_spearman),
#            hjust = 0, size = 5, color = "black")
#
# # Display the plot
# print(plot_pearson)
#
