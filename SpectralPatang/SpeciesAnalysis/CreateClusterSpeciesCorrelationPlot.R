# clean environment
rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary libraries
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)

# gound_data_path <- 'D:/MasterThesis/gound_data'
gound_data_path <- '~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/gound_data'
excel_file_path <- file.path(gound_data_path, 'Flightstrip_Plot_Mapping.xlsx')
plot_file_path <- file.path(gound_data_path, 'Cluster_Species_Correlation.png')

df <- read_excel(excel_file_path)

# Ensure column names match exactly
colnames(df) <- make.names(colnames(df))  # Standardizes column names

# Convert data from wide to long format
df_long <- df %>%
  pivot_longer(
    cols = c(ClusterNr, GroundSpeciesCount),
    names_to = "SpeciesType",
    values_to = "Count"
  )

# Plot grouped bar chart
barchart <- ggplot(df_long, aes(
  x = as.factor(Plot_Location_Shp_Subset_Name),
  y = Count,
  fill = SpeciesType
)) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bars
  labs(title = "Comparison of Species Counts per Plot Location",
       x = "Plot Location",
       y = "Species Count",
       fill = "Species Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

print(barchart)

# Save the plots with high resolution
ggsave(
  plot_file_path,
  plot = barchart,
  dpi = 300,
  width = 8,
  height = 6
)






# Load necessary library
library(readxl)

# Load your data (replace with your actual file path)
base_folder <- 'D:/MasterThesis/07_Testsite_Metrics'
file_path <- file.path(base_folder, 'Shannon_Diversity_Plotlevel.xlsx')

# Read the data
diversity_data <- read_excel(file_path)

# Perform Pearson correlation test
cor_test_result <- cor.test(diversity_data$Shannon_Diversity_Ground,
                            diversity_data$Shannon_Diversity_Spectral,
                            method = "pearson")

# Print correlation test result
print(cor_test_result)



# model <- lm(GroundSpeciesCount ~ ClusterNr, data = df)
# summary(model)
#
# anova(model)
#
# r_squared <- summary(model)$r.squared
# print(r_squared)
#
# cor_matrix <- cor(df[, c("ClusterNr", "GroundSpeciesCount", "SpectralSpeciesCount")], method = "pearson")
# print(cor_matrix)
#
# cor.test(df$GroundSpeciesCount, df$ClusterNr, method = "kendall")
#
# cor.test(df$GroundSpeciesCount, df$ClusterNr, method = "pearson")
#
#
# # Install and load the 'lme4' package
# if (!require("lme4"))
#   install.packages("lme4")
# library(lme4)
#
# selected_data <- df %>%
#   dplyr::select('Plot_Location_Shp_Subset_Name',
#                 'ClusterNr',
#                 'GroundSpeciesCount')
#
# # Convert data from wide to long format
# selected_data_long <- selected_data %>%
#   pivot_longer(
#     cols = c(ClusterNr, GroundSpeciesCount),
#     names_to = "Variable",
#     values_to = "Value"
#   )
#
# # Fit a mixed-effects model
# mixed_model <- lmer(GroundSpeciesCount ~ ClusterNr + (1 |
#                                                         Plot_Location_Shp_Subset_Name),
#                     data = selected_data_long)
#
# # Print the summary of the model
# summary(mixed_model)
#
# # You can also get an ANOVA table to test the overall effect of ClusterNr
# if (!require("lmerTest"))
#   install.packages("lmerTest")
# library(lmerTest)
# anova(mixed_model)
