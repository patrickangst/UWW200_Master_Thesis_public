rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load necessary packages
library(dplyr)
library(tibble)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(readxl)
library(lme4)
library(lmerTest)
library(DescTools)


# Load species abundance data
plot_metrics_path <- 'plot_metrics'
clustering_index_name <- 'Unique_Spectral_Species_WCSS_Custom'
index_name <-sub(".*_(.*)$", "\\1", clustering_index_name)

metrics_data <- read_excel(file.path(plot_metrics_path, "Plot_Metrics_Combined.xlsx"),
                           sheet = 'Sheet1')
output_folder_path <- 'correlation_plots_custom'

# Convert to data frame
metrics_data_df <- metrics_data %>% as.data.frame()

# Define function to rate correlation strength
interpret_r <- function(r) {
  dplyr::case_when(
    r >= 0.9           ~ "Very strong positive",
    r >= 0.7           ~ "Strong positive",
    r >= 0.4           ~ "Moderate positive",
    r >= 0.1           ~ "Weak positive",
    r > -0.1 & r < 0.1 ~ "Negligible correlation",
    r <= -0.9          ~ "Very strong negative",
    r <= -0.7          ~ "Strong negative",
    r <= -0.4          ~ "Moderate negative",
    r <= -0.1          ~ "Weak negative",
    TRUE               ~ "Undefined"
  )
}


# Create a dataframe for plotting instructions
cor_metrics <- data.frame(
  Metric = c("Plant Communities", "Habitat Types", "Plant Species"),
  x = c(
    "Unique_Plant_Cummunities",
    "Unique_Habitat_Types",
    "Unique_Plant_Species"
  ),
  filename = c(
    "correlation_plant_communities_custom.png",
    "correlation_habitat_types_custom.png",
    "correlation_plant_species_custom.png"
  )
)

# Create an empty dataframe to collect correlation stats
correlation_results <- data.frame(
  Metric = character(),
  Variable = character(),
  R = numeric(),
  P_value = numeric(),
  Abs_R = numeric(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)


# Loop over each metric and generate/save plot
for (i in 1:nrow(cor_metrics)) {
  xvar <- cor_metrics$x[i]
  metric_label <- cor_metrics$Metric[i]
  file_name <- cor_metrics$filename[i]
  file_name <- paste0(index_name,'_',file_name)
  
  # Tidy evaluation for the y-axis variable (now treated as y)
  yvar_sym <- sym(xvar)
  
  # Get the variables
  x_data <- metrics_data_df$Unique_Spectral_Species_WCSS_Custom
  y_data <- metrics_data_df[[xvar]]
  
  # Remove NAs
  complete_idx <- complete.cases(x_data, y_data)
  x_data <- x_data[complete_idx]
  y_data <- y_data[complete_idx]
  
  n_obs <- length(x_data)
  
  # Normality test
  x_normal <- shapiro.test(x_data)$p.value > 0.05
  y_normal <- shapiro.test(y_data)$p.value > 0.05
  
  # Choose correlation method
  cor_method <- if (n_obs < 10) {
    "kendall"
  } else if (x_normal && y_normal) {
    "pearson"
  } else {
    "spearman"
  }
  
  # Correlation test
  cor_test <- cor.test(x_data, y_data, method = cor_method)
  r <- cor_test$estimate
  p <- cor_test$p.value
  r_abs <- abs(r)
  interpretation <- interpret_r(r)
  
  # Store results
  correlation_results <- rbind(
    correlation_results,
    data.frame(
      Metric = metric_label,
      Variable = xvar,
      R = round(r, 3),
      P_value = signif(p, 3),
      Abs_R = round(r_abs, 3),
      Interpretation = interpretation,
      stringsAsFactors = FALSE
    )
  )
  
  # Determine text position dynamically
  x_max <- max(x_data, na.rm = TRUE)
  y_min <- min(y_data, na.rm = TRUE)
  
  # Plot with annotation
  base_plot <- ggplot(metrics_data_df, aes(x = Unique_Spectral_Species_WCSS_Custom, y = !!yvar_sym, color = Testsite)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
    labs(
      title = paste(metric_label, "vs Spectral Species Count Custom"),
      x = "Unique Spectral Species",
      y = metric_label,
      color = "Testsite",
      caption = paste0("Method: ", cor_method, "\n",
                       "R = ", round(r, 2), "\n",
                       "p = ", signif(p, 2), "\n",
                       "Corr. = ", interpretation)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "right"
    )
  
  # Save plot
  file_name <- gsub('Custom','WCSS',file_name)
  file_path <- file.path(output_folder_path, file_name)
  ggsave(file_path, base_plot, width = 8, height = 5)
  message(paste("Saved plot:", file_name))
  
  print(base_plot)
  
}


# Save the correlation results to CSV
write.csv(correlation_results, file.path(output_folder_path, "correlation_summary_all_custom.csv"), row.names = FALSE)
message("Saved correlation summary CSV.")

######
# Statistical testing
######
lm_model_input <- metrics_data_df %>%
  select(Testsite,Unique_Plant_Cummunities,Unique_Spectral_Species_WCSS_Custom,Unique_Habitat_Types_Short3,Unique_Plant_Species) %>%
  mutate(Testsite = as.factor(Testsite))

str(lm_model_input)

# Model Plant Communities
model_plant_communities_lm <- lm(Unique_Plant_Cummunities ~ Unique_Spectral_Species_WCSS_Custom,
                                 data = lm_model_input)
# anova(model_plant_communities_lm)
model_plant_communities_lm_residuals_path <- file.path(output_folder_path,"model_plant_communities_lm_residuals_all_custom.png")
png(model_plant_communities_lm_residuals_path, 
    width = 8, 
    height = 6, 
    units = "in", 
    res = 300)
par(mfrow = c(2, 2))
plot(model_plant_communities_lm)
dev.off()

summary(model_plant_communities_lm)
model_plant_communities_lm_model_summary_path <- file.path(output_folder_path,"model_plant_communities_lm_model_summary_all_custom.txt")
capture.output(summary(model_plant_communities_lm), file = model_plant_communities_lm_model_summary_path)

# ggplot(lm_model_input, aes(x = Unique_Spectral_Species_WCSS_Custom, y = Unique_Plant_Cummunities)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE)


# Model Habitat Type
model_habitat_types_lm <- lm(Unique_Habitat_Types_Short3 ~ Unique_Spectral_Species_WCSS_Custom,
                             data = lm_model_input)
# anova(model_habitat_types_lm)
model_habitat_types_lm_residuals_path <- file.path(output_folder_path,"model_habitat_types_lm_residuals_all_custom.png")
png(model_habitat_types_lm_residuals_path, 
    width = 8, 
    height = 6, 
    units = "in", 
    res = 300)
par(mfrow = c(2, 2))
plot(model_habitat_types_lm)
dev.off()

summary(model_habitat_types_lm)
model_habitat_types_lm_model_summary_path <- file.path(output_folder_path,"model_habitat_types_lm_model_summary_all_custom.txt")
capture.output(summary(model_habitat_types_lm), file = model_habitat_types_lm_model_summary_path)

# ggplot(lm_model_input, aes(x = Unique_Spectral_Species_WCSS_Custom, y = Unique_Habitat_Types_Short3)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE)

# Model Plant Species
model_plant_species_lm <- lm(Unique_Plant_Species ~ Unique_Spectral_Species_WCSS_Custom,
                             data = lm_model_input)
# anova(model_plant_species_lm)
model_plant_species_lm_residuals_path <- file.path(output_folder_path,"model_plant_species_lm_residuals_all_custom.png")
png(model_plant_species_lm_residuals_path, 
    width = 8, 
    height = 6, 
    units = "in", 
    res = 300)
par(mfrow = c(2, 2))
plot(model_plant_species_lm)
dev.off()

summary(model_plant_species_lm)
model_plant_species_lm_model_summary_path <- file.path(output_folder_path,"model_plant_species_lm_model_summary_all_custom.txt")
capture.output(summary(model_plant_species_lm), file = model_plant_species_lm_model_summary_path)

# ggplot(lm_model_input, aes(x = Unique_Spectral_Species_WCSS_Custom, y = Unique_Plant_Species)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE)

# Model Plant Communities vs. Habitat type
# model_ht_pc_lm <- lm(Unique_Habitat_Types_Short3 ~ Unique_Plant_Cummunities,
#                              data = lm_model_input)
# # anova(model_plant_species_lm)
# par(mfrow = c(2, 2))
# plot(model_ht_pc_lm)
# 
# summary(model_ht_pc_lm)
# ggplot(lm_model_input, aes(x = Unique_Habitat_Types_Short3, y = Unique_Plant_Cummunities)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE)


