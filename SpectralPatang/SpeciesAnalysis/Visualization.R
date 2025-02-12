# Clear workspace
rm(list = ls())
graphics.off()

library(ggplot2)

# Example dataset: Two test sites, each with Ground & Spectral data
df <- data.frame(
  Testsite = rep(c("Site A", "Site B"), each = 10),  # Two test sites
  DataType = rep(c("Ground", "Spectral"), times = 10),  # Ground vs. Spectral
  Value = c(12, 15, 14, 18, 13, 17, 11, 14, 16, 19,  # Site A - Ground & Spectral
            15, 18, 13, 16, 12, 15, 14, 17, 16, 20)   # Site B - Ground & Spectral
)

# Paired Boxplot
ggplot(df, aes(x = DataType, y = Value, fill = DataType)) +
  geom_boxplot(alpha = 0.5) +  # Boxplot
  #geom_point(position = position_jitter(width = 0.1), alpha = 0.8) +  # Jittered points
  facet_wrap(~Testsite) +  # Split by test site
  theme_minimal() +
  labs(title = "Paired Boxplot: Ground vs Spectral", y = "Measured Value", x = "Data Type") +
  scale_fill_manual(values = c("Ground" = "lightblue", "Spectral" = "lightgreen"))  # Custom colors

