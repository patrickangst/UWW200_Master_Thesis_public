# -------------------
# Clean workspace
# -------------------
rm(list = ls(all = TRUE))
gc()
graphics.off()

# -------------------
# Load packages
# -------------------
library(dplyr)
library(ggplot2)
library(readxl)

# -------------------
# 1. Load data
# -------------------
df <- read_excel(
  "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/11_PlotClusters/Summary.xlsx",
  sheet = "Sheet2"
)

# -------------------
# 2. Create helper columns
# -------------------
# df <- df %>%
#   mutate(
#     PixelCount = PixelX * PixelY,
#     pc_b_log = -log10(pc_b_p + 1e-10),
#     pc_c_log = -log10(pc_c_p + 1e-10),
#     ht_b_log = -log10(ht_b_p + 1e-10),
#     ht_c_log = -log10(ht_c_p + 1e-10)
#   )

df <- df %>%
  mutate(
    PixelCount = PixelX * PixelY,
    pc_b_log = pc_b_p,
    pc_c_log = pc_c_p,
    ht_b_log = ht_b_p,
    ht_c_log = ht_c_p
  )

# -------------------
# 3. Define variables to loop over
# -------------------
pval_vars <- c("pc_b_log", "pc_c_log", "ht_b_log", "ht_c_log")
meta_vars <- c("nr_of_plots", "spatial_resolution", "PixelCount", "HSArea")

# -------------------
# 4. Define interpretation function
# -------------------
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

# -------------------
# 5. Define plotting function
# -------------------
plot_corr <- function(df, xvar, yvar, method = "spearman") {
  # Run correlation
  cor_test <- cor.test(df[[xvar]], df[[yvar]], method = method)

  # Extract values
  rho_val <- round(cor_test$estimate, 2)
  p_val   <- signif(cor_test$p.value, 3)
  interpretation <- interpret_r(rho_val)

  # Build plot
  p <- ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(size = 3, alpha = 0.7, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
    labs(
      title = paste(yvar, "vs", xvar),
      x = xvar,
      y = yvar,
      caption = paste0(
        "Method: ", method, "\n",
        "ρ = ", rho_val, "\n",
        "p = ", p_val, "\n",
        "Corr. = ", interpretation
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "right"
    )

  return(p)
}

# -------------------
# 6. Create output folder
# -------------------
outdir <- "~/Documents/GitHub/UWW200_Master_Thesis_public/SpectralPatang/data/MasterThesis/11_PlotClusters/plots"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# -------------------
# 7. Loop over all combinations and save plots
# -------------------
for (yvar in pval_vars) {
  for (xvar in meta_vars) {
    p <- plot_corr(df, xvar, yvar)
    file_name <- paste0("scatter_", yvar, "_vs_", xvar, ".png")
    outfile <- file.path(outdir, file_name)
    ggsave(outfile, p, width = 8, height = 5, dpi = 300)
    message(paste("Saved plot:", file_name))
  }
}

# -------------------
# 8. Create correlation summary table
# -------------------
cor_results <- data.frame()

for (yvar in pval_vars) {
  for (xvar in meta_vars) {
    cor_test <- cor.test(df[[xvar]], df[[yvar]], method = "spearman")
    cor_results <- rbind(cor_results, data.frame(
      pval_type = yvar,
      metadata  = xvar,
      rho       = round(cor_test$estimate, 3),
      p_value   = signif(cor_test$p.value, 3),
      interpretation = interpret_r(cor_test$estimate)
    ))
  }
}

print(cor_results)

# Save results to CSV
write.csv(cor_results, file.path(outdir, "correlation_summary.csv"), row.names = FALSE)
