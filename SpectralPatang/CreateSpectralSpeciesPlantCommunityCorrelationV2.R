rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load libraries
library(terra)
library(readxl)
library(ggplot2) # Not used in the provided code, but good to keep if you use it elsewhere
library(dplyr)
library(sf)
library(writexl)
library(DescTools)

# Paths
tiff_dir <- "data/MasterThesis/03_Spectral_Species/"
excel_file <- "data/MasterThesis/11_PlotClusters/Cluster_Assignement.xlsx"
output_excel <- "data/MasterThesis/11_PlotClusters/Cluster_Raster_Stats.xlsx"

# Read Excel file once
cluster_info <- read_excel(excel_file)

# Initialize results data frame with all columns including G-test
results_df <- data.frame(
  Testsite = character(),
  Chi_p_value = numeric(),
  Chi_significance = character(),
  Fisher_p_value = numeric(),
  Fisher_significance = character(),
  G_p_value = numeric(),
  G_significance = character(),
  stringsAsFactors = FALSE
)

# List all .tiff files
tiff_files <- list.files(
  path = tiff_dir,
  pattern = "\\.tiff$",
  full.names = TRUE
)

# Iterate over all tiff files
for (tiff_path in tiff_files) {
  # Read raster
  r <- rast(tiff_path)

  # Clean name
  tiff_name <- tools::file_path_sans_ext(basename(tiff_path))
  tiff_name_clean <- sub("_SpectralSpecies$", "", tiff_name)

  # Filter matching entries
  matching_entries <- cluster_info %>%
    filter(grepl(tiff_name_clean, Testsite))

  if (nrow(matching_entries) > 0) {
    # Convert to sf
    points_sf <- st_as_sf(
      matching_entries,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )

    # Transform if CRS differs
    if (st_crs(points_sf) != st_crs(r)) {
      points_sf <- st_transform(points_sf, crs = st_crs(r))
    }

    # Extract raster values
    # Use method = "bilinear" explicitly if that's your intention as per description
    extracted_values <- terra::extract(r, vect(points_sf), method = "bilinear")
    matching_entries$Raster_Value <- extracted_values[, 2]

    # Filter and prepare
    matching_entries <- matching_entries %>%
      filter(Raster_Value != 0) %>% # Exclude background/no-data
      mutate(
        Raster_Value = as.factor(Raster_Value),
        Cluster = as.factor(Cluster)
      )

    # Only proceed if valid contingency table (more than one row AND column)
    contingency_table <- table(matching_entries$Cluster, matching_entries$Raster_Value)

    if (all(dim(contingency_table) > 1)) {
      # Helper function to get significance level
      get_significance <- function(p_value, is_simulated = FALSE) {
        suffix <- if (is_simulated) " (Sim.)" else ""
        if (p_value < 0.01) {
          return(paste0("Strong", suffix))
        } else if (p_value < 0.05) {
          return(paste0("Moderate", suffix))
        } else {
          return(paste0("None", suffix))
        }
      }

      # Chi-squared test
      chi_result <- tryCatch({
        chisq.test(contingency_table)
      }, warning = function(w) {
        message(paste("Chi-squared warning for", tiff_name_clean, ":", w$message))
        chisq.test(contingency_table) # Still return the result
      })
      chi_sig <- get_significance(chi_result$p.value)

      # Fisher's exact test
      fisher_p_value <- NA
      fisher_sig <- "Error/NA"
      is_fisher_simulated <- FALSE

      fisher_result <- tryCatch({
        # Try exact test first
        fisher.test(contingency_table)
      }, error = function(e) {
        # Catch any FEXACT error and switch to simulation
        if (grepl("FEXACT error", e$message)) {
          message(
            paste(
              "Exact Fisher's test failed for",
              tiff_name_clean,
              "due to FEXACT error (",
              e$message,
              "). Performing simulation..."
            )
          )
          is_fisher_simulated <<- TRUE
          fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000) # Increased B for more accuracy
        } else {
          stop(e) # Re-throw other unexpected errors
        }
      })
      fisher_p_value <- fisher_result$p.value
      fisher_sig <- get_significance(fisher_p_value, is_fisher_simulated)

      # G-test
      gtest_result <- GTest(contingency_table)
      gtest_p_value <- gtest_result$p.value
      gtest_sig <- get_significance(gtest_p_value)

      # Save results
      results_df <- rbind(
        results_df,
        data.frame(
          Testsite = tiff_name_clean,
          Chi_p_value = chi_result$p.value,
          Chi_significance = chi_sig,
          Fisher_p_value = fisher_p_value,
          Fisher_significance = fisher_sig,
          G_p_value = gtest_p_value,
          G_significance = gtest_sig,
          stringsAsFactors = FALSE
        )
      )
    } else {
      message(
        paste(
          "Skipping",
          tiff_name_clean,
          "- contingency table too small or single dimension after filtering."
        )
      )
    }
  } else {
    message(paste("No matching entries found for", tiff_name_clean))
  }
}

# Save to Excel
if (nrow(results_df) > 0) {
  write_xlsx(results_df, output_excel)
  cat("Results saved to:", output_excel, "\n")
} else {
  cat("No results to save. No valid test sites found or processed.\n")
}
