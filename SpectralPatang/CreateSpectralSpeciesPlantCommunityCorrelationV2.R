rm(list = ls(all = TRUE))
gc()
graphics.off()

# Load libraries
library(terra)
library(readxl)
library(ggplot2)
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

# Initialize results data frame
results_df <- data.frame(
  Testsite = character(),
  Chi_p_value = numeric(),
  Chi_significance = character(),
  Fisher_p_value = numeric(),
  Fisher_significance = character(),
  stringsAsFactors = FALSE
)

# List all .tiff files
tiff_files <- list.files(path = tiff_dir,
                         pattern = "\\.tiff$",
                         full.names = TRUE)

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
    points_sf <- st_as_sf(matching_entries,
                          coords = c("Longitude", "Latitude"),
                          crs = 4326)

    # Transform if CRS differs
    if (st_crs(points_sf) != st_crs(r)) {
      points_sf <- st_transform(points_sf, crs = st_crs(r))
    }

    # Extract raster values
    extracted_values <- terra::extract(r, vect(points_sf))
    matching_entries$Raster_Value <- extracted_values[, 2]

    # Filter and prepare
    matching_entries <- matching_entries %>%
      filter(Raster_Value != 0) %>%
      mutate(Raster_Value = as.factor(Raster_Value),
             Cluster = as.factor(Cluster))

    # Only proceed if valid contingency table
    contingency_table <- table(matching_entries$Cluster, matching_entries$Raster_Value)

    if (all(dim(contingency_table) > 1)) {
      # Chi-squared test
      chi_result <- chisq.test(contingency_table)
      chi_sig <- ifelse(
        chi_result$p.value < 0.01,
        "Strong",
        ifelse(chi_result$p.value < 0.05, "Moderate", "None")
      )

      # Initialize Fisher's p-value and significance
      fisher_p_value <- NA
      fisher_sig <- "Error/NA"
      fischer_error <- FALSE

      # Fisher's exact test
      fisher_result <- tryCatch({
        fisher_test_result <- fisher.test(contingency_table)
        fisher_p_value <- fisher_test_result$p.value

        fisher_test_result # Return the result object
      }, error = function(e) {
        if (grepl("FEXACT error 501", e$message) ||
            grepl("FEXACT error 5", e$message)) {
          cat(
            "Exact Fisher's test failed due to FEXACT error (501 or 5). Performing simulation...\n"
          )
          fischer_error <<- TRUE
          fisher_test_result_simulated <- fisher.test(contingency_table, simulate.p.value = TRUE)
          fisher_p_value <- fisher_test_result_simulated$p.value
          fisher_test_result_simulated # Return the simulated result object
        } else {
          stop(e)
        }
      })

      fischer_suffix <- ""
      if (fischer_error) {
        fischer_suffix <- " (Sim.)"
      }

      fisher_sig <- ifelse(
        fisher_result$p.value < 0.01,
        paste0("Strong", fischer_suffix),
        ifelse(
          fisher_result$p.value < 0.05,
          paste0("Moderate", fischer_suffix),
          paste0("None", fischer_suffix)
        )
      )



      # G-test
      gtest_p_value <- NA
      gtest_sig <- "Error/NA"
      gtest_available <- FALSE

      gtest_result <- GTest(contingency_table)
      gtest_p_value <- gtest_result$p.value
      gtest_sig <- ifelse(gtest_p_value < 0.01,
                          "Strong",
                          ifelse(gtest_p_value < 0.05, "Moderate", "None"))
      gtest_available <- TRUE

      # Save results (expanded with G-test)
      results_df <- rbind(
        results_df,
        data.frame(
          Testsite = tiff_name_clean,
          Chi_p_value = chi_result$p.value,
          Chi_significance = chi_sig,
          Fisher_p_value = fisher_p_value,
          Fisher_significance = fisher_sig,
          G_p_value = ifelse(gtest_available, gtest_p_value, NA),
          G_significance = ifelse(gtest_available, gtest_sig, "Not available"),
          stringsAsFactors = FALSE
        )
      )






      # Save results
      # results_df <- rbind(
      #   results_df,
      #   data.frame(
      #     Testsite = tiff_name_clean,
      #     Chi_p_value = chi_result$p.value,
      #     Chi_significance = chi_sig,
      #     Fisher_p_value = fisher_p_value,
      #     Fisher_significance = fisher_sig,
      #     stringsAsFactors = FALSE
      #   )
      # )
    } else {
      message(paste(
        "Skipping",
        tiff_name_clean,
        "- contingency table too small"
      ))
    }
  } else {
    message(paste("No matching entries found for", tiff_name_clean))
  }

}

# Save to Excel
write_xlsx(results_df, output_excel)
cat("Results saved to:", output_excel, "\n")
