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
library(colorspace)

# Paths
tiff_dir <- "data/MasterThesis/03_Spectral_Species/"
png_output <- "data/MasterThesis/ss_count_plot/"

excel_file <- "data/MasterThesis/11_PlotClusters/Cluster_Assignement.xlsx"
output_excel_cluster <- "data/MasterThesis/11_PlotClusters/Cluster_Raster_Stats_Cluster_biodivMapR.xlsx"
output_excel_habitat <- "data/MasterThesis/11_PlotClusters/Cluster_Raster_Stats_Habitat_biodivMapR.xlsx"

# Read Excel file once
cluster_info <- read_excel(excel_file)

# Initialize results data frame with all columns including G-test
results_df_cluster <- data.frame(
  PlotID = character(),
  Chi_p_value = numeric(),
  Chi_significance = character(),
  Fisher_p_value = numeric(),
  Fisher_significance = character(),
  G_p_value = numeric(),
  G_significance = character(),
  stringsAsFactors = FALSE
)

results_df_habitat <- data.frame(
  PlotID = character(),
  Chi_p_value = numeric(),
  Chi_significance = character(),
  Fisher_p_value = numeric(),
  Fisher_significance = character(),
  G_p_value = numeric(),
  G_significance = character(),
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

  # The Spectral Species map produced by the biodivMapR package has 20 iterations. Choose the last layer
  # r_last <- r[[nlyr(r)]]
  r_last <- 20
  r <- r[[r_last]]


  # Clean name
  tiff_name <- tools::file_path_sans_ext(basename(tiff_path))
  tiff_name_clean <- sub("_SpectralSpecies$", "", tiff_name)

  # Filter matching entries
  matching_entries <- cluster_info %>%
    filter(grepl(tiff_name_clean, PlotID))

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
    # Use method = "bilinear" explicitly if that's your intention as per description
    extracted_values <- terra::extract(r, vect(points_sf))

    matching_entries$Spectral_Species <- extracted_values[, 2]

    # Filter and prepare
    matching_entries <- matching_entries %>%
      filter(Spectral_Species != 0) %>% # Exclude background/no-data
      mutate(
        Spectral_Species = paste0("SS_", Spectral_Species),
        Cluster = paste0("C_", Cluster)

      ) %>%
      mutate(
        Cluster = as.factor(Cluster),
        Spectral_Species = as.factor(Spectral_Species),
        HabitatType = as.factor(HabitatType),
        HTS1 = as.factor(HTS1),
        HTS2 = as.factor(HTS2)
      )

    # Only proceed if valid contingency table (more than one row AND column)
    contingency_table <- table(matching_entries$Cluster, matching_entries$Spectral_Species)
    # mosaicplot(contingency_table, shade = TRUE, main = "Cluster vs Raster Value")

    # Number of unique Spectral_Species levels
    num_vals <- length(unique(matching_entries$Spectral_Species))
    # Generate pastel colors
    pastel_colors <- qualitative_hcl(num_vals, palette = "Dark 3")
    xy_plot <- ggplot(matching_entries, aes(x = Cluster, fill = Spectral_Species)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = pastel_colors) +
      labs(
        x = "Cluster Number",
        y = "Count",
        fill = "Spectral Species",
        title = paste0(tiff_name_clean, " Spectral Species Count biodivMapR")
      ) + # Add title here
      theme_minimal()

    print(xy_plot)

    cluster_plot_file_path <- file.path(
      png_output,
      paste0(tiff_name_clean, '_ss_count_plot_cluster_biodivmapr.png')
    )

    # Save plot
    ggsave(
      cluster_plot_file_path,
      xy_plot,
      width = 10,
      height = 8,
      dpi = 300
    )


    if (all(dim(contingency_table) > 1)) {
      # Helper function to get significance level
      get_significance <- function(p_value, is_simulated = FALSE) {
        suffix <- if (is_simulated)
          " (Sim.)"
        else
          ""
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
        message(paste(
          "Chi-squared warning for",
          tiff_name_clean,
          ":",
          w$message
        ))
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
          fisher.test(contingency_table,
                      simulate.p.value = TRUE,
                      B = 10000) # Increased B for more accuracy
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
      results_df_cluster <- rbind(
        results_df_cluster,
        data.frame(
          PlotID = tiff_name_clean,
          Chi_p_value = chi_result$p.value,
          Chi_significance = chi_sig,
          Fisher_p_value = fisher_p_value,
          Fisher_significance = fisher_sig,
          G_p_value = gtest_p_value,
          G_significance = gtest_sig,
          stringsAsFactors = FALSE
        )
      )

      print(gc())

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
if (nrow(results_df_cluster) > 0) {
  write_xlsx(results_df_cluster, output_excel_cluster)
  cat("Results saved to:", output_excel_cluster, "\n")
} else {
  cat("No results to save. No valid test sites found or processed.\n")
}


# Iterate over all tiff files
for (tiff_path in tiff_files) {
  # Read raster
  r <- rast(tiff_path)

  # The Spectral Species map produced by the biodivMapR package has 20 iterations. Choose the last layer
  # r_last <- r[[nlyr(r)]]
  r_last <- 20
  r <- r[[r_last]]

  # Clean name
  tiff_name <- tools::file_path_sans_ext(basename(tiff_path))
  tiff_name_clean <- sub("_SpectralSpecies$", "", tiff_name)

  # Filter matching entries
  matching_entries <- cluster_info %>%
    filter(grepl(tiff_name_clean, PlotID))

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
    # Use method = "bilinear" explicitly if that's your intention as per description
    extracted_values <- terra::extract(r, vect(points_sf))

    matching_entries$Spectral_Species <- extracted_values[, 2]

    # Filter and prepare
    matching_entries <- matching_entries %>%
      filter(Spectral_Species != 0) %>% # Exclude background/no-data
      mutate(
        Spectral_Species = paste0("SS_", Spectral_Species),
        Cluster = paste0("Cluster_", Cluster)

      ) %>%
      mutate(
        Cluster = as.factor(Cluster),
        Spectral_Species = as.factor(Spectral_Species),
        HabitatType = as.factor(HabitatType),
        HTS1 = as.factor(HTS1),
        HTS2 = as.factor(HTS2)
      )

    # Only proceed if valid contingency table (more than one row AND column)
    contingency_table <- table(matching_entries$HabitatType, matching_entries$Spectral_Species)
    # mosaicplot(contingency_table, shade = TRUE, main = "Cluster vs Raster Value")

    # Number of unique Spectral_Species levels
    num_vals <- length(unique(matching_entries$Spectral_Species))
    # Generate pastel colors
    pastel_colors <- qualitative_hcl(num_vals, palette = "Dark 3")
    xy_plot <- ggplot(matching_entries, aes(x = HabitatType, fill = Spectral_Species)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = pastel_colors) +
      labs(
        x = "Habitat type",
        y = "Count",
        fill = "Spectral Species",
        title = paste0(tiff_name_clean, " Spectral Species Count biodivMapR")
      ) + # Add title here
      theme_minimal()

    print(xy_plot)

    cluster_plot_file_path <- file.path(
      png_output,
      paste0(tiff_name_clean, '_ss_count_plot_habitat_biodivmapr.png')
    )

    # Save plot
    ggsave(
      cluster_plot_file_path,
      xy_plot,
      width = 10,
      height = 8,
      dpi = 300
    )


    if (all(dim(contingency_table) > 1)) {
      # Helper function to get significance level
      get_significance <- function(p_value, is_simulated = FALSE) {
        suffix <- if (is_simulated)
          " (Sim.)"
        else
          ""
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
        message(paste(
          "Chi-squared warning for",
          tiff_name_clean,
          ":",
          w$message
        ))
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
          fisher.test(contingency_table,
                      simulate.p.value = TRUE,
                      B = 10000) # Increased B for more accuracy
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
      results_df_habitat <- rbind(
        results_df_habitat,
        data.frame(
          PlotID = tiff_name_clean,
          Chi_p_value = chi_result$p.value,
          Chi_significance = chi_sig,
          Fisher_p_value = fisher_p_value,
          Fisher_significance = fisher_sig,
          G_p_value = gtest_p_value,
          G_significance = gtest_sig,
          stringsAsFactors = FALSE
        )
      )

      print(gc())

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
if (nrow(results_df_habitat) > 0) {
  write_xlsx(results_df_habitat, output_excel_habitat)
  cat("Results saved to:", output_excel_habitat, "\n")
} else {
  cat("No results to save. No valid test sites found or processed.\n")
}
