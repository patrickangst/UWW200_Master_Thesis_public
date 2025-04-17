# Install necessary packages if you don't have them
if(!require(lme4)){install.packages("lme4")}
if(!require(ape)){install.packages("ape")}
if(!require(sp)){install.packages("sp")}
if(!require(dplyr)){install.packages("dplyr")}

library(lme4)
library(ape)
library(sp)
library(dplyr)
library(MASS)

# --- 1. Generate Simulated Data ---

set.seed(123)  # For reproducibility

n_sites <- 14
n_total <- 150 # We'll simulate more rows and then downsample

data <- data.frame(
  site = sample(1:n_sites, n_total, replace = TRUE),
  # Simulate some spatial coordinates (UTM-like) for each site
  x = runif(n_total, 0, 1000) + rep(seq(0, 13000, length.out = n_sites), each=n_total/n_sites)[1:n_total], #Adding per site
  y = runif(n_total, 0, 1000) + rep(seq(0, 13000, length.out = n_sites), each=n_total/n_sites)[1:n_total]
)

# Create a relationship between plant communities and spectral species,
# including some random variation between sites.
data <- data %>%
  group_by(site) %>%
  mutate(
    plant_communities = rpois(1, lambda = 5) + sample(0:3, 1),  # Base 5, plus some variation
    #Spectral species, related to plant comm, plus random and site effects
    spectral_species = rpois(n(), lambda = plant_communities * 1.2 + rnorm(1, mean = 2, sd = 1)) # + site effect
  ) %>% ungroup()

# Make sure each site has a unique number of plant communities
while(length(unique(data$plant_communities)) < n_sites){
  unique_plant_communities <- sample(2:20, size = n_sites, replace = FALSE) #range from 2-20
  
  for (s in 1:n_sites){
    data <- data %>%
      mutate(plant_communities = ifelse(site == s, unique_plant_communities[s], plant_communities))
  }
}


# Aggregate to site level with coordinate calculation
site_data <- data %>%
  group_by(site) %>%  # Group by site
  summarise(
    plant_communities = list(unique(plant_communities)), # Store unique communities as a list
    spectral_species = mean(spectral_species, na.rm = TRUE), # Handle potential NA values
    x = mean(x, na.rm = TRUE), # Calculate mean x coordinate
    y = mean(y, na.rm = TRUE), # Calculate mean y coordinate
    .groups = 'drop' # Remove grouping after summarizing
  )



# --- 2. Spatial Autocorrelation Test (Moran's I) ---

# Create a spatial points data frame
coordinates(site_data) <- ~x+y

# Calculate distances between sites
distances <- spDists(site_data)

# Create a spatial weights matrix (inverse distance weighting)
#  Make sure there aren't any zero distances, which can cause problems
distances_inv <- 1 / (distances + 0.0001)  # Add a small constant to avoid division by zero
diag(distances_inv) <- 0 # Set diagonal to 0 (no self-correlation)

# Calculate Moran's I
moran_i_result <- Moran.I(site_data$spectral_species, distances_inv)

cat("Moran's I for Spectral Species:\n")
print(moran_i_result)

# --- 3. Generalized Linear Mixed Model (GLMM) ---
site_data_df <- as.data.frame(site_data)
site_data_df$x <- coordinates(site_data)[, 1]  # Extract X (longitude)
site_data_df$y <- coordinates(site_data)[, 2]  # Extract Y (latitude)

str(site_data_df)

site_data_df$plant_communities <- unlist(site_data_df$plant_communities)  # Flatten 'plant_communities'

str(site_data_df)

site_data_df$site <- as.factor(site_data_df$site)
site_data_df$plant_communities <- as.factor(site_data_df$plant_communities)

site_data <- site_data_df

anyNA(site_data)

table(site_data$plant_communities)
summary(site_data$spectral_species)

glmm_nb_simple <- glm.nb(spectral_species ~ plant_communities, data = site_data)
summary(glmm_nb_simple)


# Negative Binomial GLMM (preferred if overdispersion exists)
model <- lm(spectral_species ~ plant_communities + (1 | site), data = site_data)
summary(glmm_nb)

# Poisson GLMM (for comparison, if no overdispersion)
glmm_poisson <- lmer(spectral_species ~ plant_communities + (1 | site), data = site_data, family = poisson)
summary(glmm_poisson)

# Likelihood Ratio Test (LRT) to compare NB and Poisson,
#  and to compare the full model to a null model.
null_model <- glmer.nb(spectral_species ~ 1 + (1 | site), data = site_data)
lrt_result <- anova(null_model, glmm_nb)  # Compare to null model
print("Likelihood Ratio Test (vs. Null Model):")
print(lrt_result)

# --- 4. Kendall's Tau Correlation ---

kendall_result <- cor.test(site_data$plant_communities, site_data$spectral_species, method = "kendall")
cat("\nKendall's Tau Correlation:\n")
print(kendall_result)

# --- 5. Plotting (optional, but helpful for visualization) ---
# Use base R for plotting.
par(mfrow=c(1,2))

# Plotting the data points for the correlation
plot(site_data$plant_communities, site_data$spectral_species,
     xlab = "Number of Plant Communities",
     ylab = "Number of Spectral Species",
     main = "Correlation Plot",
     pch = 16,  # Solid circles
     col = "blue") # blue

# Add the Kendall correlation coefficient to the plot
legend("topleft",
       legend = paste("Kendall's tau =", round(kendall_result$estimate, 3)),
       bty = "n")  # Remove legend box


# Create a spatial plot
plot(site_data, main = "Spatial Distribution of Sites")
text(coordinates(site_data), labels = site_data$site, pos = 1, cex= 0.7)

par(mfrow=c(1,1)) # Reset to default

#--- 6. Residual analysis of the GLMM model ---
# Extract residuals
residuals <- resid(glmm_nb, type = "pearson")

# Plot residuals against fitted values
plot(fitted(glmm_nb), residuals,
     xlab = "Fitted Values",
     ylab = "Pearson Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red", lty = 2)

# Test for spatial autocorrelation in the residuals
coordinates(data) <- ~x+y
distances_res <- spDists(data)
distances_inv_res <- 1 / (distances_res + 0.0001)
diag(distances_inv_res) <- 0

moran_i_result_res <- Moran.I(residuals, distances_inv_res)

cat("Moran's I for GLMM Residuals:\n")
print(moran_i_result_res)