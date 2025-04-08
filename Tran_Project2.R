# Thuy Nhu Thao Tran, ALY 6050, Mar 2, 2025
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(MASS)  # For fitdistr
library(e1071) # For skewness

# Set seed for reproducibility
set.seed(123)

# Part 1: Monte Carlo Simulation

# Define the number of simulations
n_simulations <- 10000

# Define the benefits and costs for Dam #1 
benefits_dam1 <- list(
  B1 = runif(n_simulations, 1.1, 2.8),   # Improved navigation
  B2 = runif(n_simulations, 8, 14.9),    # Hydroelectric power
  B3 = runif(n_simulations, 1.4, 2.2),   # Fish and wildlife
  B4 = runif(n_simulations, 6.5, 14.6),  # Recreation
  B5 = runif(n_simulations, 1.7, 3.6),   # Flood control
  B6 = runif(n_simulations, 0, 2.4)      # Commercial development
)

costs_dam1 <- list(
  C1 = runif(n_simulations, 13.2, 19.1), # Annualized capital cost
  C2 = runif(n_simulations, 3.5, 7.4)    # Operations & Maintenance
)

# Calculate total benefits and costs for Dam #1
total_benefits_dam1 <- rowSums(do.call(cbind, benefits_dam1))
total_costs_dam1 <- rowSums(do.call(cbind, costs_dam1))

# Calculate benefit-cost ratio for Dam #1
alpha1 <- total_benefits_dam1 / total_costs_dam1

# Define the benefits and costs for Dam #2 (corrected values)
benefits_dam2 <- list(
  B1 = runif(n_simulations, 2.1, 4.8),   # Improved navigation
  B2 = runif(n_simulations, 8.7, 13.6),  # Hydroelectric power
  B3 = runif(n_simulations, 2.3, 3),     # Fish and wildlife
  B4 = runif(n_simulations, 5.9, 15),    # Recreation
  B5 = runif(n_simulations, 0, 3.4),     # Flood control
  B6 = runif(n_simulations, 0, 1.8)      # Commercial development
)

costs_dam2 <- list(
  C1 = runif(n_simulations, 12.8, 20.1), # Annualized capital cost
  C2 = runif(n_simulations, 3.8, 8)      # Operations & Maintenance
)

# Calculate total benefits and costs for Dam #2
total_benefits_dam2 <- rowSums(do.call(cbind, benefits_dam2))
total_costs_dam2 <- rowSums(do.call(cbind, costs_dam2))

# Calculate benefit-cost ratio for Dam #2
alpha2 <- total_benefits_dam2 / total_costs_dam2

# Part 1 (ii): Frequency Distribution and Histogram

# Create frequency distribution for alpha1 and alpha2
breaks <- seq(min(c(alpha1, alpha2)), max(c(alpha1, alpha2)), length.out = 10) # Define 10 bins
freq_alpha1 <- hist(alpha1, breaks=breaks, plot=FALSE)$counts
freq_alpha2 <- hist(alpha2, breaks=breaks, plot=FALSE)$counts

# Create a frequency distribution table
frequency_table <- data.frame(
  Bin = paste0("(", round(breaks[-length(breaks)], 2), ", ", round(breaks[-1], 2), "]"),
  Frequency_Alpha1 = freq_alpha1,
  Frequency_Alpha2 = freq_alpha2
)

# Print the frequency distribution table
print(frequency_table)

# Create frequency distribution and histogram for alpha1
hist(alpha1, breaks=30, main="Histogram of Benefit-Cost Ratio for Dam #1", 
     xlab="Benefit-Cost Ratio", col="lightblue")

# Create frequency distribution and histogram for alpha2
hist(alpha2, breaks=30, main="Histogram of Benefit-Cost Ratio for Dam #2", 
     xlab="Benefit-Cost Ratio", col="lightgreen")

# Part 1 (iii): Descriptive Statistics

# Descriptive statistics for Dam #1
mean_benefits_dam1 <- mean(total_benefits_dam1)
sd_benefits_dam1 <- sd(total_benefits_dam1)
mean_costs_dam1 <- mean(total_costs_dam1)
sd_costs_dam1 <- sd(total_costs_dam1)
mean_alpha1 <- mean(alpha1)
sd_alpha1 <- sd(alpha1)

# Descriptive statistics for Dam #2
mean_benefits_dam2 <- mean(total_benefits_dam2)
sd_benefits_dam2 <- sd(total_benefits_dam2)
mean_costs_dam2 <- mean(total_costs_dam2)
sd_costs_dam2 <- sd(total_costs_dam2)
mean_alpha2 <- mean(alpha2)
sd_alpha2 <- sd(alpha2)

# Create tables for descriptive statistics

# Theoretical Mean and SD for Uniform Distribution
theoretical_mean_uniform <- function(min, max) {
  return((min + max) / 2)
}

theoretical_sd_uniform <- function(min, max) {
  return(sqrt((max - min)^2 / 12))
}

# Theoretical Mean and SD for Total Benefits and Costs (Dam #1)
mean_benefits_dam1_theoretical <- (
  theoretical_mean_uniform(1.1, 2.8) +  # B1
    theoretical_mean_uniform(8, 14.9) +   # B2
    theoretical_mean_uniform(1.4, 2.2) +  # B3
    theoretical_mean_uniform(6.5, 14.6) + # B4
    theoretical_mean_uniform(1.7, 3.6) +  # B5
    theoretical_mean_uniform(0, 2.4)      # B6
)

sd_benefits_dam1_theoretical <- sqrt(
  theoretical_sd_uniform(1.1, 2.8)^2 +  # B1
    theoretical_sd_uniform(8, 14.9)^2 +   # B2
    theoretical_sd_uniform(1.4, 2.2)^2 +  # B3
    theoretical_sd_uniform(6.5, 14.6)^2 + # B4
    theoretical_sd_uniform(1.7, 3.6)^2 +  # B5
    theoretical_sd_uniform(0, 2.4)^2      # B6
)

mean_costs_dam1_theoretical <- (
  theoretical_mean_uniform(13.2, 19.1) + # C1
    theoretical_mean_uniform(3.5, 7.4)     # C2
)

sd_costs_dam1_theoretical <- sqrt(
  theoretical_sd_uniform(13.2, 19.1)^2 + # C1
    theoretical_sd_uniform(3.5, 7.4)^2     # C2
)

# Theoretical Mean and SD for Total Benefits and Costs (Dam #2)
mean_benefits_dam2_theoretical <- (
  theoretical_mean_uniform(2.1, 4.8) +  # B1
    theoretical_mean_uniform(8.7, 13.6) + # B2
    theoretical_mean_uniform(2.3, 3) +    # B3
    theoretical_mean_uniform(5.9, 15) +   # B4
    theoretical_mean_uniform(0, 3.4) +    # B5
    theoretical_mean_uniform(0, 1.8)      # B6
)

sd_benefits_dam2_theoretical <- sqrt(
  theoretical_sd_uniform(2.1, 4.8)^2 +  # B1
    theoretical_sd_uniform(8.7, 13.6)^2 + # B2
    theoretical_sd_uniform(2.3, 3)^2 +    # B3
    theoretical_sd_uniform(5.9, 15)^2 +   # B4
    theoretical_sd_uniform(0, 3.4)^2 +    # B5
    theoretical_sd_uniform(0, 1.8)^2      # B6
)

mean_costs_dam2_theoretical <- (
  theoretical_mean_uniform(12.8, 20.1) + # C1
    theoretical_mean_uniform(3.8, 8)       # C2
)

sd_costs_dam2_theoretical <- sqrt(
  theoretical_sd_uniform(12.8, 20.1)^2 + # C1
    theoretical_sd_uniform(3.8, 8)^2       # C2
)

# Table for Dam #1
table_dam1 <- data.frame(
  Metric = c("Mean of the Total Benefits", "SD of the Total Benefits", "Mean of the Total Cost", "SD of the Total Cost", "Mean of the Benefit-cost Ratio", "SD of the Benefit-cost Ratio"),
  Observed = c(mean_benefits_dam1, sd_benefits_dam1, mean_costs_dam1, sd_costs_dam1, mean_alpha1, sd_alpha1),
  Theoretical = c(mean_benefits_dam1_theoretical, sd_benefits_dam1_theoretical, mean_costs_dam1_theoretical, sd_costs_dam1_theoretical, NA, NA)
)

# Table for Dam #2
table_dam2 <- data.frame(
  Metric = c("Mean of the Total Benefits", "SD of the Total Benefits", "Mean of the Total Cost", "SD of the Total Cost", "Mean of the Benefit-cost Ratio", "SD of the Benefit-cost Ratio"),
  Observed = c(mean_benefits_dam2, sd_benefits_dam2, mean_costs_dam2, sd_costs_dam2, mean_alpha2, sd_alpha2),
  Theoretical = c(mean_benefits_dam2_theoretical, sd_benefits_dam2_theoretical, mean_costs_dam2_theoretical, sd_costs_dam2_theoretical, NA, NA)
)

# Print tables
print(table_dam1)
print(table_dam2)

# Part 2: Chi-squared Goodness-of-Fit Test (with combined bins)

# Calculate theoretical frequencies for alpha1 (normal distribution)
theoretical_freq <- diff(pnorm(breaks, mean=theoretical_mean, sd=theoretical_sd)) * n_simulations

# Create a theoretical frequency table
theoretical_frequency_table <- data.frame(
  Bin = paste0("(", round(breaks[-length(breaks)], 2), ", ", round(breaks[-1], 2), "]"),
  Observed_Frequency = freq_alpha1,
  Theoretical_Frequency = theoretical_freq
)

# Print the theoretical frequency table
print(theoretical_frequency_table)

# Fit a normal distribution to alpha1
fit <- fitdistr(alpha1, "normal")
theoretical_mean <- fit$estimate["mean"]
theoretical_sd <- fit$estimate["sd"]

# Create bins with fewer intervals to ensure expected frequencies > 5
breaks <- seq(min(alpha1), max(alpha1), length.out = 10) # Use 10 bins instead of 30
observed_freq <- hist(alpha1, breaks=breaks, plot=FALSE)$counts
theoretical_freq <- diff(pnorm(breaks, mean=theoretical_mean, sd=theoretical_sd)) * n_simulations

# Perform Chi-squared test
chi_squared_test <- chisq.test(observed_freq, p=theoretical_freq/sum(theoretical_freq))

# Print Chi-squared test results
print(chi_squared_test)

# Part 3: Comparison of Results

# Create a table for comparison
comparison_table <- data.frame(
  Metric = c("Minimum", "Maximum", "Mean", "Median", "Variance", "Standard Deviation", "Skewness", "P(α > 2)", "P(α > 1.8)", "P(α > 1.5)", "P(α > 1.2)", "P(α > 1)", "P(α > α2)"),
  alpha1 = c(min(alpha1), max(alpha1), mean(alpha1), median(alpha1), var(alpha1), sd(alpha1), skewness(alpha1), mean(alpha1 > 2), mean(alpha1 > 1.8), mean(alpha1 > 1.5), mean(alpha1 > 1.2), mean(alpha1 > 1), mean(alpha1 > alpha2)),
  alpha2 = c(min(alpha2), max(alpha2), mean(alpha2), median(alpha2), var(alpha2), sd(alpha2), skewness(alpha2), mean(alpha2 > 2), mean(alpha2 > 1.8), mean(alpha2 > 1.5), mean(alpha2 > 1.2), mean(alpha2 > 1), mean(alpha2 > alpha1))
)

print(comparison_table)

# Conclusion and Recommendation
# Based on the results, recommend one of the projects
if (mean(alpha1) > mean(alpha2)) {
  cat("Recommend Dam #1 based on higher mean benefit-cost ratio.")
} else {
  cat("Recommend Dam #2 based on higher mean benefit-cost ratio.")
}