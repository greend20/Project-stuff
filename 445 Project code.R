# Required packages
library(ggplot2)

# Function to calculate confidence intervals using minimum of log-transformed gamma variables
calculate_CI_min <- function(r, n, alpha) {
  sample <- rgamma(n, shape = r, rate = 1)
  x <- -log(sample)
  ci_lower <- -1/n * log(1 - alpha/2) * min(x)
  ci_upper <- -1/n * log(alpha/2) * min(x)
  c(ci_lower, ci_upper)
}

# Function to calculate confidence intervals using sum of log-transformed gamma variables
calculate_CI_sum <- function(r, n, alpha) {
  sample <- rgamma(n, shape = r, rate = 1)
  x <- log(sample)
  sum_x <- sum(x)
  chi_quantile <- qchisq(alpha/2, df = 2*n)
  ci_lower <- (-chi_quantile/(2*n^2)) * sum_x
  chi_quantile <- qchisq(1 - alpha/2, df = 2*n)
  ci_upper <- (-chi_quantile/(2*n^2)) * sum_x
  c(ci_lower, ci_upper)
}

# Simulation study parameters
n_values <- c(10, 20, 30, 40, 50)  # Sample sizes
r_values <- seq(0.001, 0.1, by = 0.001)  # Shape parameter values
alpha <- 0.05  # Significance level

# Create an empty data frame to store the results
results <- data.frame(r = numeric(), n = numeric(), method = character(), ci_lower = numeric(), ci_upper = numeric())

# Perform the simulation study
for (n in n_values) {
  for (r in r_values) {
    # Calculate confidence intervals using minimum of log-transformed gamma variables
    ci_min <- calculate_CI_min(r, n, alpha)
    results <- rbind(results, data.frame(r = r, n = n, method = "Min", ci_lower = ci_min[1], ci_upper = ci_min[2]))
    
    # Calculate confidence intervals using sum of log-transformed gamma variables
    ci_sum <- calculate_CI_sum(r, n, alpha)
    results <- rbind(results, data.frame(r = r, n = n, method = "Sum", ci_lower = ci_sum[1], ci_upper = ci_sum[2]))
  }
}

# Print the simulation results
print(results)

# Plotting the results
ggplot(results, aes(x = r, y = n, fill = method)) +
  geom_tile() +
  labs(title = "Confidence Intervals for Shape Parameter of Gamma Distribution",
       x = "Shape Parameter (r)", y = "Sample Size (n)", fill = "Method") +
  theme_bw()

#### Emprircal R code
# Required packages
library(ggplot2)

# Function to calculate the confidence interval
calculate_CI <- function(r, n, alpha = 0.05) {
  # Generate gamma random variables
  x <- rgamma(n, shape = r)
  
  # Calculate sample mean and variance
  sample_mean <- mean(x)
  sample_var <- var(x)
  
  # Calculate quantiles for confidence interval
  lower_quantile <- qgamma(alpha/2, shape = n * r, rate = n)
  upper_quantile <- qgamma(1 - alpha/2, shape = n * r, rate = n)
  
  # Calculate confidence interval
  ci <- c(sample_mean - lower_quantile/sqrt(n), sample_mean + upper_quantile/sqrt(n))
  
  return(ci)
}

# Generate values for r and n
r_values <- seq(0.01, 1.0, by = 0.01)
r_values
n_values <- seq(1, 100, by = 1)
n_values

# Create an empty matrix to store empirical coverage rates
coverage_rates <- matrix(0, nrow = length(r_values), ncol = length(n_values))

# Loop through r and n values
for (i in 1:length(r_values)) {
  for (j in 1:length(n_values)) {
    r <- r_values[i]
    n <- n_values[j]
    
    # Generate multiple samples and calculate empirical coverage rates
    num_samples <- 1000
    coverage <- rep(0, num_samples)
    
    for (k in 1:num_samples) {
      ci <- calculate_CI(r, n)
      if (r >= ci[1] && r <= ci[2]) {
        coverage[k] <- 1
      }
    }
    
    # Calculate empirical coverage rate
    coverage_rate <- sum(coverage) / num_samples
    coverage_rates[i, j] <- coverage_rate
  }
}

# Create a heatmap of empirical coverage rates
heatmap_data <- data.frame(r = rep(r_values, length(n_values)), 
                           n = rep(n_values, each = length(r_values)), 
                           coverage = c(coverage_rates))

ggplot(heatmap_data, aes(x = n, y = r, fill = coverage)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "n", y = "r", fill = "Deviation from Nominal Coverage") +
  theme_minimal()
