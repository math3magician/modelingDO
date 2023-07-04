# Load necessary libraries
library(torch)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)

# Source scripts
source('functions_torch.R')
source('functions_base.R')
source('estimator.R')
source('residuals.R')
source("data_loading.R")
source("data_preprocessing.R")

# Load your data
data_final <- preprocess_data(data)
d18O <- data_final$d18O 
Ca2 <- data_final$Ca2
h <- 0.02
n <- length(d18O)

# Create the individual time series plots
plotd18O <- ggplot(data_final, aes(x = age, y = d18O)) +
  geom_line() +
  labs(y = expression(delta^{18}*O~anomaly),
       title = expression(delta^{18}*O~anomaly~paste("and Ca"^list("2+"), "over time"))) +
  theme_minimal() +
  xlab(NULL) +
  theme(axis.text.x = element_blank()) +
  scale_x_reverse(labels = scales::comma)

plotCa2 <- ggplot(data_final, aes(x = age, y = Ca2)) +
  geom_line() +
  labs(x = "time (years BP)", y = expression(paste("Ca"^list("2+")))) +
  theme_minimal() +
  scale_x_reverse(labels = scales::comma)

# Create the individual density plots (rotated)
plotDensityd18O <- ggplot(data = data_final, aes(y = d18O)) +
  geom_density() +
  labs(title = "densities") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() + 
  theme(axis.text = element_blank(),
        plot.margin = margin(l = 0.2, r = 0.2, t = 0.4, b = 0.2, unit = "cm"))

plotDensityCa2 <- ggplot(data = data_final, aes(y = Ca2)) +
  geom_density() +
  labs(x = "density") +
  ylab(NULL) +
  theme_minimal() + 
  theme(axis.text.y = element_blank())

# Arrange the plots into a 2x2 layout with different plot sizes
grid.arrange(plotd18O, plotDensityd18O, plotCa2, plotDensityCa2,
             ncol = 2, nrow = 2, heights = c(1, 1), widths = c(3, 1))



# Compute the estimators
d18O_est_EM <- as_array(estimator(objectiveEM, d18O, h)[[1]])
d18O_est_LL <- as_array(estimator(objectiveLL, d18O, h)[[1]])
d18O_est_S <- as_array(estimator(objectiveS, d18O, h)[[1]])

Ca2_est_EM <- as_array(estimator(objectiveEM, Ca2, h)[[1]])
Ca2_est_LL <- as_array(estimator(objectiveLL, Ca2, h)[[1]])
Ca2_est_S <- as_array(estimator(objectiveS, Ca2, h)[[1]])

d18O_res_S <- quantile_residuals(d18O, mu_S_base, f_base, OmegahS, d18O_est_S, h, "S")
d18O_res_EM <- quantile_residuals(d18O, mu_EM_base, NULL, function(x, y) x[4], d18O_est_EM, h, "EM")
d18O_res_LL <- quantile_residuals(d18O, mu_LL_base, NULL, OmegahLL_base, d18O_est_LL, h, "LL")[-n]

Ca2_res_S <- quantile_residuals(Ca2, mu_S_base, f_base, OmegahS, Ca2_est_S, h, "S")
Ca2_res_EM <- quantile_residuals(Ca2, mu_EM_base, NULL, function(x, y) x[4], Ca2_est_EM, h, "EM")
Ca2_res_LL <- quantile_residuals(Ca2, mu_LL_base, NULL, OmegahLL_base, Ca2_est_LL, h, "LL")[-n]

# Create a data frame with all residuals
residuals_df <- data.frame(
  Model = rep(c("EM", "Strang", "Local Linearization"), each = length(d18O_res_EM), times = 2),
  Variable = rep(c(rep("d18O", 3 * length(d18O_res_EM)), rep("Ca2", 3 * length(Ca2_res_EM)))),
  Residuals = c(d18O_res_EM, d18O_res_S, d18O_res_LL, Ca2_res_EM, Ca2_res_S, Ca2_res_LL)
)

# Create separate data frames for each variable
residuals_df_d18O <- residuals_df[residuals_df$Variable == "d18O", ]
residuals_df_Ca2 <- residuals_df[residuals_df$Variable == "Ca2", ]

# Create QQ plots for each variable
plot_d18O <- ggplot(residuals_df_d18O, aes(sample = Residuals)) +
  geom_qq() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(y = expression(delta^{18}*O)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
  facet_grid(. ~ Model, scales = "free") +
  lims(x = c(-5, 5), y = c(-5, 5))

plot_Ca2 <- ggplot(residuals_df_Ca2, aes(sample = Residuals)) +
  geom_qq() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Theoretical Quantiles", y = expression(paste("Ca"^list("2+")))) +
  theme_minimal() +
  theme(strip.text.x = element_blank()) +
  facet_grid(. ~ Model, scales = "free") +
  lims(x = c(-5, 5), y = c(-5, 5))

# Arrange the plots vertically
grid.arrange(plot_d18O, plot_Ca2, ncol = 1)


# Define the potential function
potential_func <- function(x, a) {-(a[1]*x + a[2]*(x^2)/2 + a[3]*(x^4)/4)}

# Plot the function
potential_plotCa2 <- ggplot(data.frame(x = range(data_final$Ca2)), aes(x)) +
  stat_function(fun = function(x) potential_func(x, Ca2_est_EM), color = "red") +
  stat_function(fun = function(x) potential_func(x, Ca2_est_S), color = "green") +
  stat_function(fun = function(x) potential_func(x, Ca2_est_LL), color = "blue") +
  labs(y = "potential") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(l = 1.2, r = 0.2, t = 0.2, b = 0.1, unit = "cm"))

# Plot the function
potential_plotd18O <- ggplot(data.frame(x = range(data_final$d18O)), aes(x)) +
  stat_function(fun = function(x) potential_func(x, d18O_est_EM), aes(color = "EM")) +
  stat_function(fun = function(x) potential_func(x, d18O_est_S), aes(color = "S")) +
  stat_function(fun = function(x) potential_func(x, d18O_est_LL), aes(color = "LL")) +
  scale_color_manual(values = c("EM" = "red", 
                                "S" = "green", 
                                "LL" = "blue"), 
                     guide = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(y = "potential") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.5, 1),
        legend.justification = c(0.5, 1),
        legend.title = element_blank(), 
        plot.margin = margin(l = 0.21, r = 0.38, t = 0.21, b = 0.2, unit = "cm")) + 
  scale_y_continuous(position = "right")

# Create the individual time series plots
new_plotd18O <- ggplot(data_final, aes(x = age, y = d18O)) +
  geom_line() +
  labs(y = expression(delta^{18}*O~anomaly)) +
  theme_minimal() +
  xlab(NULL) +
  theme(axis.text.y = element_blank(), 
        plot.margin = margin(l = 0.1, r = 0.9, t = 0.2, b = 0.1, unit = "cm")) +
  scale_x_reverse(labels = scales::comma) + 
  coord_flip()

new_plotCa2 <- ggplot(data_final, aes(x = age, y = Ca2)) +
  geom_line() +
  labs(x = "time (years BP)", y = expression(paste("Ca"^list("2+")))) +
  theme_minimal() +
  scale_x_reverse(labels = scales::comma) + 
  coord_flip()

# Create the individual density plots (rotated)
new_plotDensityd18O <- ggplot(data = data_final, aes(x = d18O)) +
  geom_density() +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        plot.margin = margin(l = 0.21, r = 0.12, t = 0.21, b = 0.2, unit = "cm")) + 
  scale_y_continuous(position = "right")

new_plotDensityCa2 <- ggplot(data = data_final, aes(x = Ca2)) +
  geom_density() +
  labs(y = "density") +
  xlab(NULL) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        plot.margin = margin(l = 0.9, r = 0.2, t = 0.25, b = 0.2, unit = "cm")) 

# Arrange the plots into a 2x2 layout with different plot sizes
grid.arrange(potential_plotCa2, potential_plotd18O,
             new_plotDensityCa2, new_plotDensityd18O, 
             new_plotCa2, new_plotd18O,
             ncol = 2, nrow = 3,
             heights = c(2, 1, 4),
             widths = c(1, 1))

