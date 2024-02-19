library(readxl)
library(glmnet)
library(ggplot2)

# Clear data from the environment
rm(list=ls(all=TRUE))

# Load the dataset
cars_data <- read_excel("Cars_Data.xlsx", sheet = "Infinity Data")

# Assuming the first column is brands, and the last one is Overall Preference
x <- as.matrix(cars_data[, -c(1, ncol(cars_data))])
y <- as.numeric(cars_data[[ncol(cars_data)]]) # Ensure y is numeric

# Perform PCA via correlation matrix
cor_mat <- cor(x)
pca_result <- eigen(cor_mat)

# Determine how many factors to retain
retain_factors <- sum(pca_result$values > 1)

# Calculate PCA scores for the retained components
pca_scores <- x %*% pca_result$vectors[, 1:retain_factors]

# Convert pca_scores to a data frame for plotting
pca_scores_df <- as.data.frame(pca_scores)
names(pca_scores_df) <- paste0("PC", 1:retain_factors)

# Fit a regression model with the first two principal components
reg_model <- lm(y ~ PC1 + PC2, data = pca_scores_df)

# Create a new data frame for plotting purposes
plotting_data <- data.frame(
  PC1 = pca_scores_df$PC1,
  PC2 = pca_scores_df$PC2,
  Brand = cars_data[[1]], # Assuming the first column contains the brand names
  Preference = y
)

# Define the range for PC1 based on their actual values
pc1_range <- seq(min(plotting_data$PC1), max(plotting_data$PC1), length.out = 100)
pc2_range <- seq(min(plotting_data$PC2), max(plotting_data$PC2), length.out = 100)

# Create a grid for plotting predicted values based on PC1 and PC2
grid <- expand.grid(PC1 = pc1_range, PC2 = pc2_range)
grid$Predicted_Preference <- predict(reg_model, newdata = grid)

# Define a small number of preference levels for iso-preference lines
preference_levels <- quantile(y, probs = c(0.25, 0.5, 0.75))

# Plot the brand map with iso-preference lines and regression line
pdf("BrandMapPCA.pdf")
ggplot(plotting_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = as.factor(Preference))) +
  geom_text(aes(label = Brand), vjust = 2, hjust = 2, size = 3, check_overlap = TRUE) +
  stat_contour(data = grid, aes(x = PC1, y = PC2, z = Predicted_Preference), breaks = preference_levels, color = "blue") +
  geom_line(data = grid, aes(x = PC1, y = Predicted_Preference), linetype = "dashed", color = "red") +
  scale_color_discrete(name = "Overall Preference") +
  labs(title = "Brand Map with PCA Scores, Regression Line, and Iso-Preference Lines",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()
dev.off()
