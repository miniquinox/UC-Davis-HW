### Step 1: Build Brand Maps for Car Brands ###
###############################################
library(readxl)
library(glmnet)
library(ggplot2)
rm(list=ls(all=TRUE)) #clear data

# Load the dataset
cars_data <- read_excel("Cars_Data.xlsx", sheet = "Infinity Data")

y <-  cars_data[,17]
x <-  as.matrix(cars_data[,2:16])

# Perform PCA via correlation matrix
cor_mat <- cor(x)
pca_result <- eigen(cor_mat)


### Step 2: Determine How Many Factors to Retain ###
####################################################
retain_factors <- sum(pca_result$values > 1)


### Step 3: Assign Names to the Retained Factors ###
####################################################
loadings <- pca_result$vectors[, 1:retain_factors]

attribute_names <- c("Attractive", "Quiet", "Unreliable", "Poorly Built", "Interesting", "Sporty", "Uncomfortable", "Roomy", "Easy Service", "Prestige", "Common", "Economical", "Successful", "AvantGarde", "Poor Value")

# For each principal component, find the attribute(s) with the highest absolute loading
for (pc in 1:ncol(loadings)) {
  cat("Principal Component", pc, ":\n")
  abs_loadings <- abs(loadings[, pc])
  significant_attributes <- attribute_names[abs_loadings >= 0.3] # Adjust threshold as needed
  cat("Significant Attributes:", toString(significant_attributes), "\n\n")
}

# Based on inspection, we manually assigned names
factor_names <- c("Attractive_Quiet_Poorly_Built_Prestige_Successful", "Unreliable_Sporty_Roomy_Easy_Service", "Interesting_Economical_Poor_Value", "Common_AvantGarde")


### Step 4: Iso-Preference Line vs. Regression Line ###
#######################################################

# Iso-Preference Line: Shows where consumers like different 
# combinations of car features equally. It's like a contour line 
# on a map that marks places of equal height, but here it marks 
# equal liking.

# Regression Line: Predicts how much more or less a consumer will 
# like a car based on its features. It shows the direction of 
# increasing or decreasing preference.

# Calculate PCA scores for the retained components
pca_scores <- x %*% pca_result$vectors[, 1:retain_factors]

# Convert pca_scores to a data frame for plotting
pca_scores_df <- as.data.frame(pca_scores)
names(pca_scores_df) <- paste0("PC", 1:retain_factors)

# Fit a regression model with the first two principal components
y <- as.numeric(cars_data[[ncol(cars_data)]]) # Ensure y is numeric
reg_model <- lm(y ~ PC1 + PC2, data = pca_scores_df)

# Create a new data frame for plotting purposes
plotting_data <- data.frame(
  PC1 = pca_scores_df$PC1,
  PC2 = pca_scores_df$PC2,
  Brand = cars_data[[1]],
  Preference = y
)

# Define the range for PC1 and PC2 based on their actual values
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

### Step 5: Ideal Vector ###
############################

# The ideal vector points towards the features that make a car more 
# likable. It's a direction on the brand map where moving makes a 
# car more appealing to consumers.


### Step 6: Computing Angles ###
################################

# The angle of the iso-preference line and the ideal vector can
# provide insights into the relationship between different car
# features and overall preference. A smaller angle indicates a
# stronger relationship between the features and preference.

# Compute the slopes of the iso-preference line and ideal vector
b1 <- as.vector(coef(reg_model)[2])
b2 <- as.vector(coef(reg_model)[3])
slope.iso.preference <- -b1/b2
slope.ideal.vector <- b2/b1

# Angles of iso-preference line and ideal vector
angle.iso.preference <- atan(slope.iso.preference) * 180 / pi
angle.ideal.vector <- atan(slope.ideal.vector) * 180 / pi

# Print the values
cat("Slope of iso-preference line:", slope.iso.preference, "\n")
cat("Slope of ideal vector:", slope.ideal.vector, "\n")
cat("Angle of iso-preference line:", angle.iso.preference, "\n")
cat("Angle of ideal vector:", angle.ideal.vector, "\n")


### Step 7: Find 95% Confidence Interval for the Angle ###
###               of the Ideal Vector                  ###
##########################################################

# Set the number of bootstrap replications
set.seed(123) # for reproducibility
n_boot <- 1000

# Store the bootstrap results
angle_ideal_vector_boot <- numeric(n_boot)

yhat <- predict(reg_model)
rr <- residuals(reg_model)

# Resample and compute the angles
for(i in 1:n_boot) {
  # Resample the residuals and add to the fitted values
  y_star <- yhat + sample(rr, replace = TRUE)
  
  # Fit the model to the new response
  reg_model_star <- lm(y_star ~ PC1 + PC2, data = pca_scores_df)
  
  # Compute the angle for the new model
  b1_star <- coef(reg_model_star)[2]
  b2_star <- coef(reg_model_star)[3]
  slope_ideal_vector_star <- b2_star / b1_star
  angle_ideal_vector_boot[i] <- atan(slope_ideal_vector_star) * 180 / pi
}

# Compute the 95% confidence interval for the angle
angle_ideal_vector_ci <- quantile(angle_ideal_vector_boot, probs = c(0.025, 0.975))

# Print the confidence interval
cat("95% Confidence Interval for the Angle of the Ideal Vector:", angle_ideal_vector_ci, "\n")


### Step 8. Recommend to Infinity's managers what they should ###
###         do to improve their product design                ###
#################################################################

# Emphasize Strengths by Reinforcing the positive attributes associated 
# with the first principal component, which includes being Attractive, 
# Quiet, Prestige, and Successful. These are likely the features that 
# customers value most in Infinity's cars.

# Infinity should Address Weak Points by Looking at the attributes 
# linked to the second principal component, such as Unreliability and
# Sportiness.

# The ideal vector gives us a direction of change, so Infinity should 
# consider design changes that align with this direction, potentially 
# by increasing Prestige and Successfulness of the brand perception.

# Confidence in Changes: The 95% confidence interval for the angle of the ideal vector indicates the stability of the relationship between product features and consumer preferences. Infinity can use this information to make confident changes in design that are most likely to resonate with their target market.

# Customer Validation: Always validate these analytical insights with real customer feedback. Use surveys or focus groups to test whether the changes in design match customer expectations and desires.