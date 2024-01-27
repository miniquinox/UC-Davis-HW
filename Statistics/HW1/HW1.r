# Load profiles.xlsx file
library(readxl)
library(dplyr)

analyze_profiles <- function(stats_file) {

    profiles <- read_excel("profiles.xlsx")

    # Define the weights for each attribute
    weights <- c(Intercept = 1000, `Screen 75 inch` = 500, `Screen 85 inch` = 1000, Resolution = 250, `Sony = 1` = 250)

    # Create a function to calculate the weighted sum
    calculate_weighted_sum <- function(row, weights) {
    sum(row * weights)
    }

    # Apply the function to each row in the profiles tibble
    profiles$WeightedSum <- apply(profiles[, names(weights)], 1, calculate_weighted_sum, weights)

    # assign to a variable called net_cost the value of the weighted sum of "My design" row
    net_cost <- subset(profiles, Profiles == "My design")$WeightedSum

    # tv_data <- read.csv("stats_professor.csv")
    tv_data <- read.csv(stats_file)

    # Clean up the column names for convenience
    names(tv_data) <- c('Profile_Nos', 'Profiles', 'Preference_Rank', 'Screen_75_inch', 'Screen_85_inch', 'Resolution_4K', 'Sony', 'High_Price')

    # Reverse the order of the Preference_Rank column
    tv_data_ordered <- tv_data[order(tv_data$Preference_Rank, decreasing = TRUE), ]
    # print(tv_data_ordered)

    # Run the linear regression model
    # Screen 65 inch is the base category, 
    # Preference_Rank is the dependent variable
    model <- lm(Preference_Rank ~ Screen_75_inch + Screen_85_inch + Resolution_4K + Sony + High_Price, data = tv_data)

    # View the summary of the regression model
    summary(model)

    anova_summary <- anova(model)

    anova_summary
    # Print the summary of the linear regression model
    summary(model)

    # Extract coefficients from the model
    model_coefficients <- coef(model)

    # Extract coefficients from the model
    model_coefficients <- coef(model)

    # Define the column names exactly as in your original coefficients_matrix
    column_names <- c("Intercept", "Screen 75 inch", "Screen 85 inch", 
                    "Resolution", "Sony = 1", "Price (low = 0; hi =1)")

    # Create the matrix in the same format as before
    # Manually set the column names to match the original matrix
    coefficients_matrix <- matrix(
    model_coefficients, 
    nrow = 1, 
    byrow = TRUE,
    dimnames = list(NULL, column_names)
    )

    # Extract the summary of the model
    model_summary <- summary(model)

    # Create a matrix with the required statistics
    coefficients_summary_matrix <- matrix(
    c(model_summary$coefficients[, "Estimate"],  # Estimates
        model_summary$coefficients[, "Std. Error"],  # SE
        model_summary$coefficients[, "t value"],  # Tvals
        model_summary$coefficients[, "Pr(>|t|)"]),  # Pvals
    ncol = 4,
    byrow = FALSE,
    dimnames = list(names(model_coefficients), c("Estimates", "SE", "Tvals", "Pvals"))
    )

    cat("\n1. Partworths:\n")
    # Print the coefficients summary matrix
    print(coefficients_summary_matrix)
    cat("\n\n")

    # Define attributes and their corresponding estimates
    attributes <- c("Screen Size", "Screen Resolution", "Brand Name", "Price")

    # Extract the coefficients/partworths for comparison
    screen_size_coefs <- c(0, model_summary$coefficients["Screen_75_inch", "Estimate"], 
                        model_summary$coefficients["Screen_85_inch", "Estimate"])
    resolution_coefs <- c(0, model_summary$coefficients["Resolution_4K", "Estimate"])
    brand_name_coefs <- c(0, model_summary$coefficients["Sony", "Estimate"])
    price_coefs <- c(0, model_summary$coefficients["High_Price", "Estimate"])
    
    # Calculate the ranges for each attribute
    ranges <- c(
    max(screen_size_coefs) - min(screen_size_coefs),
    max(resolution_coefs) - min(resolution_coefs),
    max(brand_name_coefs) - min(brand_name_coefs),
    max(price_coefs) - min(price_coefs)
    )
    # Combine into a matrix
    attribute_range_matrix <- matrix(
    c(attributes, ranges),
    ncol = 2,
    byrow = FALSE,
    dimnames = list(NULL, c("Attribute", "Range"))
    )

    # Calculate the sum of all ranges
    sum_ranges <- sum(ranges)

    # Calculate the importance for each attribute as a percentage of the sum of ranges
    importance <- (ranges / sum_ranges) * 100

    # Combine the existing matrix with the importance values
    attribute_range_importance_matrix <- cbind(attribute_range_matrix, Importance = importance)

    # Print the updated matrix with the Importance column
    cat("2. Attribute Importance of each attribute: \n")
    print(attribute_range_importance_matrix)
    cat("\n\n")

    # Convert the matrix to a named vector
    coefficients <- setNames(as.vector(coefficients_matrix), 
                            colnames(coefficients_matrix))

    # Calculate utility for each profile
    profiles$Utility <- with(profiles, 
                            Intercept * coefficients['Intercept'] + 
                            `Screen 75 inch` * coefficients['Screen 75 inch'] + 
                            `Screen 85 inch` * coefficients['Screen 85 inch'] + 
                            Resolution * coefficients['Resolution'] + 
                            `Sony = 1` * coefficients['Sony = 1'] +
                            (`Price (low = 0; hi =1)` - 2000) * coefficients['Price (low = 0; hi =1)'] / (2500 - 2000))

    # Calculate the exponential of the utility values
    profiles$Attractiveness <- exp(profiles$Utility)

    #Calculating WTP
    price_partworth = abs(model_summary$coefficients["High_Price", "Estimate"])
    price_savings = 2500-2000
    util = price_savings/price_partworth
    wtp = model_coefficients*util
    wtp = wtp[!names(wtp) %in% c('(Intercept)','Price')]
    wtp =wtp[wtp > 0]
    
    cat("3. WTP:\n")
    print(wtp)
    cat("\n")
    
    # Sum the ExpUtility values
    total_exp_utility <- sum(profiles$Attractiveness)

    price_range <- seq(1500, 2600, by = 100)

    price_share_data <- data.frame(
        Price = price_range,
        Share = numeric(length(price_range))
    )
    
    # In the for loop, you can add print statements after each step:
    for (i in seq_along(price_range)) {

        # After updating the price
        profiles <- profiles %>%
            mutate(`Price (low = 0; hi =1)` = ifelse(Profiles == "My design", price_range[i], `Price (low = 0; hi =1)`))

        # After recalculating utility
        profiles$Utility <- with(profiles, 
                                Intercept * coefficients['Intercept'] + 
                                `Screen 75 inch` * coefficients['Screen 75 inch'] + 
                                `Screen 85 inch` * coefficients['Screen 85 inch'] + 
                                Resolution * coefficients['Resolution'] + 
                                `Sony = 1` * coefficients['Sony = 1'] +
                                (`Price (low = 0; hi =1)` - net_cost) * coefficients['Price (low = 0; hi =1)'] / (2500 - 2000))

        # After recalculating attractiveness
        profiles$Attractiveness <- exp(profiles$Utility)

        # After recalculating market share
        total_exp_utility <- sum(profiles$Attractiveness, na.rm = TRUE)
        profiles$MarketShare <- profiles$Attractiveness / total_exp_utility

        # After updating the price_share_data
        price_share_data$Share[i] <- profiles$MarketShare[profiles$Profiles == "My design"]
    }

    # Calculate the market share for each profile
    profiles$MarketShare <- profiles$Attractiveness / total_exp_utility

    # Convert the dataframe to a matrix
    price_share_matrix <- as.matrix(price_share_data)

    # Set the column names
    colnames(price_share_matrix) <- c("Price", "Share")

    # Define market size
    market_size <- 100

    # Add a new column to the matrix by multiplying the second column (Share) by market_size
    price_share_matrix <- cbind(price_share_matrix, 
                                Sales = price_share_matrix[, "Share"] * market_size)

    # Add a new column to the matrix for Margin
    price_share_matrix <- cbind(price_share_matrix, 
                                Margin = price_share_matrix[, "Price"] - net_cost)

    # Add a new column to the matrix for Sales * Margin
    price_share_matrix <- cbind(price_share_matrix, 
                                Profit = price_share_matrix[, "Sales"] * price_share_matrix[, "Margin"])
    
    # print(price_share_matrix)
    
    # Find the row with the maximum profit
    max_profit_row <- which.max(price_share_matrix[, "Profit"])

    # Extract the Price and Profit for the maximum profit row
    max_price <- price_share_matrix[max_profit_row, "Price"]
    max_profit <- price_share_matrix[max_profit_row, "Profit"]
    max_share <- price_share_matrix[max_profit_row, "Share"]

    # Print the statement
    cat("4. Optimal Price = $", max_price, sep="", "\n\n")

    cat("5. Max profit = $", max_profit, sep="", "\n\n")

    cat("6. Optimal Market Share = ", max_share, sep="", "\n\n")

    # Print the updated matrix
    # cat("Price share matrix: \n")
    # print(price_share_matrix)

    cat("7. Plot of market shares as a function of prices: \n")
    plot(price_share_matrix[, "Price"], 
        price_share_matrix[, "Sales"], # This is the sales column
        type = "b", # This means both points and lines
        col = "blue",
        xlab = "Price", 
        ylab = "Sales", 
        main = "Sales by Price")

    # Adding the series name as a legend
    legend("topright", legend = "Sales", col = "blue", lty = 1)

    cat("8. Plot of profit as a function of prices: \n")
    plot(price_share_matrix[, "Price"], 
        price_share_matrix[, "Profit"], # This is the sales column
        type = "b", # This means both points and lines
        col = "blue",
        xlab = "Price", 
        ylab = "Profit", 
        main = "Sales by Profit")

    # Adding the series name as a legend
    legend("topright", legend = "Profit", col = "blue", lty = 1)

}

cat("\n\n\n\nProfessor's design\n")
analyze_profiles("stats_professor.csv")

cat("\n\n\n\nJoaquin's design\n")
analyze_profiles("stats_joaquin.csv")

cat("\n\n\n\nShiv's design\n")
analyze_profiles("stats_shiv.csv")

cat("\n\n\n\nAayoshi's design\n")
analyze_profiles("stats_aayoshi.csv")

cat("\n\n\n\nHimani's design\n")
analyze_profiles("stats_himani.csv")

cat("\n\n\n\nShrunkhala's design\n")
analyze_profiles("stats_shrunkhala.csv")