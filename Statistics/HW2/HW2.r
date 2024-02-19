
# Load profiles.xlsx file
library(readxl)
library(dplyr)


calculate_wtp <- function(model){
        #Calculating WTP
    model_coefficients <- coef(model) 
    price_partworth = abs(summary(model)$coefficients["High_Price", "Estimate"])
    price_savings = 2500-2000
    util = price_savings/price_partworth
    wtp = model_coefficients*util
    wtp = wtp[!names(wtp) %in% c('(Intercept)','High_Price')]
}

analyze_profiles_residual_bootsrap <- function(stats_file) {

    profiles <- read_excel("profiles.xlsx")
    
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
    
    yhat <- predict(model)
    rr <- model$resid	
    
    nn <- nrow(tv_data)
    
    bb <- 1000							# set number of resampling trials
    wtp.out <- matrix(0, bb, 4)			# matrix to save rsq from bootstrap
    
    Screen_75_inch<- tv_data$Screen_75_inch
    Screen_85_inch<- tv_data$Screen_85_inch
    Resolution_4K<- tv_data$Resolution_4K
    Sony<- tv_data$Sony
    High_Price <- tv_data$High_Price
    
    # Do Residual Bootstrap 1000 times to get 95% CI for R^2
    for(ii in 1:bb) {
    	
    	ystar <- yhat + rr[sample(nn, nn, replace = TRUE)]		# y* with original yhat plus r*
    	out.star <- lm(ystar~Screen_75_inch + Screen_85_inch + Resolution_4K + Sony + High_Price)	
    	# lm with new y* and same x to get new bhat*
    	rsq.star <- summary(out.star)$r.squared
    	wtp.out[ii,] <- calculate_wtp(out.star)
    	# save rsq from iteration ii
    	
    }

  wtp.out <- as.data.frame(wtp.out)
  names(wtp.out) <- c( 'Screen_75_inch', 'Screen_85_inch', 'Resolution_4K', 'Sony')
  #print(wtp.out)
  quantiles <- sapply(wtp.out, quantile, probs = c(0.25, 0.5, 0.75))

  # Return Transpose of final df
  print(t(quantiles))
  return(t(quantiles))
}

cat("\n\n\n\nJoaquin's design, Residual Bootstrap:\n")
analyze_profiles_residual_bootsrap("stats_joaquin.csv")

cat("\n\n\n\nShrunkhala's design, Residual Bootstrap:\n")
analyze_profiles_residual_bootsrap("stats_shrunkhala.csv")

cat("\n\n\n\nShiv's design, Residual Bootstrap:\n")
analyze_profiles_residual_bootsrap("stats_shiv.csv")

cat("\n\n\n\nAayoshi's design, Residual Bootstrap:\n")
analyze_profiles_residual_bootsrap("stats_aayoshi.csv")

cat("\n\n\n\nHimani's design, Residual Bootstrap:\n")
analyze_profiles_residual_bootsrap("stats_himani.csv")