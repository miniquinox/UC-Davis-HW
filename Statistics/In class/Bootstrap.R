setwd("/Users/prasad/Desktop/Teaching_2024/BAX442/Lectures/Class3")



exam <- read.csv("exam.csv", header = TRUE)    # read csv file and label the data as "exam"
x <- as.matrix(exam[,1])        			 # midterm as x variable
y <- as.matrix(exam[,2])        			 # finals as y variable
data <- cbind(y,x)
nn <- nrow(data)




##### What is sampling with replacement? #####

bag1 <- rep(1:10)						# 1 thru 10 numbers in Bag 1
bag2 <- sample(bag1, 10, replace = TRUE)	

# select a number from bag1, ** replaces it back **, and then re-select another number, and so on for 10 times. Hence some numbers appear more than once. 

check <- cbind(bag1, bag2)
colnames(check) <- c("Bag 1", "Bag 2 With Replacement")

# If replace = FALSE, then each number will appear only once. 
bag2a <- sample(bag1, 10, replace = FALSE)	
check2a <- cbind(bag1, bag2a)
colnames(check2a) <- c("Bag 1", "Bag 2 Without Replacement")

## Don't do replace = FALSE. Must do replace = TRUE.




## Goal: Find 95% CI of R^2 -- It is not available analytically

##### My Approach

# Calculate the 95% Confidence interval of z = beta0 * beta1

# Fit the linear model
out <- lm(y ~ x)
out

# Extract the coefficients
beta0 <- coef(out)[1]  # Intercept
beta1 <- coef(out)[2]  # Slope

# Calculate Z as the product of beta0 and beta1
Z <- beta0 * beta1
rr <- residuals(out)


# Initialize the bootstrap procedure
bb <- 1000  # Number of bootstrap samples
Z_out <- numeric(bb)  # Vector to store bootstrap results

# Perform the residual bootstrap
for (ii in 1:bb) {
  ystar <- predict(out) + sample(rr, nn, replace = TRUE)
  out_star <- lm(ystar ~ x)
  beta0_star <- coef(out_star)[1]
  beta1_star <- coef(out_star)[2]
  Z_out[ii] <- beta0_star * beta1_star
}

# Calculate the 95% CI for Z
Z_CI_lower <- quantile(Z_out, probs = 0.025)
Z_CI_upper <- quantile(Z_out, probs = 0.975)

# Calculate the mean of Z from the bootstrap distribution
Z_avg <- mean(Z_out)
Z_avg

# Plot the histogram of Z
hist(Z_out, main = "Bootstrap Distribution of Z", xlab = "Z", breaks = 30)

# Output the results
cat("Mean of Z:", Z_avg, "\n")
cat("95% Confidence Interval for Z: [", Z_CI_lower, ",", Z_CI_upper, "]\n")


##### Approach 1: Residual Bootstrap #####

out <-  lm(y~x)
summary(out)
# R^2 is  0.1126
# The Problem: What is the lower and upper values for 95% CIs?

yhat <- predict(out)
rr <- out$resid						# residuals based on original data, to be used for resampling

bb <- 1000							# set number of resampling trials
rsq.out <- matrix(0, bb, 1)			# matrix to save rsq from bootstrap

# Do Residual Bootstrap 1000 times to get 95% CI for R^2
for(ii in 1:bb) {
	
	ystar <- yhat + rr[sample(nn, nn, replace = TRUE)]		# y* with original yhat plus r*
	out.star <- lm(ystar~x)									# lm with new y* and same x to get new bhat*
	rsq.star <- summary(out.star)$r.squared
	rsq.out[ii] <- rsq.star									# save rsq from iteration ii
	
}

# 95% CI for R^2 from sorting
rsq.CI.lower <- sort(rsq.out)[25]		# 25th value in sorted rsq.out
rsq.CI.upper <- sort(rsq.out)[975]		# 975th value in sorted rsq.out

# OR use quantile function instead of sort
rsq.CI.resid.boot <- quantile(rsq.out, probs = c(0.025, 0.975))

rsq.avg <- mean(rsq.out)		# average R^2




##### Approach 2: Data Bootstrap #####

rsq.out2 <- matrix(0, bb, 1)				# new output matrix for R^2

# Do Data Bootstrap 1000 times to get 95% CI for R^2
for(ii in 1:bb) {
	
	data.star <- data[sample(nn, nn, replace = TRUE),]		# create (y*, x*) by resampling rows in original data matrix
	ystar <- data.star[,2]
	xstar <- data.star[,1]
	out.star <- lm(ystar~xstar)							# lm with new y* and new x* to get new bhat*
	rsq.star <- summary(out.star)$r.squared
	rsq.out2[ii] <- rsq.star								# save rsq from iteration ii
	
}

# 95% CI for R^2 from sorting
rsq.CI.lower2 <- sort(rsq.out2)[25]		# 25th value in sorted rsq.out
rsq.CI.upper2 <- sort(rsq.out2)[975]		# 975th value in sorted rsq.out

# OR use quantile function instead of sort
rsq.CI.data.boot <- quantile(rsq.out2, probs = c(0.025, 0.975))

# average R^2
rsq.avg2 <- mean(rsq.out2)		





##### Approach 3: Monte Carlo Simulation #####

bhat <- as.matrix(out$coeff, 2, 1)	# coefficients from original data 
sigma <- vcov(out)					# variance-covariance matrix of bhat

# Take bb = 1000 draws from multivariate Normal distribution with mean = bhat and covariance matrix = sigma
## install.packages(c("MASS"), dependencies=TRUE,repos="https://cloud.r-project.org")
library('MASS')

bhat.star <- mvrnorm(bb, bhat, sigma)		# 1000 realizations of bhat

rsq.out3 <- matrix(0, bb, 1)	

# Compuet R^2 for each bhat.star realization -- NO y* or x* or r*


for(ii in 1:bb){
	
	yhat <- bhat.star[ii,1] + c(bhat.star[ii,2])*x
	err <- y - yhat
	sse <- t(err) %*% err
	ybar <- mean(y)
	tss <- t((y - ybar)) %*% (y - ybar)
	rsq <- 1 - sse/tss
	rsq.out3[ii] <- rsq
	
}

# 95% CI for R^2 based on Monte Carlo Simulation
rsq.CI.monte.carlo <- quantile(rsq.out3, probs = c(0.025, 0.975))

# average R^2
rsq.avg3 <- mean(rsq.out3)	



###### Summary Results from the Three Approaches #####
CI.out <- rbind(rsq.CI.resid.boot,rsq.CI.data.boot,rsq.CI.monte.carlo)






##### HW2 Deliverable #####
# What is the 95% CI for WTP for each attribute using residual bootstrap and data bootstrap? 


# Path: Bootstrap.R
setwd("/Users/prasad/Desktop/Teaching_2024/BAX442/Lectures/Class3")


##### Approach 1: Residual Bootstrap #####

# Read data
wtp <- read.csv("wtp.csv", header = TRUE)    # read csv file and label the data as "wtp"
wtp <- wtp[,-1]								# remove first column
wtp <- as.matrix(wtp)						# convert to matrix
nn <- nrow(wtp)								# number of observations




# Take the average of many values i will write in an array
# 1. Create an array of zeros
# 2. Create a for loop to go through each row of the data
# 3. For each row, calculate the average of the row and save it in the array
# Write the code

# Write 10 sample numbers in an array



# 1. Create an array of zeros
wtp.avg <- matrix(0, nn, 1)

	
# 2. Create a for loop to go through each row of the data

for(ii in 1:nn){
	
	# 3. For each row, calculate the average of the row and save it in the array
	wtp.avg[ii] <- mean(wtp[ii,])
	
}

# 4. Check the results
wtp.avg
mean(wtp.avg)		# average of the averages
