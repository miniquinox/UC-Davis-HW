rm(list=ls(all=TRUE)) #clear data

# install.packages(c("glmnet"), dependencies = TRUE, repos = "https://cloud.r-project.org")

# install the package glmnet if not already installed
# install.packages("glmnet", dependencies = TRUE, repos = "https://cloud.r-project.org")
library(glmnet)


data <-  read.csv("Kirin_Map.csv", header=T)    # read csv file and label the data as "data"
y <-  data[,19]
x <-  as.matrix(data[,2:18])
cor_mat = cor(x)									# create correlation matrix



##### Principal Components Analysis #####

out1 <-  eigen(cor_mat)		# eigen decomposition of correlation matrix
va <-  out1$values			# eigenvalues
ve <-  out1$vectors			# eigenvector


##### savings the plot as a pdf file #####
pdf("scree plot.pdf")
plot(va, ylab = "Eigenvalues", xlab = "Component Nos")				# scree plot
dev.off()

ego <- va[va > 1]							# eigenvalues > 1
nn <- nrow(as.matrix(ego))					# number of factors to retain

out2 <- ve[,1:nn]							# eigenvectors associated with the reatined factors
out3 <- ifelse(abs(out2) < 0.3, 0, out2)		# ignore small values < 0.3

rownames(out3) <- c("Rich.full.bodied.taste", "Good.taste", "No.aftertaste", "Refreshing", "Light", "Lower.price", "Good.value.for.money", "Prestigious.popular", "Masculine", "Country.with.brewing.tradition", "For.young.people", "Gives.buzz", "Drink.at.picnics", "Drink.at.bar", "Drink.with.friends", "For home.after.work", "To.serve.guests.at.home")

# flip the eigenvectors b/c later we will see that regression betas are negative. We flip the eignevector so that the slopes become positive for naming the four benefits

out4 <- (-1)*out3			# assign names based on the rownames and signs of coefficients

## Naming of Benefits Z1 and Z2 are based on the matrix of eigenvectors in out4. 
# The numbers in out4 are *correlations* between the attributes (in rows) and the benefits (in cols) 
# Positive value means attribute_i (in rows) is positively correlated with Benefit Z (in columns)
# Negative value means attribute_i (in rows) is negatively correlated with Benefit Z (in columns)
# Small value means attribute_i (in rows) is weakly correlated with Benefit Z (in columns)
# Large value means attribute_i (in rows) is strongly correlated with Benefit Z (in columns)
# Zero value means attribute_i (in rows) is not correlated with Benefit Z (in columns). 

out4





# Principal Componenent "Scores" are the linear combination of original variables, where the weights come from the eigenvectors.
# Denoted by Z in the slides

z <- x %*% out4			# Component Scores; coordinates of the brands in the map

out5 <- lm(y ~ z)		# Preference Regression to estimate how benefits drive overall preferences = f(benefits)

summary(out5)

## NOTE: you want all the slopes to be positive. If negative, then flip those Z variables by multiplying by -1. 



##### Iso Preference Line and Ideal Vector ####			

## Consider factors z1 and z2 with positive slopes on preferences
## Let z2 be the y-axis and z1 as the x-axis	
## Plot (z2, z1) to get brands in factor space of benefit 1 and benefit 2 

# coordinates of brands in (z1, z2) space
Z1 <- z[,1]
Z2 <- z[,2]
z.out <- cbind(Z1, Z2)
rownames(z.out) = c("Amstel Light", "Bass", "Becks", "Corona", "Dos Equis", "Heineken", "Kirin", "Molson", "Moosehead", "Sapporo", "St. Pauli")

# Plot, add labels, and save the brand map 
pdf("BrandMapA.pdf")
plot(Z1, Z2, main = "Brands in Z1 and Z2 space", xlab = "Benefit Z1", ylab = "Benefit Z2", col = "lightblue", pch = 19, cex = 2)		# Brand Map in Z1-Z2 space
text(z.out, labels = row.names(z.out), font = 2, cex = 0.5, pos = 1)						# labeling brands
dev.off()

# Slopes of iso-preference and ideal vector	
b1 <- as.vector(coef(out5)[2])
b2 <- as.vector(coef(out5)[3])
slope.iso.preference = - b1/b2						# Why? See slides
slope.ideal.vector = b2/b1 							# Why? See slides

# Angles of iso-preference and ideal vector	
angle.iso.preference <- atan(slope.iso.preference)*180/pi	
angle.ideal.vector <- atan(slope.ideal.vector)*180/pi

## Are these slopes perpendicular to each other? Why so? See slides

## Now do this for other pairs of significant benefit factors: (Z2, Z4), and (Z4, Z1) b/c Z3 is not significant






##### HW3 Qs #####

# 1. Build brand maps for car brands. The client's brand is Infinity

# 2. How many factors to retain? 

# 3. Assign names to the retained factors (you may need to flip the factors and then assign names)

# 4. Explain iso-preference line and its difference from the regression line

# 5. Explain what is an ideal vector and why it indicates the direction of increasing preferences 

# 6. Compute the angles of iso-preference line and ideal vector arrow

# 7. Find 95% confidence interval for the angle of the ideal vector. Use data bootstrap method to obtain CI

# 8. Recommend to Infinity's managers what they should do to improve their product design






  