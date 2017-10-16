#We implement the machine learning alogrithm simple 
#linear regression on this two dimensional data set

#Load Colors for the scatterplot
library(RColorBrewer)

#Manually enter the data
price <- c(7, 7.5, 6.6, 7.2, 7, 5.4, 6.4, 7, 5.1, 7.2)

miles <- c(90, 59, 66, 87, 90, 106, 94, 57, 138, 87)

#Select colors from the color package
cols<-brewer.pal(n=4,name="Set1")

#Scatterplot of the data
plot(miles, price, col = cols, main = "Scatterplot Of The Data", 
     xlab  = "Car's mileage in 1000s", ylab = "Car's Price in 1000s" )

#Least Square methods regression
fit <- lm(price~miles)

#Value of the slope
fit$coefficients[[2]]

#Value of the intercept
fit$coefficients[[1]]

#Regression line on the scatterplot
abline(fit, col = "purple", lwd = 2)

# Prediction of 100,000 miles equation
prediction_miles <- fit$coefficients[[2]]*100 + fit$coefficients[[1]]

#coefficient of determination r2
summary(fit)$r.squared



