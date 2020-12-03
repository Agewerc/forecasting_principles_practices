##############################################
# Chapter 3 - The forecaster's toolbox #######
##############################################

# Introduction from the Book
####################################################################################
# In this chapter, we discuss some general tools that are useful for many 
# different forecasting situations. We will describe some benchmark forecasting 
# methods, ways of making the forecasting task simpler using transformations and 
# adjustments, methods for checking whether a forecasting method has adequately 
# utilised the available information, and techniques for computing prediction 
# intervals.
####################################################################################

# load libraries
library(forecast)
library(ggplot2)
library(fpp2)

##############################################
# 3.1 - Some simple forecasting methods 
###############

## y contains the time series
## h is the forecast horizon

# Average method
# meanf(y, h)

# Na?ve method
# naive(y, h)
# rwf(y, h) # Equivalent alternative

# Seasonal na?ve method
# snaive(y, h)

# Drift method
# rwf(y, h, drift=TRUE)

## Example1: the first three methods applied to the quarterly beer production data.
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
    autolayer(meanf(beer2, h=11),
              series="Mean", PI=FALSE) +
    autolayer(naive(beer2, h=11),
              series="Na?ve", PI=FALSE) +
    autolayer(snaive(beer2, h=11),
              series="Seasonal na?ve", PI=FALSE) +
    ggtitle("Forecasts for quarterly beer production") +
    xlab("Year") + ylab("Megalitres") +
    guides(colour=guide_legend(title="Forecast"))


## Example2: the non-seasonal methods are applied to a series of 200 days of 
# the Google daily closing stock price.
autoplot(goog200) +
    autolayer(meanf(goog200, h=40),
              series="Mean", PI=FALSE) +
    autolayer(rwf(goog200, h=40),
              series="Na?ve", PI=FALSE) +
    autolayer(rwf(goog200, drift=TRUE, h=40),
              series="Drift", PI=FALSE) +
    ggtitle("Google stock (daily ending 6 Dec 2013)") +
    xlab("Day") + ylab("Closing Price (US$)") +
    guides(colour=guide_legend(title="Forecast"))



##############################################
# 3.2 - Transformations and adjustments
###############

# Calendar adjustments
# Notice how much simpler the seasonal pattern is in the average daily 
# production plot compared to the total monthly production plot.
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) +
    xlab("Years") + ylab("Pounds") +
    ggtitle("Milk production per cow")


# Population adjustments: Use per capita
# Inflation adjustments: Use price index


# Mathematical transformations
# A logarithmic transformation is often useful
# power transformations
# Box-Cox transformations
lambda <- BoxCox.lambda(elec)
autoplot(BoxCox(elec,lambda))

# drift method with a log transformation (λ = 0)
# with and Without Bias Ajustment
fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80, biasadj=TRUE)
autoplot(eggs) +
    autolayer(fc, series="Simple back transformation") +
    autolayer(fc2, series="Bias adjusted", PI=FALSE) +
    guides(colour=guide_legend(title="Forecast"))


##############################################
# 3.3 - Residual diagnostics
###############

# The following graph shows the Google daily closing stock price (GOOG)
autoplot(goog200) +
    xlab("Day") + ylab("Closing Price (US$)") +
    ggtitle("Google Stock (daily ending 6 December 2013)")


# The residuals obtained from forecasting this series using the naïve method are shown
res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
    ggtitle("Residuals from naïve method")

# Histogram of residuals
gghistogram(res) + ggtitle("Histogram of residuals")

#  ACF of the residuals from the naïve method applied to the Google stock price
ggAcf(res) + ggtitle("ACF of residuals")

# Box-Pierce test and  Ljung-Box test
Box.test(res,lag=10, fitdf=0, type="Lj")
Box.test(res, lag=10, fitdf=0)
# Thus, we can conclude that the residuals are not distinguishable from a white noise series.    


# All of these methods for checking residuals are conveniently packaged 
# into the following function 
checkresiduals(naive(goog200))


##############################################
# 3.4 - Evaluating forecast accuracy
###############

# Fncutions to subset a time series
window(ausbeer, start=1995) # specify the start and/or end of the portion

# subset allows the use of indices to choose a subset
subset(ausbeer, start=length(ausbeer)-4*5) 
subset(ausbeer, quarter = 1)# 

# head and tail are useful for extracting the first few or last few observations
tail(ausbeer, 4*5)

# Forecasts of Australian quarterly beer production using data up to the end of 2007
# The actual values for the period 2008–2010 are also shown
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
    autolayer(beerfit1, series="Mean", PI=FALSE) +
    autolayer(beerfit2, series="Naïve", PI=FALSE) +
    autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
    xlab("Year") + ylab("Megalitres") +
    ggtitle("Forecasts for quarterly beer production") +
    guides(colour=guide_legend(title="Forecast"))

# calculate Acuraccy of forecasts
beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)


# non-seasonal example, consider the Google stock price
googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
    autolayer(googfc1, PI=FALSE, series="Mean") +
    autolayer(googfc2, PI=FALSE, series="Naïve") +
    autolayer(googfc3, PI=FALSE, series="Drift") +
    xlab("Day") + ylab("Closing Price (US$)") +
    ggtitle("Google stock price (daily ending 6 Dec 13)") +
    guides(colour=guide_legend(title="Forecast"))


# calculate Acuraccy of forecasts
googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

# Cross Validation
e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))

# Compute the MSE values and remove missing values
e <- tsCV(goog200, forecastfunction=naive, h=8)

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
    ggplot(aes(x = h, y = MSE)) + geom_point()


##############################################
# 3.5 - Prediction Intervals
###############

# naive method
autoplot(naive(goog200))

##############################################
# 3.6 - The forecast package in R
###############

forecast(ausbeer, h=4)
