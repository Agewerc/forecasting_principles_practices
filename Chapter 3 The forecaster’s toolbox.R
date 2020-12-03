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

# Naïve method
# naive(y, h)
# rwf(y, h) # Equivalent alternative

# Seasonal naïve method
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
              series="Naïve", PI=FALSE) +
    autolayer(snaive(beer2, h=11),
              series="Seasonal naïve", PI=FALSE) +
    ggtitle("Forecasts for quarterly beer production") +
    xlab("Year") + ylab("Megalitres") +
    guides(colour=guide_legend(title="Forecast"))


## Example2: the non-seasonal methods are applied to a series of 200 days of 
# the Google daily closing stock price.
autoplot(goog200) +
    autolayer(meanf(goog200, h=40),
              series="Mean", PI=FALSE) +
    autolayer(rwf(goog200, h=40),
              series="Naïve", PI=FALSE) +
    autolayer(rwf(goog200, drift=TRUE, h=40),
              series="Drift", PI=FALSE) +
    ggtitle("Google stock (daily ending 6 Dec 2013)") +
    xlab("Day") + ylab("Closing Price (US$)") +
    guides(colour=guide_legend(title="Forecast"))



##############################################
# 3.2 - Transformations and adjustments
###############

# Calendar adjustments

dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) +
    xlab("Years") + ylab("Pounds") +
    ggtitle("Milk production per cow")
















