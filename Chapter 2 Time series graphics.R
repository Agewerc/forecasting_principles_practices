##############################################
# Chapter 2 - Time series graphics ###########
##############################################

# Introduction from the Book
####################################################################################
# The first thing to do in any data analysis task is to plot the data.
# Graphs enable many features of the data to be visualised, including patterns, 
# unusual observations, changes over time, and relationships between variables. 
# The features that are seen in plots of the data must then be incorporated, as 
# much as possible, into the forecasting methods to be used. Just as the type of
# data determines what forecasting method to use, it also determines what graphs 
# are appropriate. But before we produce graphs, we need to set 
# up our time series in R.
####################################################################################

# load libraries
library(forecast)
library(ggplot2)
library(fpp2)

##############################################
# 2.1 - ts objects 
###############


# Almost all of the data used in this book is already stored as ts objects
y <- ts(c(123,39,78,52,110), start=2012)
print(y)


##############################################
# 2.2 -  Time plots
###############

# Command autplot() frequently used
# Two series plots
# first: no clear pattern throughout the period
# second: Clear trend of increase and sasonality

autoplot(melsyd[,"Economy.Class"]) +
    ggtitle("Economy class passengers: Melbourne-Sydney") +
    xlab("Year") +
    ylab("Thousands")

autoplot(a10) +
    ggtitle("Antidiabetic drug sales") +
    ylab("$ million") +
    xlab("Year")


##############################################
# 2.4 - Seasonal plots 
###############

# especially useful in identifying years in which the pattern changes
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
    ylab("$ million") +
    ggtitle("Seasonal plot: antidiabetic drug sales")


# polar option
ggseasonplot(a10, polar=TRUE) +
    ylab("$ million") +
    ggtitle("Polar seasonal plot: antidiabetic drug sales")



##############################################
# 2.5 - Seasonal Subseries plots
###############

# most useful way of viewing seasonal changes over time
# The horizontal lines indicate the means for each month
ggsubseriesplot(a10) +
    ylab("$ million") +
    ggtitle("Seasonal subseries plot: antidiabetic drug sales")


##############################################
# 2.6 - Scatterplots
###############

# plloting the variables in time-series format
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
    xlab("Year: 2014") + ylab("") +
    ggtitle("Half-hourly electricity demand: Victoria, Australia")

# scatter plot: helps us to visualise the relationship between the variables
qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
    ylab("Demand (GW)") + xlab("Temperature (Celsius)")


##############################################
# 2.7 - Lag plots
###############

# A lag plot is used to help evaluate whether the values in a dataset or time 
# series are random. If the data are random, the lag plot will exhibit no 
# identifiable pattern. If the data are not random, the lag plot will demonstrate
# a clearly identifiable pattern.

beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

# Here the colours indicate the quarter of the variable on the vertical axis. 
# The lines connect points in chronological order. 
# The relationship is strongly positive at lags 4 and 8, reflecting the
# strong seasonality in the data.


##############################################
# 2.8 - Autocorrelation
###############

# autocorrelation measures the linear relationship between lagged values 
# of a time series.

# e autocorrelation coefficients are plotted to show the autocorrelation 
# function or ACF. The plot is also known as a correlogram.
ggAcf(beer2)

# Trend and seasonality in ACF plots
# When data are both trended and seasonal, you see a combination of these 
# effects. The monthly Australian electricity demand series plotted in 
# shows both trend and seasonality. 

aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec, lag=48)


##############################################
# 2.9 - White noise
###############

# Time series that show no autocorrelation are called white noise 
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")
ggAcf(y)


# Trend and seasonality in ACF plots
# When data are both trended and seasonal, you see a combination of these 
# effects. The monthly Australian electricity demand series plotted in 
# shows both trend and seasonality. 

aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec, lag=48)
