##############################################
# Chapter 5 - Time series regression models ##
##############################################

# Introduction from the Book
####################################################################################
#In this chapter we discuss regression models. The basic concept is that we 
# forecast the time series of interest  y assuming that it has a linear
# relationship with other time series x. For example, we might wish to forecast
# monthly sales y using total advertising spend x as a predictor. 
# Or we might forecast daily electricity demand y using temperature x1
# and the day of week x2 as predictors. The forecast variable y is sometimes 
# also called the regressand, dependent or explained variable. The 
# predictor variables x are sometimes also called the regressors, 
# independent or explanatory variables. In this book we will always refer to them 
# as the “forecast” variable and “predictor” variables.
####################################################################################

# load libraries
library(forecast)
library(ggplot2)
library(fpp2)

##############################################
# 5.1 - The linear model
###############

# Percentage changes in personal consumption expenditure and personal income for the US
autoplot(uschange[,c("Consumption","Income")]) +
    ylab("% change") + xlab("Year")


# Scatterplot of quarterly changes in consumption expenditure versus
# quarterly changes in personal income and the fitted regression line.
uschange %>%
    as.data.frame() %>%
    ggplot(aes(x=Income, y=Consumption)) +
    ylab("Consumption (quarterly % change)") +
    xlab("Income (quarterly % change)") +
    geom_point() +
    geom_smooth(method="lm", se=FALSE)
#> `geom_smooth()` using formula 'y ~ x'

tslm(Consumption ~ Income, data=uschange)

# A scatterplot matrix of US consumption expenditure and the four predictors
uschange %>%
    as.data.frame() %>%
    GGally::ggpairs()



##############################################
# 5.2 - The linear model
###############

# information about the fitted model
fit.consMR <- tslm(
    Consumption ~ Income + Production + Unemployment + Savings,
    data=uschange)
summary(fit.consMR)

# Time plot of actual US consumption expenditure and 
# predicted US consumption expenditure.
autoplot(uschange[,'Consumption'], series="Data") +
    autolayer(fitted(fit.consMR), series="Fitted") +
    xlab("Year") + ylab("") +
    ggtitle("Percent change in US consumption expenditure") +
    guides(colour=guide_legend(title=" "))

# Actual US consumption expenditure plotted against predicted US consumption expenditure
cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit.consMR)) %>%
    as.data.frame() %>%
    ggplot(aes(x=Data, y=Fitted)) +
    geom_point() +
    ylab("Fitted (predicted values)") +
    xlab("Data (actual values)") +
    ggtitle("Percent change in US consumption expenditure") +
    geom_abline(intercept=0, slope=1)


##############################################
# 5.3 - Evaluating the regression model
###############

# Analysing the residuals from a regression model for US quarterly consumption
checkresiduals(fit.consMR)

# Scatterplots of residuals versus each predictor
df <- as.data.frame(uschange)
df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
    geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
    geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
    geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
    geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

# Scatterplots of residuals versus fitted values
cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>%
    as.data.frame() %>%
    ggplot(aes(x=Fitted, y=Residuals)) + geom_point()



##############################################
# 5.4-  Some useful predictors
###############

# Australian quarterly beer production
beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + xlab("Year") + ylab("Megalitres")



fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)


# Time plot of beer production and predicted beer production
autoplot(beer2, series="Data") +
    autolayer(fitted(fit.beer), series="Fitted") +
    xlab("Year") + ylab("Megalitres") +
    ggtitle("Quarterly Beer Production")


# Actual beer production plotted against predicted beer production
cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
    as.data.frame() %>%
    ggplot(aes(x = Data, y = Fitted,
               colour = as.factor(cycle(beer2)))) +
    geom_point() +
    ylab("Fitted") + xlab("Actual values") +
    ggtitle("Quarterly beer production") +
    scale_colour_brewer(palette="Dark2", name="Quarter") +
    geom_abline(intercept=0, slope=1)

# fourier
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)




##############################################
# 5.5 -  Forecasting with regression
###############

# Forecasts from the regression model for beer production. The dark shaded 
# region shows 80% prediction intervals and the light shaded region shows 
# 95% prediction intervals.
beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
    ggtitle("Forecasts of beer production using regression") +
    xlab("Year") + ylab("megalitres")


#  Forecasting percentage changes in personal consumption 
# expenditure for the US under scenario based forecasting.
fit.consBest <- tslm(
    Consumption ~ Income + Savings + Unemployment,
    data = uschange)
h <- 4
newdata <- data.frame(
    Income = c(1, 1, 1, 1),
    Savings = c(0.5, 0.5, 0.5, 0.5),
    Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
    Income = rep(-1, h),
    Savings = rep(-0.5, h),
    Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)


autoplot(uschange[, 1]) +
    ylab("% change in US consumption") +
    autolayer(fcast.up, PI = TRUE, series = "increase") +
    autolayer(fcast.down, PI = TRUE, series = "decrease") +
    guides(colour = guide_legend(title = "Scenario"))


# Prediction intervals if income is increased by its historical mean of 
# 0.72 % versus an extreme increase of 5%.
fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
                      newdata = data.frame(
                          Income = rep(mean(uschange[,"Income"]), h)))
fcast.up <- forecast(fit.cons,
                     newdata = data.frame(Income = rep(5, h)))
autoplot(uschange[, "Consumption"]) +
    ylab("% change in US consumption") +
    autolayer(fcast.ave, series = "Average increase",
              PI = TRUE) +
    autolayer(fcast.up, series = "Extreme increase",
              PI = TRUE) +
    guides(colour = guide_legend(title = "Scenario"))



# Projecting forecasts from a linear, 
# exponential, piecewise linear trends and a cubic spline for the
# Boston marathon winning times

h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <- ts(pmax(0, t - t.break2), start = 1897)

fit.pw <- tslm(marathon ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
    as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(marathon ~ t + I(t^2) + I(t^3) +
                       I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)



##############################################
# 5.8 - Nonlinear regression
###############

autoplot(marathon) +
    autolayer(fitted(fit.lin), series = "Linear") +
    autolayer(fitted(fit.exp), series = "Exponential") +
    autolayer(fitted(fit.pw), series = "Piecewise") +
    autolayer(fitted(fit.spline), series = "Cubic Spline") +
    autolayer(fcasts.pw, series="Piecewise") +
    autolayer(fcasts.lin, series="Linear", PI=FALSE) +
    autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
    autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
    xlab("Year") + ylab("Winning times in minutes") +
    ggtitle("Boston Marathon") +
    guides(colour = guide_legend(title = " "))







