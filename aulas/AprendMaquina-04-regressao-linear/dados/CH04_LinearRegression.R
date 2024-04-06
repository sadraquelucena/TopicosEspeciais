#' ---
#' title: "CHAPTER 4: LINEAR REGRESSION"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "January 28th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 4")

# Install the packages needed for this code file
# install.packages(c("stats","olsrr","lmtest","car","plyr","dplyr","lubridate"))

#----------------------------------------------------------------
#' #1. Load the Data
#----------------------------------------------------------------
library(tidyverse)

# Load the data
# Use the `col_types` argument to specify type of all columns.
# 'D' stands for date, 'f' stands for factor and 'd' stands for double.
bikes <- read_csv("bikes.csv", col_types = "Dffffddddd")

# Get a preview of the bikes data.
glimpse(bikes)

# Let's take a look at the summary stats for the bikes data.
summary(bikes)

#----------------------------------------------------------------
#' #2. Correlation
#----------------------------------------------------------------
# Pearson correlation for rentals, humidity.
cov(bikes$humidity, bikes$rentals)

sd(bikes$humidity)
sd(bikes$rentals)

pearson <- cov(bikes$humidity, bikes$rentals) /  (sd(bikes$humidity) * sd(bikes$rentals))
pearson

# A more convenient way to calculate pearson correlation.
r <- cor(bikes$humidity, bikes$rentals)
r

# Pearson correlation for rentals,windspeed and rentals,temperature.
r <- cor(bikes$windspeed, bikes$rentals)
r

r <- cor(bikes$temperature, bikes$rentals)
r

# Manually derive the values for B0 and B1 (OLS)
B1 <- cov(bikes$temperature, bikes$rentals) / var(bikes$temperature)
B1

B0 <- mean(bikes$rentals) - B1 * mean(bikes$temperature)
B0

#----------------------------------------------------------------
#' #3. Simple Linear Regression Model
#----------------------------------------------------------------
# Fit a model using temperature as the only independent variable.
bikes_mod1 <- lm(data=bikes, rentals~temperature)
bikes_mod1

# Get the detailed ouput of the linear regression model.
summary(bikes_mod1)

#----------------------------------------------------------------
#' #4. Multiple Linear Regression Model
#----------------------------------------------------------------
# Multiple Linear Regression: Fit a model using humidity, windspeed and temperature.
library(stats)
bikes_mod2 <- lm(data=bikes, rentals~humidity+windspeed+temperature)

summary(bikes_mod2)

## Zero Mean of Residuals 
mean(bikes_mod2$residuals)

## Normality of Residuals
library(olsrr)
ols_plot_resid_hist(bikes_mod2)

## Homoscedasticity of Residual
ols_plot_resid_fit(bikes_mod2)

# Breusch-Pagan statistical test for heteroscedasticity.
# This is only referenced but not shown in the text.
library(lmtest)
bptest(bikes_mod2)

## Residual Autocorrelation
library(car)
durbinWatsonTest(bikes_mod2)

## Influential Point Analysis
# Create plot of influential points based on Cook's distance.
ols_plot_cooksd_chart(bikes_mod2)

# List the outliers from the plot above.
cooks_outliers <- ols_plot_cooksd_chart(bikes_mod2)$outliers
arrange(cooks_outliers, desc(cooks_distance))

# Compare a sample influential point and the statistical summary for the entire data.
bikes[69,c("rentals","humidity","windspeed","temperature")]
summary(bikes[-69,c("rentals","humidity","windspeed","temperature")])

# List the outliers from the plot above.
outlier_index <- as.numeric(unlist(cooks_outliers[,"observation"]))
outlier_index

# Compare the statistical summary between the outlier set and the set without outliers.
summary(bikes[outlier_index,c("rentals","humidity","windspeed","temperature")])
summary(bikes[-outlier_index,c("rentals","humidity","windspeed","temperature")])
summary(bikes[,c("rentals","humidity","windspeed","temperature")])

# Create new data set without the outliers.
bikes2 <- bikes[-outlier_index,]

## Multicollinearity
ols_vif_tol(bikes_mod2)

#----------------------------------------------------------------
#' #5. Improving the Model
#----------------------------------------------------------------
# Create the new polynomial predictors.
bikes2 <- bikes2 %>%
  mutate(humidity2 = humidity^2 ) %>%
  mutate(windspeed2 = windspeed^2 ) %>%
  mutate(temperature2 = temperature^2 )

# Create new model with transformed variables.
bikes_mod3 <-
  lm(data = bikes2,
     rentals ~ humidity + windspeed + temperature + 
       humidity2 + windspeed2 + temperature2)
summary(bikes_mod3)

# Create new model without windspeed2 variables.
bikes_mod3 <-
  lm(data = bikes2,
     rentals ~ humidity + windspeed + temperature + 
       humidity2 + temperature2)
summary(bikes_mod3)

## Considering Categorical Variables.
# Get a summary of the values and distribution for the categorical variables.
summary(bikes2[,c("season","holiday","weekday","weather")])

# Use revalue() function to change factor levels.
library(plyr)
library(dplyr)
bikes2 <- bikes2 %>%
  mutate(season = revalue(
    as.factor(season),
    c(
      "1" = "Winter",
      "2" = "Spring",
      "3" = "Summer",
      "4" = "Fall"
    )
  )) %>%
  mutate(holiday = revalue(as.factor(holiday), c("0" = "No", "1" = "Yes"))) %>%
  mutate(weekday = revalue(
    as.factor(weekday),
    c(
      "0" = "Sunday",
      "1" = "Monday",
      "2" = "Tuesday",
      "3" = "Wednesday",
      "4" = "Thursday",
      "5" = "Friday",
      "6" = "Saturday"
    )
  )) %>%
  mutate(weather = revalue(
    as.factor(weather),
    c("1" = "Clear", "2" = "Light precipitation", "3" = "Heavy precipitation")
  ))

# Create new model which includes the additional categorical variable "season".
bikes_mod4 <-
  lm(data = bikes2,
     rentals ~ humidity + windspeed + temperature + humidity2 + temperature2 + season)
summary(bikes_mod4)


## Considering Interactions Between Variables.
# Create new model which includes the interaction between windspeed and weather.
bikes_mod5 <-
  lm(
    data = bikes2,
    rentals ~ humidity + temperature + humidity2 + 
      temperature2 + season + windspeed * weather
  )
summary(bikes_mod5)

## Selecting the Important Variables.
# Create new variables for our model.
library(lubridate)
bikes2 <- bikes2 %>%
  mutate(day=as.numeric(date-min(date))) %>%
  mutate(month=as.factor(month(date))) %>%
  mutate(year=as.factor(year(date))) %>%
  select(-date)

# Variable selection using the mixed selection approach.
ols_step_both_p(
  model = lm(
    data = bikes2,
    rentals ~ humidity + weekday + holiday +
      temperature + humidity2 + temperature2 + season +
      windspeed * weather + realfeel + day + month + year
  ),
  pent = 0.2,
  prem = 0.01,
  details = FALSE
)

# Linear model using the predictors selected during variable selection.
bikes_mod6 <- lm(
  data = bikes2,
  rentals ~ humidity + weekday + holiday +
    temperature + humidity2 + temperature2 + season +
    windspeed * weather + month + year
)
summary(bikes_mod6)

# Recheck for Heteroscedasticity
ols_plot_resid_fit(bikes_mod6)
