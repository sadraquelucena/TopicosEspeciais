#' ---
#' title: "CHAPTER 4: LINEAR REGRESSION"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "January 28th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 4")

# Install the packages needed for this code file
# install.packages(c("olsrr","lmtest","car"))

#----------------------------------------------------------------
#' #1. Importing the Data
#----------------------------------------------------------------
library(tidyverse)
health <- read_csv("health.csv")

# Preview the imported data.
glimpse(health)

# Convert the diabetes and smoker variables to factors (categorical).
health <- health %>%
  mutate(diabetes= as.factor(diabetes)) %>%
  mutate(smoker=as.factor(smoker))

#----------------------------------------------------------------
#' #2. Exploring the Data
#----------------------------------------------------------------
# Get statistical summary.
summary(health)

# Visualize the response variable.
health %>%
  ggplot() +
  geom_histogram(mapping=aes(x=systolic), fill="lightblue", color="black") +
  theme_minimal() +
  theme(text = element_text(size=14))

# Visualize the continuous predictor variables.
health %>%
  select(-systolic) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size=14))

# Correlation between continuous variables.
cor(health[c("systolic","weight","height","bmi","waist","age","fastfood")])

#----------------------------------------------------------------
#' #3. Fitting the Simple Linear Regression Model
#----------------------------------------------------------------
# Fit a model using AGE as the only independent variable.
health_mod1 <- lm(data=health, systolic~age)
summary(health_mod1)

#----------------------------------------------------------------
#' #4. Fitting the Multiple Linear Regression Model
#----------------------------------------------------------------
# Fit a model using all variables.
health_mod2 <- lm(data=health, systolic~.)
summary(health_mod2)

#----------------------------------------------------------------
#' #5. Model Diagnostic Tests
#----------------------------------------------------------------
## Zero Mean of Residuals. 
mean(health_mod2$residuals)

## Normality of Residuals
library(olsrr)
ols_plot_resid_hist(health_mod2)

## Homoscedasticity of Residuals
ols_plot_resid_fit(health_mod2)

## Residual Autocorrelation
## The set.seed() function ensures we can get the same p-value every time.
library(car)
set.seed(123)
durbinWatsonTest(health_mod2)

## Influential Point Analysis
# Create plot of influential points based on Cook's distance.
ols_plot_cooksd_chart(health_mod2)

# Compare a sample influential point and the statistical summary for the entire data.
health[1358,]
summary(health)

# List the outliers from the plot above.
outlier_index <- as.numeric(unlist(ols_plot_cooksd_chart(health_mod2)$outliers[,"observation"]))
outlier_index

# Compare the statistical summary between the outlier set and the set without outliers.
summary(health[outlier_index,])
summary(health[-outlier_index,])

# Create new data set without the outliers.
health2 <- health[-outlier_index,]

## Multicollinearity
ols_vif_tol(health_mod2)

#----------------------------------------------------------------
#' #6. Building new models
#----------------------------------------------------------------
# Build a new model with the new data and subset of predictors.
health_mod3 <- lm(data=health2, systolic ~ weight+age+diabetes)
summary(health_mod3)

# Add two new predictors.
health2 <- health2 %>%
  mutate(age2 = age^2,
          lage = log(age))

# Build a new model with interections and non-linear predictors.
ols_step_both_p(
  model = lm(
    data = health2,
    systolic ~ weight * diabetes + age * diabetes + age2 * diabetes 
    + lage * diabetes
  ),
  pent = 0.2,
  prem = 0.01,
  details = FALSE
)
