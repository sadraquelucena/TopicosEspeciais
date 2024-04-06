#' ---
#' title: "CHAPTER 5: LOGISTIC REGRESSION"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "February 6th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 5")

# Install the packages needed for this code file
# install.packages(c("DMwR","InformationValue"))

library(tidyverse)

#----------------------------------------------------------------
#' #1. Collect the Data
#----------------------------------------------------------------
# Import the income data set from your working directory.
income <- read_csv("income.csv", col_types = "nffnfffffnff")

# Get a preview of the data.
glimpse(income)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Let's begin by looking at the categorical features in our data.
income %>%
  keep(is.factor) %>%
  summary()

# There are few features with more than 6 levels.
# We use the table() function to get the distribution for their values.
table(select(income, workClassification))
table(select(income, educationLevel))
table(select(income, occupation))
table(select(income, nativeCountry))

# There are missing values for workClassification, nativeCountry and occupation.
# The missing values are represented by an indicator variable of '?'.
# Let's replace these with 'UNK' instead.
income <- income %>%
  mutate(workClassification = dplyr::recode(workClassification, "?" = "UNK")) %>%
  mutate(nativeCountry = dplyr::recode(nativeCountry, "?" = "UNK")) %>%
  mutate(occupation = dplyr::recode(occupation, "?" = "UNK")) 

# What do we now have?
table(select(income, workClassification))
table(select(income, occupation))
table(select(income, nativeCountry))

# Before we build our model, let's also recode our class levels to 0 and 1. 
income <- income %>%
  mutate(income = dplyr::recode(income, "<=50K" = "0")) %>%
  mutate(income = dplyr::recode(income, ">50K" = "1"))

# What do we now have?
summary(income[,"income"])

# Then we split our data into training and test sets.
set.seed(1234)
sample_set <- sample(nrow(income), round(nrow(income)*.75), replace = FALSE)
income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

# What is the class distribution?

# Check the proportions for the class between all 3 datasets.
round(prop.table(table(select(income, income), exclude = NULL)), 4) * 100
round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100
round(prop.table(table(select(income_test, income), exclude = NULL)), 4) * 100

# Our data is imbalanced, so we need to balance the training set.
library(DMwR)
set.seed(1234)
income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

# What do we now have for the training data?
round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100

#----------------------------------------------------------------
#' #3. Train the Model
#----------------------------------------------------------------
# Let's build our model using only the categorical features.
income_mod1 <- income_train %>%
  keep(is.factor) %>%
  glm(formula = income ~ ., family='binomial')

summary(income_mod1)

#----------------------------------------------------------------
#' #4. Evaluate and Improve the Model
#----------------------------------------------------------------
# Generate predictions against the test data using our model.
income_pred1 <- predict(income_mod1, income_test, type = 'response')
head(income_pred1)

# Get ideal cutoff value for the model.
library(InformationValue)

ideal_cutoff <-
  optimalCutoff(
    actuals = income_test$income,
    predictedScores = income_pred1,
    optimiseFor = "Both"
  )

ideal_cutoff

# Use the decision boundary to interprete the results.
income_pred1 <- ifelse(income_pred1 >= ideal_cutoff, 1, 0)
head(income_pred1)

# What is our accuracy?
income_pred1.table <- table(income_test$income, income_pred1)
sum(diag(income_pred1.table)) / nrow(income_test)
