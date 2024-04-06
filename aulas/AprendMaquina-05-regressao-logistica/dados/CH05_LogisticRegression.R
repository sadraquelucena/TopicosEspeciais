#' ---
#' title: "CHAPTER 5: LOGISTIC REGRESSION"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "February 6th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 5")

# Install the packages needed for this code file
# install.packages(c("gridExtra","DMwR","stats","corrplot","car","InformationValue"))

library(gridExtra)

#----------------------------------------------------------------
#' #1. Collect the Data
#----------------------------------------------------------------
library(tidyverse)

# Load the data and use the `col_types` argument to specify type of 
# all columns ('n' stands for numerical and 'f' stands for factor).
donors <- read_csv("donors.csv", col_types = "nnffnnnnnnnnffffffffff")

# Get a preview of the data.
glimpse(donors)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
## Categorical features
# Get the summary statistics for the categorical data.
donors %>%
  keep(is.factor) %>%
  summary()

# Get summary statistics for the incomeRating feature. 
summary(select(donors, incomeRating))

# Get fractional frequency distribution for each of the values (including NA).
donors %>%
  select(incomeRating) %>%
  table(exclude = NULL) %>%
  prop.table()

# Since these are categorical values, let's set the values for the NAs to 'UNK'.
donors <- donors %>%
  mutate(incomeRating = as.character(incomeRating)) %>%
  mutate(incomeRating = as.factor(ifelse(is.na(incomeRating), 'UNK', incomeRating)))

# What do we have now?
donors %>%
  select(incomeRating) %>%
  table() %>%
  prop.table()

# Let's deal with the NAs for wealthRating, urbanicity, socioEconomicStatus, isHomeowner and gender the same way.
donors <- donors %>%
  mutate(wealthRating = as.character(wealthRating)) %>%
  mutate(wealthRating = as.factor(ifelse(is.na(wealthRating), 'UNK', wealthRating))) %>%
  mutate(urbanicity = as.character(urbanicity)) %>%
  mutate(urbanicity = as.factor(ifelse(is.na(urbanicity), 'UNK', urbanicity))) %>%
  mutate(socioEconomicStatus = as.character(socioEconomicStatus)) %>%
  mutate(socioEconomicStatus = as.factor(ifelse(is.na(socioEconomicStatus), 'UNK', socioEconomicStatus))) %>%
  mutate(isHomeowner = as.character(isHomeowner)) %>%
  mutate(isHomeowner = as.factor(ifelse(is.na(isHomeowner), 'UNK', isHomeowner))) %>%
  mutate(gender = as.character(gender)) %>%
  mutate(gender = as.factor(ifelse(is.na(gender), 'UNK', gender)))

# Here's another way to deal with NAs.
# First, we create a function to deal with NAs for the variable we assign into the function.
NAs <- function(x) (x = as.factor(ifelse(is.na(as.character(x)),'UNK',as.character(x))))

# Then, we assign multiple variables to the function at the same time using mutate_at() function.
donors <- donors %>% 
  mutate_at(c('wealthRating','urbanicity','socioEconomicStatus','isHomeowner','gender'), NAs)

# What do we have now?
donors %>%
  keep(is.factor) %>%
  summary()

## Continuous features
# Get the summary statistics for the continuous features.
donors %>%
  keep(is.numeric) %>%
  summary()

# Get summary statistics for the age feature. 
summary(select(donors,age))

# There are a lot of missing values for age. We will use mean imputation (by Gender) to resolve the NAs.
donors <- donors %>%
  group_by(gender) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age)) %>%
  ungroup()

summary(select(donors,age))

# Get summary statistics for the numberChildren feature.
summary(select(donors,numberChildren))

# There are a lot of missing values for numberChildren. We will use median imputation to resolve the NAs.
# Note that we used median this time instead of mean. 
# Mean wouldn't have given us reasonable values, since we cannot have 1.5 number of children.
donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren), 
                                 median(numberChildren, na.rm = TRUE), 
                                 numberChildren))

summary(select(donors,numberChildren))

# Remove outliers for mailOrderPurchases, totalGivingAmount, numberGifts, 
# smallestGiftAmount, largestGiftAmount, and averageGiftAmount.
donors <- donors %>%
  mutate(max1 = quantile(mailOrderPurchases, .75) + (1.5 * IQR(mailOrderPurchases))) %>%
  mutate(max2 = quantile(totalGivingAmount, .75) + (1.5 * IQR(totalGivingAmount))) %>%
  mutate(max3 = quantile(numberGifts, .75) + (1.5 * IQR(numberGifts))) %>%
  mutate(max4 = quantile(smallestGiftAmount, .75) + (1.5 * IQR(smallestGiftAmount))) %>%
  mutate(max5 = quantile(largestGiftAmount, .75) + (1.5 * IQR(largestGiftAmount))) %>%
  mutate(max6 = quantile(averageGiftAmount, .75) + (1.5 * IQR(averageGiftAmount))) %>%
  filter(mailOrderPurchases <= max1) %>%
  filter(totalGivingAmount <= max2) %>%
  filter(numberGifts <= max3) %>%
  filter(smallestGiftAmount <= max4) %>%
  filter(largestGiftAmount <= max5) %>%
  filter(averageGiftAmount <= max6) %>%
  select(-max1, -max2, -max3, -max4, -max5, -max6)

# Alternatively, a Outliers() function is created.
# The same function is applied to multiple variables at the same time using the filter_at() function.
Outliers <- function(x) (x <= quantile(x, .75) + (1.5 * IQR(x)))
donors <- donors %>%
  filter_at(
    c(
      'mailOrderPurchases',
      'totalGivingAmount',
      'numberGifts',
      'smallestGiftAmount',
      'largestGiftAmount',
      'averageGiftAmount'
    ),
    Outliers
  )

# What do our summary statistics now look like?
donors %>%
  keep(is.numeric) %>%
  summary()

#----------------------------------------------------------------
#' #3. Split and Balance the Data
#----------------------------------------------------------------
# Using the sample() function, let's create our training and test datasets using a 75% to 25% split.
# The set.seed() function is used to ensure that we can get the same result every time we run a random sampling process.
set.seed(1234)
sample_set <- sample(nrow(donors), round(nrow(donors)*.75), replace = FALSE)
donors_train <- donors[sample_set, ]
donors_test <- donors[-sample_set, ]

# What is the class distribution?

# Check the proportions for the class between all 3 datasets.
round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

# We will use the SMOTE() function from the DMwR package to balance the training data before we build our model.
library(DMwR)
set.seed(1234)
donors_train <- SMOTE(respondedMailing ~ ., data.frame(donors_train), perc.over = 100, perc.under = 200)

# Check the proportions for the class between all 3 datasets.
round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

# Change the values for respondMailing from TRUE/FALSE to 0/1.
donors <- donors %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing==TRUE, 1, 0)))

donors_train <- donors_train %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing==TRUE, 1, 0)))

donors_test <- donors_test %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing==TRUE, 1, 0)))

#----------------------------------------------------------------
#' #4. Train a Model
#----------------------------------------------------------------
# Build up our logistic regression model.
donors_mod1 <-
  glm(data = donors_train,
      family = binomial,
      formula = respondedMailing ~ .)

#----------------------------------------------------------------
#' #5. Evaluate the Model
#----------------------------------------------------------------
# Get the model outputs.
summary(donors_mod1)

# View the model coefficients in exponent form.
exp(coef(donors_mod1)["averageGiftAmount"])

exp(coef(donors_mod1)["monthsSinceLastDonation"])

exp(coef(donors_mod1)["incomeRating2"])

# Generate predictions against the test data using our model.
donors_pred1 <- predict(donors_mod1, donors_test, type = 'response')

# We get an error!

# The missing states may be different due to random split of training and test dataset.
# We may need to change the missing states manually according to the error message.

# Identify observations in question.
filter(donors_test, state=="VT" | state=="WV" | state=="NH" | state=="VI")

# Get rid of them.
donors_test <- donors_test %>%
  filter(state != "VT" & state != "WV" & state != "NH" & state != "VI")

# We may still get an error due to the randomness of the setting seed.
# We can use setdiff() to get the states that are not in the training data instead of typing them in manually.
donors_test <- subset(donors_test, !(state %in% setdiff(donors_test$state, donors_train$state)))

# Generate predictions against the test data using our model.
donors_pred1 <- predict(donors_mod1, donors_test, type = 'response')
head(donors_pred1)

# Using a decision boundary of 0.5 (i.e If P(y=1|X) >= 0.5 then y="1" else y="0").
donors_pred1 <- ifelse(donors_pred1 >= 0.5, 1, 0)
head(donors_pred1)

# Create confusion matrix of our results.
donors_pred1_table <- table(donors_test$respondedMailing, donors_pred1)
donors_pred1_table

# What is the accuracy of our prediction?
sum(diag(donors_pred1_table)) / nrow(donors_test)

#----------------------------------------------------------------
#' #6. Improve the Model
#----------------------------------------------------------------
# Create correlation plot for numeric features. This creates Figure 5.4.
library(stats)
library(corrplot)
donors %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

# Get VIF for features in the model, in order to assess multicollinearity.
library(car)
vif(donors_mod1)

# Create a new model which includes only the significant features 
# and excludes the collinear features we decided to remove.
donors_mod2 <-
  glm(
    data = donors_train,
    family = binomial,
    formula = respondedMailing ~ incomeRating + wealthRating + 
      mailOrderPurchases + numberGifts + yearsSinceFirstDonation + 
      monthsSinceLastDonation + sweepstakesDonor + state + 
      urbanicity + socioEconomicStatus + isHomeowner + gender
  )
summary(donors_mod2)

# What does our VIF now look like?
vif(donors_mod2)

# Let's make evaluate the predictive accuracy of our new model.
donors_pred2 <- predict(donors_mod2, donors_test, type = 'response')
head(donors_pred2)

# This time around, we're going to use a function to get us 
# the best decison boundary based on our model's predictions.
library(InformationValue)
ideal_cutoff <-
  optimalCutoff(
    actuals = donors_test$respondedMailing,
    predictedScores = donors_pred2,
    optimiseFor = "Both"
  )
ideal_cutoff

# Use the new decision boundary to interprete the results and get our accuracy.
donors_pred2 <- ifelse(donors_pred2 >= ideal_cutoff, 1, 0)
donors_pred2_table <- table(donors_test$respondedMailing, donors_pred2)
sum(diag(donors_pred2_table)) / nrow(donors_test)