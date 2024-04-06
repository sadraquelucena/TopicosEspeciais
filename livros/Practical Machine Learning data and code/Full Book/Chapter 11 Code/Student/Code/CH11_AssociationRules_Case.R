#' ---
#' title: "CHAPTER 11: ASSOCIATION RULES"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "March 6th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 11")

# Install the packages needed for this code file
# install.packages("arules")

library(arules)

#----------------------------------------------------------------
#' #1. Importing the Data
#----------------------------------------------------------------
# Load the retail data into a sparse matrix (Note the use of 'read.transactions' for this dataset).
groceries <- read.transactions("groceries.csv", sep = ",")

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Let's take a look at the summary stats for the dataset.
summary(groceries)

# Q:What does the density value mean?

# Q:What do the other summary statistics tell us?

# Let's take a look at the first five transactions.
inspect(groceries[1:5])

# Using the itemFrequency() function, we can create a tibble of the items and their support.
library(tidyverse)
groceries_frequency <-
  tibble(
    Items = names(itemFrequency(groceries)),
    Frequency = itemFrequency(groceries)
  )

# Get summary stats for the support values.
groceries_frequency %>%
  select(Frequency) %>%
  summary()

# Q: What are the 10 most frequently bought items at this store?
groceries_frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

#----------------------------------------------------------------
#' #3. "Train" the Model
#----------------------------------------------------------------
# Let's say we decide to include items that were purchased on average at least 5 times a day.
# Given that our data is for 30 days, we would need to set our support threshold at 0.015 -> ((5*30)/9835).
# We set our cofidence threshold to 0.25 and our minimum rule length to 2.
groceryrules <-
  apriori(groceries,
          parameter = list(
            support = 0.015,
            confidence = 0.25,
            minlen = 2
          ))

#----------------------------------------------------------------
#' #4. "Evaluate" the Model's Performance
#----------------------------------------------------------------
# Summary of the grocery association rules.
summary(groceryrules)

# Q:What do the summary stats mean?

# Let's take a look at the top 10 rules in terms of confidence.
groceryrules %>%
  sort(by = "confidence") %>%
  head(n=10) %>%
  inspect() 

# Let's take a look at the top 10 rules in terms of lift.
groceryrules %>%
  sort(by = "lift") %>%
  head(n=10) %>%
  inspect()

# Let's take a look at all the rules that do not have either "whole milk" or "other vegetables".
groceryrules %>%
  subset(!items %in% c("whole milk","other vegetables")) %>%
  sort(by = "lift") %>%
  inspect()