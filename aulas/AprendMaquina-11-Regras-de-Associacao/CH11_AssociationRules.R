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
#' #1. Load the Data
#----------------------------------------------------------------
# Load the retail data into a sparse matrix (Note the use of 'read.transactions' for this dataset).
supermart <- read.transactions("retail.txt", sep = "")

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Let's take a look at the summary stats for the dataset.
summary(supermart)

# Let's take a look at the first five transactions.
# Note that for a sparse matrix the arules package provides the inspect() function.
inspect(supermart[1:5])

# Using itemFrequency() function, we can examine the frequency of an item.
itemFrequency(supermart[ ,"39"])

# Now, we can convert the data to a tibble.
library(tidyverse)
supermart_frequency <-
  tibble(
    Items = names(itemFrequency(supermart)),
    Frequency = itemFrequency(supermart)
  )

# What do we have?
head(supermart_frequency)

# Q: What are the 10 most frequently bought items at this store?
supermart_frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

#----------------------------------------------------------------
#' #3. "Train" the Model
#----------------------------------------------------------------
# Let's say we decide to include items that were purchased on average at least 5 times a day.
# Given that our data is for 150 days, we would need to set our support threshold at 0.0085 -> ((5*150)/88162).
# We set our cofidence threshold to 0.5 and our minimum rule lenygth to 2.
supermartrules <-
  apriori(supermart,
          parameter = list(
            support = 0.0085,
            confidence = 0.5,
            minlen = 2
          ))

supermartrules

#----------------------------------------------------------------
#' #4. "Evaluate" the Model's Performance
#----------------------------------------------------------------
# Summary of the grocery association rules.
summary(supermartrules)

# Let's take a look at the first 10 rules.
inspect(supermartrules[1:10])

#----------------------------------------------------------------
#' #5. "Improve" the Model's Performance
#----------------------------------------------------------------
# In practice, we usually will have hundreds/thousands of rules generated from our data.
# Since we cannot (or should not) manually parse through hundreds of rules,
# we need to find ways to identify the rules that may be useful to us.

# Let's start by sorting the grocery rules by lift and examining the top 10.
supermartrules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect() 

# Suppose we want to figure out whether item '41' is also often purchased with other items. 
# We would use the subset() function to find subsets of rules containing item '41'.
supermartrules %>%
  subset(items %in% "41") %>%
  inspect()

# And, if we wanted to see the top 10 rules in terms of lift that include item '41'?
# We would combine both the sort() and subset() functions to do this.
supermartrules %>%
  subset(items %in% "41") %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()


# Note that the subset() function can be used with several keywords and operators:
# - The keyword 'items', matches an item appearing anywhere in the rule.
# - Limit the subset with 'lhs' and 'rhs' instead.
# - The operator %in% means that at least one of the items must be found in the list you defined.
# - For partial matching (%pin%) and complete matching (%ain%).
# - We can also filter by support, confidence, or lift.
# - We can also combine standard R logical operators such as and (&), or (|), and not (!).

