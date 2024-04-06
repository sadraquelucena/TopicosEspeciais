#' ---
#' title: "CHAPTER 12: K-MEANS CLUSTERING"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "March 8th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 12")

# Install the packages needed for this code file
# install.packages(c("stats","factoextra","gridExtra","cluster"))

library(tidyverse)

#----------------------------------------------------------------
#' #1. Load the Data
#----------------------------------------------------------------
# Load and preview the colleges and universities dataset.
college <- read_csv("college.csv", col_types = "nccfffffnnnnnnnnn")

# Get a preview of the data.
glimpse(college)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Let's limit our dataset to only colleges in the state of Maryland.
# We also convert the name of each college to the row labels.
maryland_college <- college %>%
  filter(state == "MD") %>%
  column_to_rownames(var = "name")

# Let's take a look at the summary stats for the dataset.
maryland_college %>%
  select(admission_rate, sat_avg) %>%
  summary()

# The summary statistics show a wide range of values for our features.
# In order to avoid features with large ranges from dominating our model,
# we need to normalize the features using the scale() function for z-score normalization.
maryland_college_scaled <- maryland_college %>%
  select(admission_rate, sat_avg) %>%
  scale()

# What do we have now?
maryland_college_scaled %>%
  summary()

#----------------------------------------------------------------
#' #3. "Train" the Model
#----------------------------------------------------------------
library(stats)

# We are now ready to attempt to cluster the data.
# We set the value for k to 3 and choose to use 25 different initial configurations.
# The use of set.seed() ensures that every time we run our code, we get the same results.
set.seed(1234)
k_3 <- kmeans(maryland_college_scaled, centers=3, nstart = 25)

#----------------------------------------------------------------
#' #4. "Evaluate" the Model's Performance
#----------------------------------------------------------------
# Let's take a look at the size of the clusters...
k_3$size

# ...and the cluster centers.
k_3$centers

# We can also visualize the clusters to get additional insight.
library(factoextra)
fviz_cluster(k_3,
             data = maryland_college_scaled,
             repel = TRUE,
             ggtheme = theme_minimal()) + theme(text = element_text(size = 14))

# To further evaluate our results, we need to look at how attributes vary by cluster.
maryland_college %>%
  mutate(cluster = k_3$cluster) %>%
  select(cluster,
         undergrads,
         tuition,
         faculty_salary_avg,
         loan_default_rate,
         median_debt) %>%
  group_by(cluster) %>%
  summarise_all("mean")

#----------------------------------------------------------------
#' #5. "Improve" the Model's Performance
#----------------------------------------------------------------
# Let's see how varying the number of clusters affects the results.
k_4 <- kmeans(maryland_college_scaled, centers = 4, nstart = 25)
k_5 <- kmeans(maryland_college_scaled, centers = 5, nstart = 25)
k_6 <- kmeans(maryland_college_scaled, centers = 6, nstart = 25)

# Plot and compare the results.
p1 <- fviz_cluster(k_3, geom = "point", data = maryland_college_scaled) + ggtitle("k = 3")
p2 <- fviz_cluster(k_4, geom = "point", data = maryland_college_scaled) + ggtitle("k = 4")
p3 <- fviz_cluster(k_5, geom = "point", data = maryland_college_scaled) + ggtitle("k = 5")
p4 <- fviz_cluster(k_6, geom = "point", data = maryland_college_scaled) + ggtitle("k = 6")

# Here, we make use of the grid.arrange() function in the gridExtra package to display several plots at the same time.
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Q: Can you tell what the ideal number of clusters should be from the visuals?

# Now let's try to choose an ideal value for k based on the elbow method.
# Note: The geom_point() function is used to create additional circles on the plot.
fviz_nbclust(maryland_college_scaled, kmeans, method = "wss") +
  geom_point(
    shape = 1,
    x = 4,
    y = 7.3,
    colour = "red",
    size = 8,
    stroke = 1.5
  ) + 
  geom_point(
    shape = 1,
    x = 7,
    y = 2.3,
    colour = "red",
    size = 8,
    stroke = 1.5
  )

# What about based on the silhouette method?
fviz_nbclust(maryland_college_scaled, kmeans, method = "silhouette") +
  geom_point(
    shape = 1,
    x = 4,
    y = 0.393,
    colour = "red",
    size = 8,
    stroke = 1.5
  ) +
  geom_point(
    shape = 1,
    x = 7,
    y = 0.375,
    colour = "red",
    size = 8,
    stroke = 1.5
  )

# Now, let's see what the gap statistic tells us.
(maryland_college_scaled, kmeans, method = "gap_stat") +
  geom_point(
    shape = 1,
    x = 1,
    y = 0.218,
    colour = "red",
    size = 8,
    stroke = 1.5
  ) +
  geom_point(
    shape = 1,
    x = 7,
    y = 0.2,
    colour = "red",
    size = 8,
    stroke = 1.5
  )

# Alternatively, we can use fviz_gap_stat() function to generate the similar plot for gap statistics.
# Note: The two plot may not be exactly alike because fviz_nbclust() function does not specify the nstart argument. 
library(cluster)
set.seed(1234)
gap_stat <- clusGap(maryland_college_scaled, FUN = kmeans, nstart = 25, K.max = 10, B = 10)
fviz_gap_stat(gap_stat)

# Q: What is the optimal value for k?

k_4 <- kmeans(maryland_college_scaled, centers = 4, nstart = 25)

fviz_cluster(
  k_4,
  data = maryland_college_scaled,
  main = "Maryland Colleges Segmented by SAT Scores and Admission Rates",
  repel = TRUE,
  ggtheme = theme_minimal()
) +
  theme(text = element_text(size = 14))
