## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# Your code here.

# Import libraries
library(tidyverse)
library(skimr)

# Import the data
reviews <- read_csv("turtle_reviews.csv")

# clean the data
reviews <- read_csv("turtle_reviews.csv") %>%
   select(-language, -platform) %>%
   rename(renumeration = `remuneration (k£)`,
          spending_score = `spending_score (1-100)`)

# View the head of the data.
head(reviews)

# Create a summary of the new data frame.
summary(reviews)

# Structure of the data frame: column types, number of rows/columns.
str(reviews)
glimpse(reviews)

# Check for missing values.
colSums(is.na(reviews))

# --- Tables: grouped summaries of loyalty_points ---

# Average loyalty points by gender.
reviews %>%
  group_by(gender) %>%
  summarise(count = n(),
            mean_loyalty = mean(loyalty_points),
            median_loyalty = median(loyalty_points),
            sd_loyalty = sd(loyalty_points)) %>%
  arrange(desc(mean_loyalty))

# Average loyalty points by education level.
reviews %>%
  group_by(education) %>%
  summarise(count = n(),
            mean_loyalty = mean(loyalty_points),
            median_loyalty = median(loyalty_points),
            sd_loyalty = sd(loyalty_points)) %>%
  arrange(desc(mean_loyalty))

# Average loyalty points by product.
reviews %>%
  group_by(product) %>%
  summarise(count = n(),
            mean_loyalty = mean(loyalty_points)) %>%
  arrange(desc(mean_loyalty)) %>%
  head(10)

# Correlation between loyalty_points and the numeric variables.
reviews %>%
  select(age, renumeration, spending_score, loyalty_points) %>%
  cor() %>%
  round(2)

# -------Create visualisations-----------------


# Scatterplot: loyalty_points vs. spending_score.
ggplot(reviews, aes(x = spending_score, y = loyalty_points)) +
  geom_point(alpha = 0.4, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "red", se = FALSE) +
  labs(title = "Loyalty points vs. Spending score",
       x = "Spending score (1-100)",
       y = "Loyalty points") +
  theme_minimal()

# Scatterplot: loyalty_points vs. renumeration.
ggplot(reviews, aes(x = renumeration, y = loyalty_points)) +
  geom_point(alpha = 0.4, colour = "darkorange") +
  geom_smooth(method = "lm", colour = "red", se = FALSE) +
  labs(title = "Loyalty points vs. Renumeration",
       x = "Renumeration (k£)",
       y = "Loyalty points") +
  theme_minimal()

# Scatterplot: loyalty_points vs. age.
ggplot(reviews, aes(x = age, y = loyalty_points)) +
  geom_point(alpha = 0.4, colour = "forestgreen") +
  geom_smooth(method = "lm", colour = "red", se = FALSE) +
  labs(title = "Loyalty points vs. Age",
       x = "Age",
       y = "Loyalty points") +
  theme_minimal()

# Scatterplot coloured by gender, to check for an interaction/grouping effect.
ggplot(reviews, aes(x = spending_score, y = loyalty_points, colour = gender)) +
  geom_point(alpha = 0.5) +
  labs(title = "Loyalty points vs. Spending score, by gender",
       x = "Spending score (1-100)",
       y = "Loyalty points") +
  theme_minimal()

# Histogram: distribution of loyalty_points.
ggplot(reviews, aes(x = loyalty_points)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white") +
  labs(title = "Distribution of loyalty points",
       x = "Loyalty points",
       y = "Number of customers") +
  theme_minimal()

# Histogram: distribution of spending_score, for comparison.
ggplot(reviews, aes(x = spending_score)) +
  geom_histogram(bins = 30, fill = "darkorange", colour = "white") +
  labs(title = "Distribution of spending score",
       x = "Spending score (1-100)",
       y = "Number of customers") +
  theme_minimal()

# Boxplot: loyalty_points by gender.
ggplot(reviews, aes(x = gender, y = loyalty_points, fill = gender)) +
  geom_boxplot() +
  labs(title = "Loyalty points by gender",
       x = "Gender",
       y = "Loyalty points") +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot: loyalty_points by education level.
ggplot(reviews, aes(x = education, y = loyalty_points, fill = education)) +
  geom_boxplot() +
  labs(title = "Loyalty points by education level",
       x = "Education level",
       y = "Loyalty points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

# Boxplot: spending_score by gender (checking for outliers/spread).
ggplot(reviews, aes(x = gender, y = spending_score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Spending score by gender",
       x = "Gender",
       y = "Spending score (1-100)") +
  theme_minimal() +
  theme(legend.position = "none")

#------Observations and recommendations (technical + business users)---------
# - The scatterplots show a clear positive linear relationship between
#   loyalty_points and both spending_score and renumeration; age shows no
#   clear linear pattern (points are scattered with no visible trend).
# - The spending_score scatterplot also hints at possible sub-groupings/bands
#   (clusters of points) rather than one uniform cloud, which lines up with
#   the customer segments found via k-means clustering in Module 1 (Python) -
#   this is worth investigating further with formal clustering in R too.
# - The loyalty_points histogram is right-skewed: most customers accumulate a
#   low-to-moderate number of points, with a smaller number of high-value,
#   high-loyalty customers forming a long tail.
# - The boxplots by gender and education show broadly similar medians and
#   spreads across groups, with a handful of outliers (particularly high
#   loyalty_points values) in most groups - worth flagging as a segment of
#   "power users" who may respond well to targeted loyalty campaigns.




###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Your code here.


library(tidyverse)

reviews <- read_csv("turtle_reviews_clean.csv")

head(reviews)
summary(reviews)

# Descriptive statistics for the numeric variables.
reviews %>%
  select(age, renumeration, spending_score, loyalty_points) %>%
  summary()

# Correlation matrix - which numeric variables relate most strongly to
# loyalty_points, and how correlated are the predictors with each other?
cor_matrix <- reviews %>%
  select(age, renumeration, spending_score, loyalty_points) %>%
  cor() %>%
  round(3)

print(cor_matrix)

#-------Create a Multiple Linear Regression model---------


# Fit the model: loyalty_points predicted by renumeration and spending_score.
mlr_model <- lm(loyalty_points ~ renumeration + spending_score, data = reviews)

# Evaluate the goodness of fit and interpret the model summary statistics.
summary(mlr_model)

# Check the individual coefficients and their confidence intervals.
coefficients(mlr_model)
confint(mlr_model)

# --- Visual demonstration of the model ---

# Predicted vs. actual loyalty points.
reviews$predicted_loyalty <- predict(mlr_model)

ggplot(reviews, aes(x = predicted_loyalty, y = loyalty_points)) +
  geom_point(alpha = 0.4, colour = "steelblue") +
  geom_abline(intercept = 0, slope = 1, colour = "red", linewidth = 1) +
  labs(title = "Multiple linear regression: predicted vs. actual loyalty points",
       x = "Predicted loyalty points",
       y = "Actual loyalty points") +
  theme_minimal()

# --- Demonstrate how the model could be used to predict given a scenario ---

# First Example: new customer with renumeration = 45 (k£) and spending_score = 60.
new_customer <- data.frame(renumeration = 45, spending_score = 60)
predicted_points <- predict(mlr_model, newdata = new_customer, interval = "confidence")
print(predicted_points)

# Second Example: a high-renumeration, high-spending customer.
new_customer_2 <- data.frame(renumeration = 80, spending_score = 90)
predicted_points_2 <- predict(mlr_model, newdata = new_customer_2, interval = "confidence")
print(predicted_points_2)

#----------------Further exploratory statistical analysis-------------------

# ANOVA table for the model - tests whether each predictor significantly
# improves the model fit.
anova(mlr_model)

# Standardised (z-scored) coefficients, to compare the relative strength of
# renumeration vs. spending_score on a common scale.
reviews_scaled <- reviews %>%
  mutate(renumeration_z = scale(renumeration),
         spending_score_z = scale(spending_score))

mlr_model_scaled <- lm(loyalty_points ~ renumeration_z + spending_score_z, data = reviews_scaled)
summary(mlr_model_scaled)

# -----------------------------------------------------------------------------
# 6. Observations, interpretations, and suggestions
# -----------------------------------------------------------------------------
# - Yes: loyalty_points can be predicted reasonably well using a simple MLR
#   model with just two numeric features (renumeration and spending_score),
#   achieving R-squared of approximately 0.83.
# - The standardised coefficients show which of the two predictors has the
#   larger effect on loyalty_points per standard deviation of change - use
#   this to priorities where marketing effort has the most leverage.
# - The business should focus marketing/loyalty efforts on customers with
#   high renumeration and high spending_score, since they are predicted to
#   accumulate the most loyalty points; customers who are high-renumeration
#   but low-spending_score represent an under-tapped opportunity to increase
#   engagement and spend.
# - The loyalty programme could be improved by directly rewarding the
#   behaviours that drive loyalty_points (i.e. spending) more explicitly,
#   and by creating a distinct track or incentive for high-renumeration,
#   low-spending customers to encourage more frequent purchases.
# - The analysis could be improved by: including non-numeric features (e.g.
#   product category, education) via dummy variables; testing non-linear
#   models to capture any remaining non-linear relationships; and validating the model on a
#   held-out test set (train/test split) rather than fitting on the full
#   data set, to get an unbiased estimate of predictive performance on new
#   customers.



















###############################################################################
###############################################################################




