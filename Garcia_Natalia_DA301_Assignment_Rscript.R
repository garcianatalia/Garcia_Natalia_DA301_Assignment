## LSE Data Analytics Online Career Accelerator

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range
## includes books, board games, video games and toys. They have a global
## customer base and have a business objective of improving overall sales
## performance by utilising customer trends.

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###################### #########################################################

# 1. Load and explore the data

# Install and import required libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyverse) 
library(dplyr)
library(ggplot2)


# Import the data set.
turtle_sales <- read.csv("turtle_sales.csv")

# Print the data frame.
head(turtle_sales)

# Here I will be exploring the dataset
str(turtle_sales)
typeof(turtle_sales)
class(turtle_sales)
dim(turtle_sales)
# We can see the type of varibles and the numer of records and columns 
# for example we can see 352 9 for the numer of records and number of columns 

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns.
# a. Remove redundant columns (Ranking, Year, Genre, Publisher) by creating a subset of the data frame.
# Here I am creating a subset and by using (- I call a boolean) which creates a subest 
#Without those columns 
turtle_sales_subset <-
  turtle_sales %>% select(-c(Ranking, Year, Genre, Publisher))
head(turtle_sales_subset)
# b. Create a summary of the new data frame.
# here I am printing the newly created dataset 
summary(turtle_sales_subset)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
# Here I am trying the suggest approach for a scatterplot
qplot(NA_Sales, EU_Sales, data=turtle_sales_subset)

# Upon further research on R scatterplot.I decided to take thh suggest approach
# With the geom_point()+ method
# Please refer to (2021) How to Create a Scatter Plot with ggplot2 in R'Accessed on:
# https://koalatea.io/r-gglot-scatter-plot/#:~:text=To%20create%20a%20scatter%20plot%20in%20ggplot2%2C%20we,geom_point%20layer%20which%20will%20display%20a%20scatter%20plot

# Here are the 3 scatterplot here I have assigned a title and a currency for better understanding 

ggplot(turtle_sales_subset, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(title = "NA Sales vs EU Sales", x = "NA Sales (Million £)", y = "EU Sales (Million £)")

ggplot(turtle_sales_subset, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  labs(title = "NA Sales vs Global Sales", x = "NA Sales (Million £)", y = "Global Sales (Million £)")

ggplot(turtle_sales_subset, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  labs(title = "EU Sales vs Global Sales", x = "EU Sales (Million £)", y = "Global Sales (Million £)")

#Observations from graphs
#For the 3 different scatter plots we can see there is an strange distribution of the data
#where is highly concetrete towards the left from 0-10 

## 2b) Histograms

# Create histograms.

# Here I am trying the suggest approach for a scatterplot
# However is too simple and does not allow me to interpret the data much
# Hence, I have decided to undertake further research
qplot(NA_Sales, data=turtle_sales_subset)


# Hence, I have decided to undertake further research
#Please refer Ebner, J (2021) ‘How to make a histogram in R with ggplot2’.
# Accessed on: How to make a histogram in R with ggplot2 - Sharp Sight (sharpsightlabs.com)

#Here I will be undertaking 3 different graphs with all of the varibles
#I have assigned  a title and name to the variables 
# And I have plotted a line on red that is the mean 
# This will aloww us to see the difference between global sales, europe sales and north america sales 
ggplot(turtle_sales_subset, aes(x = NA_Sales)) +
  geom_histogram(color = "black", fill = "#FFFDD0") +
  labs(title = "North America Sales", x = "Sales (Million £)", y = "Frequency") +
  geom_vline(
    aes(xintercept = mean(NA_Sales)),
    color = "red",
    linetype = "dashed",
    size = 1
  )

ggplot(turtle_sales_subset, aes(x = EU_Sales)) +
  geom_histogram(color = "black", fill = "#FFFDD0") +
  labs(title = "Europe Sales", x = "Sales (Million £)", y = "Frequency") +
  geom_vline(
    aes(xintercept = mean(NA_Sales)),
    color = "red",
    linetype = "dashed",
    size = 1
  )

ggplot(turtle_sales_subset, aes(x = Global_Sales)) +
  geom_histogram(color = "black", fill = "#FFFDD0") +
  labs(title = "Global Sales", x = "Sales (Million £)", y = "Frequency") +
  geom_vline(
    aes(xintercept = mean(NA_Sales)),
    color = "red",
    linetype = "dashed",
    size = 1
  )

#Observations from graphs
#Notice how the graphs are skewed towards the right showing the distribution of the data is concenrtrated 
# From 0-10 fro all graphs 
# Moreover, notice the mean for the graphs varies for exmaple we have means from 3 for north america  and  2.5 for europe



## 2c) Boxplots
# Create boxplots.

#Here I have taken the same approach from my previous reserach
# That of using geom_boxplot

ggplot(turtle_sales_subset, aes(x = NA_Sales)) +
  geom_boxplot() +
  labs(title = "North America Sales", x = "Sales (Million £)", y = "Frequency")

ggplot(turtle_sales_subset, aes(x = EU_Sales)) +
  geom_boxplot() +
  labs(title = "Europe Sales", x = "Sales (Million £)", y = "Frequency")


ggplot(turtle_sales_subset, aes(x = Global_Sales)) +
  geom_boxplot() +
  labs(title = "Global Sales", x = "Sales (Million £)", y = "Frequency")
Global_Sales_outliers <-
  boxplot(turtle_sales_subset$Global_Sales, plot = FALSE)$out

#For this third graph I have decided to  count the number of outleirs 
# This is due to a  concern over the numebr of outleirs and the distirbution of the data
# In all the boxplots where the distribution of teh data is clearly skewed to one side 


Global_Sales_outliers
number_of_outliers <- length(Global_Sales_outliers)
number_of_outliers      
# For example the number of outliers is 22 

#At this point of the exploration I am concern over the distribution of the data 
# and its consistent skweness 
# I am concern over the impact this might have when creating linear regression modelsZ
###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

## In order to be able to analyze all that has been done and observed in the
## diagrams, we must take into account the fact that sales records are highly
## variable data which depend on many external factors.
## 
## Taking into account the above -  for the scatterplot, we can
## notice a visible correlation between NA_Sales and Global_Sales unlike the
## other two correlations that were performed (NA_Sales vs EU_Sales & EU_Sales
## vs Global_Sales) which show more dispersion of the data. 

##The histograms and box plots made from sales in North America, Europe and global sales,
## show an asymmetry to the right where sales increase. It is also interesting to
## note that the box plots show a considerable amount of outliers which are
## identified as events were there was a  spike in sales, in the case of
## global sales the maximum outlier was £67.85 million.

#  since the  data we are  working with is from  sales 
# outliers should be taken into account when performing calculations, analysis and predictions, since they can affect the
## statistical parameters of our data because they are so far away from the  mean
#therefore, it is important to keep them in mind and verify if their  value 

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore,
## strongly encouraged to first clean the data as per provided guidelines and
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment.
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into
##     the Sales data.

##  - Note your observations and diagrams that could be used to provide
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.

head(turtle_sales_subset)
sales <- turtle_sales_subset %>%
  select(NA_Sales, EU_Sales, Global_Sales)

# Check output: Determine the min, max, and mean values.
sales_min <- sales %>%
  summarise_all(min)
sales_max <- sales %>%
  summarise_all(max)
sales_mean <- sales %>%
  summarise_all(mean)
(sales_min)
(sales_max)
(sales_mean)

#Here we can calling the function and then we call the metadata
# By doing so, we can then call for the summary of all min , max and mean
# This is valuable information for example we can double check the mean found
# Previouslty on the histograms
# or we can checl for the max sales in the EU which is 23.8

# View the descriptive statistics.
(summary(sales))


#sense check the data 
sum(is.na(sales))
#There are non null values 
###############################################################################

# 2. Determine the impact on sales per product_id.


## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

sales_by_product_id <- turtle_sales_subset %>%
  group_by(Product) %>%
  summarise(
    NA_Sales = sum(NA_Sales),
    EU_Sales = sum(EU_Sales),
    Global_Sales = sum(Global_Sales)
  )



# View the data frame.

(head(sales_by_product_id))

#Here we can see the sm of the product per category 


# Explore the data frame.

summary(sales_by_product_id)

## 2b) Determine which plot is the best to compare game sales.

# Create scatterplots.

ggplot(sales_by_product_id, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(title = "NA Sales vs EU Sales", x = "NA Sales (Million £)", y = "EU Sales (Million £)")

ggplot(sales_by_product_id, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  labs(title = "NA Sales vs Global Sales", x = "NA Sales (Million £)", y = "Global Sales (Million £)")

ggplot(sales_by_product_id, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  labs(title = "EU Sales vs Global Sales", x = "EU Sales (Million £)", y = "Global Sales (Million £)")

#Obsevations
# Notice after aggregation the data with the sum function 
# The scatter plot  changes - the data seem more conceetrate towars the y axis 
# Create histograms.

ggplot(sales_by_product_id, aes(x = NA_Sales)) +
  geom_histogram(color = "black", fill = "#FFFDD0") +
  labs(title = "NA Sales", x = "NA Sales (Million £)", y = "Frequency")+
  geom_vline(
    aes(xintercept = mean(NA_Sales)),
    color = "red",
    linetype = "dashed",
    size = 1
  )
ggplot(sales_by_product_id, aes(x = EU_Sales)) +
  geom_histogram(color = "black", fill = "#FFFDD0") +
  labs(title = "EU Sales", x = "EU Sales (Million £)", y = "Frequency")+
  geom_vline(
    aes(xintercept = mean(NA_Sales)),
    color = "red",
    linetype = "dashed",
    size = 1
  )

ggplot(sales_by_product_id, aes(x = Global_Sales)) +
  geom_histogram(color = "black", fill = "#FFFDD0") +
  labs(title = "Global Sales", x = "Global Sales (Million £)", y = "Frequency")+
  geom_vline(
    aes(xintercept = mean(NA_Sales)),
    color = "red",
    linetype = "dashed",
    size = 1
  )

# Create boxplots.

ggplot(sales_by_product_id, aes(x = NA_Sales)) +
  geom_boxplot(color = "black", fill = "#FFFDD0") +
  labs(title = "NA Sales", x = "NA Sales (Million £)", y = "Frequency")

ggplot(sales_by_product_id, aes(x = EU_Sales)) +
  geom_boxplot(color = "black", fill = "#FFFDD0") +
  labs(title = "EU Sales", x = "EU Sales (Million £)", y = "Frequency")

ggplot(sales_by_product_id, aes(x = Global_Sales)) +
  geom_boxplot(color = "black", fill = "#FFFDD0") +
  labs(title = "Global Sales", x = "Global Sales (Million £)", y = "Frequency")

#Observations 
#We can see with the for both histograms and boxplots after the dum function 
# The distribution of data has changed slightly 
# more towards the right

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Here I am crating Q plots with the different variables 

#Graph 1 
qqnorm(sales_by_product_id$NA_Sales)
qqline(sales_by_product_id$NA_Sales)
# This graph displays data is skewed towards the right
# As it showeed previously on the other plots 

#Graph 2 
qqnorm(sales_by_product_id$EU_Sales)
qqline(sales_by_product_id$EU_Sales)

#Graph 3
qqnorm(sales_by_product_id$Global_Sales)
qqline(sales_by_product_id$Global_Sales)

# We can see all of the graphs are skewed towards the right 
# This means the distribution of the data is not normally distributed 

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.

library(moments)


# Perform Shapiro-Wilk test.
# Here I will performing the shapiro test to check the p value 
=k8

#This test confirmed my doubts the data is not normally distributed 
# The p value is greater than 0.05 adn hence there is a rejection of the Ho
# In other other words this data i not optimal for analysis 

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# Here I will be checking for Skewness and kurotis of the varibles in sales 

skewness_NA <- skewness(sales_by_product_id$NA_Sales)
skewness_EU <- skewness(sales_by_product_id$EU_Sales)
skewness_Global <- skewness(sales_by_product_id$Global_Sales)
kurtosis_NA <- kurtosis(sales_by_product_id$NA_Sales)
kurtosis_EU <- kurtosis(sales_by_product_id$EU_Sales)
kurtosis_Global <- kurtosis(sales_by_product_id$Global_Sales)
print(paste("Skewness NA_Sales:", skewness_NA))
print(paste("Skewness EU_Sales:", skewness_EU))
print(paste("Skewness Global_Sales:", skewness_Global))
print(paste("Kurtosis NA_Sales:", kurtosis_NA))
print(paste("Kurtosis EU_Sales:", kurtosis_EU))
print(paste("Kurtosis Global_Sales:", kurtosis_Global))

#These values are very concerning 
# The skewness is of 2.8 - 3 
# The kurtosis os of 15- 17 
# Both values  are way above baseline values showning the data is assmetrical 
# and that is not well distributed 

## 3d) Determine correlation
# Determine correlation.

correlation_NA_EU <-
  cor(sales_by_product_id$NA_Sales, sales_by_product_id$EU_Sales)
correlation_NA_Global <-
  cor(sales_by_product_id$NA_Sales,
      sales_by_product_id$Global_Sales)
correlation_EU_Global <-
  cor(sales_by_product_id$EU_Sales,
      sales_by_product_id$Global_Sales)
print(paste("Correlation NA_Sales and EU_Sales:", correlation_NA_EU))
print(paste(
  "Correlation NA_Sales and Global_Sales:",
  correlation_NA_Global
))
print(paste(
  "Correlation EU_Sales and Global_Sales:",
  correlation_EU_Global
))
# here the number for correlation is showing a better value 
# Good correlation is showned for every variable


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.

# Choose the type of plot you think best suits the data set and what you want
# to investigate. Explain your answer in your report.

# Scatter plots:

ggplot(sales_by_product_id, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(title = "NA Sales vs EU Sales", x = "NA Sales (Million £)", y = "EU Sales (Million £)")

ggplot(sales_by_product_id, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  labs(title = "NA Sales vs Global Sales", x = "NA Sales (Million £)", y = "Global Sales (Million £)")

ggplot(sales_by_product_id, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  labs(title = "EU Sales vs Global Sales", x = "EU Sales (Million £)", y = "Global Sales (Million £)")





###############################################################################

# 5. Observations and insights
# Your observations and insights here...

## Based on all the diagrams constructed and the data analyzed by the Shapiro-Wilk test,
## Skewness and Kurtosis and the correlation calculated and the Q-Q graph which shows that the sales  are not close to the trend line.

# the Shapiro- Wilk test shows a value less than 0.05 which means we  fail to reject the H0
#for   skewness the US, Europe and global sales have positive values, therefore the graph is positively skewed with most of the data smaller than
## the mean and with an interesting concentration on the left side of the graph.


## kurtosis values are greater than 3, which represents a leptokurtic shape in the
## distribution of the data and there should be a sharp peak in the graph.
## Finally,  there is a high correlation between North American sales and global sales.



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment.
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
(sales_by_product_id)
# Determine a summary of the data frame.
(summary(sales_by_product_id))

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
sales_cor1 <-
  cor(sales_by_product_id$NA_Sales, sales_by_product_id$EU_Sales)
sales_cor2 <-
  cor(sales_by_product_id$Global_Sales,
      sales_by_product_id$NA_Sales)
sales_cor3 <-
  cor(sales_by_product_id$Global_Sales ,
      sales_by_product_id$EU_Sales)
print(sales_cor1)
print(sales_cor2)
print(sales_cor3)
# Create a linear regression model on the original data.
linear_regression_model1 <-
  lm(Global_Sales ~ NA_Sales, data = sales_by_product_id)
print(summary(linear_regression_model1))
## 2b) Create a plot (simple linear regression)
# Basic visualisation

ggplot(sales_by_product_id, aes(Global_Sales, NA_Sales)) + geom_point() + geom_smooth(method = 'lm', se = TRUE)+
  labs(title = "EU Sales vs Global Sales", x = "EU Sales (Million £)", y = "Global Sales (Million £)")


###############################################################################

# 3. Create a multiple linear regression model

# Here I am creating a mutiple lienar regression model. 
# with NA-SALES AND EU sales 

# Select only numeric columns from the original data frame.
sales_numeric <- select_if(turtle_sales, is.numeric)
multiple_linear_regression_model1 <-
  lm(Global_Sales ~ NA_Sales + EU_Sales , data = sales_numeric)
#View summary of the multiple linear regression model
print(summary(multiple_linear_regression_model1))

#For the plot, we can see there is a clear correlation between two variables 
# The line of best fit seems to fit very nicely with many outleirs 
#

###############################################################################
# 4. Predictions based on given values
# Compare with observed values for a number of records.
#Predict global sales based on provided
#values. Compare your prediction to the observed value(s).
#a. NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
#b. NA_Sales_sum of 3.93
#and EU_Sales_sum of 1.56.
#c. NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
#d. NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
#e. NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

#creating a new dataframe for assing the given values, predict, and compare
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
Global_Sales <- c(67.85, 6.04, 4.32,  3.53, 23.21)
sales_predict <- data.frame(NA_Sales, EU_Sales, Global_Sales)
sales_predict$Global_Sales_predict <-
  predict(multiple_linear_regression_model1, newdata = sales_predict)
print(sales_predict)


#To double check model I have created a global_sales_predict 
# In order to see if the prediction were correct 
# and the predictions were very good proving the model is good 

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

## Again, we emphasize the correlation between North American
## sales and global sales, with a correlation coefficient greater than 0.90.


## This strong correlation was of great use when
## performing the prediction models by linear regression and multiple
## linear regression, yielding good results.


## the final comparison of the actual values and
## the values predicted by the model were  used to make the predictions and then stored in a dataframe.


## Europe and global sales, its correlation was higher than 0.80 

###############################################################################
###############################################################################
