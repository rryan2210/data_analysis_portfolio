## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

## In particular, Turtle Games wants to understand:
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

###############################################################################

# 1. Load and explore the data

# Determine the working directory
getwd()

# Change the current directory.
setwd(dir = 'C:/Users/roser/Documents/LSE Data Analytics/Course 3/Assignment') 

# Install and import Tidyverse and other packages. 
install.packages('tidyverse')
install.packages('DataExplorer') 
install.packages('skimr') 
install.packages('ggplot2')

# Load the packages into current working environment. 
library(tidyverse)
library(skimr)
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
library(moments)

# 2. Import the data set.
data <- read.csv('turtle_sales.csv')

# Print the data frame.
data
View(data)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
newdata <- select(data, -Ranking, -Year, -Genre, -Publisher) 

# View the data frame.
View(newdata)

# View the descriptive statistics.
summary(newdata)

skimr(newdata)

# Create a downloadable HTML file containing summary stats of
# the data set.
DataExplorer::create_report(newdata)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

qplot(Product, Global_Sales, data=newdata)

qplot(x = Product, y=Global_Sales, data=newdata)


## 2b) Histograms
# Create histograms.
qplot(Product, data=newdata)

qplot(x = Product, data = newdata, geom = "histogram", bins = 15) +
  labs(title = "Global Sales", x = "Product", y = "Sales") +
  theme_minimal()


## 2c) Boxplots
# Create boxplots.

qplot(Product, Global_Sales, data=newdata, geom='boxplot')

qplot(Product, EU_Sales, data=newdata, geom='boxplot')

qplot(Product, NA_Sales, data=newdata, geom='boxplot')


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
View(newdata)

summary(newdata)

# Check output: Determine the min, max, and mean values.
mean(newdata$Global_Sales) 
min(newdata$Global_Sales)
max(newdata$Global_Sales)

mean(newdata$EU_Sales) 
min(newdata$EU_Sales)
max(newdata$EU_Sales)

mean(newdata$NA_Sales) 
min(newdata$NA_Sales)
max(newdata$NA_Sales)

# View the descriptive statistics.
library(pastecs)
stat.desc(dat)

summary(newdata)

###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots

# Specify qqnorm function (draw a qqplot).
qqnorm(newdata$Global_Sales)

# Specify qqline function.
qqline(newdata$Global_Sales)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments') 
library(moments)

# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(newdata$Global_Sales)

## 3c) Determine Skewness and Kurtosis
skewness(newdata$Global_Sales)
kurtosis(newdata$Global_Sales)

skewness_value <- skewness(newdata$Global_Sales)
kurtosis_value <- kurtosis(newdata$Global_Sales)

# Print skewness and kurtosis values
cat("Skewness:", skewness_value, "\n")
cat("Kurtosis:", kurtosis_value, "\n")

## 3d) Determine correlation
# Determine correlation.
cor(newdata$NA_Sales, newdata$EU_Sales)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

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

# 1. Load and explore the data
# View data frame created in Week 5.
View(newdata)

# Determine a summary of the data frame.
summary(newdata)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
# Import libraries for linear regression:
library(tidyverse)
library('datarium')

data("newdata", package = "datarium")
head(newdata)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

ggplot(newdata, aes(x = Product, y = Global_Sales)) + geom_point() + ggtitle("Global Sales")

ggplot(newdata, aes(x = Product, y = EU_Sales)) + geom_point() + ggtitle("EU Sales")

ggplot(newdata, aes(x = Product, y = NA_Sales)) + geom_point() + ggtitle("NA Sales")

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.





