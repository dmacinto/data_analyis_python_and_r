####################################################
##                                                ##
##      BUS32100 - HW 6                           ##
##                                                ##
####################################################

# For this assignment, let's make sure we know how to do in R what we can
# already do in Python! 

# Please recreate HW 1: exploratory data analysis (EDA)
# Use the same Seattle (King County) home prices dataset on Canvas
# I've copied over the high-level instructions from the Python version of the assignment below
# This time, though, there's no helper code! 
# ***PLEASE*** ask questions if you're stuck for more than 15 minutes! 

# 1) Set up the environment by importing required libraries -- which will you need?

library(tidyverse) #for data analysis
library(ggplot2) #for graphing
library(GGally) #for ggpairs
library(reshape2) #for correlation heatmap

# 2) Read in the dataset (refer to the prior HW for how to read in a csv!)

kc_data <- read_csv("/Users/douglasmacintosh/Downloads/kc_house_data.csv")
options(na.action = na.warn)

# 3) Report the summary statistics (min, max, median, etc) of the numeric columns

summary(kc_data)

# 4) Report some information on all the columns (number of nulls, for example). 
# In Python, we used .info() for this. 

colSums(is.na(kc_data))

str(kc_data)

sapply(kc_data, class)

sapply(kc_data, length)

# 5) Rename a column in the dataset

kc_data <- kc_data %>% rename(price_sold = price)

# 6) Count how many 2, 3, 4-bedroom houses are in the dataset

dplyr::count(kc_data, bedrooms, sort = TRUE)

#To pull out specific bedroom values

three_bed <- filter(kc_data, bedrooms == 3)
four_bed <- filter(kc_data, bedrooms == 4)
five_bed <- filter(kc_data, bedrooms == 5)

count(three_bed)
count(four_bed)
count(five_bed)

kc_data %>% filter(bedrooms == 3) %>% count() #for another dplyr method 
kc_data %>% filter(bedrooms == 4) %>% count() #for another dplyr method 
kc_data %>% filter(bedrooms == 5) %>% count() #for another dplyr method 

# 7) Plot a bar chart of the number of bedrooms in Seattle homes

ggplot(kc_data) +
  geom_bar(aes(x = bedrooms))

# 8) What is the mean number of bedrooms? 

mean(kc_data$bedrooms)

#We also saw this information when we used the summary function in Q3. 

# 9) Subsetting and selecting rows: 
# Perhaps we're interested only in the data for houses with 4 or fewer bedrooms. 
# Perhaps homes with 5 or more bedrooms are quite different and 
# we're more interested in more typical homes. 
# Can we create a copy of the dataset with only homes with fewer than 5 bedrooms? 

five_bed_less <- filter(kc_data, bedrooms < 5)
five_bed_less

# 10) Filter on multiple criteria
# You don't have to limit yourself to filtering on a single criteria. 
# Try it again, this time by combining any filtering criteria you'd 
# like (e.g. bathrooms < 3 or sqft_living > 500). You pick the criteria! 

filter(kc_data, bedrooms < 3, sqft_living >800, bathrooms > 2)

# 11) Find null values for price_sold
# Are there any houses where the sold price is null? 

sum(is.na(kc_data$price_sold))

#There are no houses with null price. We discovered this in Q2 as well. 

# 12) Create any groupby and apply any function (e.g. mean) you would like

kc_data %>% group_by(bedrooms)  %>%   
  summarise(median_baths = median(bathrooms),
            min_price_sold = min(price_sold),
            max_price_sold = max(price_sold),
            avg_price_sold = mean(price_sold),
            median_condition = median(condition),
            avg_sqft_lot = mean(sqft_lot),
            .groups = 'drop')

# 13) Create a pairplot of the numeric columns in the dataset
# Refer to HW 4 for how to do a pairplot in R

new_df <- kc_data %>% select(-id, -date) #id and date were the only columns that aren't not numeric (see #1)

ggpairs(new_df) 

# 14) OPTIONAL: Create a correlation heatmap 
# Correlation tells us how much two variables move in the same direction (or not). 
# Correlation is measured for each pair of columns/fields. 
# You can put this information in a table. 
# But a table of correlations has a lot of numbers and can be hard to interpret, 
# which is why a heatmap is commonly used. 
# The heatmap just turns the numbers in a correlation table into colors, 
# so that we can more quickly spot fields/columns that are highly correlated.

corr_df <- kc_data %>% select(-date)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_df)

head(melted_corr_mat)
# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=variable, y=value,
                                   fill=value)) +
      geom_tile()

#I couldn't figure out why my correlation matrix wasn't generating boxes on the chart. 


