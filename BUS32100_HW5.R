####################################################
##                                                ##
##      BUS32100 - HW 5                           ##
##                                                ##
####################################################

# Notes:  

# 1) You do NOT need to submit the tidy data! Submit only your code
# If we have any doubts about whether your code works, we will re-run it ourselves

# 2) Choose THREE of the four possible problems below! 
# You can try all four, but you only _need_ to do three for full credit 

# 3) The answer _can_ be that the data is tidy already
# If you'd like to claim a dataset is already tidy, 
# please provide a few sentences explaining your rationale

# $) To-do: For each of the following datasets, provide code to tidy the dataset 
# (or explanation for how it's already tidy)
# If you need a refresher on what tidy data means: https://r4ds.had.co.nz/tidy-data.html
# We also covered several examples in class

library(tidyverse)
library(readxl)

# you may need to change the below to fit your folder organization! 
#getwd()
#setwd("../week7")

# Remember the rules of tidy data? 
# 1. Each variable must have its own column.
# 2. Each observation must have its own row.
# 3. Each value must have its own cell.

####

### I completed question 1, 3, and 4. 

#####

########################
###### 1) Climate change data
# Source: https://data.world/worldbank/climate-change-data
# Don't download from source, use Canvas version as I've made edits to make it easier

# Remember the rules of tidy data? 
# 1. Each variable must have its own column.
# 2. Each observation must have its own row.
# 3. Each value must have its own cell.

climate <- read_excel('/Users/douglasmacintosh/Downloads/climate_change_edit.xls')
View(climate)

climate <- climate %>% 
  pivot_longer(c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`,
                 `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`,
                 `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`,`2011`), names_to = "Year", values_to = "Value Count", names_transform = list(Year=as.integer))

summary(climate)

unique(climate[[4]])

#It appears that the Series Name items are all in different units, which may be confusing.

unique(climate[[5]])

#After reviewing the SCALE column, it appears that it is 0 throughout the entire column so we should be able to remove that with no issues to the dataset. 

climate <- climate %>% select(-SCALE)

unique(climate[[5]])

unique(climate[[3]])

#After reviewing information on the dataset, it is unclear to me exactly what the Decimals column provides. It appears to be 1 for values in the Series name column,
#if that item was a percentage and 0 if the item was a kg or other unit of weight. If we were to do further data analysis, I would likely remove it.
#but since it's an isolated column. It's also already tidy because it is a variable single column that has each of its values in a cell, we can keep it in the data. 

climate_clean <- climate[climate$'Value Count' != '..', ]

#I removed all of the .. values which should leave remove all of the years that observations were not recorded. 

View(climate_clean)

#I've gone back and forth if I should pivot_wider Series Name/Series Code. Similar to Country Code and Country Name,
#Series Name and Series Code are the same information presented in different ways. 
#I chose to keep the columns since the information in them was tidy already.
#I decided to not pivot_wider the Series Name column because I felt like it was unnecessary 
#since if we look at our tidy rules, I felt that the Series Name was the variable 
#and each entry was a value of that variable, each observation had it's 
#own row after reviewing the clean data, and each value has its own cell.

########################
###### 2) City health data
# Source: https://data.world/health/big-cities-health

# Remember the rules of tidy data? 
# 1. Each variable must have its own column.
# 2. Each observation must have its own row.
# 3. Each value must have its own cell.
health <- read_csv('/Users/douglasmacintosh/Downloads/Big_Cities_Health_Data_Inventory.csv')

# there are duplicates! how do we check and then get rid of duplicate rows?
which(duplicated(health))
health <- health %>% distinct() 

which(duplicated(health))

########################
###### 3) Activities
# Source: https://cfss.uchicago.edu/notes/tidy-exercise/
# Note: the link above has the answer! 
# We're relying on your honesty to not just copy/paste it :)

# Remember the rules of tidy data? 
# 1. Each variable must have its own column.
# 2. Each observation must have its own row.
# 3. Each value must have its own cell.
load(file="/Users/douglasmacintosh/Downloads/activities.rda")
View(activities)

#It looks like we will need to separate actions from the times in the columns
#and make sure the counts go to the correct cells. 
activities <- pivot_longer(
  data = activities, cols = c(work.T1, play.T1, talk.T1, work.T2, play.T2, talk.T2), 
  names_to = "stuff", values_to = "count", names_transform = list(stuff=as.character)) %>%
  separate(stuff, into = c("action", "times"))

activities_clean <- activities%>%
  pivot_wider(names_from = action, values_from = count)


########################
###### 4) TB cases
# Source: https://www.who.int/teams/global-tuberculosis-programme/data
# Hints for similar dataset (code is in Python, just need to translate to R!): https://towardsdatascience.com/whats-tidy-data-how-to-organize-messy-datasets-in-python-with-melt-and-pivotable-functions-5d52daa996c9
# Don't download from source, use Canvas version as I've made edits to make it easier

# Remember the rules of tidy data? 
# 1. Each variable must have its own column.
# 2. Each observation must have its own row.
# 3. Each value must have its own cell.
tb <- read_csv('/Users/douglasmacintosh/Downloads/TB_notifications_2020_edited.csv')
View(tb)

#Country, iso3, and iso_numeric are all country codes. It probably isn't necessary to have all 3. Maybe remove 2 or join the 3 columns into one. 
#I won't delete any of those 3 columns as they are in tidy form. It is probably too much unnecessary information for me when analyzing later but for
#the purpose of the exercise, it meets the tidy data intent. 

tb <- pivot_longer(
  data = tb, cols = c(new_sp, new_sp_m04,new_sp_m514, new_sp_m014, new_sp_m1524,
                              new_sp_m2534, new_sp_m3544, new_sp_m4554, new_sp_m5564,
                              new_sp_m65, new_sp_mu, new_sp_f04, 
                              new_sp_f514, new_sp_f014, new_sp_f1524,
                              new_sp_f2534, new_sp_f3544, new_sp_f4554, new_sp_f5564, 
                              new_sp_f65, new_sp_fu), names_to = "notification type", values_to = "count") 

#I will remove the NAs from the table to clean up the data further and remove 
#unnecessary rows so that each observation has its own row. 

tb_clean <- na.omit(tb)







