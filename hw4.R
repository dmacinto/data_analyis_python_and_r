####################################################
##                                                ##
##      BUS32100 - Hints for HW 4, #8             ##
##                                                ##
####################################################

library("tidyverse")

# 8a) read in college.csv
college <- read_csv("/Users/douglasmacintosh/Downloads/College.csv")

# 8b) remove the first column and make it a row name 
college <- rename(college, college_name = ...1)

# 8c) i) Numerical summary of the columns 

# use base R
summary(college)

# tidy option 1 - use across (preferred method)
# more info: https://dplyr.tidyverse.org/reference/across.html
college %>% summarise(across(where(is.numeric),
                         .fns = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
                        pivot_longer(cols = everything(),
                                     names_sep = "_",
                                     names_to  = c("variable", ".value"))

# 8c) ii) 

library("GGally") 

# create new tibble with only the numeric columns 
#college_num <- college %>% select(is.numeric) # older tidyverse
college_num <- college %>% select(where(is.numeric)) # newer tidyverse

# pairplot 
ggpairs(college_num) 

# 8c) iii) side-by-side boxplots of Outstate versus Private

# box plot 
ggplot(data=college, aes(y= Outstate, x=Private, fill=Outstate ) ) + 
  geom_boxplot()

# 8c) iv) new qualitative variable, called Elite, by binning the Top10perc variable

# create new feature with mutate and case statement for the condition >50, <=50
college <- college %>% mutate(elite = case_when(Top10perc > 50 ~ "Yes",
                                                Top10perc <=  50 ~ "No") ) %>% glimpse()

# count number elite colleges - option 1
college %>% group_by(elite) %>% summarise(count = n())

# boxplot 
ggplot(data=college, aes(y= Outstate, x=elite, fill=Outstate ) ) + 
  geom_boxplot()

# 8c) v) 

p1 <- ggplot(college, aes(x = F.Undergrad)) + 
  geom_histogram(bins = 60) 

p2 <- ggplot(college, aes(x = Accept)) + 
  geom_histogram(bins = 60) 

p3 <- ggplot(college, aes(x = Enroll)) + 
  geom_histogram(bins = 60) 

p4 <- ggplot(college, aes(x = P.Undergrad)) + 
  geom_histogram(bins = 60) 

# library for creating grids with ggplot2
library("gridExtra")

# create grid with charts from above 
grid.arrange(p1, p2, p3, p4, nrow = 2)

# 8c) vi) Continue exploring the data, and provide a brief summary
#of what you discover.

colMeans(college_num)

#We can see from our mean data that there are more people in fulltime undergrad 
#than parttime, which makes sense. From our pairplots earlier, we can see more 
#fulltime undergrads get their education from more faculity that have PhDs than 
#parttime. Acceptance and Enrollment are linear relationships and correlated 
#as expected. Enrollment with fulltime students is similar to Acceptance but
#parttime is not the same. Fulltime also graduate at higher rates than part time.

################################
########### (9)

# 9a) import Auto dataset and remove missing values 

auto <- read_csv('/Users/douglasmacintosh/Downloads/Auto.csv', na=c("","NA"))

head(auto)

sum(is.na(auto))

#Since the horsepower column has question marks in it, I will remove the 
#question mark rows and change the horsepower column to an integer. 
auto_cleaner = filter(auto, horsepower != '?')

auto_cleaner$horsepower = as.numeric(auto_cleaner$horsepower)

sum(is.na(auto_cleaner))

#Which of the predictors are quantitative, and which are qualitative?

#quantitative predictors: mpg, cylinders, displacement, horsepower, weight, acceleration, origin, year 
#qualitative predictors: name

auto_clean <- subset (auto_cleaner, select = -name)

head(auto_clean)

# 9b) What is the range of each quantitative predictor? You can answer
#this using the range() function.

summary(auto_clean)

auto_clean %>% summarize(across(everything(), range))

max(auto_clean$mpg, na.rm=TRUE) - min(auto_clean$mpg, na.rm=TRUE)
max(auto_clean$cylinders, na.rm=TRUE) - min(auto_clean$cylinders, na.rm=TRUE)
max(auto_clean$displacement, na.rm=TRUE) - min(auto_clean$displacement, na.rm=TRUE)
max(auto_clean$horsepower, na.rm=TRUE) - min(auto_clean$horsepower, na.rm=TRUE)
max(auto_clean$weight, na.rm=TRUE) - min(auto_clean$weight, na.rm=TRUE)
max(auto_clean$acceleration, na.rm=TRUE) - min(auto_clean$acceleration, na.rm=TRUE)
max(auto_clean$year, na.rm=TRUE) - min(auto_clean$year, na.rm=TRUE)
max(auto_clean$origin, na.rm=TRUE) - min(auto_clean$origin, na.rm=TRUE)

# 9c) What is the mean and standard deviation of each quantitative
#predictor?

colMeans(auto_clean)
sapply(auto_clean, sd)

# 9d) Now remove the 10th through 85th observations. What is the
#range, mean, and standard deviation of each predictor in the
#subset of the data that remains?

auto_less <- auto_clean[-c(10:85), ]

auto_less %>% summarize(across(everything(), range))

max(auto_less$mpg, na.rm=TRUE) - min(auto_less$mpg, na.rm=TRUE)
max(auto_less$cylinders, na.rm=TRUE) - min(auto_less$cylinders, na.rm=TRUE)
max(auto_less$displacement, na.rm=TRUE) - min(auto_less$displacement, na.rm=TRUE)
max(auto_less$horsepower, na.rm=TRUE) - min(auto_less$horsepower, na.rm=TRUE)
max(auto_less$weight, na.rm=TRUE) - min(auto_less$weight, na.rm=TRUE)
max(auto_less$acceleration, na.rm=TRUE) - min(auto_less$acceleration, na.rm=TRUE)
max(auto_less$year, na.rm=TRUE) - min(auto_less$year, na.rm=TRUE)
max(auto_less$origin, na.rm=TRUE) - min(auto_less$origin, na.rm=TRUE)

colMeans(auto_less)
sapply(auto_less, sd)

# 9e) Using the full data set, investigate the predictors graphically,
#using scatterplots or other tools of your choice. Create some plots
#highlighting the relationships among the predictors. Comment
#on your findings.

ggpairs(auto_clean) 

#We can see from this chart that displacement has a positive correlation with horsepower and weight 
#MPG has a negative nonlinear relationship with displacement, weight, and horsepower 
#Horsepower has a negative linear relationship with acceleration.

ggplot(data = auto_cleaner, mapping = aes(x = acceleration)) +
  geom_histogram(binwidth = 0.01)

#Acceleration seems to have a normal distribution curve

ggplot(data = auto_cleaner, mapping = aes(x = cylinders, colour = horsepower)) +
  geom_freqpoly(binwidth = 0.1)

#4, 6, and 8 cylinders seem to be the largest quantities in relation to horsepower 

# 9f) Suppose that we wish to predict gas mileage (mpg) on the basis
#of the other variables. Do your plots suggest that any of the
#other variables might be useful in predicting mpg? Justify your
#answer.

#Yes we can see that displacement, weight, and horsepower can contribute to 
#predicting mpg. The other variables in the dataset would likely have much
#less of an effect but could contribute to the predictive model. We would 
#have to determine the statistical significance of each of the variables before
#incorporating them into a model. 

################################
########### (10)

# 10a) load in Boston dataset

library(MASS)

?Boston
dim(Boston)

View(Boston)

#How many rows are in this data set? How many columns? What
#do the rows and columns represent?
#14 columns and 506 rows. 
#Each row is an area or town located in or around Boston. The column are categories that can affect housing prices. 

# 10b) pairplot 

#boston_numeric <- Boston %>% dplyr::select(is.numeric) # older tidyverse
boston_numeric <- Boston %>% dplyr::select(where(is.numeric)) # newer tidyverse

#Make some pairwise scatterplots of the predictors (columns) in
#this data set. Describe your findings.

#I used this article as a reference https://medium.com/analytics-vidhya/a-simple-ml-project-in-r-using-the-boston-dataset-e1143146ffb0

library(reshape2) # To reshape data

selected.attributes = dplyr::select(boston_numeric, c(crim, rm, age, rad, tax, lstat, medv))
melted = melt(selected.attributes,id="medv")
ggplot(melted, aes(x = value, y = medv, colour = variable)) +
  geom_point(alpha = 0.7) +
  geom_smooth(color='black') +
  facet_wrap(~variable, scales = "free", ncol = 2) +  # Divide the plots based on variable in 2 columns with a free scale that adjusts
  labs(x = "Variable Value", y = "Median House Price x $1000") +
  theme_minimal()

GGally::ggpairs(boston_numeric,
                progress = FALSE,
                lower = list(combo = GGally::wrap("facethist",  
                                                  bins = 40)))

#Medv appears to have a postive linear relationship with rm and a negative 
#nonlinear relationship with lstat. Lower income people typically aren't 
#able to afford high value homes and more rooms typically means a
#more expensive house. 

# 10c)

#Are any of the predictors associated with per capita crime rate?
#If so, explain the relationship.

data_cor <- cor(boston_numeric[ , colnames(boston_numeric) != "crim"],  # Calculate correlations
                boston_numeric$crim)

data_cor

#indus    0.40658341***
#nox      0.42097171***
#age      0.35273425***
#rad      0.62550515****
#tax      0.58276431****
#ptratio  0.28994558****
#lstat    0.45562148***

#As we can see with the correlation numbers, rad, tax, and lstat have the 
#highest correlations to crim. We can also see from our crim plots in the previous
#problem that a category like lstat has data thats a nonlinear relationship. 

# 10d) Do any of the suburbs of Boston appear to have particularly
#high crime rates? Tax rates? Pupil-teacher ratios? Comment on
#the range of each predictor.

boston_numeric %>% 
  arrange(desc(crim)) %>% 
  slice(1:5)

boston_numeric %>% ggplot(aes(crim)) +
  geom_histogram(bins = 30)

#Crime seems to be low for most of the Boston suburbs based on this chart. Very
#few places seem to be greater than 25. Based on our slice, we can see that the 
#top 5 crime locations at outlyers in our data. 

boston_numeric %>% 
  arrange(desc(tax)) %>% 
  slice(1:5)

boston_numeric %>% ggplot(aes(tax)) +
  geom_histogram(bins = 30)

#Tax rates span from the mid 100ks to 711k but there is an uneven distribution
#across the suburbs. There is a normal distribution around 300k and then a large 
#peak at the mid 650ks with some residual at the peak of 711k. 

boston_numeric %>% 
  arrange(desc(ptratio)) %>% 
  slice(1:5)

boston_numeric %>% ggplot(aes(ptratio)) +
  geom_histogram(bins = 30)

#There is a peak at about 20 but there seems to be some even distribution 
#throughout all of the values of ptratio. 

# 10e) How many of the suburbs in this data set bound the Charles
#river?

boston_numeric %>% count(chas == 1)

#There are 35 suburbs bound to the Charles River.

# 10f) What is the median pupil-teacher ratio among the towns in this
#data set?

summarise(boston_numeric, median_ptratio = median(ptratio, na.rm = TRUE))

#Median ptratio is 19.05.

# 10g) Which suburb of Boston has lowest median value of owner occupied
#homes? What are the values of the other predictors
#for that suburb, and how do those values compare to the overall
#ranges for those predictors? Comment on your findings.

filter(boston_numeric, medv == min(medv))

#The 2 suburbs identified in the filter above have the lowest median value
#of owner occupied homes. 

colMeans(boston_numeric)
boston_numeric %>% summarize(across(everything(), range))

#Comparing those predictors to the means, we can see that the follow predictors
#are above average: crime, indus, nox, rm, age, rad, tax, ptratio, black, and lstat.
#The following are below the mean: zn, and medv. 
#Looking at the ranges, it appears that this is the oldest aged suburb and they are 
#located near industrial zones and have a large black population. They are near 
#employment centers. The suburbs have high taxes with many zoned lots.

# 10h) In this data set, how many of the suburbs average more than
#seven rooms per dwelling? More than eight rooms per dwelling?
#Comment on the suburbs that average more than eight rooms
#per dwelling.

seven_rm = filter(boston_numeric, rm > 7 & rm < 8)
eight_rm = filter(boston_numeric, rm > 8)
seven_rm
eight_rm

colMeans(boston_numeric)
colMeans(seven_rm)
colMeans(eight_rm)

#13 suburbs average 8 or more rooms and 51 suburbs average between 7 and 8. 

#We can see that for between seven and eight rooms, they have less crime, more zoned land,
#less industrial, more likely to be on the Charles River, have more rooms than the average,
#are younger, further away from employment centers, have less access to radial highways,
#pay less taxes, better ptratio, have a large black population, less lower income people, 
#and higher median home value. 

#We can see that for greater than eight rooms, they have much less crime, more zoned land,
#less industrial, much more likely to be on the Charles River, have more rooms than the average,
#are older, closer to employment centers, have less access to radial highways,
#pay less taxes, better ptratio, have a large black population, less lower income people, 
#and much higher median home value. 

