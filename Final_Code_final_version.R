# Used Cars Price Prediction

setwd("D:\\Dissertation\\New_version")
getwd()

# Install the libraries
 
# install.packages("skimr")        # Summary statistics
# install.packages("pastecs")      # Summary statistics
# install.packages("Hmisc")        # Correlation matrix
# install.packages('ggplot2')      # Data Visualization
# install.packages('dplyr')        # Data manipulation
# install.packages('tidyr')        # Data manipulation
# install.packages("stats")        # summary statistics
# install.packages("ggcorrplot")   # Correlation Plots
# install.packages("agricolae")    # Kruskal-wallis Test 
# install.packages("caret")        # regression training
# install.packages("Boruta")       # Feature selection
# install.packages("MASS")         # Step-wise regression
# install.packages("caTools")      # Sample split of data
# install.packages("earth")        # Multivariate analysis
# install.packages("readr")        # Parse numbers from currency percentage values 
# install.packages("janitor")      # groupby function
# install.packages("randomForest") # Random forest model 
# install.packages("xgboost")      # eXtreme Gradient Boosting
# install.packages("car")

#Load the libraries

library(readr)
library(skimr)
library(pastecs)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(tidyr) 
library(stats)
library(ggcorrplot)
library(agricolae)
library(Boruta)
library(MASS)
library(caret)
library(caTools)
library(earth)
library(janitor)
library(randomForest)
library(xgboost)
library(car)

#Import the Used cars data set
data <- read.csv("vehicles.csv")

#View the data set
View(data)

#Check the dimension of the data set
dim(data)

#There are 426880 rows/observations and 26 columns/attributes

glimpse(data)
str(data)

#The data set contains 4 numeric, 19 character datatypes, 2 integer datatypes
#and 1 logical datatype

# A better overview of the complete data set through skim() function

skim(data)
summary(data)
unique(data$manufacturer)
unique(tail(data$model))
length(unique(data$model))
unique(data$condition)
unique(data$cylinders)
unique(data$fuel)
unique(data$title_status)
unique(data$transmission)
length(unique(data$VIN))
unique(data$drive)
unique(data$size)
unique(data$type)
unique(data$paint_color)
unique(data$state)

#From the summary listed. The basic observations are:

# 1) The data set is imported from local directory (Primarily downloaded 
#    from Kaggle)
# 2) There are 426880 observations and 26 attributes
# 3) There are more character variables than numerical
# 4) There are a huge number of NA and missing values
# 5) Specially 'count' variable is NULL attribute
# 6) The statistical data of numerical variables is given but not very clear
# 7) Distribution of numerical data is also given but it can be opened 
#    in markdown file
# 8) The target variable is price and it is continuous variable
# 9) Regression analysis is the requirement in this task

# Further observation of data set
describe(data)

# through this function analyzing the whole data set is difficult since, 
# description has lot of content which is not feasible to show in single window

stat.desc(data, basic=TRUE, desc=TRUE, norm=FALSE)

# In this we can understand that there are outliers in the numerical variable
# The range of variables is huge, but max value and avg value difference is more
# Further in the process, using boxplots let us visualize the outliers

NAsummary<- summarize_all(data, funs(sum(is.na(.))))
View(NAsummary)

data_Null <- data    #Duplicate the data frame
data_Null[data_Null== "" | data_Null == " "] <- NA    
View(data_Null)

NAsummary_NAs<- summarize_all(data_Null, funs(sum(is.na(.))))
View(NAsummary_NAs)


# With all the variables let us proceed building the baseline model
#___________________
# Baseline Model
#___________________

# baseline <- lm(price~., data = data)
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
# contrasts can be applied only to factors with 2 or more levels

# You get this error because factors are more than 2 levels
# When creating the baseline model with each attribute, you encounter another error

# baseline <- lm(price~id+region+url, data = data)
# Error: cannot allocate vector of size 1357.7 GB

# ((The reason for commenting is when run at once error pops up))

#________________________________________
# Elimination of Non-value Add variables
#________________________________________

# Attributes have NULL values and urls in the observation
# These attributes will not add any value for analysis

# Let us count the unique values of all the attributes

unique_values <- sapply(data, unique) # Create a list of unique values
unique_counts <- sapply(unique_values, length) # Create a vector of length
unique_values<- data.frame(Attribute = names(data), Unique_Values = unique_counts) 
View(unique_values)


# A data frame with unique values is created
View(unique_values)

#It is observed that id, URL, posting date, description and image_url has more than 300000 unique values
#Hence, let us remove these attributes and run the baseline.model

df <- subset(data, select = -c(url, id, posting_date, description, image_url))

#baseline <- lm(price~., data = df)
# Repeated errors is leading to suspicious for noise in data set
# Hence, evaluating each attribute and elimination of attributes is required

#summary(baseline)

#Let us consider attributes which are misleading

#Analysis of county attribute 
#Analysis of number of unique values in other categorical attributes

unique(df$county)
length(unique(df$county))
table(df$county)

#All values are NULL Values in county attribute
#Hence, it is better to eliminate the attribute

df <- subset(df, select = -county)

#Analysis of region attribute
#Analysis of number of unique values in other categorical attributes
unique(df$region)
length(unique(df$region))
table(df$region)

#Related to region other attribute state is considered
#Analysis of number of unique values in other categorical attributes
unique(df$state)
length(unique(df$state))
table(df$state)

#Since attributes region and state are similar and state is more general,
#Region attribute can be removed due to 404 unique values 
#State has 51 unique values

df <- subset(df, select = -region)
dim(df)

#Likewise, region_url will also be eliminated since source link will not add any value

df <- subset(df, select = -region_url)

#There are 18 attributes and 8 attributes are eliminated

#Analysis of Vehicle Identification Number
unique(df$VIN)
length(unique(df$VIN))
table(df$VIN)

#VIN - Vehicle Identification Number will be eliminated 
#There are 118265 values which will not add value to price 

df <- subset(df, select = -VIN)
dim(df)

#After filtering data set 17 attributes are left

#Analysis of unique values in 'model' attribute
unique(df$model)
table(df$model)
length(unique(df$model))

#There are a lot of unique cars and label encoding will mis-lead the results and hence we remove the column

df <- subset(df, select = -model)

#The location of the car is of non-value added

df <- subset(df, select = -lat)
df <- subset(df , select = -long)

dim(df)
colnames(df)

#Analysis of 'Manufacturer' attribute
unique(df$manufacturer)
freq_manufacturer <- table(df$manufacturer)
View(freq_manufacturer)
length(unique(df$manufacturer))
names(freq_manufacturer)

#There are 43 different manufacturers but mostly, the main contenders in the market are:
# Ford, Toyota, Dodge, BMW, Chevrolet, GMC ,Honda, Jeep, Nissan and Benz

#Based on initial analysis of data, 12 variables are removed

#___________________________________________
#Removal of NULL values is the next step
#___________________________________________

NAsummary<- summarize_all(df, funs(sum(is.na(.))))
View(NAsummary)

# There are very few NA values when compared to the entire data set observation number
# But more than NA values, missing values are more in the data set

df_new <- df    #Duplicate the data frame
df_new[df_new== "" | df_new == " "] <- NA    
View(df_new)

#To make things simpler missing values are filled with NA values

NA_summary<- summarize_all(df_new, funs(sum(is.na(.))))
View(NA_summary)

#If we observe there more than 3 lakh NA values in Size, hence it will be removed

df_new <- subset(df_new, select = -size)

df_new <- drop_na(df_new)
NA_summary<- summarize_all( df_new, funs(sum(is.na(.))))
print(NA_summary)

#No NA values in the data set

dim(df_new)
#The dimension is reduced to 117169 rows and 13 attributes which is still high in number for further analysis

View(df_new)
str(df_new)
glimpse(df_new)
# Finally all the NA values are removed from the data set

# Converting year into number of years the car has aged before selling or posting the ad.
# But Posting date in the data set, all the dates are from 2021 and a certain months and the highest year is 2022
# Considering the negative value occurrence, let us consider 2022

df_new <- df_new %>% mutate(year = 2022 - year)

# Deleting the duplicate rows to reduce redundancy and inconsistency in the model
# Distinct Function can be used for this action

df2 <- distinct(df_new)
dim(df2)

# There are 90775 rows and 13 attributes
# All NULL values are successfully eliminated

#______________________________
# Understanding how market is
#______________________________

#### Manufacturer

manu_group <- df2 %>% group_by(manufacturer)

# Calculate mean, median of price
manu_price <- manu_group %>% 
  summarize(mean = mean(price),
            median = median(price),
            by = manufacturer)

View(manu_price)

s <- df5 %>%
  group_by(manufacturer) %>%
  summarise(mean_price = mean(price))
View(s)

# As expected that top brands like Ferrari has very high average price $107,438
# But Jeep has also very high average price $150,717 indicating 
# only top-end models are being sold
# Surprisingly Chevrolet has high price, around $115,676 
# Implying Corvette models are also in used market
# Chevy, merc-benz, ferrari and volvo has high average price but median is less
# The data has outliers which has few cars having high priced cars

#### State 

state_group <- data %>% group_by(state)

# Calculate mean, median of price
state_price <- state_group %>% 
  summarize(mean = mean(price),
            median = median(price))

View(state_price)

# Though the medians are more or less comparable, the average price is high
# In Delaware it has an average price of $3205055, might be due to zero state tax
# New jersey a commercial place and Indiana where basket ball players live, 
# the average price of the cars is high or it can be said expensive cars exist

#### Paint_color

paint_group <- data %>% group_by(paint_color)

# Calculate mean, median of price
paint_price <- paint_group %>% 
  summarize(mean = mean(price),
            median = median(price))

View(paint_price)

# Surprisingly green has the highest average though not preferred much
# May be due to rare cars or vintage cars the price would have shooted up
# Not a surprise, silver has high average and comparable median
# The resale price is always high with respect to silver color

#___________________________
# Exploratory Data Analysis
#___________________________

theme_set(theme_bw())

# Comparing each manufacturer with respect to price

manu_count <- ggplot(df2, aes(x = manufacturer)) +
  geom_bar(stat = "count", fill = "skyblue") + 
  xlab("Manufacturers in U.S.A") +
  ylab("Number of cars") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
manu_count + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) 
manu_count + theme(axis.title.size = rel(1.5), axis.text.size = rel(1.5))

# The top three contenders in the used car market race is 'Ford' 'Chevy' and 'Toyota'

# Comaparing State attribute

state_count <- ggplot(df2, aes(x = state)) +
  geom_bar(stat = "count", fill = "skyblue") + 
  xlab("States in U.S.A") +
  ylab("Number of cars") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
state_count + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) 

state_price <- ggplot(df5, aes(price, state, fill=state)) +
  geom_boxplot(alpha = 0.5)
state_price + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) 

# Comparing the 'Paint Color' Attribute

paint_price<- ggplot(df5, aes(price, paint_color, fill = paint_color)) +
  geom_boxplot(alpha = 0.5)
paint_price + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20)) 

ggplot(df5, aes(price, fill = state)) +
  geom_boxplot(alpha = 0.5, color = 'black')








ggplot(df2, aes(x = type, y = price)) +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue") + 
  xlab("Manufacturers") +
  ylab("Average Price of the car") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df2, aes(x = , y = price)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue") + 
  xlab("Manufacturers") +
  ylab("Average Price of the car") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#__________________________
# Outlier Detection
#__________________________

# There are only 3 numerical variables 
# In the processed data after removing NaNs and duplicates

# Starting with the primary dependent variable ~ 'PRICE'

summary(df2$price)
# The minimum value is 0 and maximum value has power of up to 9
boxplot(df2$price, ylab = "Price of used-cars", 
        varwidth = TRUE,
        col = "skyblue")
hist(df2$price)

q1 <- quantile(df2$price, probs = 0.882)
q1.1 <- quantile(df2$price, probs = 0.05)

# Percentage of quantity of data set was tried several times 
# Hence, we have tried all the data possible
# Finally this value is arrived to completely eliminate outliers
df3<- df2

df3 <- subset(df2, price <= q1 & price >= q1.1)

summary(df3$price)
dim(df3)
boxplot(df3$price, col = "skyblue",
        varwidth = TRUE, 
        ylab = "Priceof Used-cars")

# Outliers in 'Price' attribute is completely eliminated
# Distribution plot of the dependent variable

hist(df3$price, col = 'skyblue')

# Alternative Plot
theme_set(theme_bw())
ggplot(df3, aes(x = price))+
  geom_histogram(bins=50)

# Next continuous variable is 'Year'
summary(df3$year)

# Box plot to check if outliers are present
boxplot(df3$year)  

q2 <- quantile(df2$year, probs = 0.942)

# Probability or the percentage of quantity of data set was tried several times 
# If the value is 0, it is not eliminated because there are cars which are new
# Finally this value is arrived to completely eliminate outliers

df4 <- subset(df3, year<= q2)

stat.desc(df4$year)
boxplot(df4$year, varwidth = TRUE, col = 'skyblue', 
        ylab = 'Age of the car (in yrs)')
dim(df4)
summary(df4$year)

# Outliers are completely eliminated from 'Year' variable
# Distribution of the same attribute

hist(df4$year, col = 'green')

# With more bins ggplot can be used
theme_set(theme_bw())
ggplot(df4, aes(x = year))+
  geom_histogram(bins=25)

# The last numerical attribute is "Odometer"

stat.desc(df4$odometer)
summary(df4$odometer)
View(table(df4$odometer))
boxplot(df4$odometer)

q3 <- quantile(df4$odometer, probs = 0.985)

df5 <- subset(df4, odometer <= q3)

dim(df5)
summary(df5$odometer)
boxplot(df5$odometer, varwidth = TRUE, 
        col = 'skyblue', ylab = 'Number of Miles run by car')

# The odometer which is zero is not eliminated, 
# because new cars would have driven

# Distribution of Odometer
hist(df5$odometer)

# With more bins ggplot can be used
theme_set(theme_bw())
ggplot(df5, aes(x = odometer))+
  geom_histogram(bins=50)

# Successfully elimination of outliers are removed from the attribute

#____________________________
# Correlation Analysis
#____________________________

dim(df5)
# After elimination of outliers in the data set 
# 74,205 observations and 13 attributes are left

str(df5)

# Considering only numerical attributes for correlation analysis

correlation_df <- subset(df5, select = c(price, year, odometer))

# Creating a dataframe with correlation co-efficients 
cor_matrix <- rcorr(as.matrix(correlation_df))
cor_matrix

cor_m <- cor(correlation_df)

# Plot the correlation matrix as a heat map
ggcorrplot(cor_m, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, method = "square")

# Low correlation is observed between the variables
# But logically and technically these attributes are important for car pricing
# The variables are retained if the variables are statistically significant

# Checking whether the same two numerical variables are significant 

cor.test(df5$price, df5$year)
cor.test(df5$price, df5$odometer)

# Though the variables are not highly correlated, the attributes are statistically significant
# Both the numerical variables are checked with the respondent variable and they are found significant

# Let us visualize the distribution of price and year
f1 <- lm(df5$price ~ df5$year)

ggplot(df5, aes(x = year, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

# Let us visualize the distribution of price and odometer
f2 <- lm(df5$price ~ df5$odometer)

ggplot(df5, aes(x = odometer, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

# The points are scattered vigorously due to low correlation even then, 
# the attributes with us having numerical values are only two and 
# cannot be eliminated due to technical reason also.

# Since considering Pearson Correlation cannot give judgement about a categorical 
# and numerical variable. Hence, other tests are tried in the next step

#________________________________________________________
# Statistical Significance tests between 
# respondent variable and other categorical variables
#________________________________________________________

#It is observed that data is right skewed and the distribution is not normal

#Let us try normalize using log() and scale() functions

log_price <- log(df5$price)  #Taking logarithmic of price attribute
log_price <- as.data.frame(log_price)
hist(log_price)

scale_price <- scale(df5$price) #Scaling using min-max for price attribute
scale_price <- as.data.frame(scale_price)
hist(scale_price)

# The target variable is not normally distributed
# The variable is right-skewed before and after scaling 
# With logarithmic, the variable is left-skewed

#Hence, the 'price' attribute does not follow normal distribution

#Test for equal variances between a categorical & numerical variable
bartlett.test(price ~ manufacturer, data =df5) 

#It is observed that both normality and variance quality is not observed between 'price' and 'manufacturer'

#Hence, Let us proceed with Non-parametric test Kruskal-wallis test

#Non-parametric statiscal significance test 

kruskal.test(price~manufacturer, data = df5)

#In-more detail let us check for category wise and filter the data using agricolae package
#The package and code is extracted from 
#Kruskal-Wallis H-Test for Oneway Analysis of Variance (ANOVA) by Ranks by 
#Thomas W. MacFarland & Jan M. Yates (Reference Paper)

# Load the agricolae package.
help(package=agricolae) # Show the information page.
sessionInfo() # Confirm all attached packages.

options(max.print=1000000)
agricolae::kruskal(df5$price, df5$manufacturer, 
                   alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)
# More or less all the manufacturers are significantly,
# but Land rover cannot be compared to any manufacturer
# Let eliminate the rows related to it
# 
df5 <- filter(df5, manufacturer != "land rover")
table(df5$manufacturer)
dim(df5)

#Next Attribute is 'Condition'

bartlett.test(price ~ condition, data = df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~condition, data = df5)

agricolae::kruskal(df5$price, df5$condition, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#All the categories in condition is statistically significant
#Hence we do not need to eliminate any rows

#Next Attribute is 'Cylinders'

bartlett.test(price~cylinders, data = df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~cylinders, data = df5)

agricolae::kruskal(df5$price, df5$cylinders, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#Taking the majority and the number of rows is relatively very less
#We shall remove remove this specific category

#Next attribute 'Fuel'

bartlett.test(price~fuel, data = df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~fuel, data = df5)

agricolae::kruskal(df5$price, df5$fuel, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#All the categories are significant with each other 
#Hence none of the categories are eliminated

#Next attribute is 'title_status'

bartlett.test(price~title_status, data=df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~title_status, data = df5)

agricolae::kruskal(df5$price, df5$title_status, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#All the categories are significant with each other 
#Hence none of the categories are eliminated

#Next attribute is 'Transmission'

bartlett.test(price~transmission, data=df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~transmission, data=df5)

agricolae::kruskal(df5$price, df5$transmission, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#All the categories are significant with each other
#Hence none of the categories are eliminated

# Next Attribute is 'Drive'

bartlett.test(price~drive, data=df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~drive, data=df5)

agricolae::kruskal(df5$price, df5$drive, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#All the categories are significant with each other
#Hence none of the categories are eliminated

# Next Attribute is 'Type'

bartlett.test(price~type, data = df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~type, data=df5)

agricolae::kruskal(df5$price, df5$type, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#More or less all the categories are significant with each other
#Hence none of the categories are eliminated

# Next attribute is 'Paint_Color'

bartlett.test(price~paint_color, data=df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~paint_color, data=df5)

agricolae::kruskal(df5$price, df5$paint_color, alpha = 0.05, group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#More or less all the categories are significant with each other
#Hence none of the categories are eliminated

# Next attribute is 'State'

bartlett.test(price~state, data=df5)
#Null hypothesis of variances being equal is rejected

kruskal.test(price~state, data=df5)

agricolae::kruskal(df5$price, df5$state, alpha = 0.05, 
                   group = FALSE, 
                   p.adj="holm",
                   main="Kruskal-Wallis Using agricolae::kruskal() Function", 
                   console=TRUE)

#More or less all the categories are significant with each other

dim(df5)
View(df5)

# Due to the data set constraint, only 2 independent variables are numeric
# All the other variables are categorical

#__________________________________________
# Data Preparation and Data Partitioning
#__________________________________________

#Preparation of the data by encoding the attributes since categorical

# Before proceeding into further steps, title status and condition is similar
# I was expecting one of the attribute will not significant and other can be eliminated
# But both the attributes are significant, hence, title status does not match with dataset,
# because many titles are missing in the data set
# Hence, removal title status is necessary
df5 <- subset(df5, select = -title_status)

df6<- df5
dim(df6)
View(df6)
glimpse(df6)

#First let us convert all the Character strings into Factors

df6$manufacturer <- as.factor(df6$manufacturer)
df6$cylinders <- as.factor(df6$cylinders)
df6$fuel <- as.factor(df6$fuel)
df6$transmission <- as.factor(df6$transmission)
df6$drive <- as.factor(df6$drive)
df6$state <- as.factor(df6$state)
df6$paint_color <- as.factor(df6$type)
df6$type <- as.factor(df6$type)
df6$condition <- as.factor(df6$condition)
str(df6)

#All the variables are encoded using label encoding

#The units of price, odometer and year are different
#By scaling, making unit less will provide better results

final <- df6 %>% mutate(price = scale(price), year = scale(year), 
                        odometer = scale(odometer))
View(final)
str(final)

dim(final)
str(final)
final <- as.data.frame(final)

# Setting the seed number to make sure results will be repeat whenever it runs
# The random number generated will remain the same
# In R usually most of the people use 123
set.seed(123) 

#Splitting the data set using sample.split function from caret package
splitting_data <- sample.split(row.names(final), 0.7)    
train <- final[splitting_data, ]  #70% train data
test <- final[!splitting_data,]   #30% test data

#______________________
# Feature Selection
#______________________
#_____________________________________________________________________________
# Using Boruta Package, Random Forest algorithm is used for feature selection
#_____________________________________________________________________________

set.seed(123)
Boruta.cars <- Boruta(price ~ ., data = train, doTrace = 2)
head(Boruta.cars)
boruta_signif <- names(Boruta.cars$finalDecision[Boruta.cars$finalDecision %in% c("Confirmed", "Tentative")])  

# collect Confirmed and Tentative variables
print(boruta_signif)
plot(Boruta.cars, cex.axis=1.0, las=2, main="Variable Importance by Boruta Package")  # plot variable importance
View(attStats(Boruta.cars))

#__________________________________________
# Step-wise Regression
#__________________________________________

base_reg <- lm(price ~ 1 , data= train)  # base intercept only model
every_reg <- lm(price ~ . , data= train)   # Model with all predictors
summary(every_reg)

#Performing step-wise regression in both forward and backward selection
#Hence, in step we can eliminate
stepwise_reg <- step(base_reg, scope = list(lower = base_reg, 
                                            upper = every_reg), 
                     direction = "both", 
                     trace = 0, steps = 1000)  # perform step-wise algorithm
summary(stepwise_reg)
imp_var <- names(unlist(stepwise_reg[[1]]))
shortlistedVars <- names(unlist(stepwise_reg[[1]])) # get the shortlisted variable.
# get the shortlisted variable.
imp_var <- imp_var[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(imp_var)
summary(stepwise_reg)

# The regression results indicate that 'Paint_color' is not significant
# So even if the variable is present or not the R-square wil not change much
# After testing with and without this variable, adjusted-Rsquare value remained same
# But in business perspective this attribute is important
# In approach-2 let us give some meaning to this variable
# Hence, we retain this variable for modelling

#_________________________
# Multi-linear Regression
#_________________________

mod_lm <- lm(price~., data = train)
summary(mod_lm)

# Adjusted R-square value is 71.22% which is explaining the variance percentage

avPlots(mod_lm)

#These plots give a scatter plot for each predictor variable w.r.t respondent variable
# Keeping other predictors constant

pred_lm <- predict(mod_lm, test)
pred_lm #Predicted values of price based on test data

#Scatter plot between predicted and actual values
fit_lm <- lm(price ~ pred_lm, data = test)

ggplot(test, aes(x = pred_lm, y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab(" Predicted values of price from Multi-lieanr regression")+
  ylab("Actual Values of Price")

diff_act_pred <- (test$price - pred_lm)
mse = mean((diff_act_pred)^2)
mae = mean(abs(diff_act_pred))
rmse = sqrt(mse)
R2 = 1-(sum((diff_act_pred)^2)/sum((test$price-mean(test$price))^2))

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", R2)

#__________________
# Random Forest
#__________________


set.seed(123)
mod_rf <- randomForest(price~., data = train, ntree = 150 ,
                       importance = TRUE, mtry = 2)

print(mod_rf)
plot(mod_rf)
str(mod_rf)
pred_rf <- predict(mod_rf, test)

# Fit the line between actual and predicted values

f1 <- lm(price ~ pred_rf, data = test)

ggplot(test, aes(x = pred_rf, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

# RMSE on training set
RMSE(pred_rf, test$price)
# RMSE of this optimal random forest
sqrt(mod_rf$mse[which.min(mod_rf$mse)])

# Get variable importance from the model fit
imp_var_rf <- as.data.frame(importance(mod_rf))
imp_var_rf$Var.Names <- row.names(imp_var_rf)

ggplot(imp_var_rf, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
# Both function and calculation are giving same RMSE score
# The RMSE score is very less and the Rsquare value is good

plot(P11, train$price, pch = 19, col = "black")
points(P11, train$price, col = "green", pch = 19)
varImp(mod_rf)
varImpPlot(mod_rf)

#Scatter plot between predicted and actual values
fit_rf <- lm(price ~ pred_prices_rf, data = test)

ggplot(test, aes(x = pred_prices_rf, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

# Source of code: 

#_________________________________
# eXtreme Gradient Boost (XGBoost)
#_________________________________

boston = MASS::Boston
dim(boston)

library(xgboost)

#define final training and testing sets
View(train)
xtrain <- data.matrix(train[,-1])
ytrain<- train[,1]

xtest <- data.matrix(test[,-1])
ytest <- test[,1]

xgb_train = xgb.DMatrix(data = xtrain, label = ytrain)
xgb_test = xgb.DMatrix(data = xtest, label = ytest)

#defining a watch list
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteration
mod_RMSE = xgb.train(data = xgb_train, max.depth = 3, 
                       watchlist=watchlist, nrounds = 555, 
                       params = list(booster = 'gblinear', 
                                     objective = 'reg:linear'))

#define final model
mod_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 555, verbose = 0)
print(mod_xgboost)

#use model to make predictions on test data
pred_xgb = predict(mod_xgboost, xgb_test)

d <- ytest - pred_xgb
mse = mean((ytest - pred_xgb)^2)
mae = caret::MAE(ytest, pred_xgb)
rmse = caret::RMSE(ytest, pred_xgb)
R2 = 1-(sum((d)^2)/sum((ytest-mean(ytest))^2))

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse, "R-Square", R2)

View(test)

fit_xgb <- lm(price~pred_xgb, test)
ggplot(test, aes(x = pred_xgb, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(xgb_train), model = mod_xgboost)
importance_matrix

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

# Source of code: https://www.datatechnotes.com/2020/08/regression-example-with-xgboost-in-r.html
#___________________________________________
# Light Gradient Boosting Machine Algorithm
#___________________________________________

install.packages("lightgbm")
library(lightgbm)
xtrain <- train[,12]
ytrain <- train[,-12]

xtest <- test[,12]
ytest <- test[,-12]

train_lgb = lgb.Dataset(xtrain, label = ytrain)
test_lgb = lgb.Dataset.create.valid(train_lgb, xtest, label = ytest)

# define parameters
params = list(
  objective = "regression"
  , metric = "l2"
  , min_data = 1L
  , learning_rate = .3
)

# validataion data
valids = list(test = test_lgb)

# train model 
mod_lgb = lgb.train(
  params = params
  , data = train_lgb
  , nrounds = 5L
  , valids = valids
)

lgb.get.eval.result(mod_lgb, "test", "l2")

# prediction
pred_lgb = predict(mod_lgb, xtest)

d2 <- ytest - pred_lgb
# accuracy check
mse = mean((ytest - pred_lgb)^2)
mae = caret::MAE(ytest, pred_lgb)
rmse = caret::RMSE(ytest, pred_lgb)
R2 = 1-(sum((d2)^2)/sum((ytest-mean(ytest))^2))


cat("MSE: ", mse, "\nMAE: ", mae, "\nRMSE: ", rmse, "R-square", R2)

fit_lgb <- lm(price~pred_lgb, test)
ggplot(test, aes(x = pred_lgb, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

lgb_imp = lgb.importance(mod_lgb, percentage = TRUE)
lgb.plot.importance(lgb_imp, top_n = 12L, measure = "Gain")

#Source of code : https://www.datatechnotes.com/2022/04/lightgbm-regression-example-in-r.html#:~:text=LightGBM%20is%20an%20open%2Dsource,and%20other%20machine%20learning%20tasks.

#*******************************************************************************

cat("NOTE: After many iterations and trials, the categorical variables converted
      as factors, it was understood that ensemble machine learning algorithms
      will not work due to factorization. Hence, data is not sufficient for 
      analysis. Therefore, for few attributes numerical equivalence is 
      introduced and making the study interesting")

#*******************************************************************************

write.csv(df5,"D:/Dissertation/New_version/update_dataset.csv")

# To handle categories some research is required. Since the categorical 
# variables are having more than 45 categories With label encoding and 
# further factorizing will not give solution in regression trees

updated <- read.csv('update_dataset.csv')
View(updated)

dim(updated)

head(updated)

# There is also noise in this data set (because of lookup function)
# Those columns to be removed before analysis 
updated <- subset(updated, select = -X)
updated <- subset(updated, select = -c(X.1, X.2, X.3, X.4))

# There is also source attribute which contains url which is a non-value add

updated <- subset(updated, select = -Source)
dim(updated)

summary(updated)

#NULL values observed in major repair risk and sales tax

str(updated)
glimpse(updated)

updated <- subset(updated, select = -title_status)
#Title status and condition is similar to each other and hence,
# condition has better info than, hence eliminating it

# Maintenance cost and major repair risk are in strings datatype, 
# though it has a value

updated$Maintenance.Cost <- parse_number(updated$Maintenance.Cost)
updated$Major.Repair.Risk <- parse_number(updated$Major.Repair.Risk)
updated$Depreciation.percentage <- parse_number(updated$Depreciation.percentage)
updated$Sales.Tax <- parse_number(updated$Sales.Tax)

str(updated)

#Let us find the descriptive statistics of new attributes

summary(updated$Maintenance.Cost)
skim(updated$Maintenance.Cost)

#No missing values in this attribute
#The minimum value is $1600 and maximum is 22,075$ with average of $10000

boxplot(updated$Maintenance.Cost)

#The reason we have taken boxplot is checking which measure is better to consider
#There are outliers implying there are cars which are having very high maintenance
#It is better to consider median instead of mean since extreme values can be ignored

hist(updated$Maintenance.Cost)

#The maintenance costs can be divided into three categories
#High maintenance, minimum maintenance and average maintenance

updated$Maintenance.Hierarchy[updated$Maintenance.Cost <= 9625] ="Minimum Maintenance"
updated$Maintenance.Hierarchy[updated$Maintenance.Cost > 9625 & updated$Maintenance.Cost <= 11079] = "Average Maintenance"
updated$Maintenance.Hierarchy[updated$Maintenance.Cost > 11079] = "High Maintenance"

head(updated)

#Next variable is major repair risk
#The percentage of major repair of a car in 10 years
#Since, the median and mean value of the age of car is greater than 10 years
#Assuming car is in good condition, we will calculate the probability of occurrence

update_1 <-updated

update_1$Major.Repair.Risk[is.na(update_1$Major.Repair.Risk)] <- 
  mean(update_1$Major.Repair.Risk, na.rm = TRUE) 

summary(update_1$Major.Repair.Risk)
boxplot(update_1$Major.Repair.Risk)
hist(update_1$Major.Repair.Risk)

update_1$Repair_risk[update_1$Major.Repair.Risk <= 18.61] ="Low Risk"
update_1$Repair_risk[update_1$Major.Repair.Risk > 18.61 & 
                            update_1$Major.Repair.Risk <= 22.74] = "Medium Risk"
update_1$Repair_risk[update_1$Major.Repair.Risk > 22.74] = "High Risk"

View(update_1)
unique(update_1$Repair_risk)

update_1$Repair_risk[update_1$condition == 'salvage'] = "Low Risk"

# Salvage vehicles would have undergone major breakdown and insured 
# Assuming the cars are insured and repaired, the vehicles are at low risk 

# Next attribute introduced is Sales tax which is with respect to state wise in US

summary(update_1$Sales.Tax)

#NAs are present in this attribute
boxplot(update_1$Sales.Tax)
hist(update_1$Sales.Tax)

summary(update_1$Sales.Tax)

update_1$Sales.Tax[is.na(update_1$Sales.Tax)] <- 
  mean(update_1$Sales.Tax, na.rm = TRUE) 

update_1$Sales.Tax

summary(update_1$Sales.Tax)

update_1$State_tax[update_1$Sales.Tax <= 5.36] ="Low tax"
update_1$State_tax[update_1$Sales.Tax > 5.36] = "High tax"

head(update_1)
unique(update_1$State_tax)

#Depreciation Percentage based upon paint_color

summary(update_1$Depreciation.percentage)

update_1$Value_dip[update_1$Depreciation.percentage <= 14] ="Low resale"
update_1$Value_dip[update_1$Depreciation.percentage > 14 
                   &update_1$Depreciation.percentage<= 15.5] = "Medium resale"
update_1$Value_dip[update_1$Depreciation.percentage > 15.5] = "High resale"

View(update_1)
unique(update_1$type)
colnames(update_1)

#Let us perform one-hot encoding for all the variables

one_hot <- subset(update_1, select = -c(manufacturer, Maintenance.Cost, 
                                        Major.Repair.Risk, 
                                        paint_color, Depreciation.percentage, 
                                        state, Sales.Tax))

# These variables are replaced with Repair_risk, Value_dip, State_tax & Maintenance Hierarchy

View(one_hot)
str(one_hot)

one_hot$condition <- as.factor(one_hot$condition)
one_hot$cylinders <- as.factor(one_hot$cylinders)
one_hot$fuel <- as.factor(one_hot$fuel)
one_hot$transmission <- as.factor(one_hot$transmission)
one_hot$drive <- as.factor(one_hot$drive)
one_hot$type <- as.factor(one_hot$type)
one_hot$Maintenance.Hierarchy <- as.factor(one_hot$Maintenance.Hierarchy)
one_hot$State_tax <- as.factor(one_hot$State_tax)
one_hot$Repair_risk <- as.factor(one_hot$Repair_risk)
one_hot$Value_dip <- as.factor(one_hot$Value_dip)

str(one_hot)

dummy<- dummyVars('~.', data=one_hot)
one_hot <- data.frame(predict(dummy, newdata = one_hot ))

View(one_hot)

final <- one_hot %>% mutate(price = scale(price), year = scale(year), 
                        odometer = scale(odometer))
head(final)

# Hence, all the variables are converted into  one_hot encoding
# All the numerical variables are scaled
# Let us proceed with feature selection and training and testing data split

#____________________________________________________
# Data Partition for Feature Selection and Modeling
#____________________________________________________

dim(final)
glimpse(final)
final <- as.data.frame(final)

# Setting the seed number to make sure results will be repeat whenever it runs
# The random number generated will remain the same
# In R usually most of the people use 123
set.seed(123) 

#Splitting the data set using sample.split function from caret package
New_split <- sample.split(row.names(final), 0.7)    
train_new <- final[New_split, ]  #70% train data
test_new <- final[!New_split,]   #30% test data

head(train_new)
x <- train_new[,-1]   #All predictor variables saved as dataframe
Y <- train_new$price  #Dependent variable stored as vector


library(glmnet)


set.seed(123)
train_index <- sample(1:nrow(final), 0.8 * nrow(final))
train <- final[train_index, ]
test <- final[-train_index, ]

# Create a matrix of predictors for the training set
x_train <- model.matrix(~ ., data = train)[, -1]

# Create a vector of responses for the training set
y_train <- train$price

ridge_model <- glmnet(x_train, y_train, alpha = 0.5)

x_test <- model.matrix(~ ., data = test)[, -1]
y_test <- test$price

predictions <- predict(ridge_model, newx = x_test)
lamba_grid <- 10^seq(5,-5, length = 500)
model <- train(price~., data = train, method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, lambda = lamba_grid ))
summary(model)

coef(model$finalModel, model$bestTune$lambda)
               
model$bestTune$lambda
               
varImp(model)
plot(varImp(model))

 dim(predictions)
dim(y_test)
mse <- mean((predictions$s0 - y_test)^2)
#_____________________
# Feature Selection
#_____________________
#_____________________________________________________________
# Checking multi-collinearity using Variance Inflation Factor
#_____________________________________________________________

#Build Linear regression model with training_new data
m1 <- lm(price~., data = train_new)
summary(m1)

# vif(m1)
# Error in vif.default(m1) : there are aliased coefficients in the model
# You get this error and it means that multi-collinearity observed
# But fetching correlation matrix will not give any fruit
# Because none of the variables will perfectly collinear
# It actually means that two or more variables will be linearly dependent
# This can be eliminated only by removing the attributes having NA in summary of the linear regression model

# After inspecting the summary function =, these attributes are removed
train_new <- subset(train_new, 
                   select = -c(condition.salvage,cylinders.other,
                               fuel.other,transmission.other,drive.rwd,
                               type.wagon,
                               Maintenance.Hierarchy.Minimum.Maintenance,
                               Repair_risk.Medium.Risk, Value_dip.Medium.resale))
# Again run the model
m2 <- lm(price~., data = train_new)
summary(m2)

#Now State_tax.Low.tax is removed from the next model

train_new <- subset(train_new, select = -State_tax.Low.tax)

m3 <- lm(price~., data = train_new)
summary(m3)

k <- subset(train_new, select = -c(Value_dip.High.resale, 
                                   Maintenance.Hierarchy.High.Maintenance,
                                   cylinders.4.cylinders))
m4 <- lm(price~., data = k)
summary(m4)
#_____________________________________________________________________________
# Using Boruta Package, Random Forest algorithm is used for feature selection
#_____________________________________________________________________________

set.seed(131)
Boruta.cars <- Boruta(price ~ ., data = train_new, doTrace = 2)
head(Boruta.cars)
boruta_signif <- names(Boruta.cars$finalDecision[Boruta.cars$finalDecision %in% c("Confirmed", "Tentative")])  

# collect Confirmed and Tentative variables
print(boruta_signif)
plot(Boruta.cars, cex.axis=1.0, las=2,main="Attributes based Mean Importance", pch = 16, bty = "n")  # plot variable importance
View(attStats(Boruta.cars))

colnames(New_data)

train_new <- subset(train_new, select = -State_tax.High.tax)
# After significance test State. 
# The ranking of importance is given
# From this all attributes are confirmed
# To further filter let us try another method feature selection
# To check whether any attribute with least importance can be removed

#__________________________________________
# Step-wise Regression
#__________________________________________

base_reg_new <- lm(price ~ 1 , data= train_new)  # base intercept only model
every_reg_new <- lm(price ~ . , data= train_new)   # Model with all predictors
summary(every_reg_new)

#Performing step-wise regression in both forward and backward selection
#Hence, in step we can eliminate
stepwise_reg_new <- step(base_reg_new, scope = list(lower = base_reg_new, 
                                            upper = every_reg_new), 
                     direction = "both", 
                     trace = 0, steps = 1500)  # perform step-wise algorithm
imp_var_new <- names(unlist(stepwise_reg_new[[1]])) # get the shortlisted variable.
imp_var_new <- imp_var_new[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(imp_var_new)
summary(stepwise_reg_new)

# There is only variable which has to be eliminated from this regression

train_new <- subset(train_new, select = -Value_dip.Low.resale)

########################
# Random Forest
########################
# 
# library("randomForestSRC")
# library("ggRandomForests")
# 
# colnames(train_new)
# 
# #Convert into logical
# 
# train_rfsrc <- train_new
# 
# train_rfsrc$condition.excellent <- as.logical(train_rfsrc$condition.excellent)
# train_rfsrc$condition.fair <- as.logical(train_rfsrc$condition.fair)
# train_rfsrc$condition.good <- as.logical(train_rfsrc$condition.good)
# train_rfsrc$condition.like.new <- as.logical(train_rfsrc$condition.like.new) 
# train_rfsrc$condition.new <- as.logical(train_rfsrc$condition.new) 
# train_rfsrc$cylinders.10.cylinders <- as.logical(train_rfsrc$cylinders.10.cylinders) 
# train_rfsrc$cylinders.12.cylinders <- as.logical(train_rfsrc$cylinders.12.cylinders)
# train_rfsrc$cylinders.3.cylinders <- as.logical(train_rfsrc$cylinders.3.cylinders)
# train_rfsrc$cylinders.4.cylinders <- as.logical(train_rfsrc$cylinders.4.cylinders)
# train_rfsrc$cylinders.5.cylinders <- as.logical(train_rfsrc$cylinders.5.cylinders)
# train_rfsrc$cylinders.6.cylinders <- as.logical(train_rfsrc$cylinders.6.cylinders)
# train_rfsrc$cylinders.8.cylinders <- as.logical(train_rfsrc$cylinders.8.cylinders)
# train_rfsrc$fuel.diesel <- as.logical(train_rfsrc$fuel.diesel)
# train_rfsrc$fuel.electric <- as.logical(train_rfsrc$fuel.electric)
# train_rfsrc$fuel.gas <- as.logical(train_rfsrc$fuel.gas)
# train_rfsrc$fuel.hybrid <- as.logical(train_rfsrc$fuel.hybrid)
# train_rfsrc$transmission.automatic <- as.logical(train_rfsrc$transmission.automatic)
# train_rfsrc$transmission.manual <- as.logical(train_rfsrc$transmission.manual)
# train_rfsrc$drive.4wd <- as.logical(train_rfsrc$drive.4wd)
# train_rfsrc$drive.fwd <- as.logical(train_rfsrc$drive.fwd)
# train_rfsrc$type.bus <- as.logical(train_rfsrc$type.bus)
# train_rfsrc$type.convertible <- as.logical(train_rfsrc$type.convertible)
# train_rfsrc$type.coupe <- as.logical(train_rfsrc$type.coupe)
# train_rfsrc$type.hatchback <- as.logical(train_rfsrc$type.hatchback)
# train_rfsrc$type.mini.van <- as.logical(train_rfsrc$type.mini.van)
# train_rfsrc$type.offroad <- as.logical(train_rfsrc$type.offroad)
# train_rfsrc$type.other <- as.logical(train_rfsrc$type.other)
# train_rfsrc$type.pickup <- as.logical(train_rfsrc$type.pickup)
# train_rfsrc$type.sedan <- as.logical(train_rfsrc$type.sedan)
# train_rfsrc$type.SUV <- as.logical(train_rfsrc$type.SUV)
# train_rfsrc$type.truck <- as.logical(train_rfsrc$type.truck)
# train_rfsrc$type.van <- as.logical(train_rfsrc$type.van)
# train_rfsrc$Maintenance.Hierarchy.Average.Maintenance <- as.logical(train_rfsrc$Maintenance.Hierarchy.Average.Maintenance)
# train_rfsrc$Maintenance.Hierarchy.High.Maintenance <- as.logical(train_rfsrc$Maintenance.Hierarchy.High.Maintenance)
# train_rfsrc$Repair_risk.High.Risk <- as.logical(train_rfsrc$Repair_risk.High.Risk)
# train_rfsrc$Repair_risk.Low.Risk <- as.logical(train_rfsrc$Repair_risk.Low.Risk)
# train_rfsrc$Value_dip.High.resale <- as.logical(train_rfsrc$Value_dip.High.resale)
# 
# 
# model_rfsrc <- rfsrc(price~., data=train_rfsrc)
# 
# model_rfsrc
# dev.off()
# gg_e <- gg_error(model_rfsrc)
# plot(gg_e)
# 
# plot(gg_vimp(model_rfsrc), lbls=st.labs)


#__________________
# Random Forest
#__________________

#Verifying all the categorical variables are factors
str(train_new)
View(k)

mod_rf_new <- randomForest(price~., data = k, ntree = 100, 
                       importance = TRUE, mtry = 2)
g <- lm(price~., data = k)
summary(g)
k <- subset(final, select = -c(condition.salvage, 
                               cylinders.other, fuel.other, transmission.other,
                               drive.rwd, type.wagon, 
                               Maintenance.Hierarchy.Minimum.Maintenance, 
                               Repair_risk.Medium.Risk, Value_dip.Medium.resale))
summary(g)
print(mod_rf_new)
plot(mod_rf)

#Using the model predict the values for test data set

pred_prices_rf <-  predict(mod_rf, test)

plot(mod_rf)

# RMSE on training set
RMSE(pred_prices_rf, test$price)
# RMSE of this optimal random forest
sqrt(mod_rf$mse[which.min(mod_rf$mse)])

# Get variable importance from the model fit
imp_var_rf <- as.data.frame(importance(mod_rf))
imp_var_rf$Var.Names <- row.names(imp_var_rf)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
# Both function and calculation are giving same RMSE score
# The RMSE score is very less and the Rsquare value is good

plot(P11, train$price, pch = 19, col = "black")
points(P11, train$price, col = "green", pch = 19)
varImp(mod_rf)
varImpPlot(mod_rf)

#Scatter plot between predicted and actual values
fit_rf <- lm(price ~ pred_prices_rf, data = test)

ggplot(test, aes(x = pred_prices_rf, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")








############################
# XGBoost
############################

#define final training and testing sets

x_new = data.matrix(train_new[, -1])
Y_new = train_new[, 1]

xgb_train_new = xgb.DMatrix(data = x_new, label = Y_new)
xgb_test_new = xgb.DMatrix(data = x_new, label = Y_new)

#defining a watch list
watchlist = list(train=xgb_train_new, test=xgb_test_new)

#fit XGBoost model and display training and testing data at each iteration
model_XGboost_new = xgb.train(data = xgb_train_new, max.depth = 3, 
                              watchlist=watchlist, nrounds = 10000, 
                              params = list(booster = 'gblinear', 
                                            objective = 'reg:linear'))

#define final model
model_xgboost_new = xgboost(data = xgb_train_new, max.depth = 3, 
                            nrounds = 2000, verbose = 0)

pred_xgboost_new <- tibble(pred = predict(xboost_RMSE, newdata = xgb_train_new)
            , obs = xgb_train_new$mpg) %>% mutate(resid = pred - obs, 
                                           resid_sq = resid^2)
sstot <- sum((d$pred - mean(d$obs))^2)
ssresid <- sum(d$resid_sq)
sprintf("percent variance explained, R^2: %1.1f%%", 100 * (1 - ssresid / sstot))

#use model to make predictions on test data
pred_xgboost_new = predict(model_xgboost_new, xgb_test_new)
pred_xgboost_new

xg <- train_new %>% mutate(pred_xgboost_new)
ggplot(xg, aes(x = pred_xgboost_new, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")


xg_fit <- lm(price~pred_xgboost_new, xg)
plot(xg_fit)


























