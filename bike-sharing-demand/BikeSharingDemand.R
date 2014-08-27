# Kaggle's
# BIKE SHARING DEMAND
# R implementation by Ana Narciso

# Setting working directory
setwd("D:/NB19342/Dropbox/Novabase/Analytics Lab/Use Cases/Bike Sharing Demand")

require(lattice) 
library(lattice) 
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Loading training and test datasets
train <- read.csv("D:/NB19342/Dropbox/Novabase/Analytics Lab/Use Cases/Bike Sharing Demand/train.csv")
test <- read.csv("D:/NB19342/Dropbox/Novabase/Analytics Lab/Use Cases/Bike Sharing Demand/test.csv")

# Get the train set structure and summary
str(train)
summary(train)

# No NA's
table(complete.cases(train))


#
# Feature Engineering
#



# Add dummy values to test dataframe
test$casual = NA
test$registered = NA
test$count = NA

# Bind train and test data together - for applying the same set of operations
combi = rbind(train, test)

# Convert some features to factors
combi$season = as.factor(combi$season)
combi$holiday = as.factor(combi$holiday)
combi$workingday = as.factor(combi$workingday)
combi$weather = as.factor(combi$weather)

# Extract hour, weekday, month, and year from datetime
combi$datetime2 = strptime(as.character(combi$datetime), format="%Y-%m-%d %H:%M:%S")
##combi$datetime = strptime(as.character(combi$datetime), format="%Y-%m-%d %H:%M:%S")
combi$weekday = as.factor(combi$datetime2$wday)
combi$month = as.factor(combi$datetime2$mon)
combi$hour = as.factor(combi$datetime2$hour)
combi$year = as.factor(1900 + combi$datetime2$year)

# Build categories for continuous variables

combi$temp_cat = '40+'
combi$temp_cat[combi$temp < 40 & combi$temp >= 30] = '30-40'
combi$temp_cat[combi$temp < 30 & combi$temp >= 20] = '20-30'
combi$temp_cat[combi$temp < 20 & combi$temp >= 10] = '10-20'
combi$temp_cat[combi$temp < 10] = '10-'

combi$wind_cat = '50+'
combi$wind_cat[combi$windspeed < 50 & combi$windspeed >= 45] = '45-50'
combi$wind_cat[combi$windspeed < 45 & combi$windspeed >= 30] = '30-45'
combi$wind_cat[combi$windspeed < 30 & combi$windspeed >= 15] = '15-30'
combi$wind_cat[combi$windspeed < 15] = '15-'

combi$humidity_cat = '80+'
combi$humidity_cat[combi$humidity < 80 & combi$humidity >= 60] = '60-80'
combi$humidity_cat[combi$humidity < 60 & combi$humidity >= 40] = '40-60'
combi$humidity_cat[combi$humidity < 40 & combi$humidity >= 20] = '20-40'
combi$humidity_cat[combi$humidity < 20] = '20-'

combi$hour_cat = 'night'
combi$hour_cat[as.numeric(combi$hour) < 18 & as.numeric(combi$hour) >= 12] = 'afternoon'
combi$hour_cat[as.numeric(combi$hour) < 12 & as.numeric(combi$hour) >= 6] = 'morning'
combi$hour_cat[as.numeric(combi$hour) < 6] = 'dawn'


# Split the sets
train = combi[0:10886,]
test = combi[10887:17379,]


#
# Test
#

# 




# Multiple Linear Regression Example http://www.statmethods.net/stats/regression.html
fit <- lm(count ~ hour_cat, data=train) # 278
fit <- lm(count ~ weekday, data=train) # 278

fit <- lm(count ~ as.numeric(hour) + I(as.numeric(hour)^2) + I(as.numeric(hour)^3), data=train) 

fit <- lm(count ~ as.numeric(month) + I(as.numeric(month)^2), data=train) 



fit <- lm(count ~ hour_cat + humidity_cat + wind_cat + temp_cat, data=train) # 278

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)


#fancyRpartPlot(fit)

#Predict and save file
prediction <- predict(fit, test, type = "response")
submit <- data.frame(datetime = test$datetime, count = prediction)
write.csv(submit, file = "predictions.csv", row.names = FALSE)

# Simple linear regression
xyplot(count ~ hour, data = train,
       xlab = "Hour",
       ylab = "Count",
       main = "Count by Hour"
)

