#
# TITANIC R TUTORIAL
#

# Setting working directory
#setwd("~/Dropbox/Novabase/Analytics Lab/Use Cases/Titanic")

# Loading training and test datasets
train <- read.csv("~/Dropbox/Novabase/Analytics Lab/Use Cases/Titanic/train.csv")
test <- read.csv("~/Dropbox/Novabase/Analytics Lab/Use Cases/Titanic/test.csv")

# Proportion of people that survived
table(train$Survived)
prop.table(table(train$Survived))

# Add new Survived column with value 0 for each test record (everyone dies)
test$Survived <- rep(0, 418)


# Extract passenger ID and Survived columns from test and save into a CSV
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# Get the summary of Sex variable
summary(train$Sex)

prop.table(table(train$Sex, train$Survived))
# Women survive most!
prop.table(table(train$Sex, train$Survived),1)

# Hammer time: females survive, males do not
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

# Get the summary of Age variable
summary(train$Age)

# Create Child variable which is 1 when passenger is younger than 28 yrs
train$Child <- 0
train$Child[train$Age < 18] <- 1

# How many survived given the Sex and if it's a Child
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# Total number of people given the Sex and Child
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# Proportion of survivoer give then Sex andChild
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column Fare2 with fare categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# See survival rate considering Fare, social class and sex
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Readjust prediction
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0


# Extract passenger ID and Survived columns from test and save into a CSV
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "predictions.csv", row.names = FALSE)

#
# Decision Trees!
#

# Load lib
library(rpart)
# Run decision tree alg!!!
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

plot(fit)
text(fit)

#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

#Predict and save file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Unleash complexity AND 2 passengers at least for splitting the bucket
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#Predict and save file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Trim the tree
new.fit <- prp(fit,snip=TRUE)$obj

#Predict and save file
Prediction <- predict(new.fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#
# FEATURE ENGINEERING
#

# First passenger's names
train$Name[1]

#Extract the titles from name
# 1 - Merge both datasets (test set needs more variables)
test$Survived <- NA
test$Child <- 0
test$Child[test$Age < 18] <- 1
test$Fare2 <- '30+'
test$Fare2[test$Fare < 30 & test$Fare >= 20] <- '20-30'
test$Fare2[test$Fare < 20 & test$Fare >= 10] <- '10-20'
test$Fare2[test$Fare < 10] <- '<10'
#  MERGE
combi <- rbind(train, test)
# 2 - Convert names from factors to strings
combi$Name <- as.character(combi$Name)
combi$Name[1]
# 3 - Split name parts
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2] # always Mr
# Apply split function to every row - column Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title) #trim spaces

table(combi$Title)

#Combine madam and mademoiselle
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

#Rich guys
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Change Title back to factor
combi$Title <- factor(combi$Title)

# New variable - family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#Extract surname
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# Bluid familiy ID: size+surname
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
# small families
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

# Table of famIDs and their frequency
famIDs <- data.frame(table(combi$FamilyID))
# obtain families with lower freq
famIDs <- famIDs[famIDs$Freq <= 2,]
# Classify those families as "small"
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Back to factor
combi$FamilyID <- factor(combi$FamilyID)

# Finally break train and test set apart
train <- combi[1:891,]
test <- combi[892:1309,]

# Tun decision tree with newly added variables!
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

#Predict and save file
Prediction <- predict(new.fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "predictions.csv", row.names = FALSE)

#
# RANDOM FORESTS
#

sample(1:10, replace = TRUE)

summary(combi$Age)

# Try to predict the sge'd missing values based on other variables
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
# Fill the NAs with the predicted age
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
fancyRpartPlot(Agefit)

summary(combi$Age)
summary(combi)

summary(combi$Embarked)

# there are 2 persons without embark, 62 and 830
which(combi$Embarked == '')

# Replace them with Southampton (the majority) and back to factor
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Fare with 1 NA
summary(combi$Fare)
which(is.na(combi$Fare)) # 1044
# Set NA's Fare as the median
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Too many factor levels for FamilyID (suuports up to 32 diferent familyIDs)
# Got to increase cutt-off to be a "Small" family from 2 to 3 people
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2) # 22 levels

train<-combi[1:891,]
test<-combi[892:1309,]

# Random forest algorithm - install and load
install.packages('randomForest')
library(randomForest)

#setting the seed
set.seed(415)

# 2000 random trees
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title +
                    FamilySize + FamilyID2, data=train, importance=TRUE, ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Random forest of conditional inference trees
install.packages('party')
library(party)


set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))


