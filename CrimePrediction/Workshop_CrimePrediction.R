# Load the packages
library(dplyr)
library(corrplot)
library(ggplot2)
library(data.table)
library(caTools)

# Set the working directory : /Users/ashivarudrappa/Desktop/Study Docs/SuperDataScienceProjects/CrimePrediction
getwd()

# Load the data
crime_data <- read.csv("data.csv", header = F)
str(crime_data)
View(crime_data)
variable_name <- read.csv("variable_names.csv", header = F)
View(variable_name)

# 1. Clean variable_names and set the names of data to these clean names
# drop first 3 character
variable_name$clean <- substr(variable_name$V1, 4, 1000)
# drop all the characters after ":"
variable_name$clean <- gsub(pattern = ":.+", "", x = variable_name$clean)
View(variable_name)
# add names to the variables of crime_data
names(crime_data) <- variable_name$clean


# 2.The police chief is only interested in the following variables so create a new data frame with just
# these variables included: 'ViolentCrimesPerPop', 'pctUrban', 'agePct16t24', 'PctUnemployed', 'medIncome'
crime_small_data <- select(crime_data, ViolentCrimesPerPop, pctUrban, agePct16t24, PctUnemployed, medIncome)
str(crime_small_data)

# 3. Check for correlations between medIncome and PctUnemployed. Plot these variables to confirm correlations
correlation <- cor(crime_small_data$medIncome, crime_small_data$PctUnemployed)
plot(crime_small_data$medIncome, crime_small_data$PctUnemployed)
corr_full <- cor(crime_small_data)
corr_full
corrplot(corr_full)
corrplot(corr_full, method = "number")

# 4. Split the data into training and testing sets using set.seed(123)
set.seed(222)
split_data <- sample.split(crime_small_data$ViolentCrimesPerPop, SplitRatio = 0.8)
train_data <- crime_small_data[split_data, ]
test_data <- crime_small_data[!(split_data), ]


# 5. Build a linear regression model using pctUrban, agePct16t24 and whichever of the 
# variables from question 3 is best correlated with ViolentCrimesPerPop
model = lm(formula = ViolentCrimesPerPop ~ agePct16t24 + PctUnemployed + pctUrban, 
           data = train_data)
summary(model)

# 6. Predict the ViolentCrimesPerPop for the testing set
predict_test_data <- predict(model, newdata = test_data)

# 7. Calculate the R2 of the testing set
SSE = sum((predict_test_data - test_data$ViolentCrimesPerPop)^2)
SST = sum((mean(test_data$ViolentCrimesPerPop) - test_data$ViolentCrimesPerPop)^2)
R2 = 1 - SSE/SST
R2

