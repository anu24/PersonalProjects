# Load the packages
library(dplyr)
library(corrplot)
library(ggplot2)

# To get more information on variables lookinto : https://www.kaggle.com/c/titanic/data

# Set the working directory : /Users/ashivarudrappa/Desktop/Study Docs/SuperDataScienceProjects/Titanic Project 
getwd()

# Load and Clean the data
titanic_data <- read.csv("titanic_data.csv")
colnames(titanic_data) = c('passenger_id', 'survived', 'passenger_class', 'name',
                           'sex', 'age', 'number_siblings_spouse', 'number_parents_children',
                           'ticket', 'fare', 'cabin', 'embarked')

# Get a overview on data
head(titanic_data)
str(titanic_data)

# Update the passenger class to factor and update level names
titanic_data$passenger_class <-  as.factor(titanic_data$passenger_class)
levels(titanic_data$passenger_class) <- c("Upper", "Middle", "Lower")
class(titanic_data$passenger_class)


# 1. How many passengers are included in the data? 
summary(titanic_data)
max(titanic_data$passenger_id) # method1
nrow(titanic_data) # method2

# 2. Who bought the most expensive tickets? 
# method1
max(titanic_data$fare)
titanic_data[ (titanic_data$fare == max(titanic_data$fare)) , ]
# method2
View(titanic_data[ (order(titanic_data$fare, decreasing = T)), ])


# 3. How many men and how many women survived? (please provide a bar chart) 
titanic_data_survived <- titanic_data[titanic_data$survived == 1, ]
head(titanic_data_survived)
survived_m_f <- as.data.frame( titanic_data_survived %>% group_by(sex) %>% summarise(count = n()) )
survived_m_f
# method1
barplot(survived_m_f$count, col = c('pink', 'blue'), 
        ylab = ("Count"),
        xlab = ("Men and Women who suvived"),
        main = ("Men and Women survived in Titanic incident"),
        names.arg = c("Women", "Men"))
# method2
ggplot(titanic_data_survived, aes(x = sex)) + geom_bar(fill = c('pink', 'blue'))


# 4. How many passengers are included in the data by class? (please provide a bar chart) 

Booking_Class <- as.data.frame( titanic_data %>% group_by(Pclass) %>% summarise(count=n()) )
Booking_Class$Pclass <- as.factor(Booking_Class$Pclass)
levels(Booking_Class$Pclass) <- c("Upper", "Middle", "Lower")
Booking_Class
barplot(Booking_Class$count, col = rainbow(3), names.arg = c("Upper", "Middle", "Lower"),
        main = ("Passenger Data by Booking Class"),
        ylab = ("Count"),
        xlab = ("Socio Economic Class"),
        ylim = c(0,500))


# 5. How many people had the title 'Mrs'?
table (grepl(pattern = ".*Mrs.*", titanic_data$name))
title_having_Mrs <- titanic_data[(grepl(pattern = ".*Mrs.*", titanic_data$name)) , ]
nrow(title_having_Mrs)

# 6. What is the correlation between Age and Fare? (please provide a scatter plot) 
table(is.na(titanic_data$age))
table(is.na(titanic_data$fare))
#since age vatiable is having na variable, using "complete.obs" function in the correlation function
sub_titanic <- subset(titanic_data, select = c(age, fare))
correlation <- cor(sub_titanic, use = 'complete.obs')
correlation
corrplot(correlation)
corrplot(correlation, method = "number")
plot (sub_titanic, type = "p", main = "Correlation Between Age and Fare",
      col = "dark Blue")
# method 2
qplot(age, fare, data = sub_titanic) +
  ggtitle("Correlation Between Age and Fare") +
  xlab("Age") +
  ylab("Fare")


# 7. What is the distribution of fares? (please provide a histogram)
head(titanic_data)
ggplot(titanic_data, aes(x = fare)) + geom_histogram(binwidth = 20, fill = 'red')  + 
  ggtitle("Fare Distribution") + ylab("Number of Passengers")




