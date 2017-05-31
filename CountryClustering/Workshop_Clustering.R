# Load the packages
library(dplyr)
library(corrplot)
library(ggplot2)
library(data.table)
library(cluster)
library(tidyr)

# Set the working directory : /Users/ashivarudrappa/Desktop/Study Docs/SuperDataScienceProjects/CountryClustering
getwd()

# Load and Clean the data
data <- read.csv("data.csv", header = T)
View(data)

# 1. Use spread from the tidyr package to make 1 row for each country and two columns
# with GDP per capita and CO2 per capita
data <- data %>% spread(key = IndicatorName, value = Value)


# 2. Update column names to be easier to use
names(data) <- c('country_name', 'co2', 'gdp')
summary(data) # we can see the NA's value in C01 and gdp
data <- na.omit(data)
summary(data)
# data_new <- <- data[!(is.na(data$co2)), ]

# 4. Add columns with normalised variables which have a mean of 0 and a variance of 1.
# the reason why we have to normalize the data is, K-means clustering uses "euclidean distance". 
# So one of the variable has large spread(column gdp) and another vaiable (c02) has small spread.
# so if we don't normalize the variables, one of the variable will be fadded when we use Kmeans cluster
data$normCo2 <- (data$co2 - mean(data$co2)) / sd(data$co2)
data$normgdp <- (data$gdp - mean(data$gdp)) / sd(data$gdp)
head(data)

# 5. Plot the elbow curve for these normalised variables to determine how many clusters to use.
data_norm = data[,c('normCo2', 'normgdp')]
elbow_plot = vector()
# let's check the k-means cluster for 1 to 15 cluster option
for (i in 1:15){
  elbow_plot[i] = sum(kmeans(data_norm, i)$withinss)
}
plot(elbow_plot, type = 'b') # let's take 4 as cluster count

# 6. Create k-means clusters
#  The kmeans() function has an nstart option that attempts multiple initial configurations 
# and reports on the best one. For example, adding nstart=25 will generate 25 initial 
# configurations. This approach is often recommended.
set.seed(333)
kmeans_model = kmeans(data_norm, 4, iter.max = 500, nstart = 15)



# 7: plot kmeans clusters on graph
clusplot(data_norm,
         kmeans_model$cluster,
         lines = 0,
         shade = T,
         color = T,
         labels = 0,
         plotchar = F,
         span = T,
         xlab = 'gdp',
         ylab = 'co2')

# 8: calculate average gdp, average co2 and number of countries by cluster
data$cluster = kmeans_model$cluster
data_summary = data %>% 
  group_by(cluster) %>% 
  summarise(avg_gdp = mean(gdp),
            avg_co2 = mean(co2),
            number_countries = n())
View(data %>% arrange(cluster))







