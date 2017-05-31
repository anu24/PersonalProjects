# Load the packages
library(dplyr)
library(corrplot)
library(ggplot2)
library(data.table)

# Set the working directory : /Users/ashivarudrappa/Desktop/Study Docs/SuperDataScienceProjects/Wiki Project
getwd()

# Load and Clean the data
# Wiki_data <- fread("wikipedia_data.csv", header = T)
wiki_full_data <- fread("wikipedia_data_full.csv", header = T)

# Get a overview on data
head(wiki_full_data)

# 3. Complete the following data cleaning steps
# a. Name the columns as follows "source", "page_name", "views", "size"
names(wiki_full_data) <- c("source", "page_name", "views", "size")
# b. Format the variables
data$page_name = as.character(data$page_name)
data$source = as.character(data$source)
# c. Remove duplicate rows
wiki_clean <- unique(wiki_full_data)
# d. Remove rows with NA in column “views”
wiki_clean <- wiki_clean[!(is.na( wiki_clean$views)) ,]
# e. Add a column called “language” (see hints page for details) 
head(wiki_clean)
wiki_clean$language <- gsub("_.+", "", wiki_clean$source)
str(wiki_clean)

# 4. Which language has the largest number of view? (please create a histogram of the top 10 languages) 
wiki_top_language <- wiki_clean %>% group_by(language) %>% summarise(count = n()) %>% as.data.frame() 
wiki_top_language <- wiki_top_language[(order(wiki_top_language$count, decreasing = T)),]
wiki_top_language <- head(wiki_top_language, 10)
wiki_top_language$language <- as.factor(wiki_top_language$language)
# Reference : http://ggplot2.tidyverse.org/reference/geom_bar.html
ggplot(data = wiki_top_language, aes(x = wiki_top_language$language, fill = language)) + 
  geom_bar(aes(weight = count)) + ggtitle("Views by language") +
  xlab("Language") +
  ylab("Number of Views")


# 5. Which page, excluding Main Pages, has the highest number of views? 
table(wiki_clean$page_name!='Main_Page')
wiki_excl_main <- wiki_clean[!(wiki_clean$page_name!='Main_Page'), ]
wiki_excl_main




