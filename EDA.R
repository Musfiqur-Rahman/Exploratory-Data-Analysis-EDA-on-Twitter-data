#------Exploratory Data Analysis-----#
library(tidyverse)
library(ggplot2)
library(corrplot)

#Load & View the Data

twitterdata <- read.csv(file.choose()) #insert dataset dynamically

head(twitterdata)  #view first six rows of dataset
view(twitterdata)  #tableview of dataset

str(twitterdata)  #data type indicates

#display rows and columns

dim(twitterdata)

#Summarize the Data

summary(twitterdata)


#count total missing values in each column
sapply(twitterdata, function(x) sum(is.na(x)))

#drop the insignificant column
newdata <- twitterdata[ -c(1,6,8,10:11) ]
view(newdata)
head(newdata)
str(newdata)

# Summary statistics for numeric variables
summary(newdata$rating)

# Calculate the standard deviation of the 'rating' variable
std_dev_rating <- sd(newdata$rating)
cat("Standard Deviation of Rating: ", std_dev_rating, "\n")



# Rating distribution
ggplot(newdata, aes(x = rating)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Rating Distribution", x = "Rating", y = "Frequency")

#Visualization of Usernames Count for Ratings 1 to 5

# Subsetting the dataset for ratings 1 to 5
subset_data <- newdata[newdata$rating >= 1 & newdata$rating <= 5, ]

# Usernames count for ratings 1 to 5
username_counts <- as.data.frame(table(subset_data$rating))
colnames(username_counts) <- c("Rating", "Username_Count")

ggplot(username_counts, aes(x = as.factor(Rating), y = Username_Count, fill = as.factor(Rating))) +
  geom_bar(stat = "identity") +
  labs(title = "Usernames Count for Ratings 1 to 5", x = "Rating", y = "Number of Usernames")

#Visualization of Rating by Country
# Boxplot for rating by country
ggplot(newdata, aes(x = country, y = rating, fill = country)) +
  geom_boxplot() +
  labs(title = "Rating by Country", x = "Country", y = "Rating")

# #Correlation Heatmap using corrplot
# 
# library(corrplot)
# 
# newdata$rating <- as.numeric(newdata$rating)
# newdata$review <- as.numeric(newdata$review)
# 
# str(newdata$rating)
# str(newdata$review)
# 
# # Subset numeric variables
# numeric_variables <- newdata[, sapply(newdata, is.numeric)]
# 
# # Create a correlation matrix
# correlation_matrix <- cor(numeric_variables)
# 
# # Plot the correlation matrix as a heatmap using corrplot
# corrplot(correlation_matrix, method = "color")



#Visualization of Comments Count by Country

# Comments count by country
comment_counts <- as.data.frame(table(newdata$country))
colnames(comment_counts) <- c("Country", "Comment_Count")

ggplot(comment_counts, aes(x = Country, y = Comment_Count, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Comments Count by Country", x = "Country", y = "Number of Comments")



# Create a contingency table for two categorical variables (country and rating variable)
contingency_table <- table(newdata$country, newdata$rating)

# Perform the chi-squared test
chi_square_result <- chisq.test(contingency_table)

# Print the result
print(chi_square_result)

# Create a heatmap using ggplot
ggplot(contingency_df, aes(x = Category, y = Country, fill = Frequency)) +
  geom_tile() +
  labs(title = "Heatmap of Categorical Variables:country vs rating",
       x = "Rating", y = "Country",
       fill = "Frequency") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# # Visualize the mosaic plot
# mosaicplot(contingency_table, main = "Mosaic Plot of Categorical Variables: country vs rating")
# 
# # Convert the table to a data frame for ggplot
# contingency_df <- as.data.frame(as.table(contingency_table))
# colnames(contingency_df) <- c("Country", "Category", "Frequency")
# 
# # Bar plot using ggplot
# ggplot(contingency_df, aes(x = Country, y = Frequency, fill = Category)) +
#   geom_bar(stat = "identity", position = "stack") +
#   labs(title = "Bar Plot of Categorical Variables:country vs rating",
#        x = "Country", y = "Frequency",
#        fill = "Rating") +
#   theme_minimal()




