install.packages("glm2")
library(glm2)       
install.packages("corrplot")
library(tidyverse)   
library(corrplot)    
library(car)         
library(MASS)        
library(stats)      
library(dplyr)
library(tidyr)
df <- read.csv("/Users/xiaohouailaoyue/Desktop/dataset19.csv")

# Check the data structure
str(df)
summary(df)
head(df)

# Check for missing values
colSums(is.na(df))

# Histogram: Distribution of the target variable Y
# Histogram of target variable (update 'time_at_shelter' if needed)
ggplot(df, aes(x = time_at_shelter)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  ggtitle("Distribution of Time at Shelter")
##Right-Skewed Distribution: The chart shows that the majority of the data is concentrated towards the left side, near zero, and as the shelter stay time increases, the frequency drops rapidly. This is typical of a right-skewed (positively skewed) distribution, indicating that most individuals spent very little time at the shelter, with only a few individuals staying for a longer duration.

install.packages("vcd")
library(grid)
library(vcd)  # Load visualization package
# Replace 'category1' and 'category2' with actual column names
mosaic(~ animal_type + outcome_type, data = df, shade = TRUE, legend = TRUE)
mosaic(~ intake_type + chip_status, data = df, shade = TRUE, legend = TRUE)
mosaic(~ outcome_type + month, data = df, shade = TRUE, legend = TRUE)
mosaic(~ chip_status + year, data = df, shade = TRUE, legend = TRUE)


install.packages("ggplot2")
library(ggplot2)
# Replace 'target_variable' and 'category_variable' with actual names
ggplot(df, aes(x = outcome_type, y = time_at_shelter)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  ggtitle("Boxplot of Target Variable by Category")

install.packages("dplyr")
install.packages("tidyr")
# Install and load necessary libraries

library(dplyr)  # Load dplyr
library(tidyr)  # Load tidyr

# Example to pivot the data to long format
df_long <- df %>%
  pivot_longer(cols = c(animal_type, intake_type, outcome_type, chip_status),
               names_to = "Category_Type", values_to = "Category")

# View the transformed data
head(df_long)
#group bar charts
# Install ggplot2 if not installed
install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)

# Example plot
ggplot(df, aes(x = outcome_type, y = time_at_shelter)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  ggtitle("Boxplot of Target Variable by Category")
# Convert data to long format
df_long <- df %>%
  pivot_longer(cols = c(animal_type, intake_type, outcome_type, chip_status),
               names_to = "Category_Type", values_to = "Category")

ggplot(df_long, aes(x = Category_Type, fill = Category)) +
  geom_bar(position = "fill") +  # Stacked bar plot (relative proportions)
  theme_minimal() +
  ggtitle("Proportional Comparison of 6 Categorical Variables") +
  xlab("Variable") + ylab("Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# log transformation
df$log_time_at_shelter <- log(df$time_at_shelter + 1)

ggplot(df, aes(x = outcome_type, y = log_time_at_shelter)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  ggtitle("Boxplot of Target Variable by Category")

write.csv(df, "Group19_log_transformed.csv", row.names = FALSE)
print("Log-transformed dataset has been saved as 'Group19_log_transformed.csv'")
