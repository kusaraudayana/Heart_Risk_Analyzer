library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Set to script's folder
data <- read.csv("heart.csv", stringsAsFactors = FALSE)
head(data)
str(data)
#Changing column names
colnames(data)
colnames(data)[2] <- 'Gender'
colnames(data)[3] <- 'Angina'
colnames(data)[10] <- 'ST_Change'
colnames(data)[4] <- 'Resting_Bloodpressure'
colnames(data)[6] <- 'Fasting_Bloodsugar'
colnames(data)[8] <- 'Maximum_Heartrate'
#Cheking for missing values and duplicates
sum(is.na(data))
sum(duplicated(data))
#Changing labels of catgories for more convinience
data$Gender <- ifelse(data$Gender=='M','Male','Female')
data$RestingECG <- ifelse(data$RestingECG=='ST','Abnormal-ST',data$RestingECG)
data$HeartDisease <- ifelse(data$HeartDisease==1,'Yes','No')
data$ExerciseAngina <- ifelse(data$ExerciseAngina=='N','No','Yes')
data$Angina <- ifelse(data$Angina == "TA", "Typical",
                      ifelse(data$Angina == "ATA", "Atypical",
                             ifelse(data$Angina == "NAP", "Non-Anginal", "Asymptomatic")))
data$Fasting_Bloodsugar <- ifelse(data$Fasting_Bloodsugar==1,'High','Normal')
str(data)
#Univariate 
#Age distributon
library(ggplot2)
ggplot(data, aes(x = Age, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Curve", x = "Values", y = "Density") +
  theme_minimal()
#ST change distribution
ggplot(data, aes(x = ST_Change, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Curve", x = "Values", y = "Density") +
  theme_minimal()
#Maximum heartrate distribution
ggplot(data, aes(x = Maximum_Heartrate, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Curve", x = "Values", y = "Density") +
  theme_minimal()
#Cholestrol level distribution
ggplot(data, aes(x = Cholesterol, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Curve", x = "Values", y = "Density") +
  theme_minimal()
#found lotof values 0 but when I explore more I understood it is a wrong data entry
sum(data$Cholesterol == 0)
#I decided to impute all 0 values by applying knn for continuous variables.

library(DMwR2)

# Convert 0 values in 'Cholesterol' column to NA
data$Cholesterol[data$Cholesterol == 0] <- NA

# Select only numeric columns for imputation
data_numeric <- data[sapply(data, is.numeric)]

# Apply KNN imputation to the numeric columns
data_numeric_imputed <- knnImputation(data_numeric, k = 5)

# Replace the original numeric columns with the imputed ones
data[sapply(data, is.numeric)] <- data_numeric_imputed

# Now, the 'Cholesterol' column will be imputed, and other numeric columns will also be imputed if needed
#Resting Bloodpressure distribution
ggplot(data, aes(x = Resting_Bloodpressure, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Curve", x = "Values", y = "Density") +
  theme_minimal()
#Distribution of gender
ggplot(data=data, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
  scale_fill_brewer(palette = "Set2")  # Use a nice color palette
#Distribution of Angina
ggplot(data=data, aes(x = Angina, fill = Angina)) +
  geom_bar() +
  labs(title = "Distribution of Angina",
       x = "Angina",
       y = "Count") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
  scale_fill_brewer(palette = "Set2")  # Use a nice color palette
#Distribution of Fasting Blood sugar
ggplot(data=data, aes(x = Fasting_Bloodsugar, fill = Fasting_Bloodsugar)) +
  geom_bar() +
  labs(title = "Distribution of Fsting Bloodsugar",
       x = "Fasting bloodsugar",
       y = "Count") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
  scale_fill_brewer(palette = "Set2")  # Use a nice color palette
#Distribution of Resting ECG
ggplot(data=data, aes(x = RestingECG, fill = RestingECG)) +
  geom_bar() +
  labs(title = "Distribution of Resting ECG",
       x = "Resting ECG",
       y = "Count") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
  scale_fill_brewer(palette = "Set2")  # Use a nice color palette
#Distribution of Exercise Angina
ggplot(data=data, aes(x = ExerciseAngina, fill = ExerciseAngina)) +
  geom_bar() +
  labs(title = "Distribution of Exercise Angina",
       x = "Exercise Angina",
       y = "Count") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
  scale_fill_brewer(palette = "Set2")  # Use a nice color palette
#Distribution of ST Slope
ggplot(data=data, aes(x = ST_Slope, fill = ST_Slope)) +
  geom_bar() +
  labs(title = "Distribution of ST Slope",
       x = "ST Slope",
       y = "Count") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
  scale_fill_brewer(palette = "Set2")  # Use a nice color palette
#Distribution of heart Disease
ggplot(data=data, aes(x = HeartDisease, fill = HeartDisease)) +
  geom_bar() +
  labs(title = "Distribution of Heart Disease",
       x = "Heart Disease",
       y = "Count") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels if needed
  scale_fill_brewer(palette = "Set2")  # Use a nice color palette
##bivariate
ggplot(data=data, aes(x = Age, y = Maximum_Heartrate, color = HeartDisease)) +
geom_point(alpha = 0.8) +  # Adjust transparency for better visibility
  labs(title = "Scatter Plot with Bubble Size Based on Oldpeak",
       x = "Age", 
       y = "MaxHR", 
       color = "Exercise Angina") +  # Legend for size
  theme_minimal()
#After preprocessing cleaned data set taken into a csv and created dashboard from that
#write.csv(data, "Heartdiseaseclean.csv", row.names = FALSE)