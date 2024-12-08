# Load required libraries
library(readr)
library(ggplot2)
library(corrplot)
library(car)
library(factoextra)

# Load the AQI dataset
aqi_data <- read.csv("C:/Users/Dinesh K/Desktop/ssd assignment/aqi.csv")

## Univariate Analysis

# 1. Data Overview
str(aqi_data)
dim(aqi_data)
head(aqi_data)

# 2. Summary Statistics
summary(aqi_data$AQI.Value)
sd(aqi_data$AQI.Value)

# 3. Distribution Visualization
ggplot(aqi_data, aes(x = AQI.Value)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of AQI Values")

ggplot(aqi_data, aes(y = AQI.Value)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of AQI Values")

# 4. Categorical Variable Analysis
ggplot(aqi_data, aes(x = AQI.Category)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of AQI Categories") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Multivariate Analysis

# 5. Correlation Analysis
cor(aqi_data$AQI.Value, aqi_data$PM2.5.AQI.Value)

# 6. Scatter Plot Visualization
ggplot(aqi_data, aes(x = PM2.5.AQI.Value, y = AQI.Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "PM2.5 AQI vs Overall AQI")

# 7. Multiple Regression
model <- lm(AQI.Value ~ PM2.5.AQI.Value + Ozone.AQI.Value + NO2.AQI.Value, data = aqi_data)
summary(model)

# 8. Model Diagnostics
par(mfrow = c(2, 2))
plot(model)

## Advanced Analysis

# 9. Principal Component Analysis (PCA)
aqi_numeric <- aqi_data[numeric_columns]
pca_result <- prcomp(aqi_numeric, scale. = TRUE)
fviz_eig(pca_result, addlabels = TRUE)

# 10. PCA Interpretation
fviz_pca_biplot(pca_result, 
                label = "var",
                habillage = aqi_data$Country,
                addEllipses = FALSE,
                ellipse.level = 0.95,
                geom = "points")

warnings()
