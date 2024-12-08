# Load required libraries
library(readr)
library(ggplot2)
library(corrplot)
library(car)
library(factoextra)

# Load the California Housing dataset
housing <- read.csv("C:/Users/Dinesh K/Desktop/ssd assignment/california_housing.csv")

## Univariate Analysis

# 1. Data Overview
str(housing)
dim(housing)
head(housing)

# 2. Summary Statistics
summary(housing$median_house_value)
sd(housing$median_house_value)

# 3. Distribution Visualization
ggplot(housing, aes(x = median_house_value)) +
  geom_histogram(binwidth = 50000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of House Values",
       x = "Median House Value",
       y = "Frequency")

ggplot(housing, aes(y = median_house_value)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of House Values")

# 4. Categorical Variable Analysis
ggplot(housing, aes(x = ocean_proximity)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Ocean Proximity",
       x = "Ocean Proximity",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Multivariate Analysis

# 5. Correlation Analysis
numeric_vars <- housing[, c("longitude", "latitude", "housing_median_age", 
                            "total_rooms", "total_bedrooms", "population",
                            "households", "median_income", "median_house_value")]
correlation_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(correlation_matrix, method = "color")
cor(housing$median_house_value, housing$median_income)

# 6. Scatter Plot Visualization
ggplot(housing, aes(x = median_income, y = median_house_value)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "House Value vs Median Income",
       x = "Median Income",
       y = "Median House Value")

# 7. Multiple Regression
model <- lm(median_house_value ~ median_income + total_rooms + housing_median_age, 
            data = housing)
summary(model)

# 8. Model Diagnostics
par(mfrow = c(2, 2))
plot(model)

## Advanced Analysis

# 9. Principal Component Analysis
housing_numeric <- scale(numeric_vars)
pca_result <- prcomp(housing_numeric)
fviz_eig(pca_result, addlabels = TRUE)

# 10. PCA Biplot
fviz_pca_biplot(pca_result,
                label = "var",
                habillage = housing$ocean_proximity,
                addEllipses = FALSE,
                title = "PCA Biplot of Housing Data")
