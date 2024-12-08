# Load required libraries
library(readr)
library(ggplot2)
library(corrplot)
library(car)
library(factoextra)

# Load the 80 Cereals dataset
file_path <- "C:/Users/Dinesh K/Desktop/ssd assignment/cereal.csv"
cereals <- read_csv(file_path)

## Univariate Analysis

# 1. Data Overview
str(cereals)

dim(cereals)

head(cereals)
# 2. Summary Statistics
summary(cereals$calories)
sd(cereals$calories)

# 3. Distribution Visualization
ggplot(cereals, aes(x = calories)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Calories")

ggplot(cereals, aes(y = calories)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Calories")

# 4. Categorical Variable Analysis
ggplot(cereals, aes(x = mfr)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Cereal Manufacturers")

## Multivariate Analysis

# 5. Correlation Analysis
cor(cereals$calories, cereals$sugars)

# 6. Scatter Plot Visualization
ggplot(cereals, aes(x = calories, y = sugars)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Calories vs Sugars")

# 7. Multiple Regression
model <- lm(rating ~ calories + sugars, data = cereals)
summary(model)

# 8. Model Diagnostics
par(mfrow = c(2, 2))
plot(model)

## Advanced Analysis

# 9. Principal Component Analysis (PCA)
cereals_numeric <- cereals[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", "potass", "vitamins", "rating")]
pca_result <- prcomp(cereals_numeric, scale. = TRUE)
fviz_eig(pca_result, addlabels = TRUE)

# 10. PCA Interpretation
fviz_pca_biplot(pca_result, label = "var", habillage = cereals$mfr,
                addEllipses = FALSE, ellipse.level = 0.95)