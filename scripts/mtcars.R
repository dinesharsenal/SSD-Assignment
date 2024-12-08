# Load required libraries
library(ggplot2)
library(corrplot)
library(car)
library(factoextra)

# Load the mtcars dataset (built into R)
data(mtcars)

## Univariate Analysis

# 1. Data Overview
str(mtcars)
dim(mtcars)
head(mtcars)

# 2. Summary Statistics
summary(mtcars$mpg)
sd(mtcars$mpg)

# 3. Distribution Visualization
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Histogram of MPG")

ggplot(mtcars, aes(y = mpg)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of MPG")

# 4. Categorical Variable Analysis
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Cylinders")

## Multivariate Analysis

# 5. Correlation Analysis
cor(mtcars$mpg, mtcars$wt)

# 6. Scatter Plot Visualization
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Weight vs MPG")

# 7. Multiple Regression
model <- lm(mpg ~ wt + hp, data = mtcars)
summary(model)

# 8. Model Diagnostics
par(mfrow = c(2, 2))
plot(model)

## Advanced Analysis

# 9. Principal Component Analysis (PCA)
mtcars_pca <- prcomp(mtcars, scale. = TRUE)
fviz_eig(mtcars_pca, addlabels = TRUE)

# 10. PCA Interpretation
fviz_pca_biplot(mtcars_pca, label = "var",
                addEllipses = FALSE, ellipse.level = 0.95)