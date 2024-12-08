# --------------------------------------------------------------------------------
# Code for Statistical Analysis of the Journals Dataset
# --------------------------------------------------------------------------------

# Install and load required libraries
install.packages("AER")
install.packages("corrplot")
install.packages("factoextra")
install.packages("skimr")
install.packages("broom")
install.packages("gridExtra")
install.packages("reshape2")
library(AER)
library(corrplot)
library(factoextra)
library(skimr)
library(broom)
library(gridExtra)
library(reshape2)

# Load the Dataset 
data("Journals")
j_df = Journals
# View the first few rows
head(j_df)

# --------------------------------------------------------------------------------
# Univariate Analysis
# --------------------------------------------------------------------------------

# 1. Data Overview
cat("Structure of the Journals Dataset:\n")
str(j_df)
cat("\nDimensions of the Dataset (Rows x Columns):\n")
print(dim(j_df))

# Get a summary of the dataset
summary(j_df)

# Separate Numeric and Categorical Columns
numeric_columns <- names(j_df)[sapply(j_df, is.numeric)]
categorical_columns <- names(j_df)[sapply(j_df, function(x) is.factor(x) | is.character(x))]


# 2. Summary Statistics

# Process Numeric Columns
cat("### Summary Statistics for Numeric Columns ###\n")
for (column_name in numeric_columns) {
  column_data <- j_df[[column_name]]
  
  # Calculate metrics
  min_value <- min(column_data, na.rm = TRUE)
  max_value <- max(column_data, na.rm = TRUE)
  mean_value <- mean(column_data, na.rm = TRUE)
  median_value <- median(column_data, na.rm = TRUE)
  std_dev <- sd(column_data, na.rm = TRUE)
  
  # Print the metrics for the column
  cat("Statistics for column:", column_name, "\n")
  cat("  Min:     ", min_value, "\n")
  cat("  Max:     ", max_value, "\n")
  cat("  Mean:    ", mean_value, "\n")
  cat("  Median:  ", median_value, "\n")
  cat("  Std Dev: ", std_dev, "\n\n")
}

print(summary(j_df))
cat("\nDetailed Summary (skimr):\n")
skim(j_df)

# 3. Distribution Visualization

# Combined Box Plot and Histogram for Each Feature
par(mfrow = c(1, 1))
for (column in numeric_columns) {
  par(mfrow = c(1, 2))
  boxplot(j_df[[column]], 
          main = paste("Box plot of", column), 
          col = 'lightgreen'
          ,horizontal = TRUE)
  hist(j_df[[column]], 
       main = paste("Histogram of", column), 
       xlab = column, 
       col = 'lightgreen', 
       border = 'black', 
       breaks = 25)
}

# 4. Categorical Variable Analysis

# Bar plot
barplot(table(j_df$society),
        main = "Distribution of Journals by Society Status",
        xlab = "Journal Society Status",
        ylab = "Frequency",
        col = c("lightcoral","lightgreen"),
        border = "black")


# --------------------------------------------------------------------------------
# Multivariate Analysis
# --------------------------------------------------------------------------------

# 4. Correlation Analysis
numeric_data <- j_df[, sapply(j_df, is.numeric)]
cor_matrix <- cor(numeric_data)
cat("Correlation Matrix:\n")
print(cor_matrix)
corrplot(cor_matrix, method = "circle", order = "hclust",
         col = colorRampPalette(c("darkblue", "white","tomato"))(200),
         mar = c(2, 2, 4, 2) ,
         tl.col = "black",number.cex = 0.7,
         tl.cex = 0.7, cl.cex = 0.7,  pch.cex = 0.1,
         addCoef.col = "black",
         title = "Correlation Matrix of Journals Dataset")

# 5. Scatter Plot with Trend Line
ggplot(j_df, aes(x = pages, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot between Price and Pages",
       x = "Pages", y = "Price") +
  theme_minimal()

# 6. Multiple Regression
model_1  <- lm(price ~ pages, data = j_df)
model_2  <- lm(price ~ pages + subs, data = j_df)
summary(model_1)
summary(model_2)

# Selecting model_2 as it has slight better R2 score


# 7. Model Diagnostics
model_data <- augment(model_2)
p1 <- ggplot(model_data, aes(.fitted, .resid)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "tomato", se = FALSE) +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal()
p2 <- ggplot(model_data, aes(sample = .std.resid)) +
  stat_qq(color = "darkgreen") +
  stat_qq_line(color = "darkblue") +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
grid.arrange(p1, p2, nrow = 1, top = "Diagnostic Plots for Regression Model")

# --------------------------------------------------------------------------------
# Advanced Analysis
# --------------------------------------------------------------------------------

# 8. Principal Component Analysis (PCA)
# Perform PCA
pca <- prcomp(numeric_data, scale. = TRUE)

#PCA Summary
summary(pca)

# Extract values
explained_variance <- summary(pca)$importance["Proportion of Variance", ] * 100
cumulative_variance <- summary(pca)$importance["Cumulative Proportion", ] * 100

# Step 1: Draw the barplot and capture bar centers
bar_centers <- barplot(cumulative_variance, 
                       col = "skyblue", 
                       ylim = c(0, 100), 
                       ylab = "Cumulative Variance (%)", 
                       xlab = "Principal Component", 
                       cex.lab = .8,
                       cex.axis = .7,
                       names.arg = paste0("PC", 1:length(explained_variance)))

# Step 2: Overlay the line chart (auto-scaled)
par(new = TRUE)
plot(bar_centers, explained_variance, 
     type = "o", col = "purple", lwd = 2.5, pch = 16, 
     axes = FALSE, xlab = "", ylab = "")
axis(4, col.axis = "purple", las = 1,cex.axis=.7)  # Add right y-axis for line plot
mtext("Proportion of Variance (%)", side = 4, line = 3, col = "purple",cex=.2)
par(xpd = TRUE)  # Allow drawing outside the plot

# 9. PCA Interpretation
plot(pca$x[, 1], pca$x[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Biplot of the Journals Dataset", 
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "lightgreen",
     cex.lab = .8,
     cex.axis = .7)
arrows(0, 0, pca$rotation[, 1] * max(pca$x[, 1]), pca$rotation[, 2] * max(pca$x[, 2]), 
       col = "sienna", length = 0.1)
text(pca$rotation[, 1] * max(pca$x[, 1]) * 1.1, pca$rotation[, 2] * max(pca$x[, 2]) * 1.1, 
     labels = rownames(pca$rotation), col = "sienna", cex = .4)
