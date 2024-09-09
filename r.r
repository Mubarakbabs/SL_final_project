# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)
library(factoextra)
library(ggpubr)
library(scales)
library(caret)
library(stats)
library(skimr)

# Load the dataset
df <- read_csv("Mall_Customers.csv")

# View the first few rows and descriptive statistics
head(df)
summary(df)
skim_without_charts(df)
# Rename columns using snake case
colnames(df) <- c('customer_id', 'gender', 'age', 'annual_income', 'spending_score')
# Plotting distributions
columns <- c('age', 'annual_income', 'spending_score')
colnames(df)


par(mfrow = c(2, 2))
# Plotting distributions
for (col in columns) {
    hist(df[[col]], 
         main = paste("Distribution of", col), 
         xlab = col, 
         col = 'blue', 
         border = 'black', 
         breaks = 10)  # Adjust 'breaks' to control bin width
}
# Clear current plot settings


# Kernel density estimation plots with gender hue
ggplot(df, aes(x = `annual_income`, fill = gender)) + 
  geom_density(alpha = 0.5) +
  theme_minimal()

for (col in columns) {
  ggplot(df, aes_string(x = col, fill = 'gender')) + 
    geom_density(alpha = 0.5) +
    theme_minimal()
}

# Boxplots by gender
for (col in columns) {
  ggplot(df, aes(x = gender, y = df[[col]])) + 
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Boxplot of", col, "by gender"))
}

# gender distribution
prop.table(table(df$gender))

# Scatter plot
ggplot(df, aes(x = `annual_income`, y = `spending_score`)) + 
  geom_point() +
  theme_minimal()

# Pairplot equivalent in R using GGally
library(GGally)
ggpairs(df, aes(color = gender))

# Group by gender and summarize means
df %>%
  group_by(gender) %>%
  summarize(across(c(age, `annual_income`, `spending_score`), mean))
library('ggcorrplot')
1# Correlation matrix and heatmap
corr_matrix <- cor(df %>% select(-gender))
ggcorrplot(corr_matrix, method = "circle", type = "lower", 
           lab = TRUE, lab_size = 3, colors = c("red", "white", "blue"))

# Bivariate clustering
set.seed(123)
kmeans2 <- kmeans(df[, c('annual_income', 'spending_score')], centers = 5)
df$SpendingIncomeCluster <- as.factor(kmeans2$cluster)

# Check inertia for bivariate clustering
intertia_scores2 <- sapply(1:10, function(k) {
  kmeans(df[, c('annual_income', 'spending_score')], centers = k)$tot.withinss
})

plot(1:10, intertia_scores2, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

# Cluster centers
centers <- as.data.frame(kmeans2$centers)
names(centers) <- c('x', 'y')

# Scatter plot with cluster centers
ggplot(df, aes(x = `annual_income`, y = `spending_score`, color = SpendingIncomeCluster)) + 
  geom_point(alpha = 0.7) +
  geom_point(data = centers, aes(x = x, y = y), color = "black", size = 3, shape = 8) +
  theme_minimal()

# Cross-tabulation of clusters by gender
table(df$SpendingIncomeCluster, df$gender) %>%
  prop.table(margin = 1)

# Group by cluster and calculate means
df %>%
  group_by(SpendingIncomeCluster) %>%
  summarize(across(c(age, `annual_income`, `spending_score`), mean))

# Multivariate clustering
df <- df %>%
  mutate(gender_Male = ifelse(gender == "Male", 1, 0))

# Scale the data
scale_model <- preProcess(df[, c('age', 'annual_income', 'spending_score', 'gender_Male')], method = c("center", "scale"))
scaled_data <- predict(scale_model, df[, c('age', 'annual_income', 'spending_score', 'gender_Male')])

# Fit k-means for multiple clusters
intertia_scores3 <- sapply(1:10, function(k) {
  kmeans(scaled_data, centers = k)$tot.withinss
})

plot(1:10, intertia_scores3, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")

kmeans3 <- kmeans(df[, c('annual_income', 'spending_score', 'gender_Male', 'age')], centers = 6)
df$multicluster <- as.factor(kmeans3$cluster)
table(df$multicluster)

# Pair scatterplots with multicluster colors
library('GGally')
dev.off()
ggpairs(df[,c('annual_income', 'spending_score', 'gender_Male', 'age', 'multicluster')], aes(color = multicluster))
