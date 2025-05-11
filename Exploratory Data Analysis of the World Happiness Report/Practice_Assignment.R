install.packages("dplyr")
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(ggplot2)

#1.importing the csv file
mydata<-read.csv("D:/R Programming/Final Term/2016 (1).csv",header=TRUE,sep=",")
mydata

#2.Summary & Structure of the dataset
summary(mydata)
str(mydata)

#3. checking for missing values
is.na(mydata)
colSums(is.na(mydata))

#4.Calculate the correlation matrix(Pearson)
# Calculate Pearson correlation matrix for numeric features
cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "pearson")
print(cor_matrix)

# Extract the correlation of each feature with Happiness.Score
correlation_with_happiness <- cor_matrix["Happiness.Score", ]
print(correlation_with_happiness)

# Categorize the correlations as Strong, Moderate, or Weak
correlation_category <- ifelse(abs(correlation_with_happiness) >= 0.7, "Strong",
                               ifelse(abs(correlation_with_happiness) >= 0.3, "Moderate", "Weak"))

# Combine correlations with their categories into a data frame
correlation_df <- data.frame(
  Feature = names(correlation_with_happiness),
  Correlation = correlation_with_happiness,
  Strength = correlation_category
)

# Remove the correlation of Happiness.Score with itself
correlation_df <- correlation_df[correlation_df$Feature != "Happiness.Score", ]

# View the categorized correlations
print(correlation_df)

#4.1 Calculate the correlation matrix(spearman)
# Calculate Spearman correlation matrix for numeric features
cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "spearman")
print(cor_matrix)

# Extract the correlation of each feature with Happiness.Score
correlation_with_happiness <- cor_matrix["Happiness.Score", ]
print(correlation_with_happiness)

# Categorize the correlations as Strong, Moderate, or Weak
correlation_category <- ifelse(abs(correlation_with_happiness) >= 0.7, "Strong",
                               ifelse(abs(correlation_with_happiness) >= 0.3, "Moderate", "Weak"))

# Combine correlations with their categories into a data frame
correlation_df <- data.frame(
  Feature = names(correlation_with_happiness),
  Correlation = correlation_with_happiness,
  Strength = correlation_category
)

# Remove the correlation of Happiness.Score with itself
correlation_df <- correlation_df[correlation_df$Feature != "Happiness.Score", ]

# View the categorized correlations
print(correlation_df)


#5 positive Correlation
#matrix mode
positive_corr_matrix <- cor_matrix
positive_corr_matrix[positive_corr_matrix <= 0] <- NA  # Set non-positive values to NA
print(positive_corr_matrix)

#5.1 Negative Correlation
#matrix mode
negative_corr_matrix <- cor_matrix
negative_corr_matrix[negative_corr_matrix >= 0] <- NA  # Set non-negative values to NA
print(negative_corr_matrix)

#5.2 Zero Correlation
zero_corr_matrix <- cor_matrix
zero_corr_matrix[zero_corr_matrix != 0] <- NA  # Set non-zero values to NA
print(zero_corr_matrix)





#8. Feature Selection

# Ensure you exclude 'Country' and 'Region' from the model, focusing on numeric variables
numeric_predictors <- mydata %>%
  select(Happiness.Rank,Happiness.Score, Lower.Confidence.Interval, Upper.Confidence.Interval, 
         Economy..GDP.per.Capita., Family, Health..Life.Expectancy., 
         Freedom, Trust..Government.Corruption., Generosity, Dystopia.Residual)

# Fit the linear model using the numeric predictors
full_model <- lm(Happiness.Score ~ ., data = numeric_predictors)

# Get the summary of the model
model_summary <- summary(full_model)

# Extract p-values for each feature
p_values <- model_summary$coefficients[, "Pr(>|t|)"]

# Convert to data frame for easier manipulation
p_values_df <- data.frame(Feature = rownames(model_summary$coefficients), P_Value = p_values)

# Exclude the intercept from the list of features
p_values_df <- p_values_df %>% filter(Feature != "(Intercept)")

# View the p-values
print(p_values_df)

# Set a threshold for significant features
threshold <- 0.05

# Filter features with p-value below the threshold
significant_features <- p_values_df %>%
  filter(P_Value < threshold) %>%
  pull(Feature)

# View selected significant features
print(significant_features)










#9.ScatterPlot(Pearson)

#9.1 Scatterplot for Happiness Score vs GDP per Capita
ggplot(mydata, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs GDP per Capita", 
       x = "GDP per Capita", 
       y = "Happiness Score")

#9.2 Scatterplot for Happiness Score vs Life Expectancy
ggplot(mydata, aes(x = Health..Life.Expectancy., y = Happiness.Score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Life Expectancy", 
       x = "Life Expectancy", 
       y = "Happiness Score")

#9.3 Scatterplot for Happiness Score vs Freedom to Make Life Choices
ggplot(mydata, aes(x = Freedom, y = Happiness.Score)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Freedom to Make Life Choices", 
       x = "Freedom to Make Life Choices", 
       y = "Happiness Score")

#9.4 Scatterplot for Happiness Score vs Generosity
ggplot(mydata, aes(x = Generosity, y = Happiness.Score)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Generosity", 
       x = "Generosity", 
       y = "Happiness Score")

#9.5 Scatterplot for Happiness Score vs Generosity
ggplot(mydata, aes(x = Family, y = Happiness.Score)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Family", 
       x = "Family", 
       y = "Happiness Score")


#10 Visualization(Spearman)

#10.1 Scatterplot for Spearman correlation (Happiness Score vs GDP per Capita)
ggplot(mydata, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_point(color = "blue") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "green") +  # Non-linear smoother for Spearman
  labs(title = "Happiness Score vs GDP per Capita (Spearman)",
       x = "GDP per Capita", 
       y = "Happiness Score")

#10.2 Scatterplot for Spearman correlation (Happiness Score vs Life Expectancy)
ggplot(mydata, aes(x = Health..Life.Expectancy., y = Happiness.Score)) +
  geom_point(color = "green") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear smoother for Spearman
  labs(title = "Happiness Score vs Life Expectancy (Spearman)", 
       x = "Life Expectancy", 
       y = "Happiness Score")
#10.3 Scatterplot for Spearman correlation (Happiness Score vs Freedom to Make Life Choices)
ggplot(mydata, aes(x = Freedom, y = Happiness.Score)) +
  geom_point(color = "orange") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear smoother for Spearman
  labs(title = "Happiness Score vs Freedom to Make Life Choices (Spearman)", 
       x = "Freedom to Make Life Choices", 
       y = "Happiness Score")
#10.4 Scatterplot for Spearman correlation (Happiness Score vs Generosity)
ggplot(mydata, aes(x = Generosity, y = Happiness.Score)) +
  geom_point(color = "darkgreen") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear smoother for Spearman
  labs(title = "Happiness Score vs Generosity (Spearman)", 
       x = "Generosity", 
       y = "Happiness Score")

#10.5 Scatterplot for Spearman correlation (Happiness Score vs Family)
ggplot(mydata, aes(x = Family, y = Happiness.Score)) +
  geom_point(color = "purple") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Non-linear smoother for Spearman
  labs(title = "Happiness Score vs Family (Spearman)", 
       x = "Family", 
       y = "Happiness Score")





#11.Heatmaps


#11.1 Melt the correlation matrix into long format
corr_melted <- as.data.frame(as.table(cor_matrix))
colnames(corr_melted) <- c("Var1", "Var2", "value")

# Create the heatmap
ggplot(corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Correlation") +
  labs(title = "Heatmap of Correlations", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#11.2 Melt the correlation matrix into long format
corr_melted <- as.data.frame(as.table(cor_matrix_spearman))
colnames(corr_melted) <- c("Var1", "Var2", "value")

# Create the heatmap
ggplot(corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "yellow", mid = "purple", midpoint = 0, name = "Correlation") +
  labs(title = "Heatmap of Correlations", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







#99.Strong or weak correlation(pearson)
# Calculate Pearson correlation matrix for numeric features
cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "pearson")
print(cor_matrix)

# Extract the correlation of each feature with Happiness.Score
correlation_with_happiness <- cor_matrix["Happiness.Score", ]
print(correlation_with_happiness)

# Categorize the correlations as Strong, Moderate, or Weak
correlation_category <- ifelse(abs(correlation_with_happiness) >= 0.7, "Strong",
                               ifelse(abs(correlation_with_happiness) >= 0.3, "Moderate", "Weak"))

# Combine correlations with their categories into a data frame
correlation_df <- data.frame(
  Feature = names(correlation_with_happiness),
  Correlation = correlation_with_happiness,
  Strength = correlation_category
)

# Remove the correlation of Happiness.Score with itself
correlation_df <- correlation_df[correlation_df$Feature != "Happiness.Score", ]

# View the categorized correlations
print(correlation_df)






#100.Strong or weak Correlation
# Calculate Spearman correlation matrix for numeric features
cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "spearman")
print(cor_matrix)

# Extract the correlation of each feature with Happiness.Score
correlation_with_happiness <- cor_matrix["Happiness.Score", ]
print(correlation_with_happiness)

# Categorize the correlations as Strong, Moderate, or Weak
correlation_category <- ifelse(abs(correlation_with_happiness) >= 0.7, "Strong",
                               ifelse(abs(correlation_with_happiness) >= 0.3, "Moderate", "Weak"))

# Combine correlations with their categories into a data frame
correlation_df <- data.frame(
  Feature = names(correlation_with_happiness),
  Correlation = correlation_with_happiness,
  Strength = correlation_category
)

# Remove the correlation of Happiness.Score with itself
correlation_df <- correlation_df[correlation_df$Feature != "Happiness.Score", ]

# View the categorized correlations
print(correlation_df)




#101.Positive Correlation
positive_corr <- cor_matrix[cor_matrix > 0]
print(positive_corr)

#matrix mode
positive_corr_matrix <- cor_matrix
positive_corr_matrix[positive_corr_matrix <= 0] <- NA  # Set non-positive values to NA
print(positive_corr_matrix)



#102.Negative Correlation
negative_corr <- cor_matrix[cor_matrix < 0]
print(negative_corr)

#matrix mode
negative_corr_matrix <- cor_matrix
negative_corr_matrix[negative_corr_matrix >= 0] <- NA  # Set non-negative values to NA
print(negative_corr_matrix)


#103. Zero Correaltion
zero_corr_matrix <- cor_matrix
zero_corr_matrix[zero_corr_matrix != 0] <- NA  # Set non-zero values to NA
print(zero_corr_matrix)


