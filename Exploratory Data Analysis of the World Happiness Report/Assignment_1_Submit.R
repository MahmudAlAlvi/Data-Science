install.packages("dplyr")
install.packages("tidyr")
library(tidyr)
library(dplyr)
library(ggplot2)


mydata<-read.csv("D:/R Programming/Final Term/2016 (1).csv",header=TRUE,sep=",")
mydata


summary(mydata)
str(mydata)


is.na(mydata)
colSums(is.na(mydata))





cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "pearson")
print(cor_matrix)

correlation_with_happiness <- cor_matrix["Happiness.Score", ]
print(correlation_with_happiness)

correlation_category <- ifelse(abs(correlation_with_happiness) >= 0.7, "Strong",
                               ifelse(abs(correlation_with_happiness) >= 0.3, "Moderate", "Weak"))

correlation_df <- data.frame(
  Feature = names(correlation_with_happiness),
  Correlation = correlation_with_happiness,
  Strength = correlation_category
)

correlation_df <- correlation_df[correlation_df$Feature != "Happiness.Score", ]

print(correlation_df)







cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "spearman")
print(cor_matrix)

correlation_with_happiness <- cor_matrix["Happiness.Score", ]
print(correlation_with_happiness)

correlation_category <- ifelse(abs(correlation_with_happiness) >= 0.7, "Strong",
                               ifelse(abs(correlation_with_happiness) >= 0.3, "Moderate", "Weak"))

correlation_df <- data.frame(
  Feature = names(correlation_with_happiness),
  Correlation = correlation_with_happiness,
  Strength = correlation_category
)

correlation_df <- correlation_df[correlation_df$Feature != "Happiness.Score", ]

print(correlation_df)







positive_corr_matrix <- cor_matrix
positive_corr_matrix[positive_corr_matrix <= 0] <- NA  
print(positive_corr_matrix)


negative_corr_matrix <- cor_matrix
negative_corr_matrix[negative_corr_matrix >= 0] <- NA  
print(negative_corr_matrix)


zero_corr_matrix <- cor_matrix
zero_corr_matrix[zero_corr_matrix != 0] <- NA  
print(zero_corr_matrix)





numeric_predictors <- mydata %>%
  select(Happiness.Rank,Happiness.Score, Lower.Confidence.Interval, Upper.Confidence.Interval, 
         Economy..GDP.per.Capita., Family, Health..Life.Expectancy., 
         Freedom, Trust..Government.Corruption., Generosity, Dystopia.Residual)

full_model <- lm(Happiness.Score ~ ., data = numeric_predictors)


model_summary <- summary(full_model)


p_values <- model_summary$coefficients[, "Pr(>|t|)"]


p_values_df <- data.frame(Feature = rownames(model_summary$coefficients), P_Value = p_values)


p_values_df <- p_values_df %>% filter(Feature != "(Intercept)")


print(p_values_df)


threshold <- 0.05


significant_features <- p_values_df %>%
  filter(P_Value < threshold) %>%
  pull(Feature)


print(significant_features)









ggplot(mydata, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs GDP per Capita", 
       x = "GDP per Capita", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Health..Life.Expectancy., y = Happiness.Score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Life Expectancy", 
       x = "Life Expectancy", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Freedom, y = Happiness.Score)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Freedom to Make Life Choices", 
       x = "Freedom to Make Life Choices", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Generosity, y = Happiness.Score)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Generosity", 
       x = "Generosity", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Family, y = Happiness.Score)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Happiness Score vs Family", 
       x = "Family", 
       y = "Happiness Score")










ggplot(mydata, aes(x = Economy..GDP.per.Capita., y = Happiness.Score)) +
  geom_point(color = "red") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") + 
  labs(title = "Happiness Score vs GDP per Capita (Spearman)",
       x = "GDP per Capita", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Health..Life.Expectancy., y = Happiness.Score)) +
  geom_point(color = "green") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  
  labs(title = "Happiness Score vs Life Expectancy (Spearman)", 
       x = "Life Expectancy", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Freedom, y = Happiness.Score)) +
  geom_point(color = "orange") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  
  labs(title = "Happiness Score vs Freedom to Make Life Choices (Spearman)", 
       x = "Freedom to Make Life Choices", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Generosity, y = Happiness.Score)) +
  geom_point(color = "darkgreen") +  # Scatterplot points
  geom_smooth(method = "loess", se = FALSE, color = "blue") + 
  labs(title = "Happiness Score vs Generosity (Spearman)", 
       x = "Generosity", 
       y = "Happiness Score")



ggplot(mydata, aes(x = Family, y = Happiness.Score)) +
  geom_point(color = "purple") +  
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  
  labs(title = "Happiness Score vs Family (Spearman)", 
       x = "Family", 
       y = "Happiness Score")









corr_melted <- as.data.frame(as.table(cor_matrix))
colnames(corr_melted) <- c("Var1", "Var2", "value")

ggplot(corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Correlation") +
  labs(title = "Heatmap of Correlations", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






corr_melted <- as.data.frame(as.table(cor_matrix_spearman))
colnames(corr_melted) <- c("Var1", "Var2", "value")

ggplot(corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "yellow", mid = "purple", midpoint = 0, name = "Correlation") +
  labs(title = "Heatmap of Correlations", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









stacked_table <- mydata %>%
  group_by(Region) %>%
  summarize(
    mean_Happiness.Score = mean(Happiness.Score, na.rm = TRUE),
    mean_GDP.per.Capita = mean(Economy..GDP.per.Capita., na.rm = TRUE),
    mean_Life.Expectancy = mean(Health..Life.Expectancy., na.rm = TRUE),
    mean_Freedom = mean(Freedom, na.rm = TRUE),
    mean_Generosity = mean(Generosity, na.rm = TRUE),
    mean_Trust.Government.Corruption = mean(Trust..Government.Corruption., na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("mean_"), 
               names_to = "Indicator", 
               values_to = "Value") %>%
  arrange(Region)


print(stacked_table)


ggplot(stacked_table, aes(x = Region, y = Value, fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Mean Values for Key Indicators by Region",
       x = "Region",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()




