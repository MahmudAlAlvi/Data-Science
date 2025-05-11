install.packages("dplyr")
install.packages("tidyr")
install.packages("moments")
install.packages("fmsb")
install.packages("GGally")

library(tidyr)
library(dplyr)
library(ggplot2)
library(moments)
library(fmsb)


#1.importing the csv file
mydata<-read.csv("D:/R Programming/Final Term/2016 (1).csv",header=TRUE,sep=",")
mydata

#2.Summary & Structure of the dataset
summary(mydata)
str(mydata)

#3. checking for missing values
is.na(mydata)
colSums(is.na(mydata))

#4. Histogram 


# First Histogram(Happiness.score)
ggplot(mydata, aes(x = Happiness.Score)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Happiness Score",
       x = "Happiness Score",
       y = "Frequency") +
  theme_minimal()



# Second Histogram( Economy..GDP.per.Capita.)
ggplot(mydata, aes(x = Economy..GDP.per.Capita.)) +
  geom_histogram(binwidth = 0.05, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Economy (GDP per Capita)",
       x = "Economy (GDP per Capita)",
       y = "Frequency") +
  theme_minimal()


# Third Histogram(Health..Life.Expectancy.)
ggplot(mydata, aes(x = Health..Life.Expectancy.)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Health (Life Expectancy)",
       x = "Health (Life Expectancy)",
       y = "Frequency") +
  theme_minimal()

#Fourth Histogram(Generocity)
# Histogram for Generosity
ggplot(mydata, aes(x = Generosity)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Generosity",
       x = "Generosity",
       y = "Frequency") +
  theme_minimal()






#5. Distribution of histogram


# Function to determine skewness type
check_skewness <- function(skewness_value) {
  if (skewness_value > 0) {
    return("Right Skewed (Positively Skewed)")
  } else if (skewness_value < 0) {
    return("Left Skewed (Negatively Skewed)")
  } else {
    return("Approximately Normal Distribution")
  }
}

# Calculate skewness for Happiness Score
skewness_happiness <- skewness(mydata$Happiness.Score, na.rm = TRUE)
cat("Skewness of Happiness Score: ", skewness_happiness, "-", check_skewness(skewness_happiness), "\n")

# Calculate skewness for Economy (GDP per Capita)
skewness_gdp <- skewness(mydata$Economy..GDP.per.Capita., na.rm = TRUE)
cat("Skewness of Economy (GDP per Capita): ", skewness_gdp, "-", check_skewness(skewness_gdp), "\n")

# Calculate skewness for Health (Life Expectancy)
skewness_health <- skewness(mydata$Health..Life.Expectancy., na.rm = TRUE)
cat("Skewness of Health (Life Expectancy): ", skewness_health, "-", check_skewness(skewness_health), "\n")

# Calculate skewness for Generosity
skewness_generosity <- skewness(mydata$Generosity, na.rm = TRUE)
cat("Skewness of Generosity: ", skewness_generosity, "-", check_skewness(skewness_generosity), "\n")




#6. Line Histogram

# Line histogram (density plot) for Happiness Score
ggplot(mydata, aes(x = Happiness.Score)) +
  geom_density(color = "blue", fill = "blue", alpha = 0.3) +
  labs(title = "Density Plot of Happiness Score",
       x = "Happiness Score",
       y = "Density") +
  theme_minimal()

# Line histogram (density plot) for Economy (GDP per Capita)
ggplot(mydata, aes(x = Economy..GDP.per.Capita.)) +
  geom_density(color = "green", fill = "green", alpha = 0.3) +
  labs(title = "Density Plot of Economy (GDP per Capita)",
       x = "Economy (GDP per Capita)",
       y = "Density") +
  theme_minimal()

# Line histogram (density plot) for Health (Life Expectancy)
ggplot(mydata, aes(x = Health..Life.Expectancy.)) +
  geom_density(color = "purple", fill = "purple", alpha = 0.3) +
  labs(title = "Density Plot of Health (Life Expectancy)",
       x = "Health (Life Expectancy)",
       y = "Density") +
  theme_minimal()

# Line histogram (density plot) for Generosity
ggplot(mydata, aes(x = Generosity)) +
  geom_density(color = "blue", fill = "blue", alpha = 0.3) +
  labs(title = "Density Plot of Generosity",
       x = "Generosity",
       y = "Density") +
  theme_minimal()






#07. Checking Distribution (Mean, median, mode)
# Define a function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to check if the distribution is normal, left or right skewed
check_skewness <- function(mean_val, median_val, mode_val, var_name) {
  if (mean_val == median_val && median_val == mode_val) {
    cat(var_name, "- Distribution is likely normal.\n")
  } else if (mean_val > median_val && median_val > mode_val) {
    cat(var_name, "- Distribution is right-skewed.\n")
  } else if (mean_val < median_val && median_val < mode_val) {
    cat(var_name, "- Distribution is left-skewed.\n")
  } else {
    cat(var_name, "- Distribution doesn't clearly follow normal or skewed pattern.\n")
  }
}

# Step 1: Mean, Median, Mode calculation and skewness check

# Mean, Median, Mode for Happiness.Score
mean_happiness <- mean(mydata$Happiness.Score, na.rm = TRUE)
median_happiness <- median(mydata$Happiness.Score, na.rm = TRUE)
mode_happiness <- get_mode(mydata$Happiness.Score)

cat("Happiness Score - Mean:", mean_happiness, " Median:", median_happiness, " Mode:", mode_happiness, "\n")
check_skewness(mean_happiness, median_happiness, mode_happiness, "Happiness Score")

# Mean, Median, Mode for Economy..GDP.per.Capita.
mean_gdp <- mean(mydata$Economy..GDP.per.Capita., na.rm = TRUE)
median_gdp <- median(mydata$Economy..GDP.per.Capita., na.rm = TRUE)
mode_gdp <- get_mode(mydata$Economy..GDP.per.Capita.)

cat("GDP per Capita - Mean:", mean_gdp, " Median:", median_gdp, " Mode:", mode_gdp, "\n")
check_skewness(mean_gdp, median_gdp, mode_gdp, "GDP per Capita")

# Mean, Median, Mode for Health..Life.Expectancy.
mean_health <- mean(mydata$Health..Life.Expectancy., na.rm = TRUE)
median_health <- median(mydata$Health..Life.Expectancy., na.rm = TRUE)
mode_health <- get_mode(mydata$Health..Life.Expectancy.)

cat("Life Expectancy - Mean:", mean_health, " Median:", median_health, " Mode:", mode_health, "\n")
check_skewness(mean_health, median_health, mode_health, "Life Expectancy")


# Mean, Median, Mode for Generosity
mean_generosity <- mean(mydata$Generosity, na.rm = TRUE)
median_generosity <- median(mydata$Generosity, na.rm = TRUE)
mode_generosity <- get_mode(mydata$Generosity)


cat("Generosity - Mean:", mean_generosity, " Median:", median_generosity, " Mode:", mode_generosity, "\n")
check_skewness(mean_generosity, median_generosity, mode_generosity, "Generosity")



# Step 2: Visualize the distributions using histograms with mean, median, and mode

# Function to plot histogram with mean, median, and mode (with y-axis as frequency)
plot_distribution <- function(data, variable_name, var_title) {
  mean_val <- mean(data, na.rm = TRUE)
  median_val <- median(data, na.rm = TRUE)
  mode_val <- get_mode(data)
  
  ggplot(mydata, aes_string(x = variable_name)) +
    geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
    geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median_val), color = "green", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mode_val), color = "blue", linetype = "dashed", size = 1) +
    labs(title = paste("Distribution of", var_title),
         x = var_title, 
         y = "Frequency") +   # Changed y-axis label to Frequency
    theme_minimal() +
    annotate("text", x = mean_val, y = 0.05, label = "Mean", color = "red", vjust = -1, angle = 90) +
    annotate("text", x = median_val, y = 0.05, label = "Median", color = "green", vjust = -1, angle = 90) +
    annotate("text", x = mode_val, y = 0.05, label = "Mode", color = "blue", vjust = -1, angle = 90)
}

# Plot for Happiness.Score
plot_distribution(mydata$Happiness.Score, "Happiness.Score", "Happiness Score")

# Plot for Economy..GDP.per.Capita.
plot_distribution(mydata$Economy..GDP.per.Capita., "Economy..GDP.per.Capita.", "GDP per Capita")

# Plot for Health..Life.Expectancy.
plot_distribution(mydata$Health..Life.Expectancy., "Health..Life.Expectancy.", "Life Expectancy")

# Plot for Generosity
plot_distribution(mydata$Generosity, "Generosity", "Generosity Distribution")


#8.bar plot

#8.1 Bar Plot for Country 
ggplot(mydata, aes(x = Country)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Frequency of Countries in World Happiness Report",
       x = "Country", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#8.2 Bar Plot for Region
ggplot(mydata, aes(x = Region)) +
  geom_bar(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Frequency of Regions in World Happiness Report",
       x = "Region", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Create a new variable for Happiness Category
mydata$Happiness_Category <- cut(mydata$Happiness.Score,
                                 breaks = c(-Inf, 4, 6, Inf),
                                 labels = c("Low Happiness", "Moderate Happiness", "High Happiness"),
                                 right = FALSE)

# Create a bar plot for the new variable
ggplot(mydata, aes(x = Happiness_Category, fill = Happiness_Category)) +
  geom_bar() +
  labs(title = "Distribution of Happiness Categories",
       x = "Happiness Category",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") +  # Use distinct color palette
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend for a cleaner look





#9. Boxplot

#Boxplot for Happiness.Score
ggplot(mydata, aes(x = "", y = Happiness.Score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Happiness Score", y = "Happiness Score") +
  theme_minimal()


#Boxplot for  Economy..GDP.per.Capita.
ggplot(mydata, aes(x = "", y = Economy..GDP.per.Capita.)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Economy (GDP per Capita)", y = "Economy (GDP per Capita)") +
  theme_minimal()


#Boxplot for Health..Life.Expectancy.
ggplot(mydata, aes(x = "", y = Health..Life.Expectancy.)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Boxplot of Health (Life Expectancy)", y = "Health (Life Expectancy)") +
  theme_minimal()

#Boxplot for Freedom
ggplot(mydata, aes(x = "", y = Freedom)) +
  geom_boxplot(fill = "lightyellow", color = "black") +
  labs(title = "Boxplot of Freedom", y = "Freedom") +
  theme_minimal()

#Boxplot for Generosity
ggplot(mydata, aes(x = "", y = Generosity)) +
  geom_boxplot(fill = "lightpink", color = "black") +
  labs(title = "Boxplot of Generosity", y = "Generosity") +
  theme_minimal()




#8.Calculate the correlation matrix(Pearson)
# Calculate Pearson correlation matrix for numeric features
cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "pearson")
print(cor_matrix)


#8.1 Calculate the correlation matrix(spearman)
# Calculate Spearman correlation matrix for numeric features
cor_matrix <- cor(mydata[, sapply(mydata, is.numeric)], method = "spearman")
print(cor_matrix)



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



#11. Violin Plot


# Select 5 variables along with Happiness.Score
selected_vars <- mydata %>%
  select(Happiness.Score, Economy..GDP.per.Capita., Health..Life.Expectancy., 
         Freedom, Generosity, Trust..Government.Corruption.)

# Reshape data into long format using pivot_longer()
long_data <- selected_vars %>%
  pivot_longer(cols = -Happiness.Score, names_to = "Variable", values_to = "Value")

# Create the violin plot with Happiness.Score on the y-axis and other variables on the x-axis
ggplot(long_data, aes(x = Variable, y = Happiness.Score, fill = Variable)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
  # Overlay with boxplot to show median and quartiles
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA, alpha = 0.5) +
  labs(title = "Violin Plot of Happiness Score vs Selected Variables",
       x = "Variables",
       y = "Happiness Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()




#11.1 violin plot(Region vs Happiness.score)

# Ensure Region is treated as a factor (categorical variable)
mydata$Region <- as.factor(mydata$Region)

# Create the violin plot with Happiness.Score on the y-axis and Region on the x-axis
ggplot(mydata, aes(x = Region, y = Happiness.Score, fill = Region)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7, scale = "width") +  # Adjust width for better clarity
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Overlay boxplot
  labs(title = "Violin Plot of Happiness Score by Region",
       x = "Region",
       y = "Happiness Score") +
  scale_fill_brewer(palette = "Set3") +  # Use distinct color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        legend.position = "none",  # Hide the legend for a cleaner look
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  coord_flip()  # Flip the coordinates to make the violins larger and easier to read

#11.2  violin plot(Region vs  Economy..GDP.per.Capita.)

# Ensure Region is treated as a factor (categorical variable)
mydata$Region <- as.factor(mydata$Region)

# Create the violin plot with Economy (GDP per Capita) on the y-axis and Region on the x-axis
ggplot(mydata, aes(x = Region, y = Economy..GDP.per.Capita., fill = Region)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7, scale = "width") +  # Adjust width for better clarity
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Overlay boxplot
  labs(title = "Violin Plot of Economy (GDP per Capita) by Region",
       x = "Region",
       y = "Economy (GDP per Capita)") +
  scale_fill_brewer(palette = "Set3") +  # Use distinct color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        legend.position = "none",  # Hide the legend for a cleaner look
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  coord_flip()  # Flip the coordinates for larger violins



# Ensure Region is treated as a factor (categorical variable)
mydata$Region <- as.factor(mydata$Region)

# Create the violin plot with Happiness.Score on the y-axis and Region on the x-axis
ggplot(mydata, aes(x = Region, y = Happiness.Score, fill = Region)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7, scale = "width") +  # Adjust width for better clarity
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Overlay boxplot
  labs(title = "Violin Plot of Happiness Score by Region",
       x = "Region",
       y = "Happiness Score") +
  scale_fill_brewer(palette = "Set3") +  # Use distinct color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        legend.position = "none",  # Hide the legend for a cleaner look
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  coord_flip()  # Flip the coordinates to make the violins larger and easier to read

#11.3 violin plot (Region vs  Health..Life.Expectancy.)
# Ensure Region is treated as a factor (categorical variable)
mydata$Region <- as.factor(mydata$Region)

# Create the violin plot with Life Expectancy on the y-axis and Region on the x-axis
ggplot(mydata, aes(x = Region, y = Health..Life.Expectancy., fill = Region)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7, scale = "width") +  # Adjust width for better clarity
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Overlay boxplot
  labs(title = "Violin Plot of Life Expectancy by Region",
       x = "Region",
       y = "Life Expectancy") +
  scale_fill_brewer(palette = "Set3") +  # Use distinct color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        legend.position = "none",  # Hide the legend for a cleaner look
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  coord_flip()  # Flip the coordinates for larger violins

#11.4 violin plot (Region vs Generosity)
# Ensure Region is treated as a factor (categorical variable)
mydata$Region <- as.factor(mydata$Region)

# Create the violin plot with Generosity on the y-axis and Region on the x-axis
ggplot(mydata, aes(x = Region, y = Generosity, fill = Region)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7, scale = "width") +  # Adjust width for better clarity
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Overlay boxplot
  labs(title = "Violin Plot of Generosity by Region",
       x = "Region",
       y = "Generosity") +
  scale_fill_brewer(palette = "Set3") +  # Use distinct color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        legend.position = "none",  # Hide the legend for a cleaner look
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  coord_flip()  # Flip the coordinates for larger violins





#12. line graph

#12.1 Region vs Happiness.Score
# Ensure Region is treated as a factor
mydata$Region <- as.factor(mydata$Region)

# Calculate the average Happiness.Score for each Region
region_avg <- mydata %>%
  group_by(Region) %>%
  summarise(Average_Happiness = mean(Happiness.Score, na.rm = TRUE))

# Create the line graph with Region on the x-axis and Happiness.Score on the y-axis
ggplot(region_avg, aes(x = Region, y = Average_Happiness, group = 1)) +
  geom_line(color = "blue", size = 1.2) +  # Add line
  geom_point(color = "red", size = 3) +   # Add points on the line for clarity
  labs(title = "Average Happiness Score by Region",
       x = "Region",
       y = "Average Happiness Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5))  # Center the title


#12.2 Region vs Economy..GDP.per.Capita.

# Ensure Region is treated as a factor
mydata$Region <- as.factor(mydata$Region)

# Calculate the average Economy..GDP.per.Capita. for each Region
region_avg_gdp <- mydata %>%
  group_by(Region) %>%
  summarise(Average_GDP = mean(Economy..GDP.per.Capita., na.rm = TRUE))

# Create the line graph with Region on the x-axis and Average_GDP on the y-axis
ggplot(region_avg_gdp, aes(x = Region, y = Average_GDP, group = 1)) +
  geom_line(color = "blue", size = 1.2) +  # Add line
  geom_point(color = "red", size = 3) +   # Add points on the line for clarity
  labs(title = "Average GDP per Capita by Region",
       x = "Region",
       y = "Average GDP per Capita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5))  # Center the title


#12.3 Region vs Dystopia.Residual

# Ensure Region is treated as a factor
mydata$Region <- as.factor(mydata$Region)

# Calculate the average Dystopia.Residual for each Region
region_avg_dystopia <- mydata %>%
  group_by(Region) %>%
  summarise(Average_Dystopia_Residual = mean(Dystopia.Residual, na.rm = TRUE))

# Create the line graph with Region on the x-axis and Average_Dystopia_Residual on the y-axis
ggplot(region_avg_dystopia, aes(x = Region, y = Average_Dystopia_Residual, group = 1)) +
  geom_line(color = "green", size = 1.2) +  # Add line
  geom_point(color = "orange", size = 3) +  # Add points on the line for clarity
  labs(title = "Average Dystopia Residual by Region",
       x = "Region",
       y = "Average Dystopia Residual") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5))  # Center the title





#13. Radar Chart

#13.1 North America VS Southern Asia

# Select two regions
radar_data <- mydata %>% 
  filter(Region %in% c("North America", "Southern Asia")) %>% 
  select(Region, Happiness.Score, Economy..GDP.per.Capita., Health..Life.Expectancy., Freedom, Generosity)

# Calculate mean of the 5 numeric variables for each region
radar_data <- radar_data %>% 
  group_by(Region) %>% 
  summarise(
    Happiness.Score = mean(Happiness.Score, na.rm = TRUE),
    Economy = mean(Economy..GDP.per.Capita., na.rm = TRUE),
    Health = mean(Health..Life.Expectancy., na.rm = TRUE),
    Freedom = mean(Freedom, na.rm = TRUE),
    Generosity = mean(Generosity, na.rm = TRUE)
  )
radar_data
# Add max and min values for scaling the radar chart
max_min <- data.frame(
  Region = c("Max", "Min"),
  Happiness.Score = c(10, 0),  # Adjust these values based on the expected range
  Economy = c(2, 0),
  Health = c(1.2, 0),
  Freedom = c(1, 0),
  Generosity = c(0.8, 0)
)

# Bind the max, min and actual data
radar_data <- rbind(max_min, radar_data)

# Remove the Region column for plotting
radar_data <- radar_data[, -1]

# Create radar chart
radarchart(
  radar_data,
  axistype = 1,
  pcol = c("red", "blue"),    # Colors for regions
  pfcol = c(rgb(0.2,0.5,0.5,0.5), rgb(0.8,0.2,0.5,0.5)),  # Fill colors
  plwd = 2,                   # Line width
  cglcol = "grey",            # Color of the grid
  cglty = 1,                  # Line type of the grid
  axislabcol = "grey",        # Color of axis labels
  caxislabels = seq(0, 10, 2), # Axis labels
  cglwd = 0.8,                # Grid line width
  vlcex = 0.8                 # Label size
)

legend(x = "topright", legend = c("North America", "Southern Asia"), fill = c("red", "blue"))

#13.2 Eastern Asia VS Western Europe


# Select two regions (Eastern Asia and Western Europe)
radar_data <- mydata %>% 
  filter(Region %in% c("Eastern Asia", "Western Europe")) %>% 
  select(Region, Happiness.Score, Economy..GDP.per.Capita., Health..Life.Expectancy., Freedom, Generosity)

# Calculate mean of the 5 numeric variables for each region
radar_data <- radar_data %>% 
  group_by(Region) %>% 
  summarise(
    Happiness.Score = mean(Happiness.Score, na.rm = TRUE),
    Economy = mean(Economy..GDP.per.Capita., na.rm = TRUE),
    Health = mean(Health..Life.Expectancy., na.rm = TRUE),
    Freedom = mean(Freedom, na.rm = TRUE),
    Generosity = mean(Generosity, na.rm = TRUE)
  )

# Add max and min values for scaling the radar chart
max_min <- data.frame(
  Region = c("Max", "Min"),
  Happiness.Score = c(10, 0),  # Adjust these values based on the expected range
  Economy = c(2, 0),
  Health = c(1.2, 0),
  Freedom = c(1, 0),
  Generosity = c(0.8, 0)
)

# Bind the max, min and actual data
radar_data <- rbind(max_min, radar_data)

# Remove the Region column for plotting
radar_data <- radar_data[, -1]

# Create radar chart
radarchart(
  radar_data,
  axistype = 1,
  pcol = c("red", "purple"),    # Colors for regions
  pfcol = c(rgb(0.2,0.7,0.3,0.5), rgb(0.6,0.3,0.7,0.5)),  # Fill colors
  plwd = 2,                   # Line width
  cglcol = "grey",            # Color of the grid
  cglty = 1,                  # Line type of the grid
  axislabcol = "grey",        # Color of axis labels
  caxislabels = seq(0, 10, 2), # Axis labels
  cglwd = 0.8,                # Grid line width
  vlcex = 0.8                 # Label size
)

# Add a legend
legend(x = "topright", legend = c("Eastern Asia", "Western Europe"), fill = c("red", "purple"))


#13.3 Central and Eastern Europe VS Sub-Saharan Asia

# Select two regions (Central and Eastern Europe, Sub-Saharan Africa)
radar_data <- mydata %>% 
  filter(Region %in% c("Central and Eastern Europe", "Sub-Saharan Africa")) %>% 
  select(Region, Happiness.Score, Economy..GDP.per.Capita., Health..Life.Expectancy., Freedom, Generosity)

# Calculate mean of the 5 numeric variables for each region
radar_data <- radar_data %>% 
  group_by(Region) %>% 
  summarise(
    Happiness.Score = mean(Happiness.Score, na.rm = TRUE),
    Economy = mean(Economy..GDP.per.Capita., na.rm = TRUE),
    Health = mean(Health..Life.Expectancy., na.rm = TRUE),
    Freedom = mean(Freedom, na.rm = TRUE),
    Generosity = mean(Generosity, na.rm = TRUE)
  )

# Add max and min values for scaling the radar chart
max_min <- data.frame(
  Region = c("Max", "Min"),
  Happiness.Score = c(10, 0),  # Adjust these values based on the expected range
  Economy = c(2, 0),
  Health = c(1.2, 0),
  Freedom = c(1, 0),
  Generosity = c(0.8, 0)
)

# Bind the max, min, and actual data
radar_data <- rbind(max_min, radar_data)

# Remove the Region column for plotting
radar_data <- radar_data[, -1]

# Create radar chart
radarchart(
  radar_data,
  axistype = 1,
  pcol = c("orange", "blue"),    # Colors for regions
  pfcol = c(rgb(1,0.5,0,0.5), rgb(0,0.5,1,0.5)),  # Fill colors
  plwd = 2,                   # Line width
  cglcol = "grey",            # Color of the grid
  cglty = 1,                  # Line type of the grid
  axislabcol = "grey",        # Color of axis labels
  caxislabels = seq(0, 10, 2), # Axis labels
  cglwd = 0.8,                # Grid line width
  vlcex = 0.8                 # Label size
)

# Add a legend
legend(x = "topright", legend = c("Central and Eastern Europe", "Sub-Saharan Africa"), fill = c("orange", "blue"))


##########













###



# Histogram with normal curve for Happiness.Score
ggplot(mydata, aes(x = Happiness.Score)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(mydata$Happiness.Score, na.rm = TRUE), sd = sd(mydata$Happiness.Score, na.rm = TRUE)), color = "red", size = 1) +
  labs(title = "Histogram of Happiness Score with Normal Curve", x = "Happiness Score", y = "Density") +
  theme_minimal()

# Histogram with normal curve for Economy..GDP.per.Capita.
ggplot(mydata, aes(x = Economy..GDP.per.Capita.)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "green", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(mydata$Economy..GDP.per.Capita., na.rm = TRUE), sd = sd(mydata$Economy..GDP.per.Capita., na.rm = TRUE)), color = "red", size = 1) +
  labs(title = "Histogram of GDP per Capita with Normal Curve", x = "GDP per Capita", y = "Density") +
  theme_minimal()

# Histogram with normal curve for Health..Life.Expectancy.
ggplot(mydata, aes(x = Health..Life.Expectancy.)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "purple", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(mydata$Health..Life.Expectancy., na.rm = TRUE), sd = sd(mydata$Health..Life.Expectancy., na.rm = TRUE)), color = "red", size = 1) +
  labs(title = "Histogram of Life Expectancy with Normal Curve", x = "Life Expectancy", y = "Density") +
  theme_minimal()

# Histogram with normal curve for Generosity
ggplot(mydata, aes(x = Generosity)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "yellow", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(mydata$Generosity, na.rm = TRUE), sd = sd(mydata$Generosity, na.rm = TRUE)), color = "red", size = 1) +
  labs(title = "Histogram of Generosity with Normal Curve", x = "Generosity", y = "Density") +
  theme_minimal()






######
# Function to determine skewness type
check_skewness <- function(skewness_value) {
  if (skewness_value > 0) {
    return("Right Skewed (Positively Skewed)")
  } else if (skewness_value < 0) {
    return("Left Skewed (Negatively Skewed)")
  } else {
    return("Approximately Normal Distribution")
  }
}

# Calculate skewness for all numeric columns in the dataset
numeric_columns <- sapply(mydata, is.numeric)  # Identify numeric columns
skewness_results <- sapply(mydata[, numeric_columns], skewness, na.rm = TRUE)  # Calculate skewness

# Print skewness results with descriptions
for (col_name in names(skewness_results)) {
  cat("Skewness of", col_name, ":", skewness_results[col_name], "-", check_skewness(skewness_results[col_name]), "\n")
}



#########
# Install GGally if not already installed
install.packages("GGally")

# Load required libraries
library(GGally)
library(dplyr)

# Assuming your dataset is named 'mydata' and you've already loaded it
# Select only the numeric columns for the scatter matrix
numericCols <- select(mydata, Happiness.Score, Economy..GDP.per.Capita., 
                      Health..Life.Expectancy., Freedom, Generosity)

# Create the scatterplot matrix
ggpairs(numericCols) + 
  ggtitle("Scatterplot Matrix: World Happiness Report")




# Display the plot
print(scatter_matrix)




