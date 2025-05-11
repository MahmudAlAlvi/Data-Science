install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(tidyr)





#1.importing the csv file
mydata<-read.csv("D:/R Programming/Midterm Project/Mid_Dataset.csv",header=TRUE,sep=",")
mydata

#1.1 Keeping "NA" at the position of the missing value
mydata$Gender <- ifelse(mydata$Gender == "", NA, mydata$Gender)
mydata$fare <- ifelse(mydata$fare == "", NA, mydata$fare)
mydata$embarked <- ifelse(mydata$embarked == "", NA, mydata$embarked)
mydata$class <- ifelse(mydata$class == "", NA, mydata$class)
mydata$who <- ifelse(mydata$who == "", NA, mydata$who)
mydata
 
#1.2Graph of missing value
# Calculate the number of missing values for each column
missing_values <- sapply(mydata, function(x) sum(is.na(x)))
missing_values_df <- data.frame(Variable = names(missing_values), Missing = missing_values)

# Create a bar plot of missing values
missing_values_plot <- ggplot(missing_values_df, aes(x = reorder(Variable, -Missing), y = Missing)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Missing Values in Dataset",
       x = "Variables",
       y = "Number of Missing Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(missing_values_plot)

#1.3measure of central tendency and spread
summary(mydata)

#1.4Standard deviation and mean median visualization
numerical_columns <- sapply(mydata, is.numeric)
standard_deviations <- numeric(length = sum(numerical_columns))
names(standard_deviations) <- names(mydata)[numerical_columns]
for (col in names(mydata)[numerical_columns]) {
  standard_deviations[col] <- sd(mydata[[col]], na.rm = TRUE)
}
print(standard_deviations)


#correcting invalid values
mydata$who <- gsub("mannn", "man", mydata$who)
mydata

# Replace 'man' with 'woman' and 'woman' with 'man' using recode
mydata <- mydata %>%
  mutate(who = recode(who, "man" = "woman", "woman" = "man"))
mydata

#subset function-filter
check_data<-filter(mydata,who=="child")
check_data

# Convert 'fare' from character to numeric
mydata$fare <- as.numeric(mydata$fare)

#factor function- for making level of categorical data
mydata <- mydata %>%
  mutate(
    Gender = factor(Gender, levels = c("male", "female"), labels = c(1, 2)),
    embarked   = factor(embarked, levels = c("C","Q","S"), labels = c(1,2,3)),
    class    = factor(class , levels = c("First", "Second", "Third"), labels = c(1, 2, 3)),
    who = factor(who, levels = c("man", "woman","child"), labels = c(1, 2,3)),
    alone = factor(alone, levels = c("TRUE", "FALSE"), labels = c(1, 2))
  )
mydata



#Finding numeric columns
numeric_columns <- sapply(mydata, is.numeric)
numeric_columns



#2.detecting null values for the data set
mydata[mydata == ""] <- NA
is.na(mydata)

#3.counting number of null values in each column
colSums(is.na(mydata))

#4.Finding specific rows for null values
which(is.na(mydata$Gender))
which(is.na(mydata$age))
which(is.na(mydata$sibsp))
which(is.na(mydata$parch))
which(is.na(mydata$fare))
which(is.na(mydata$embarked))
which(is.na(mydata$class))
which(is.na(mydata$who))
which(is.na(mydata$alone))

#4.1another way to find the row numbers of missing value
missing_indices <- lapply(mydata, function(x) which(is.na(x)))
missing_indices

#5.to know the structure of the data set
str(mydata)

#6.Discard Instances
remove<-na.omit(mydata)
remove



#7.imputation method
# 7.1 Calculate mean age
mean_age <- as.integer(mean(mydata$age, na.rm = TRUE))
print(mean_age)  

# Replace missing values with mean 
mydata <- mydata %>%
  mutate(age = ifelse(is.na(age), mean_age, age))
mydata

#7.2calculate median age
median_age <- as.integer(median(mydata$age, na.rm = TRUE))
print(median_age)  # Output the median age as an integer

# Replace missing values with median
mydata <- mydata %>%
  mutate(age = ifelse(is.na(age), median_age, age))
mydata

#8.find mode value and replace the NA value with mode value
#8.1 calculate mode of sibsp
column_name <- "sibsp"
mode_value <- as.numeric(names(which.max(table(mydata[[column_name]]))))
print(mode_value)
column_name <- "sibsp"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$sibsp ))
mydata

#8.2 calculate mode of parch
column_name <- "parch"
mode_value <- as.numeric(names(which.max(table(mydata[[column_name]]))))
print(mode_value)
column_name <- "parch"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$parch ))
mydata

#8.3 calculate mode of alone
column_name <- "alone"
mode_value <- as.logical(names(which.max(table(mydata[[column_name]]))))
print(mode_value)
column_name <- "alone"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$alone ))
mydata

#8.4 calculate mode of Gender
column_name <- "Gender"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "Gender"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$Gender ))
mydata

#8.5 calculate mode of class
column_name <- "class"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "class"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$class ))
mydata

#8.6 calculate mode of embarked
column_name <- "embarked"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "embarked"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$embarked ))
mydata

#8.7 calculate mode of who
column_name <- "who"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "who"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$who ))
mydata

#9.nadim's graph


#10.Outlier Detection
#age
#Outlier Detection and Removal with Interquartile Range (IQR) Method
Q1 <- quantile(mydata$age[mydata$age >= 40 & mydata$age <= 95], 0.25)
Q3 <- quantile(mydata$age[mydata$age >= 40 & mydata$age <= 95], 0.75)
IQR_value <- Q3 - Q1
threshold <- 1.5
outlier_condition <- (mydata$age < (Q1 - threshold * IQR_value)) | (mydata$age > (Q3 + threshold * IQR_value))
mydata <- mydata[!outlier_condition, ]
mydata

#Serial Update After Removing Outliers
# mydata <- mydata %>%
#   filter(!outlier_condition) %>%
#   mutate(row_number = row_number())

#10.1  Keep only numeric values in the fare column and remove invalid values
#fare
mydata <- mydata %>%
  mutate(fare = as.numeric(as.character(fare))) %>%  # Invalid values will become NA
  filter(!is.na(fare))  # Filter out NA values
mydata


#10.2  Identify and remove the rows with invalid 'who' values
mydata <- mydata %>%
  mutate(row_number = row_number()) %>%
  filter(who %in% c("man", "woman", "child"))
mydata

#11.factor function- categorical value to numerical value
mydata <- mydata %>%
  mutate(
    Gender = factor(Gender, levels = c("male", "female"), labels = c(1, 2)),
    embarked   = factor(embarked, levels = c("C","Q","S"), labels = c(1,2,3)),
    class    = factor(class , levels = c("First", "Second", "Third"), labels = c(1, 2, 3)),
    who = factor(who, levels = c("man", "woman","child"), labels = c(1, 2,3)),
    alone = factor(alone, levels = c("TRUE", "FALSE"), labels = c(1, 2))
  )
mydata

#12.factor function- numerical value to categorical value
mydata <- mydata %>%
  mutate(
    survived = factor(survived, levels = c(0, 1), labels = c("Dead", "Alive")),
    
  )
mydata

#13. Categorize the age attribute into different sets
mydata <- mydata %>%
  mutate(age_category = case_when(
    age < 18 ~ "child",
    age >= 18 & age <= 30 ~ "young",
    age > 30 & age <= 50 ~ "middle_aged",
    age > 50 ~ "old"
  ))
mydata

#14.normalization of age attribute
#First impute the missing values then normalize it
column_name <- "age"
column <- mydata[[column_name]]
min_value <- min(column)
max_value <- max(column)
normalized_column <- (column - min_value) / (max_value - min_value)
mydata[[column_name]] <- normalized_column
mydata$age

mydata

#15. who attribute mismatch



#16.Finding duplicate value
#age
duplicates_age<-distinct(mydata,age,.keep_all = TRUE)
duplicates_age
#fare
duplicates_fare<-distinct(mydata,fare,.keep_all = TRUE)
duplicates_fare


#17.using filtering method
data<-filter(mydata,Gender=="female")
data

data<-filter(mydata,age > 15 & age < 45)
data

data<-filter(mydata,sibsp >1 )
data

data<-filter(mydata,parch >1 )
data

#data<-filter(mydata,fare>20 )
#data

data<-filter(mydata,embarked=="Q")
data

data<-filter(mydata,who=="child")
data

data<-filter(mydata,class=="Third")
data


#18.imbalanced dataset into balanced dataset
# Print the first few rows of the dataset
head(mydata)

# Check the distribution of the target variable
table(mydata$survived)
table(mydata$Gender)

# Balancing the dataset using ROSE without specifying a seed
mydata$Gender <- as.factor(mydata$Gender)

# Balancing the dataset using ROSE
balanced_data <- ROSE(Gender ~ ., data = mydata)$mydata


table(mydata$Gender)

str(mydata)

mydata$survived <- as.numeric(mydata$survived)
balanced_data <- ROSE(survived ~ ., data = mydata)$mydata

balanced_data <- ROSE(survived ~ ., data = data)$mydata
mydata
table(mydata$survived)




#chatgpt
# Convert necessary columns to factors
mydata$Gender <- as.factor(mydata$Gender)
mydata$embarked <- as.factor(mydata$embarked)
mydata$class <- as.factor(mydata$class)
mydata$who <- as.factor(mydata$who)
mydata$alone <- as.factor(mydata$alone)

# Check the structure of your dataset
str(mydata)

# Apply the ROSE function to balance the dataset
balanced_data <- ROSE(Gender ~ ., data = mydata)$data

# Check the class distribution of the balanced data
table(balanced_data$Gender)

#19.graph
#histogram
numerical_columns <- sapply(mydata, as.numeric)
for (col in names(mydata)[numerical_columns]) {
  hist(mydata[[col]], main = paste(col, "Distribution"), xlab = col, col = "red",
       border = "black", breaks =20, col.axis = "darkblue", density = 10)
}

#boxplot
numerical_columns <- sapply(mydata, is.numeric)
for (col in names(mydata)[numerical_columns]) {
  boxplot(mydata[[col]], main = paste(col, "Box Plot"), ylab = col, col = "skyblue",
          border = "red", notch = FALSE, horizontal = TRUE)
}

#barplot
#age
mydata$age_group <- cut(mydata$age, breaks = c(0, 18, 30,50, Inf),
                        labels = c("1-18", "19-30","31-50", "50+"), include.lowest = TRUE)
age_plot <- ggplot(mydata, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Age Distribution",
       x = "Age Group",
       y = "Count") +
  scale_fill_manual(values = c("1-18" = "yellow", "19-30" = "lightblue","31-50" = "green", "50+" = "red")) +
  scale_x_discrete(labels = c("1-18" = "1-18", "19-30" = "19-30","31-50" = "31-50", "50+"= "50+"))
print(age_plot)


# suvived
mydata$survived_group <- cut(mydata$survived, breaks = c(-Inf, 0.5, Inf),
                             labels = c("0", "1"))

 
survived_plot <- ggplot(mydata, aes(x = survived_group, fill = survived_group)) +
  geom_bar() +
  labs(title = "Survived Distribution",
       x = "Survived Group",
       y = "Count") +
  scale_fill_manual(values = c("0" = "green", "1" = "red")) +
  scale_x_discrete(labels = c("0" = "0", "1" = "1"))

print(survived_plot)

#Gender
sex_plot <- ggplot(mydata, aes(x = factor(Gender), fill = factor(Gender))) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink")) +
  scale_x_discrete(labels = c("male" = "male", "female" = "female"))

print(sex_plot)

#Embarked
embarked_plot <- ggplot(mydata, aes(x = factor(embarked), fill = factor(embarked))) +
  geom_bar() +
  labs(title = "Distribution by Embarked",
       x = "Boarding Place",
       y = "Count") +
  scale_fill_manual(values = c("C" = "red", "Q" = "green","S" = "blue")) +
  scale_x_discrete(labels = c("C" = "C", "Q" = "Q","S" = "S"))

print(embarked_plot)


#class
class_plot <- ggplot(mydata, aes(x = factor(class), fill = factor(class))) +
  geom_bar() +
  labs(title = "Distribution by class",
       x = "Class",
       y = "Count") +
  scale_fill_manual(values = c("First" = "purple", "Second" = "lightblue","Third" = "pink3")) +
  scale_x_discrete(labels = c("First" = "First", "Second" = "Second","Third" = "Third"))

print(class_plot)

#alone
alone_plot <- ggplot(mydata, aes(x = factor(alone), fill = factor(alone))) +
  geom_bar() +
  labs(title = "Distribution of alone",
       x = "alone or not alone",
       y = "Count") +
  scale_fill_manual(values = c("TRUE" = "green4", "FALSE" = "red4")) +
  scale_x_discrete(labels = c("TRUE" = "ALONE", "FALSE" = "NOT ALONE"))

print(alone_plot)


#20. mean, median and mode on graph
selected_columns <- c("age", "sibsp", "parch", "survived")
selected_data <- mydata[selected_columns]
means <- sapply(selected_data, mean, na.rm = TRUE)
medians <- sapply(selected_data, median, na.rm = TRUE)
mode_func <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
modes <- sapply(selected_data, mode_func)
summary_data <- data.frame(Columns = names(selected_data),Mean = means,Median = medians,Mode = modes)
print(summary_data)
barplot(t(summary_data[, -1]), beside = TRUE, main = "Mean, Median, and Mode for numeric columns",xlab = "Columns", ylab = "Values",col = c("red", "green", "blue"),legend.text = TRUE)
legend("topright", legend = colnames(summary_data)[-1], fill = c("red", "green", "blue"))

#21. working with fare attribute
str(mydata$fare)
mydata$fare <- as.numeric(mydata$fare)

#21.1  Calculate mean fare
mean_fare <- as.integer(mean(mydata$fare, na.rm = TRUE))
print(mean_fare)  

# Replace missing values with mean 
mydata <- mydata %>%
  mutate(fare = ifelse(is.na(fare), mean_fare, fare))
mydata

#21.2calculate median age
median_fare <- as.integer(median(mydata$fare, na.rm = TRUE))
print(median_fare) 

# Replace missing values with median
mydata <- mydata %>%
  mutate(fare = ifelse(is.na(fare), median_fare, fare))
mydata

#round, ceiling, floor function on fare
round(mydata$fare)
ceiling(mydata$fare)
floor(mydata$fare)

#22.solving the problem of who attribute
# Fix mismatches between 'Gender' and 'who'
mydata <- mydata %>%
  mutate(who = case_when(
    Gender == "male" & who != "child" ~ "man",
    Gender == "female" & who != "child" ~ "woman",
    TRUE ~ who
  ))

# Verify the changes
table(mydata$Gender, mydata$who)





#23.Imbalance to balance dataset(3rd trial)

#23.1 Survived(balancing)

# Check the distribution of the 'survived' column
survived_distribution <- mydata %>% count(survived)
print(survived_distribution)

# Determine majority and minority classes
majority_survived_label <- survived_distribution %>% filter(n == max(n)) %>% pull(survived)
minority_survived_label <- survived_distribution %>% filter(n == min(n)) %>% pull(survived)

print(paste("Majority class:", majority_survived_label))
print(paste("Minority class:", minority_survived_label))
 
# Separate the majority and minority classes
majority_survived <- mydata %>% filter(survived == majority_survived_label)
minority_survived <- mydata %>% filter(survived == minority_survived_label)

# Undersample the majority class
set.seed(123)
undersampled_majority <- majority_survived %>% sample_n(nrow(minority_survived))

# Combine the undersampled majority class with the minority class
balanced_dataset <- bind_rows(undersampled_majority, minority_survived)
balanced_dataset

# Check the distribution of the balanced dataset
balanced_distribution <- balanced_dataset %>%
  count(survived) %>%
  mutate(percentage = n / sum(n) * 100)

print(balanced_distribution)


#23.2 Gender(balancing)

# Check the distribution of the 'survived' column
Gender_distribution <- mydata %>% count(Gender)
print(Gender_distribution)

# Determine majority and minority classes
majority_Gender_label <- Gender_distribution %>% filter(n == max(n)) %>% pull(Gender)
minority_Gender_label <- Gender_distribution %>% filter(n == min(n)) %>% pull(Gender)

print(paste("Majority class:", majority_Gender_label))
print(paste("Minority class:", minority_Gender_label))

# Separate the majority and minority classes
majority_Gender <- mydata %>% filter(Gender == majority_Gender_label)
minority_Gender <- mydata %>% filter(Gender == minority_Gender_label)

# Undersample the majority class
set.seed(123)
undersampled_majority <- majority_Gender %>% sample_n(nrow(minority_Gender))

# Combine the undersampled majority class with the minority class
balanced_dataset <- bind_rows(undersampled_majority, minority_Gender)
balanced_dataset

# Check the distribution of the balanced dataset
balanced_distribution <- balanced_dataset %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n) * 100)

print(balanced_distribution)


#23.3 Alone(balancing)

# Check the distribution of the 'survived' column
alone_distribution <- mydata %>% count(alone)
print(alone_distribution)

# Determine majority and minority classes
majority_alone_label <- alone_distribution %>% filter(n == max(n)) %>% pull(alone)
minority_alone_label <- alone_distribution %>% filter(n == min(n)) %>% pull(alone)

print(paste("Majority class:", majority_alone_label))
print(paste("Minority class:", minority_alone_label))

# Separate the majority and minority classes
majority_alone <- mydata %>% filter(alone == majority_alone_label)
minority_alone <- mydata %>% filter(alone == minority_alone_label)

# Undersample the majority class
set.seed(123)
undersampled_majority <- majority_alone %>% sample_n(nrow(minority_alone))

# Combine the undersampled majority class with the minority class
balanced_dataset <- bind_rows(undersampled_majority, minority_alone)
balanced_dataset

# Check the distribution of the balanced dataset
balanced_distribution <- balanced_dataset %>%
  count(alone) %>%
  mutate(percentage = n / sum(n) * 100)

print(balanced_distribution)






















 

#measure of central tendency and spread
summary(mydata[vars])

#to find the verience
varience<-var(mydata$sibsp)# there are missing values
varience

#to find the standard deviation
s<-mydata$age
sd(s)