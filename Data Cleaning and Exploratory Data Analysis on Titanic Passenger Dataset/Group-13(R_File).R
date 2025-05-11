install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(tidyr)






mydata<-read.csv("D:/R Programming/Midterm Project/Mid_Dataset.csv",header=TRUE,sep=",")
mydata



mydata$Gender <- ifelse(mydata$Gender == "", NA, mydata$Gender)
mydata$fare <- ifelse(mydata$fare == "", NA, mydata$fare)
mydata$embarked <- ifelse(mydata$embarked == "", NA, mydata$embarked)
mydata$class <- ifelse(mydata$class == "", NA, mydata$class)
mydata$who <- ifelse(mydata$who == "", NA, mydata$who)
mydata

 


missing_values <- sapply(mydata, function(x) sum(is.na(x)))
missing_values_df <- data.frame(Variable = names(missing_values), Missing = missing_values)

missing_values_plot <- ggplot(missing_values_df, aes(x = reorder(Variable, -Missing), y = Missing)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Missing Values in Dataset",
       x = "Variables",
       y = "Number of Missing Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(missing_values_plot)




summary(mydata)







mydata[mydata == ""] <- NA
is.na(mydata)




colSums(is.na(mydata))




which(is.na(mydata$Gender))
which(is.na(mydata$age))
which(is.na(mydata$sibsp))
which(is.na(mydata$parch))
which(is.na(mydata$fare))
which(is.na(mydata$embarked))
which(is.na(mydata$class))
which(is.na(mydata$who))
which(is.na(mydata$alone))




str(mydata)




remove<-na.omit(mydata)
remove




mean_age <- as.integer(mean(mydata$age, na.rm = TRUE))
print(mean_age)  

mydata <- mydata %>%
  mutate(age = ifelse(is.na(age), mean_age, age))
mydata





median_age <- as.integer(median(mydata$age, na.rm = TRUE))
print(median_age) 
 
mydata <- mydata %>%
  mutate(age = ifelse(is.na(age), median_age, age))
mydata




column_name <- "sibsp"
mode_value <- as.numeric(names(which.max(table(mydata[[column_name]]))))
print(mode_value)
column_name <- "sibsp"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$sibsp ))
mydata




column_name <- "parch"
mode_value <- as.numeric(names(which.max(table(mydata[[column_name]]))))
print(mode_value)
column_name <- "parch"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$parch ))
mydata




column_name <- "alone"
mode_value <- as.logical(names(which.max(table(mydata[[column_name]]))))
print(mode_value)
column_name <- "alone"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$alone ))
mydata




column_name <- "Gender"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "Gender"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$Gender ))
mydata




column_name <- "class"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "class"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$class ))
mydata




column_name <- "embarked"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "embarked"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$embarked ))
mydata




column_name <- "who"
mode_value <- as.character(names(which.max(table(mydata[[column_name]], useNA = "no"))))
print(mode_value)
column_name <- "who"
replacement_value <- mode_value
mydata[[column_name]][is.na(mydata[[column_name]])] <- replacement_value
which(is.na(mydata$who ))
mydata





numerical_columns <- sapply(mydata, is.numeric)
standard_deviations <- numeric(length = sum(numerical_columns))
names(standard_deviations) <- names(mydata)[numerical_columns]
for (col in names(mydata)[numerical_columns]) {
  standard_deviations[col] <- sd(mydata[[col]], na.rm = TRUE)
}
print(standard_deviations)




Q1 <- quantile(mydata$age[mydata$age >= 40 & mydata$age <= 95], 0.25)
Q3 <- quantile(mydata$age[mydata$age >= 40 & mydata$age <= 95], 0.75)
IQR_value <- Q3 - Q1
threshold <- 1.5
outlier_condition <- (mydata$age < (Q1 - threshold * IQR_value)) | (mydata$age > (Q3 + threshold * IQR_value))
mydata <- mydata[!outlier_condition, ]
mydata




mydata <- mydata %>%
  mutate(fare = as.numeric(as.character(fare))) %>%  
  filter(!is.na(fare))  
mydata



mydata <- mydata %>%
  mutate(row_number = row_number()) %>%
  filter(who %in% c("man", "woman", "child"))
mydata




mydata <- mydata %>%
  mutate(
    Gender = factor(Gender, levels = c("male", "female"), labels = c(1, 2)),
    embarked   = factor(embarked, levels = c("C","Q","S"), labels = c(1,2,3)),
    class    = factor(class , levels = c("First", "Second", "Third"), labels = c(1, 2, 3)),
    who = factor(who, levels = c("man", "woman","child"), labels = c(1, 2,3)),
    alone = factor(alone, levels = c("TRUE", "FALSE"), labels = c(1, 2))
  )
mydata




mydata <- mydata %>%
  mutate(
    survived = factor(survived, levels = c(0, 1), labels = c("Dead", "Alive")),
    
  )
mydata




mydata <- mydata %>%
  mutate(age_category = case_when(
    age < 18 ~ "child",
    age >= 18 & age <= 30 ~ "young",
    age > 30 & age <= 50 ~ "middle_aged",
    age > 50 ~ "old"
  ))
mydata




column_name <- "age"
column <- mydata[[column_name]]
min_value <- min(column)
max_value <- max(column)
normalized_column <- (column - min_value) / (max_value - min_value)
mydata[[column_name]] <- normalized_column
mydata$age
mydata







duplicates_age<-distinct(mydata,age,.keep_all = TRUE)
duplicates_age



duplicates_fare<-distinct(mydata,fare,.keep_all = TRUE)
duplicates_fare





data<-filter(mydata,Gender=="female")
data

data<-filter(mydata,age > 15 & age < 45)
data

data<-filter(mydata,sibsp >1 )
data

data<-filter(mydata,parch >1 )
data


data<-filter(mydata,embarked=="Q")
data

data<-filter(mydata,who=="child")
data

data<-filter(mydata,class=="Third")
data









numerical_columns <- sapply(mydata, is.numeric)
for (col in names(mydata)[numerical_columns]) {
  boxplot(mydata[[col]], main = paste(col, "Box Plot"), ylab = col, col = "skyblue",
          border = "red", notch = FALSE, horizontal = TRUE)
}





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




sex_plot <- ggplot(mydata, aes(x = factor(Gender), fill = factor(Gender))) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink")) +
  scale_x_discrete(labels = c("male" = "male", "female" = "female"))

print(sex_plot)




embarked_plot <- ggplot(mydata, aes(x = factor(embarked), fill = factor(embarked))) +
  geom_bar() +
  labs(title = "Distribution by Embarked",
       x = "Boarding Place",
       y = "Count") +
  scale_fill_manual(values = c("C" = "red", "Q" = "green","S" = "blue")) +
  scale_x_discrete(labels = c("C" = "C", "Q" = "Q","S" = "S"))

print(embarked_plot)





class_plot <- ggplot(mydata, aes(x = factor(class), fill = factor(class))) +
  geom_bar() +
  labs(title = "Distribution by class",
       x = "Class",
       y = "Count") +
  scale_fill_manual(values = c("First" = "purple", "Second" = "lightblue","Third" = "pink3")) +
  scale_x_discrete(labels = c("First" = "First", "Second" = "Second","Third" = "Third"))

print(class_plot)




alone_plot <- ggplot(mydata, aes(x = factor(alone), fill = factor(alone))) +
  geom_bar() +
  labs(title = "Distribution of alone",
       x = "alone or not alone",
       y = "Count") +
  scale_fill_manual(values = c("TRUE" = "green4", "FALSE" = "red4")) +
  scale_x_discrete(labels = c("TRUE" = "ALONE", "FALSE" = "NOT ALONE"))

print(alone_plot)





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





str(mydata$fare)
mydata$fare <- as.numeric(mydata$fare)

mean_fare <- as.integer(mean(mydata$fare, na.rm = TRUE))
print(mean_fare)  

mydata <- mydata %>%
  mutate(fare = ifelse(is.na(fare), mean_fare, fare))
mydata





round(mydata$fare)
ceiling(mydata$fare)
floor(mydata$fare)





mydata <- mydata %>%
  mutate(who = case_when(
    Gender == "male" & who != "child" ~ "man",
    Gender == "female" & who != "child" ~ "woman",
    TRUE ~ who
  ))
mydata












survived_distribution <- mydata %>% count(survived)
print(survived_distribution)

majority_survived_label <- survived_distribution %>% filter(n == max(n)) %>% pull(survived)
minority_survived_label <- survived_distribution %>% filter(n == min(n)) %>% pull(survived)

print(paste("Majority class:", majority_survived_label))
print(paste("Minority class:", minority_survived_label))

majority_survived <- mydata %>% filter(survived == majority_survived_label)
minority_survived <- mydata %>% filter(survived == minority_survived_label)

set.seed(123)
undersampled_majority <- majority_survived %>% sample_n(nrow(minority_survived))

balanced_dataset <- bind_rows(undersampled_majority, minority_survived)
balanced_dataset

balanced_distribution <- balanced_dataset %>%
  count(survived) %>%
  mutate(percentage = n / sum(n) * 100)

print(balanced_distribution)







Gender_distribution <- mydata %>% count(Gender)
print(Gender_distribution)


majority_Gender_label <- Gender_distribution %>% filter(n == max(n)) %>% pull(Gender)
minority_Gender_label <- Gender_distribution %>% filter(n == min(n)) %>% pull(Gender)

print(paste("Majority class:", majority_Gender_label))
print(paste("Minority class:", minority_Gender_label))

majority_Gender <- mydata %>% filter(Gender == majority_Gender_label)
minority_Gender <- mydata %>% filter(Gender == minority_Gender_label)

set.seed(123)
undersampled_majority <- majority_Gender %>% sample_n(nrow(minority_Gender))

balanced_dataset <- bind_rows(undersampled_majority, minority_Gender)
balanced_dataset

balanced_distribution <- balanced_dataset %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n) * 100)

print(balanced_distribution)







alone_distribution <- mydata %>% count(alone)
print(alone_distribution)

majority_alone_label <- alone_distribution %>% filter(n == max(n)) %>% pull(alone)
minority_alone_label <- alone_distribution %>% filter(n == min(n)) %>% pull(alone)

print(paste("Majority class:", majority_alone_label))
print(paste("Minority class:", minority_alone_label))

majority_alone <- mydata %>% filter(alone == majority_alone_label)
minority_alone <- mydata %>% filter(alone == minority_alone_label)

set.seed(123)
undersampled_majority <- majority_alone %>% sample_n(nrow(minority_alone))

balanced_dataset <- bind_rows(undersampled_majority, minority_alone)
balanced_dataset

balanced_distribution <- balanced_dataset %>%
  count(alone) %>%
  mutate(percentage = n / sum(n) * 100)

print(balanced_distribution)






















 





