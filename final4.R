setwd("C://Users//Parijat//Documents//R")
data <- read.csv("cleaned_test.csv")
library(datasets)
head(data)
install.packages("GGally") 
install.packages("tidyverse")
library(GGally)
library(tidyverse)
data_subset <- data[c("Result", "Time", "Age")]
data_subset$Result <- as.factor(data_subset$Result)
ggpairs(data_subset,aes(color = Result, alpha = 0.6),title = "Pairwise Plot: Result, Time, and Age")
install.packages("dplyr")
library("dplyr")
data$Age_group <- dplyr::case_when(
  data$Age %in% c("0-20", "21-30") ~ "<30",
  data$Age %in% c("31-45", "46-60") ~ "30-60",
  data$Age %in% c("60-70", "70-100") ~ ">60",
  TRUE ~ "Unknown"
)
data$Age_group <- relevel(factor(data$Age_group), ref = "30-60")
data$Time <- relevel(factor(data$Time), ref = "morning")
data <- data %>%
  mutate(Age_group = case_when(
    Age %in% c("0-20", "21-30") ~ "<30",
    Age %in% c("31-45", "46-60") ~ "30-60",
    Age %in% c("60-70", "70-100") ~ ">60",
    TRUE ~ "Unknown"
  ))
data$Result_bin <- ifelse(data$Result == "positive", 1, 0)
table(data$Result_bin)
model_clean <- glm(Result_bin ~ Age_group + Time, family = "binomial", data = data)
data$Age_group <- relevel(factor(data$Age_group), ref = "30-60")
summary(model_clean)
clean_data <- data[complete.cases(data[c("Result_bin", "Age_group", "Time")]), ]
full_model <- glm(Result_bin ~ Age_group + Time, family = "binomial", data = clean_data)
null_model <- glm(Result_bin ~ 1, family = "binomial", data = clean_data)
anova(null_model, full_model, test = "Chisq")

