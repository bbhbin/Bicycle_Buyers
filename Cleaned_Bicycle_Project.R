# Loading packages

library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)

# Import bicycle_buyers.csv

getwd()
Bicycle <- read_csv("bicycle_buyers.csv")

# Review dataset

View(Bicycle)
colnames(Bicycle)
head(Bicycle)

# Clean empty values

Cleaned_Bicycle_1  <- drop_na(Bicycle)

# Review dataset again

View(Cleaned_Bicycle_1)

# Rename "North America" in Regions column to "Americas"

Cleaned_Bicycle_1$Region <- gsub("North America" , "Americas" , Cleaned_Bicycle_1$Region)

# Add an `Income Level` column for easier comparison.

Cleaned_Bicycle_1$Income_level <- NA
Cleaned_Bicycle_1$Income_level[Cleaned_Bicycle_1$Income<=49999] <- "Lower"
Cleaned_Bicycle_1$Income_level[Cleaned_Bicycle_1$Income>=50000] <- "Middle"
Cleaned_Bicycle_1$Income_level[Cleaned_Bicycle_1$Income>=100000] <- "Upper"

# Change `Income_Level` column to `Income Level` to keep naming consistency

Cleaned_Bicycle_1 <- Cleaned_Bicycle_1 %>% rename("Income Level" = Income_level)

# Order `Commute Distance`, `Education`, and `Purchased Bike` Columns to make charts look nicer

Cleaned_Bicycle_1$`Commute Distance` <- factor(Cleaned_Bicycle_1$`Commute Distance`, levels = c("0-1 Miles", "1-2 Miles","2-5 Miles", "5-10 Miles", "10+ Miles"))

Cleaned_Bicycle_1$Education <- factor(Cleaned_Bicycle_1$Education, levels = c("Partial High School", "High School","Partial College", "Bachelors", "Graduate Degree"))

Cleaned_Bicycle_1$`Purchased Bike` <- factor(Cleaned_Bicycle_1$`Purchased Bike`, levels = c("Yes", "No"))


# Make final cleaned table

Cleaned_Bicycle <- Cleaned_Bicycle_1

View(Cleaned_Bicycle)
head(Cleaned_Bicycle)

# Assign colors to 'Yes' and 'No' option on `Purchased Bike` column

yesno_theme <- scale_fill_manual(values=c("#93CE7D", "#db3a34"))

# Analyze demographics of people purchasing bike

# 1. Graph for amount of bikes purchased vs not purchased

ggplot(Cleaned_Bicycle) + geom_bar(mapping=aes(x=`Purchased Bike`, fill=`Purchased Bike`)) + ylab("Amount") +
  yesno_theme

# 1.1 Save graph for amount of bikes purchased vs not purchased
#ggsave("1_Purchased.JPG")

# 2. Graph for Regions where bikes are purchased

ggplot(Cleaned_Bicycle) + geom_bar(mapping=aes(x=Region, fill=`Purchased Bike`)) + ylab("Amount") + 
  theme_light() + yesno_theme

# 2.1 Save graph for regions where bikes are purchased
#ggsave("2_Regions.JPG")

# 3. Graph for commute distance versus amount of cars owned

ggplot(Cleaned_Bicycle) + geom_bar(mapping=aes(x=Cars, fill=`Purchased Bike`)) + 
  facet_wrap(~`Commute Distance`) + ylab("Amount") + xlab("Amount of Car(s) Owned") +
  theme_light() + yesno_theme

# 3.1 Save graph commute distance versus amount of cars owned
#ggsave("3_Commute_vs_Cars.JPG")

# 4. Graph for income level

ggplot(Cleaned_Bicycle) + geom_bar(mapping=aes(x=`Income Level`, fill=`Purchased Bike`)) + 
  facet_wrap(~Occupation) + ylab("Amount") + theme_light() + yesno_theme

# 4.1 Save graph for income level versus job type
#ggsave("4_Income_Level_vs_Job.JPG")

# 5. Graph for gender versus marital status based by percentage

ggplot(Cleaned_Bicycle, aes(x = factor(Gender), fill = factor(`Purchased Bike`))) +
  geom_bar(position="fill") + facet_wrap(~`Marital Status`) + xlab("Gender") + ylab("Percentage") + 
  labs(fill="Purchased Bike") + theme_bw() + yesno_theme

# 5.1 Save graph for gender versus marital status based by percentage
#ggsave("5_Gender_vs_Marital.JPG")

# 6. Graph for education level

ggplot(Cleaned_Bicycle, aes(x = factor(`Education`), fill = factor(`Purchased Bike`))) +
  geom_bar(position="fill") + xlab("Education") + ylab("Percentage") + labs(fill="Purchased Bike") +
  theme_bw() + yesno_theme

# 6.1 Save graph for education level
#ggsave("6_Education_Level.JPG")

# 7. Graph for age level

ggplot(Cleaned_Bicycle) + geom_jitter(mapping=aes(x=Age, y=Income, color=`Purchased Bike`)) +
  theme_gray() + scale_color_manual(values=c("#93CE7D", "#db3a34"))

# 7.1 Save graph for age level
#ggsave("7_Age_Level.JPG")

# Save Cleaned_bicycle as a .csv
#write_csv(Cleaned_Bicycle, "Cleaned_Bicycle.csv")
