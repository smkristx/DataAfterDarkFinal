#Install Packages
install.packages("PerformanceAnalytics")
install.packages("corrplot")

#Import Libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(PerformanceAnalytics)
library(corrplot)

#Import Data

sac_data = sac_aggregate_dataset_2019_2021

#Data Wrangling

## Rename State column
colnames(sac_data)    #Get the names of every column

names(sac_data)[1] <- "State"

## Create new columns to total Intakes

sac_data$totalCatIntake <- rowSums(sac_data[, c("Intake...Relinquished.By.Owner.Total.Feline", "Intake...Stray.At.Large.Total.Feline", "Intake...Transferred.In.Total.Feline")])

sac_data$totalDogIntake <- rowSums(sac_data[, c("Intake...Relinquished.By.Owner.Total.Canine", "Intake...Stray.At.Large.Total.Canine", "Intake...Transferred.In.Total.Canine")])

sac_data$totalIntake <- rowSums(sac_data[, c("totalCatIntake", "totalDogIntake")])

sac_data$totalEuthanized <- rowSums(sac_data[, c("Other.Outcome...Shelter.Euthanasia.Total.Feline",  "Other.Outcome...Shelter.Euthanasia.Total.Canine")])

sac_data

#Linear Regression

sac_datalm <- ggplot(sac_data, aes(x = totalEuthanized, y = totalIntake ))
sac_datalm + geom_point() + geom_smooth(method=lm)

## Perform a correlation test

cor.test(sac_data$totalEuthanized, sac_data$totalIntake, method="pearson", use = "complete.obs")

## Create a correlation matrix for corr.chart in Performance Analytics Package

sac_data_quant <- sac_data[, c(31,32,33,34)]

sac_data_quant

chart.Correlation(sac_data_quant, histogram=FALSE, method="pearson")

corr_matrix <- cor(sac_data_quant)
corr_matrix

corrplot::corrplot(corr_matrix, type="upper", order="hclust", p.mat = corr_matrix, sig.level = 0.01, insig="blank")

corrplot::cor.mtest(corr_matrix)

## Linear Regression

lin_reg <- lm(totalEuthanized ~ totalIntake, sac_data)
print(lin_reg)

summary(lin_reg)

##Scatterplot with Best Fit Line

d <- ggplot(sac_data, aes(x = totalIntake, y = totalEuthanized))
d + geom_point() + geom_smooth(method=lm, se=TRUE)

## Graphing Intakes and Euthanasia

ggplot(sac_data, aes(x = factor(Year), y = totalIntake)) + geom_boxplot() + scale_y_log10()

ggplot(sac_data, aes(x = factor(Year), y = totalEuthanized)) + geom_boxplot() + scale_y_log10()

## Determining Outliers in Euthansia by Year

ggplot(sac_data, aes(x = factor(Year), y = totalEuthanized)) + geom_boxplot()

summary(totalEuthanized)

filter(sac_data, totalEuthanized > 7797.5) #Using the Interquartile Range of 3119 X 1.5 + 4678.5 to identify the outliers

sac_data_CA <- filter(sac_data, State == "CA")
View(sac_data_CA)

sac_data_TX <- filter(sac_data, State == "TX")
View(sac_data_TX)

sac_data_GA <- filter(sac_data, State == "GA")
View(sac_data_GA)

sac_data_FL <- filter(sac_data, State == "FL")
View(sac_data_FL)

sac_data_NC <- filter(sac_data, State == "NC")
View(sac_data_NC)

sac_data_SC <- filter(sac_data, State == "SC")
View(sac_data_SC)

sac_data_WA <- filter(sac_data, State == "WA")
View(sac_data_WA)

sac_data_TN <- filter(sac_data, State == "TN")
View(sac_data_TN)

sac_data_NY <- filter(sac_data, State == "NY")
View(sac_data_NY)
