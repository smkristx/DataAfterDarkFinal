# Load libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(PerformanceAnalytics)
library(corrplot)
library(readr)
library(tidyr)
library(reshape2)

#MASTERdata <- read_csv("C:/Users/smkri/Documents/ENTITYAcademy/FINALPROJECT/DataWrangling/MASTERdata1.csv")
MASTERdata = MASTERdata1

#Data Wrangling - columns of National data for analysis
National2019 <- MASTERdata[1:53, 1:34]
National2020 <- MASTERdata[54:106, 1:34]
National2021 <- MASTERdata[107:159, 1:34]

#Data Wrangling - columns of Texas data for analysis
Texas2019 <- MASTERdata[45:45, 1:34]
Texas2020 <- MASTERdata[98:98, 1:34]
Texas2021 <- MASTERdata[151:151, 1:34]

#Data Wrangling - columns of National Feline data for analysis
NationalCatIntakes19 <- MASTERdata[1:53, 3:7]
NationalCatIntakes20 <- MASTERdata[54:106, 3:7]
NationalCatIntakes21 <- MASTERdata[107:159, 3:7]
NationalCatEuth19 <- MASTERdata[1:53, 16]
NationalCatEuth19
NationalCatEuth20 <- MASTERdata[54:106, 16]
NationalCatEuth21 <- MASTERdata[107:159, 16]

#Data Wrangling - columns of National Canine data for analysis
NationalDogIntakes19 <- MASTERdata[1:53, 17:26]
NationalDogIntakes20 <- MASTERdata[54:106, 17:26]
NationalDogIntakes21 <- MASTERdata[107:159, 17:26]
NationalDogEuth19 <- MASTERdata[1:53, 30:31]
NationalDogEuth19
NationalDogEuth20 <- MASTERdata[54:106, 30:31]
NationalDogEuth21 <- MASTERdata[107:159, 30:31]

#National Feline Intake stats 2019
NFI2019min <- min(National2019$`Feline Total Intakes`)
NFI2019max <- max(National2019$`Feline Total Intakes`)
NFI2019sum <- sum(National2019$`Feline Total Intakes`)
NFI2019range <- range(NationalCatIntakes19)
NFI2019mean <- mean(National2019$`Feline Total Intakes`)
NFI2019sd <- sd(National2019$`Feline Total Intakes`)

print(paste("National Feline Intake 2019 - Range: ", NFI2019range))
print(paste("National Feline Intake 2019 - Sum: ", NFI2019sum))
print(paste("National Feline Intake 2019 - Mean: ", NFI2019mean))
print(paste("National Feline Intake 2019 - SD: ", NFI2019sd))

#National Feline Euthanasia stats 2019
NFE2019min <- min(National2019$`Feline Total Euth`)
NFE2019max <- max(National2019$`Feline Total Euth`)
NFE2019range <- range(NationalCatEuth19)
NFE2019sum <- sum(National2019$`Feline Total Euth`)
NFE2019sd <- sd(National2019$`Feline Total Euth`)
NFE2019mean <- mean(National2019$`Feline Total Euth`)

print(paste("National Feline Euthanasia 2019 - Range: ", NFE2019range))
print(paste("National Feline Euthanasia 2019 - Sum: ", NFE2019sum))
print(paste("National Feline Euthanasia 2019 - Mean: ", NFE2019mean))
print(paste("National Feline Euthanasia 2019 - SD: ", NFE2019sd))

#National Feline Intake stats 2020
NFI2020min <- min(National2020$`Feline Total Intakes`)
NFI2020max <- max(National2020$`Feline Total Intakes`)
NFI2020range <- range(NationalCatIntakes20)
NFI2020sum <- sum(National2020$`Feline Total Intakes`)
NFI2020mean <- mean(National2020$`Feline Total Intakes`)
NFI2020sd <- sd(National2020$`Feline Total Intakes`)

print(paste("National Feline Euthanasia 2020 - Range: ", NFE2020range))
print(paste("National Feline Intake 2020 - Sum: ", NFI2020sum))
print(paste("National Feline Intake 2020 - Mean: ", NFI2020mean))
print(paste("National Feline Intake 2020 - SD: ", NFI2020sd))

#National Feline Euthanasia stats for 2020
NFE2020min <- min(National2020$`Feline Total Euth`)
NFE2020max <- max(National2020$`Feline Total Euth`)
NFE2020range <- range(NationalCatEuth20)
NFE2020sum <- sum(National2020$`Feline Total Euth`)
NFE2020mean <- mean(National2020$`Feline Total Euth`)
NFE2020sd <- sd(National2020$`Feline Total Euth`)

print(paste("National Feline Euthanasia 2020 - Range: ", NFE2020range))
print(paste("National Feline Euthanasia 2020 - Sum: ", NFE2020sum))
print(paste("National Feline Euthanasia 2020 - Mean: ", NFE2020mean))
print(paste("National Feline Euthanasia 2020 - SD: ", NFE2020sd))

#National Feline Intake stats 2021
NFI2021min <- min(National2021$`Feline Total Intakes`)
NFI2021max <- max(National2021$`Feline Total Intakes`)
NFI2021range <- range(NationalCatIntakes21)
NFI2021sum <- sum(National2021$`Feline Total Intakes`)
NFI2021mean <- mean(National2021$`Feline Total Intakes`)
NFI2021sd <- sd(National2021$`Feline Total Intakes`)

print(paste("National Feline Intake 2021 - Range: ", NFI2021range))
print(paste("National Feline Intake 2021 - Sum: ", NFI2021sum))
print(paste("National Feline Intake 2021 - Mean: ", NFI2021mean))
print(paste("National Feline Intake 2021 - SD: ", NFI2021sd))

#National Feline Euthanasia stats 2021
NFE2021min <- min(National2021$`Feline Total Euth`)
NFE2021max <- max(National2021$`Feline Total Euth`)
NFE2021range <- range(National2021$`Feline Total Euth`)
NFE2021sum <- sum(National2021$`Feline Total Euth`)
NFE2021mean <- mean(National2021$`Feline Total Euth`)
NFE2021sd <- sd(National2021$`Feline Total Euth`)

print(paste("National Feline Euthanasia 2021 - Range: ", NFE2021range))
print(paste("National Feline Euthanasia 2021 - Sum: ", NFE2021sum))
print(paste("National Feline Euthanasia 2021 - Mean: ", NFE2021mean))
print(paste("National Feline Euthanasia 2021 - SD: ", NFE2021sd))

#National Canine Intake stats 2019
NCI2019min <- min(National2019$`Canine Total Intake`)
NCI2019max <- max(National2019$`Canine Total Intake`)
NCI2019sum <- sum(National2019$`Canine Total Intake`)
NCI2019mean <- mean(National2019$`Canine Total Intake`)
NCI2019sd <- sd(National2019$`Canine Total Intake`)

print(paste("National Canine Intake 2019 - Range: ", NCI2019min, NCI2019max))
print(paste("National Canine Intake 2019 - Sum: ", NCI2019sum))
print(paste("National Canine Intake 2019 - Mean: ", NCI2019mean))
print(paste("National Canine Intake 2019 - SD: ", NCI2019sd))

#National Canine Euthanasia stats 2019
NCE2019min <- min(National2019$`Canine Total Euth`)
NCE2019max <- max(National2019$`Canine Total Euth`)
NCE2019sum <- sum(National2019$`Canine Total Euth`)
NCE2019mean <- mean(National2019$`Canine Total Euth`)
NCE2019sd <- sd(National2019$`Canine Total Euth`)

print(paste("National Canine Euthanasia 2019 - Range: ", NCE2019min, NCE2019max))
print(paste("National Canine Euthanasia 2019 - Sum: ", NCE2019sum))
print(paste("National Canine Euthanasia 2019 - Mean: ", NCE2019mean))
print(paste("National Canine Euthanasia 2019 - SD: ", NCE2019sd))

#National Canine Intake stats 2020
NCI2020min <- min(National2020$`Canine Total Intake`)
NCI2020max <- max(National2020$`Canine Total Intake`)
NCI2020sum <- sum(National2020$`Canine Total Intake`)
NCI2020mean <- mean(National2020$`Canine Total Intake`)
NCI2020sd <- sd(National2020$`Canine Total Intake`)

print(paste("National Canine Intake 2020 - Range: ", NCI2020min, NCI2020max))
print(paste("National Canine Intake 2020 - Sum: ", NCI2020sum))
print(paste("National Canine Intake 2020 - Mean: ", NCI2020mean))
print(paste("National Canine Intake 2020 - SD: ", NCI2020sd))

#National Canine Euthanasia stats 2020
NCE2020min <- min(National2020$`Canine Total Euth`)
NCE2020max <- max(National2020$`Canine Total Euth`)
NCE2020sum <- sum(National2020$`Canine Total Euth`)
NCE2020mean <- mean(National2020$`Canine Total Euth`)
NCE2020sd <- sd(National2020$`Canine Total Euth`)

print(paste("National Canine Euthanasia 2020 - Range: ", NCE2020min, NCE2020max))
print(paste("National Canine Euthanasia 2020 - Sum: ", NCE2020sum))
print(paste("National Canine Euthanasia 2020 - Mean: ", NCE2020mean))
print(paste("National Canine Euthanasia 2020 - SD: ", NCE2020sd))

#National Canine Intake stats 2021
NCI2021min <- min(National2021$`Canine Total Intake`)
NCI2021max <- max(National2021$`Canine Total Intake`)
NCI2021range <- range(National2021$'Canine Total Intake')
NCI2021sum <- sum(National2021$`Canine Total Intake`)
NCI2021mean <- mean(National2021$`Canine Total Intake`)
NCI2021sd <- sd(National2021$`Canine Total Intake`)

print(paste("National Canine Intake 2021 - Range: ", NCI2021range))
print(paste("National Canine Intake 2021 - Sum: ", NCI2021sum))
print(paste("National Canine Intake 2021 - Mean: ", NCI2021mean))
print(paste("National Canine Intake 2021 - SD: ", NCI2021sd))

#National Canine Euthanasia stats 2021
NCE2021min <- min(National2021$`Canine Total Euth`)
NCE2021max <- max(National2021$`Canine Total Euth`)
NCE2021range <- range(National2021$`Other Outcome - Shelter Euthanasia Total-Canine`)
NCE2021sum <- sum(National2021$`Canine Total Euth`)
NCE2021mean <- mean(National2021$`Canine Total Euth`)
NCE2021sd <- sd(National2021$`Canine Total Euth`)

print(paste("National Canine Euthanasia 2021 - Range: ", NCE2021range))
print(paste("National Canine Euthanasia 2021 - Sum: ", NCE2021sum))
print(paste("National Canine Euthanasia 2021 - Mean: ", NCE2021mean))
print(paste("National Canine Euthanasia 2021 - SD: ", NCE2021sd))

#t-tests - T-tests on Intake and Euthanasia for 2019 - 2021
NFIvE2019ttest <- t.test(National2019$`Feline Total Intakes`, National2019$`Feline Total Euth`, paired = TRUE)
print(NFIvE2019ttest)

NFIvE2020ttest <- t.test(National2020$`Feline Total Intakes`, National2020$`Feline Total Euth`, paired = TRUE)
print(NFIvE2020ttest)

NFIvE2021ttest <- t.test(National2021$`Feline Total Intakes`, National2021$`Feline Total Euth`, paired = TRUE)
print(NFIvE2021ttest)

NFIvE2019ttest <- t.test(National2019$`Canine Total Intakes`, National2019$`Feline Total Euth`, paired = TRUE)
print(NFIvE2019ttest)

NFIvE2020ttest <- t.test(National2020$`Canine Total Intakes`, National2020$`Feline Total Euth`, paired = TRUE)
print(NFIvE2020ttest)

NFIvE2021ttest <- t.test(National2021$`Canine Total Intakes`, National2021$`Feline Total Euth`, paired = TRUE)
print(NFIvE2021ttest)

#There is a significant correlation between intakes and euthanasia rates at the national level for all 3 years. The mean difference was the lowest in 2020.


# National Feline Euthanasia Histograms
p <- ggplot(NationalCatEuth19, aes(x=`Other Outcome - Shelter Euthanasia Total-Feline`)) + geom_histogram(binwidth=1000, color = "black", fill="deepskyblue4") + ggtitle("Cat Euthanasia Rates for 2019")
p

p2<- ggplot(NationalCatEuth20, aes(x=`Other Outcome - Shelter Euthanasia Total-Feline`)) + geom_histogram(binwidth=1000, color = "black", fill="deepskyblue4") + ggtitle("Cat Euthanasia Rates for 2020")
p2

p3<- ggplot(NationalCatEuth21, aes(x=`Other Outcome - Shelter Euthanasia Total-Feline`)) + geom_histogram(binwidth=1000, color = "black", fill="deepskyblue4") + ggtitle("Cat Euthanasia Rates for 2021")
p3

# National Canine Euthanasia Histograms
p4 <- ggplot(NationalDogEuth19, aes(x=`Other Outcome - Shelter Euthanasia Total-Canine`)) + geom_histogram(binwidth=1000, color = "black", fill="deepskyblue4") + ggtitle("Dog Euthanasia Rates for 2019")
p4

p5 <- ggplot(NationalDogEuth20, aes(x=`Other Outcome - Shelter Euthanasia Total-Canine`)) + geom_histogram(binwidth=1000, color = "black", fill="deepskyblue4") + ggtitle("Dog Euthanasia Rates for 2020")
p5

p6<- ggplot(NationalDogEuth21, aes(x=`Other Outcome - Shelter Euthanasia Total-Canine`)) + geom_histogram(binwidth=1000, color = "black", fill="deepskyblue4") + ggtitle("Canine Euthanasia Rates for 2021")
p6

#What are the percentages of euthanasia from intakes for Texas and the nation as a whole?
TXFelEuthRate2019 <- (Texas2019$`Feline Total Euth`)/(Texas2019$`Feline Total Intakes`)
print(paste("Texas Cat Euthanasia Rate for 2019: ", TXFelEuthRate2019))

TXCanEuthRate2019 <- (Texas2019$`Canine Total Euth`)/(Texas2019$`Canine Total Intake`)
print(paste("Texas Dog Euthanasia Rate for 2019: ", TXCanEuthRate2019))

TXFelEuthRate2020 <- (Texas2020$`Feline Total Euth`)/(Texas2020$`Feline Total Intakes`)
print(paste("Texas Cat Euthanasia Rate for 2020: ", TXFelEuthRate2020))

TXCanEuthRate2020 <- (Texas2020$`Canine Total Euth`)/(Texas2020$`Canine Total Intake`)
print(paste("Texas Dog Euthanasia Rate for 2020: ", TXCanEuthRate2020))

TXFelEuthRate2021 <- (Texas2021$`Feline Total Euth`)/(Texas2021$`Feline Total Intakes`)
print(paste("Texas Cat Euthanasia Rate for 2021: ", TXFelEuthRate2021))

TXCanEuthRate2021 <- (Texas2021$`Canine Total Euth`)/(Texas2021$`Canine Total Intake`)
print(paste("Texas Dog Euthanasia Rate for 2021: ", TXCanEuthRate2021))

NatlFelEuthRate2019 <- (NFE2019sum)/(NFI2019sum)
print(paste("National Cat Euthanasia Rate for 2019: ", NatlFelEuthRate2019))

NatlCanEuthRate2019 <- (NCE2019sum)/(NCI2019sum)
print(paste("National Dog Euthanasia Rate for 2019: ", 
NatlCanEuthRate2019))

NatlFelEuthRate2020 <- (NFE2020sum)/(NFI2020sum)
print(paste("National Cat Euthanasia Rate for 2020: ", NatlFelEuthRate2020))

NatlCanEuthRate2020 <- (NCE2020sum)/(NCI2020sum)
print(paste("National Dog Euthanasia Rate for 2020: ", NatlCanEuthRate2020))

NatlFelEuthRate2021 <- (NFE2021sum)/(NFI2021sum)
print(paste("National Cat Euthanasia Rate for 2021: ", NatlFelEuthRate2021))

NatlCanEuthRate2021 <- (NCE2021sum)/(NCI2021sum)
print(paste("National Canine Euthanasia Rate for 2021: ", NatlCanEuthRate2021))
#The percentage of animal intakes that are then euthanized are slightly lower in Texas than that of the Nation as a whole.

# - make total columns - use line graph

NatlInt2019 <- NFI2019sum + NCI2019sum
NatlEuth2019 <- NFE2019sum + NCE2019sum
NatlInt2020 <- NFI2020sum + NCI2020sum
NatlEuth2020 <- NFE2020sum + NCE2020sum
NatlInt2021 <- NFI2021sum + NCI2021sum
NatlEuth2021 <- NFE2021sum + NCE2021sum
TXInt2019 <- Texas2019$`Feline Total Intakes` + Texas2019$`Canine Total Intake`
TXEuth2019 <- Texas2019$`Feline Total Euth` + Texas2019$`Canine Total Euth`
TXInt2020 <- Texas2020$`Feline Total Intakes` + Texas2020$`Canine Total Intake`
TXEuth2020 <- Texas2020$`Feline Total Euth` + Texas2020$`Canine Total Euth`
TXInt2021 <- Texas2021$`Feline Total Intakes` + Texas2021$`Canine Total Intake`
TXEuth2021 <- Texas2021$`Feline Total Euth` + Texas2021$`Canine Total Euth`

# Create the Vector for the National vs. Texas data
Year <- c(2019, 2020, 2021)
NatlInts <- c(NatlInt2019, NatlInt2020, NatlInt2021)
NatlEuths <- c(NatlEuth2019, NatlEuth2020, NatlEuth2021)
TXInts <- c(TXInt2019, TXInt2020, TXInt2021)
TXEuths <- c(TXEuth2019, TXEuth2020, TXEuth2021)

Totals <- data.frame(Year, NatlInts, NatlEuths, TXInts, TXEuths)

#Line plot to display the National data vs. Texas
Totals_plot <- ggplot(Totals, aes(x=Year)) +
  geom_line(aes(y=NatlInts, colour="National Intakes")) + 
  geom_line(aes(y=NatlEuths, colour="National Euthanasias")) +
  geom_line(aes(y=TXInts, colour="Texas Intakes")) +
  geom_line(aes(y=TXEuths, colour="Texas Euthanasias")) +
  xlab("Year") + 
  ylab("Number of Animals") + 
  ggtitle("National Intake and Euthanasia Vs Texas")
Totals_plot
