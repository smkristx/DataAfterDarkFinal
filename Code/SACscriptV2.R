library(readr)
library(ggplot2)
library(tidyr)
library(reshape2)
MASTERdata <- read_csv("C:/Users/smkri/Documents/ENTITYAcademy/FINALPROJECT/DataWrangling/MASTERdata1.csv")

National2019 <- MASTERdata[1:53, 1:34]
National2020 <- MASTERdata[54:106, 1:34]
National2021 <- MASTERdata[107:159, 1:34]
Texas2019 <- MASTERdata[45:45, 1:34]
Texas2020 <- MASTERdata[98:98, 1:34]
Texas2021 <- MASTERdata[151:151, 1:34]


#National Feline stats 2019
NFI2019min <- min(National2019$`Feline Total Intakes`)
NFI2019max <- max(National2019$`Feline Total Intakes`)
print(paste("National Feline Intake 2019 - Range: ", NFI2019min, NFI2019max))
NFI2019sum <- sum(National2019$`Feline Total Intakes`)
print(paste("National Feline Intake 2019 - Sum: ", NFI2019sum))
NFI2019mean <- mean(National2019$`Feline Total Intakes`)
print(paste("National Feline Intake 2019 - Mean: ", NFI2019mean))
NFI2019sd <- sd(National2019$`Feline Total Intakes`)
print(paste("National Feline Intake 2019 - SD: ", NFI2019sd))

NFE2019min <- min(National2019$`Feline Total Euth`)
NFE2019max <- max(National2019$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2019 - Range: ", NFE2019min, NFE2019max))
NFE2019sum <- sum(National2019$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2019 - Sum: ", NFE2019sum))
NFE2019mean <- mean(National2019$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2019 - Mean: ", NFE2019mean))
NFE2019sd <- sd(National2019$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2019 - SD: ", NFE2019sd))

#National Feline stats 2020
NFI2020min <- min(National2020$`Feline Total Intakes`)
NFI2020max <- max(National2020$`Feline Total Intakes`)
print(paste("National Feline Intake 2020 - Range: ", NFI2020min, NFI2020max))
NFI2020sum <- sum(National2020$`Feline Total Intakes`)
print(paste("National Feline Intake 2020 - Sum: ", NFI2020sum))
NFI2020mean <- mean(National2020$`Feline Total Intakes`)
print(paste("National Feline Intake 2020 - Mean: ", NFI2020mean))
NFI2020sd <- sd(National2020$`Feline Total Intakes`)
print(paste("National Feline Intake 2020 - SD: ", NFI2020sd))

NFE2020min <- min(National2020$`Feline Total Euth`)
NFE2020max <- max(National2020$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2020 - Range: ", NFE2020min, NFE2020max))
NFE2020sum <- sum(National2020$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2020 - Sum: ", NFE2020sum))
NFE2020mean <- mean(National2020$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2020 - Mean: ", NFE2020mean))
NFE2020sd <- sd(National2020$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2020 - SD: ", NFE2020sd))

#National Feline stats 2021
NFI2021min <- min(National2021$`Feline Total Intakes`)
NFI2021max <- max(National2021$`Feline Total Intakes`)
print(paste("National Feline Intake 2021 - Range: ", NFI2021min, NFI2021max))
NFI2021sum <- sum(National2021$`Feline Total Intakes`)
print(paste("National Feline Intake 2021 - Sum: ", NFI2021sum))
NFI2021mean <- mean(National2021$`Feline Total Intakes`)
print(paste("National Feline Intake 2021 - Mean: ", NFI2021mean))
NFI2021sd <- sd(National2021$`Feline Total Intakes`)
print(paste("National Feline Intake 2021 - SD: ", NFI2021sd))

NFE2021min <- min(National2021$`Feline Total Euth`)
NFE2021max <- max(National2021$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2021 - Range: ", NFE2021min, NFE2021max))
NFE2021sum <- sum(National2021$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2021 - Sum: ", NFE2021sum))
NFE2021mean <- mean(National2021$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2021 - Mean: ", NFE2021mean))
NFE2021sd <- sd(National2021$`Feline Total Euth`)
print(paste("National Feline Euthanasia 2021 - SD: ", NFE2021sd))

#National Canine stats 2019
NCI2019min <- min(National2019$`Canine Total Intake`)
NCI2019max <- max(National2019$`Canine Total Intake`)
print(paste("National Canine Intake 2019 - Range: ", NCI2019min, NCI2019max))
NCI2019sum <- sum(National2019$`Canine Total Intake`)
print(paste("National Canine Intake 2019 - Sum: ", NCI2019sum))
NCI2019mean <- mean(National2019$`Canine Total Intake`)
print(paste("National Canine Intake 2019 - Mean: ", NCI2019mean))
NCI2019sd <- sd(National2019$`Canine Total Intake`)
print(paste("National Canine Intake 2019 - SD: ", NCI2019sd))


NCE2019min <- min(National2019$`Canine Total Euth`)
NCE2019max <- max(National2019$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2019 - Range: ", NCE2019min, NCE2019max))
NCE2019sum <- sum(National2019$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2019 - Sum: ", NCE2019sum))
NCE2019mean <- mean(National2019$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2019 - Mean: ", NCE2019mean))
NCE2019sd <- sd(National2019$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2019 - SD: ", NCE2019sd))

#National Canine stats 2020
NCI2020min <- min(National2020$`Canine Total Intake`)
NCI2020max <- max(National2020$`Canine Total Intake`)
print(paste("National Canine Intake 2020 - Range: ", NCI2020min, NCI2020max))
NCI2020sum <- sum(National2020$`Canine Total Intake`)
print(paste("National Canine Intake 2020 - Sum: ", NCI2020sum))
NCI2020mean <- mean(National2020$`Canine Total Intake`)
print(paste("National Canine Intake 2020 - Mean: ", NCI2020mean))
NCI2020sd <- sd(National2020$`Canine Total Intake`)
print(paste("National Canine Intake 2020 - SD: ", NCI2020sd))

NCE2020min <- min(National2020$`Canine Total Euth`)
NCE2020max <- max(National2020$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2020 - Range: ", NCE2020min, NCE2020max))
NCE2020sum <- sum(National2020$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2020 - Sum: ", NCE2020sum))
NCE2020mean <- mean(National2020$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2020 - Mean: ", NCE2020mean))
NCE2020sd <- sd(National2020$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2020 - SD: ", NCE2020sd))

#National Canine stats 2021
NCI2021min <- min(National2021$`Canine Total Intake`)
NCI2021max <- max(National2021$`Canine Total Intake`)
print(paste("National Canine Intake 2021 - Range: ", NCI2021min, NCI2021max))
NCI2021sum <- sum(National2021$`Canine Total Intake`)
print(paste("National Canine Intake 2021 - Sum: ", NCI2021sum))
NCI2021mean <- mean(National2021$`Canine Total Intake`)
print(paste("National Canine Intake 2021 - Mean: ", NCI2021mean))
NCI2021sd <- sd(National2021$`Canine Total Intake`)
print(paste("National Canine Intake 2021 - SD: ", NCI2021sd))

NCE2021min <- min(National2021$`Canine Total Euth`)
NCE2021max <- max(National2021$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2021 - Range: ", NCE2021min, NCE2021max))
NCE2021sum <- sum(National2021$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2021 - Sum: ", NCE2021sum))
NCE2021mean <- mean(National2021$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2021 - Mean: ", NCE2021mean))
NCE2021sd <- sd(National2021$`Canine Total Euth`)
print(paste("National Canine Euthanasia 2021 - SD: ", NCE2021sd))



#t-tests
NFIvE2019ttest <- t.test(National2019$`Feline Total Intakes`, National2019$`Feline Total Euth`, paired = TRUE)
print(NFIvE2019ttest)
NFIvE2020ttest <- t.test(National2020$`Feline Total Intakes`, National2020$`Feline Total Euth`, paired = TRUE)
print(NFIvE2020ttest)
NFIvE2021ttest <- t.test(National2021$`Feline Total Intakes`, National2021$`Feline Total Euth`, paired = TRUE)
print(NFIvE2021ttest)
#There is a significant correlation between intakes and euthanasia rates at the national level for all 3 years


# - put in histograms *********************************************************
National2019N <- as.numeric(as.character(National2019$`Feline Total Intakes`))
National2019NR <- data.frame(National2019N)
NFI2019_plot <-  ggplot(National2019NR, aes(x="Feline Total Intakes"))
NFI2019_plot + geom_histogram(binwidth = 30, fill = "deepskyblue4") +
  ggtitle("Histogram of National Feline Euthanasia Rates in 2019") + xlab("Rate")


#What are the percentages of euthanasia from intakes for Texas and the nation as a whole?
TXFelEuthRate2019 <- (Texas2019$`Feline Total Euth`)/(Texas2019$`Feline Total Intakes`)
print(paste("Texas Feline Euthanasia Rate for 2019: ", TXFelEuthRate2019))
TXCanEuthRate2019 <- (Texas2019$`Canine Total Euth`)/(Texas2019$`Canine Total Intake`)
print(paste("Texas Canine Euthanasia Rate for 2019: ", TXCanEuthRate2019))
TXFelEuthRate2020 <- (Texas2020$`Feline Total Euth`)/(Texas2020$`Feline Total Intakes`)
print(paste("Texas Feline Euthanasia Rate for 2020: ", TXFelEuthRate2020))
TXCanEuthRate2020 <- (Texas2020$`Canine Total Euth`)/(Texas2020$`Canine Total Intake`)
print(paste("Texas Canine Euthanasia Rate for 2020: ", TXCanEuthRate2020))
TXFelEuthRate2021 <- (Texas2021$`Feline Total Euth`)/(Texas2021$`Feline Total Intakes`)
print(paste("Texas Feline Euthanasia Rate for 2021: ", TXFelEuthRate2021))
TXCanEuthRate2021 <- (Texas2021$`Canine Total Euth`)/(Texas2021$`Canine Total Intake`)
print(paste("Texas Canine Euthanasia Rate for 2021: ", TXCanEuthRate2021))

NatlFelEuthRate2019 <- (NFE2019sum)/(NFI2019sum)
print(paste("National Feline Euthanasia Rate for 2019: ", NatlFelEuthRate2019))
NatlCanEuthRate2019 <- (NCE2019sum)/(NCI2019sum)
print(paste("National Canine Euthanasia Rate for 2019: ", NatlCanEuthRate2019))
NatlFelEuthRate2020 <- (NFE2020sum)/(NFI2020sum)
print(paste("National Feline Euthanasia Rate for 2020: ", NatlFelEuthRate2020))
NatlCanEuthRate2020 <- (NCE2020sum)/(NCI2020sum)
print(paste("National Canine Euthanasia Rate for 2020: ", NatlCanEuthRate2020))
NatlFelEuthRate2021 <- (NFE2021sum)/(NFI2021sum)
print(paste("National Feline Euthanasia Rate for 2021: ", NatlFelEuthRate2021))
NatlCanEuthRate2021 <- (NCE2021sum)/(NCI2021sum)
print(paste("National Canine Euthanasia Rate for 2021: ", NatlCanEuthRate2021))
#The percentage of animal intakes that are then euthanized are slightly lower in Texas than of the Nation as a whole.

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

Year <- c(2019, 2020, 2021)
NatlInts <- c(NatlInt2019, NatlInt2020, NatlInt2021)
NatlEuths <- c(NatlEuth2019, NatlEuth2020, NatlEuth2021)
TXInts <- c(TXInt2019, TXInt2020, TXInt2021)
TXEuths <- c(TXEuth2019, TXEuth2020, TXEuth2021)

Totals <- data.frame(Year, NatlInts, NatlEuths, TXInts, TXEuths)


Totals_plot <- ggplot(Totals, aes(Year))
Totals_plot + geom_line(aes(y=NatlInts, colour="National Intakes")) + 
  geom_line(aes(y=NatlEuths, colour="National Euthanasias")) +
  geom_line(aes(y=TXInts, colour="Texas Intakes")) +
  geom_line(aes(y=TXEuths, colour="Texas Euthanasias")) +
  xlab("Year") + 
  ylab("Number of Animals") + 
  ggtitle("Intakes vs. Euthanasias Nationally and in Texas")

# Reshaping the data to see if I can get a better looking side-by-side barchart
molten.data <- melt(Totals, id = c("Year"))

ggplot(molten.data, aes(variable, Year, fill=value)) + 
  geom_bar(position="dodge")
