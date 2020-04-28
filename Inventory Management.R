library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)

# Reading base dataset on loan and returns
POSData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 1, colNames = TRUE)
nrow(POSData) # Checking number of rows
head(POSData)

LoyaltyData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 2, colNames = TRUE)
nrow(LoyaltyData) # Checking number of rows
head(LoyaltyData)

BarcodeData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 3, colNames = TRUE)
nrow(BarcodeData) # Checking number of rows
head(BarcodeData)
BarcodeData <- mutate(BarcodeData, DCS = paste0(CategoryA,"/",CategoryB,"/", CategoryC, "/", CategoryD))

TaxonomyData <- read.xlsx(xlsxFile = "D:\\Data Science and Big Data Analytics\\Assignment 2\\Data.xlsx", sheet = 4, colNames = TRUE)
nrow(TaxonomyData) # Checking number of rows
head(TaxonomyData)
TaxonomyData <- mutate(TaxonomyData, DCS = paste0(CategoryA,"/",CategoryB,"/", CategoryC, "/", CategoryD))

POSData <- filter(POSData, Sum_Units >= 0)
POSData <- filter(POSData, Sum_Value >= 0)
POSData$Date <- as.Date(POSData$Date, origin = "1899-12-30")

Merge1<- merge(POSData, BarcodeData, by.x = "Barcode", by.y = "Barcode", all.x = TRUE)
Merge1 <- Merge1 %>% drop_na(CategoryA)
head(Merge1)

Merge2 <- merge(Merge1, TaxonomyData, by.x = "DCS", by.y = "DCS", all.x = TRUE)
head(Merge2)

Inventory <- Merge2 %>% group_by(DCS) %>% summarise(Sum_Units = sum(Sum_Units))
FlowRate <- Merge2 %>% group_by(Date, DCS) %>% summarise(Sum_Units = sum(Sum_Units))
FlowRate <- FlowRate %>% group_by(DCS) %>% summarise(Sum_Units = mean(Sum_Units))
head(FlowRate)
head(Inventory)

InvTable<- merge(Inventory, FlowRate, by.x = "DCS", by.y = "DCS")
head(InvTable)
InvTable <- rename(InvTable, Inventory = Sum_Units.x, FlowRate = Sum_Units.y)
InvTable <- mutate(InvTable, FlowTime = Inventory/FlowRate)
InvTable$Rank <- rank(InvTable$FlowTime)/length(InvTable$FlowTime)
InvTable$R <- ifelse(InvTable$Rank < 0.25, 1, ifelse(InvTable$Rank >= 0.25 & InvTable$Rank < 0.5, 2, ifelse(InvTable$Rank >= 0.5 & InvTable$Rank < 0.75, 3,4)))
I <- InvTable  %>% count(R)

ggplot(InvTable, aes(R, color = R)) + geom_histogram()
