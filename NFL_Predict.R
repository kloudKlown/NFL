setwd("D:/NFL")
source('RIncludes.R')

###### Clean and Analyse BureauData
AllNFLData = read.csv(file = "TableData.csv", header = TRUE)
AllNFLData$PlayerName = gsub("(?=[A-Z])", " ", AllNFLData$PlayerName , perl = TRUE)
AllNFLData$PlayerName = gsub("^\\s", "", AllNFLData$PlayerName , perl = TRUE)
colnames(AllNFLData)[colnames(AllNFLData)=="ï..Date"] <- "Date"

View(AllNFLData)

All2017 = subset(AllNFLData,as.Date(AllNFLData$Date) > "2017-07-01" & as.Date(AllNFLData$Date) < "2018-04-01")

