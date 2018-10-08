#setwd("D:/NFL")
setwd("~/SuhasHome/NFL")
source('RIncludes.R')

###setwd### Clean and Analyse BureauData
AllNFLData = read.csv(file = "TableData.csv", header = TRUE)
AllNFLData$PlayerName = gsub("(?=[A-Z])", " ", AllNFLData$PlayerName , perl = TRUE)
AllNFLData$PlayerName = gsub("^\\s", "", AllNFLData$PlayerName , perl = TRUE)
colnames(AllNFLData)[colnames(AllNFLData)=="ï..Date"] <- "Date"
AllNFLData[AllNFLData == "NULL"] = 0
AllNFLData$Date = as.Date(AllNFLData$Date)

colnames(AllNFLData)[colnames(AllNFLData)=="oneGS"] <- "Team"
# Defensive stats 3-15, 16-20
DefensiveStats = data.frame(matrix(ncol = 20))
colnames(DefensiveStats) = c("oneTm", "Date", "TacklesComb", "TacklesTFL", "TacklesQBHits", "DefInterceptionsQBHits", "DefInterceptionsInt",
                             "DefInterceptionsYds", "DefInterceptionsTD", "KickReturnsRt", "KickReturnsYds", "KickReturnsYRt",
                             "KickReturnsTD", "PuntReturnsYds", "PuntReturnsTD", "RushingYds", "PassingYds", "ReceivingYds", "ScoringTD", "RushingTD")
DefensiveStats[is.na(DefensiveStats)] = 0


OffensiveStats = data.frame(matrix(ncol=13))
colnames(OffensiveStats) = c("PlayerName", "oneTm", "oneOpp" , "Date", "RushingYds", "PassingYds", "ReceivingYds",
                             "RushingYA", "PassingYA", "ReceivingYR",
                             "ScoringTD", "RushingTD", "PassingTD")

Teams = unique(AllNFLData$oneTm)


###### Get 2017 data to check

  All2017 = subset(AllNFLData,as.Date(AllNFLData$Date) > "2018-07-01" & as.Date(AllNFLData$Date) < "2018-12-01")
  # View(All2017)
  
  DateLevels = as.factor(unique(All2017[order(All2017$Date , decreasing = FALSE ),]$Date))
  All2017[is.na(All2017)] = 0
  All2017[is.null(All2017)] = 0
  AllColumnNames = colnames(All2017)
  
  
  for (colname in 13:71){
    All2017[,colname] = as.numeric(levels(All2017[,colname]))[All2017[,colname]]
    
    #as.numeric(levels(All2017[,colname]))[All2017[,colname]]
    
  }
  className = data.frame(sapply(All2017, class))
  colNames = colnames(All2017)
  
  
  ### Get Defensive stats for each team
  for (eachTeam in Teams) {
    # Iterate over each team
    subsetTeamData = subset(All2017, All2017$oneTm == eachTeam)  
    if (nrow(subsetTeamData) == 0)
    {
      next;
    }
    DateLevels = as.factor(unique(subsetTeamData[order(subsetTeamData$Date , decreasing = FALSE ),]$Date))  
    
    for (date in 2:length(DateLevels)){
      # Iterate over each date
      temp = DefensiveStats[1,]
      subsetTeamData = subset(All2017, All2017$oneTm == eachTeam & as.Date(All2017$Date) < as.Date(DateLevels[date]) )  
      
      temp$Date = DateLevels[date]
      temp$oneTm = eachTeam
      
      for (column in 3:length(colnames(temp))){
        print(colnames(temp)[column])
        temp[, colnames(temp)[column]]  = sum(subsetTeamData[, colnames(temp)[column]]) / length(DateLevels)
      }
      
      subsetOppData = subset(All2017, All2017$oneTm %in% unique(subsetTeamData$oneOpp) & as.Date(All2017$Date) < as.Date(DateLevels[date]) )
      
      for (column in 16:length(colnames(temp)) ){
        print(colnames(temp)[column])
        temp[, colnames(temp)[column]]  = sum(subsetOppData[, colnames(temp)[column]])/ length(DateLevels)
      }
      
      DefensiveStats = rbind(temp, DefensiveStats)
    }
  }

##########


#write.csv(DefensiveStats, file = "DefensiveStats_2018.csv")
