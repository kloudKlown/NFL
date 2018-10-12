setwd("D:/NFL")
# setwd("~/SuhasHome/NFL")
source('RIncludes.R')

###setwd### Clean and Analyse BureauData
AllNFLData = read.csv(file = "TableData.csv", header = TRUE)
AllNFLData$PlayerName = gsub("(?=[A-Z])", " ", AllNFLData$PlayerName , perl = TRUE)
AllNFLData$PlayerName = gsub("^\\s", "", AllNFLData$PlayerName , perl = TRUE)
colnames(AllNFLData)[colnames(AllNFLData)=="ï..Date"] <- "Date"
AllNFLData[AllNFLData == "NULL"] = 0
AllNFLData$Date = as.Date(AllNFLData$Date)

colnames(AllNFLData)[colnames(AllNFLData)=="oneGS"] <- "Team"

Teams = unique(AllNFLData$oneTm)


###### Get 2017 data to check

  All2017 = subset(AllNFLData, as.Date(AllNFLData$Date) > "2015-07-01" & as.Date(AllNFLData$Date) < "2018-12-01")
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
  All2017[is.na(All2017)] = 0
  All2017[is.null(All2017)] = 0
  
  
  # Defensive stats 3-15, 16-20
  DefensiveStats = data.frame(matrix(ncol = 20))
  colnames(DefensiveStats) = c("oneTm", "Date", "TacklesComb", "TacklesTFL", "TacklesQBHits", "DefInterceptionsQBHits", "DefInterceptionsInt",
                               "DefInterceptionsYds", "DefInterceptionsTD", "KickReturnsRt", "KickReturnsYds", "KickReturnsYRt",
                               "KickReturnsTD", "PuntReturnsYds", "PuntReturnsTD", "RushingYds", "PassingYds", "ReceivingYds", "ReceivingTD", "RushingTD")
  DefensiveStats[is.na(DefensiveStats)] = 0
  
  ### Get Defensive stats for each team
  for (eachTeam in Teams) {
    # Iterate over each team
    subsetTeamData = subset(All2017, All2017$oneTm == eachTeam)  
    if (nrow(subsetTeamData) == 0)
    {
      next;
    }
    DateLevels = as.factor(unique(subsetTeamData[order(subsetTeamData$Date , decreasing = FALSE ),]$Date))  
    print(eachTeam)
        ## Iterate over date
        for (date in 2:length(DateLevels)){
          print(date)
          # Iterate over each date
          temp = DefensiveStats[1,]
          subsetTeamData = subset(All2017, All2017$oneTm == eachTeam 
                                    & as.Date(All2017$Date) < as.Date(DateLevels[date]) &
                                    as.Date(All2017$Date) > (as.Date(DateLevels[date]) - 120)
                                  )  
          
          temp$Date = DateLevels[date]
          temp$oneTm = eachTeam
          
          #### How good the defense is
          for (column in 3:length(colnames(temp))){
            temp[, colnames(temp)[column]]  = sum(subsetTeamData[, colnames(temp)[column]]) / length(DateLevels)
          }
          
          subsetOppData = subset(All2017, All2017$oneTm %in% unique(subsetTeamData$oneOpp) 
                                 & as.Date(All2017$Date) < as.Date(DateLevels[date]) 
                                 & as.Date(All2017$Date) > (as.Date(DateLevels[date]) - 120)
                                 )
          
          #### How many points have been allowed
          for (column in 16:length(colnames(temp)) ){
            temp[, colnames(temp)[column]]  = sum(subsetOppData[, colnames(temp)[column]])/ length(DateLevels)
          }
          
          DefensiveStats = rbind(temp, DefensiveStats)
        }
        ## Iterate over date
    
  }

##########
write.csv(DefensiveStats, file = "DefensiveStats_All.csv")

  
######### Offensive Stats
  OffensiveStats = data.frame(matrix(ncol=15))
  colnames(OffensiveStats) = c("PlayerName", "oneTm", "PlayerPosition" , "Date", "oneOpp",  "TotalPoints", "RushingYds", "PassingYds", "ReceivingYds",
                               "RushingYA", "PassingYA", "ReceivingYR",
                               "ReceivingTD", "RushingTD", "PassingTD")
  
  All2017$TotalPoints = All2017$RushingYds * 0.1 + All2017$PassingYds * 0.2 + 
                        All2017$ReceivingTD * 6 + All2017$PassingTD * 6
  
  allPlayers = unique(All2017$PlayerName)
  
  OffensiveStats$Date[OffensiveStats$Date == "2018-10-07"] <- "2018-10-08"
  
  for (player in allPlayers) {
    
    ## Get Playerdata
    subsetPlayerData = subset(All2017, All2017$PlayerName == player)  
    if (nrow(subsetPlayerData) == 0)
    {
      next;
    }

    DateLevels = as.factor(unique(subsetPlayerData[order(subsetPlayerData$Date , decreasing = FALSE ),]$Date))  
    # Add current Date
    # DateLevels = factor(c(levels(DateLevels),substring(Sys.time(),0,10)))
    if (as.character(subsetPlayerData$PlayerPosition[1]) == "WR" 
        | as.character(subsetPlayerData$PlayerPosition) == "TE"
        | as.character(subsetPlayerData$PlayerPosition) == "RB"
        | as.character(subsetPlayerData$PlayerPosition) == "QB" 
        | as.character(subsetPlayerData$PlayerPosition) == "K" 
        )
    {
      print(player)  
    }
    else{
      next;
    }
    
    ## Iterate over date
    for (date in 2:length(DateLevels)){
      # Iterate over each date
      temp = OffensiveStats[1,]
      subsetPlayerData = subset(All2017, All2017$PlayerName == player 
                                & as.Date(All2017$Date) < as.Date(DateLevels[date]) 
                                & as.Date(All2017$Date) > (as.Date(DateLevels[date]) - 120)
                                )  
      
      currentGame = subset(All2017, All2017$PlayerName == player 
                                & as.Date(All2017$Date) == as.Date(DateLevels[date]) 
      ) 
      
      if (nrow(currentGame) == 0 ){
        next
      }
      
      temp$Date = DateLevels[date]
      temp$PlayerName = player
      temp$PlayerPosition = as.character(subsetPlayerData$PlayerPosition[1])
      temp$oneTm = as.character(subsetPlayerData$oneTm[1])
      temp$oneOpp = as.character(currentGame$oneOpp[1])     
      temp$TotalPoints = currentGame$TotalPoints[1]
      #### How good the defense is
      for (column in 7:length(colnames(temp))){
        temp[, colnames(temp)[column]]  = sum(subsetPlayerData[, colnames(temp)[column]])
      }
      
      OffensiveStats = rbind(temp, OffensiveStats)
    }
    ## Iterate over date
    
  }
  
######### Offensive Stats

  
  
#Combine  Offensive and team defensive stats
  
  CombinedStats = merge(OffensiveStats, DefensiveStats, by = c("Date"), by.x = c("Date", "oneOpp"), by.y = c("Date", "oneTm") )
  allPlayers = unique(CombinedStats$PlayerName)
  DateCheck = "2018-09-23"
  
  
  Results = data.frame( RFPred = numeric(), player = factor(), position = factor())
  
  
  
  for (player in allPlayers){
    
    Data_Cleaned_Test = subset(CombinedStats, as.Date(CombinedStats$Date) == as.Date(DateCheck) 
                               & CombinedStats$PlayerName == as.character(player) )
    
    Data_Cleaned_Train = subset(CombinedStats, as.Date(CombinedStats$Date) < as.Date(DateCheck)
                                & as.Date(CombinedStats$Date) > (as.Date(DateCheck) - 365)
                                & CombinedStats$PlayerName == as.character(player) )
    
    if (nrow(Data_Cleaned_Train) == 0 | nrow(Data_Cleaned_Test) == 0){
      next;
    }
    
    rf = randomForest( Data_Cleaned_Train[,c("RushingYds.x","PassingYds.x","ReceivingYds.x",
                                             "RushingYA","PassingYA","ReceivingYR","ReceivingTD.x",
                                             "RushingTD.x","PassingTD","TacklesComb",
                                             "TacklesTFL","TacklesQBHits","DefInterceptionsQBHits","DefInterceptionsInt",
                                             "DefInterceptionsYds","DefInterceptionsTD","KickReturnsRt","KickReturnsYds",
                                             "KickReturnsYRt","KickReturnsTD","PuntReturnsYds","PuntReturnsTD",
                                             "RushingYds.y","PassingYds.y","ReceivingYds.y","ReceivingTD.y",
                                             "RushingTD.y"
                                             )], 
                       y = Data_Cleaned_Train[,c("TotalPoints")], ntree=100
                       ,type='regression')
  
    RFPred = predict( rf,  Data_Cleaned_Test[,c("RushingYds.x","PassingYds.x","ReceivingYds.x",
                                                "RushingYA","PassingYA","ReceivingYR","ReceivingTD.x",
                                                "RushingTD.x","PassingTD","TacklesComb",
                                                "TacklesTFL","TacklesQBHits","DefInterceptionsQBHits","DefInterceptionsInt",
                                                "DefInterceptionsYds","DefInterceptionsTD","KickReturnsRt","KickReturnsYds",
                                                "KickReturnsYRt","KickReturnsTD","PuntReturnsYds","PuntReturnsTD",
                                                "RushingYds.y","PassingYds.y","ReceivingYds.y","ReceivingTD.y",
                                                "RushingTD.y"
                                              )] ,type = c("response") )
    print(player)
    Prediction2 =  as.data.frame(RFPred)
    Prediction2["player"] = player
    Prediction2["position"] = Data_Cleaned_Test$PlayerPosition
    Results = rbind(Results, Prediction2)
    
  }
  

  write.csv(Results, file = "Results.csv")
  