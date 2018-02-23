library(stringr)

# Returns country
getCountry <- function(x) {
  if(str_detect(x, "england")) {
    return("en")
  } else if (str_detect(x, "france")) {
    return("fr")
  } else if (str_detect(x, "italy")) {
    return("it")
  } else if (str_detect(x, "espana")) {
    return("es")
  }
}

# Returns season 
getSeason <- function(season) {
  split <- str_split_fixed(season, "/", 4)
  
  if (split[,1] == "en-england") {
    return(split[,3])
  } else {
    return(split[,2])
  }
}

# Calculates cumulative points season by season
calculatePoints <- function(data) {
  print("calculatePoints")
  cumHomePts <- rep(0, length(data$Date))
  cumAwayPts <- rep(0, length(data$Date))
  cumHomeMeanPts <- rep(0, length(data$Date))
  cumAwayMeanPts <- rep(0, length(data$Date))
  seasons <- unique(data$Season)
  print("calculatePoints2")
  for (season in seasons) {
    print(season)
    teams <- unique (data$Team1[data$Season == season])
    for(team in teams) {
      homeIndexes <- which(data$Team1 == team & data$Season == season)
      cumHomePts[homeIndexes] <- cumsum(data[homeIndexes,]$PointsHome)
      homePts <- cumHomePts[homeIndexes]
      homePts <- c(0, homePts[1:length(homePts) -1])
      homeMeanPts <- round(homePts/c(1,cumsum(rep(1, length(homePts) - 1))), 2)
      cumHomePts[homeIndexes] <- homePts
      cumHomeMeanPts[homeIndexes] <- homeMeanPts
      
      awayIndexes <- which(data$Team2 == team & data$Season == season)
      cumAwayPts[awayIndexes] <- cumsum(data[awayIndexes,]$PointsAway)
      awayPts <- cumAwayPts[awayIndexes]
      awayPts <- c(0, awayPts[1:length(awayPts) -1])
      awayMeanPts <- round(awayPts/c(1,cumsum(rep(1, length(awayPts) - 1))), 2)
      cumAwayMeanPts[awayIndexes] <- awayMeanPts
      
    }
  }
  
  data <- cbind(data, cumHomePts, cumAwayPts, cumHomeMeanPts, cumAwayMeanPts)
  return(data)
}

# Loading files
# First: sets the working directory
setwd("~/Documents/Perso/Data analysis/football_analysis")


loadData <- function(files) {
  print(loadData)
  data <- data.frame(read.csv(files[1]))
  country <- rep(getCountry(files[1]), length(data$Date))
  season <- getSeason(files[1])
  data <- cbind(data, country, season)
  for (i in 2:length(files)) {
    print(i)
    season <- getSeason(files[i])
    print(season)
    temp <- read.csv(files[i])
    country <- rep(getCountry(files[i]), length(temp$Date))
    season <- rep(season, length(temp$Date))
    temp <- cbind(temp, country, season)
    data <- rbind(data, temp)
  }
  
  return(data)
}

cleanData <- function(data) {
  print("cleanData")
  # Fourth : eventually cleans results with no half time scores
  data <- subset(x = data, is.na(data$HT) == FALSE)
  data <- subset(x = data, data$HT != "")
  
  return(data)
} 

addFullTimeGoals <- function(data) {
  Score <- str_split_fixed(paste(data$FT), "-", 2)
  data<- cbind(data, as.numeric(as.character(Score[,1])), as.numeric(as.character(Score[,2])))
  
  return(data)
}

addFullTimeResults <- function(data) {
  print("addFullTimeResults")
  data$FTResult <- c(rep("Home", length(data$Date)))
  data$FTResult[data$Score1 == data$Score2] <- "Draw"
  data$FTResult[data$Score1 < data$Score2] <- "Away"
  
  return(data)
}

generateData <- function(files) {
  # Loads data
  data <- loadData(files)
  # Cleans data 
  data <- cleanData(data)
  names <- c("Date", "Team1", "Team2", "FT", "HT", "Country", "Season")
  names(data) <- names
  # Adds full time goals to database  
  data <- addFullTimeGoals(data)
  names <- c(names, "Score1", "Score2")
  names(data) <- names
  # Adds full time result to database
  data$FTResult <- c(rep("Home", length(data$Date)))
  data$FTResult[data$Score1 == data$Score2] <- "Draw"
  data$FTResult[data$Score1 < data$Score2] <- "Away"
  names <- c(names, "FTResult")
  names(data) <- names
  
  # Adds half time goals to database
  ScoreHt <- str_split_fixed(paste(data$HT), "-", 2)
  data<- cbind(data, as.numeric(as.character(ScoreHt[,1])), as.numeric(as.character(ScoreHt[,2])))
  names <- c(names, "ScoreHt1", "ScoreHt2")
  names(data) <- names

  # Adds half time result to database
  data$HTResult <- c(rep("Home", length(data$Date)))
  data$HTResult[data$ScoreHt1 == data$ScoreHt2] <- "Draw"
  data$HTResult[data$ScoreHt1 < data$ScoreHt2] <- "Away"
  names <- c(names, "HTResult")
  names(data) <- names

  # Calculates points team by team, season by season
  PointsHome <- rep(0, length(data$Date))
  PointsAway <- rep(0, length(data$Date))

  PointsHome[data$FTResult == "Home"] <- 3
  PointsHome[data$FTResult == "Draw"] <- 1
  PointsAway[data$FTResult == "Away"] <- 3
  PointsAway[data$FTResult == "Draw"] <- 1

  data <- cbind(data, PointsHome, PointsAway)
  
  data <- calculatePoints(data)
  
  return(data)
}

exportModel <- function(data, filename) {
  totalGames <- length(data$Date)
  wins <- c(rep(0, totalGames))
  
  wins[data$FTResult == "Draw"] <- 1
  wins[data$FTResult == "Away"] <- 2
  data <- cbind(data, wins)
  
  dataTraining <- data.frame(data$ScoreHt1, data$ScoreHt2, data$cumHomeMeanPts, data$cumAwayMeanPts, data$wins)
  names(dataTraining) <- c("ScoreHt1", "ScoreHt2", "CumHomeMeanPts", "CumAwayMeanPts", "Results")
  print(data)
  write.csv(dataTraining, filename, row.names = FALSE, quote = FALSE)
}

# Gets files list
files <- list.files(recursive = TRUE, pattern = "*.csv") 

temp <- generateData(files)

exportModel(temp[temp$Season != "2016-17"], "training.txt")
exportModel(temp[temp$Season == "2016-17"], "test.txt")




