library(RCurl)
library(XML)
library(tidyverse)
#Scraper to get different playoffs from basketball reference
getPostseason <- function(year){
  url <- paste0("https://www.basketball-reference.com/playoffs/NBA_",year,"_games.html")
  newurl <- getURL(url)
  postseason <- as.data.frame(readHTMLTable(newurl))
  postseason <- postseason[,c(3,5,4,6,8)]
  colnames(postseason) <- c("Vis","Home","VisPts","HomePts","OT")
  #Creating unique series identifiers
  postseason <- postseason %>%
    unite(., series, 1:2, remove=FALSE)
  postseason$series <- postseason$series %>% 
    str_split(., '_') %>% 
    lapply(., 'sort') %>%  
    lapply(., 'paste', collapse=' ') %>% 
    unlist(.)
  postseason$VisPts <- as.integer(as.character(postseason$VisPts))
  postseason$HomePts <- as.integer(as.character(postseason$HomePts))
  postseason$Vis <- as.character(postseason$Vis)
  postseason$Home <- as.character(postseason$Home)
  return(postseason)
}

getMets <- function(postseason, year){
  #Getting standings for seeding
  url <- paste0("https://www.espn.com/nba/standings/_/season/",year,"/group/league")
  newurl <- getURL(url)
  psTeams <- as.data.frame(readHTMLTable(newurl)) %>%
    select(Var.1)
  psTeams$Var.1 <- as.character(psTeams$Var.1)
  if(year < 2016){
    for(i in 1:30){
      if(isTRUE(str_detect(psTeams$Var.1[i],"--"))){
      }
      else{
        psTeams$Var.1[i] <- paste0("e --",psTeams$Var.1[i])
      }
    }
  }
  psTeams <- unlist(str_split(psTeams$Var.1, pattern = "--"))
  psTeams <- data.frame(PS = psTeams[seq(1,length(psTeams),2)], 
                           Teams = psTeams[seq(2,length(psTeams),2)])
  #ESPN has Clippers as "LA" while Basketball-Reference has it as "Los Angeles"
  psTeams <- psTeams %>%
    mutate(., Teams = ifelse(str_detect(Teams, "LACLA"),"LACLos Angeles Clippers",as.character(Teams)))
  #Removing unwanted abbreviations
  teams1 <- str_extract(psTeams$Teams, '[A-Z][a-z]+')
  teams2 <- word(psTeams$Teams,2,sep = " ")
  teams3 <- word(psTeams$Teams,3,sep = " ")
  teams <- paste(teams1, teams2, teams3, sep = " ")
  psTeams$Teams <- unlist(str_remove(teams, " NA"))
  psTeams$PS <- as.character(psTeams$PS)
  #Finding which seed belongs to which team
  Seed <- rep(0,30)
  j <- 0
  for(i in 1:30){
    if(isTRUE(psTeams$PS[i] != "e ")){
      j <- j+1
      Seed[i] <- j
    }
    else{
      Seed[i] <- 0
    }
  }
  #Combining seeding with teams
  psTeams <- cbind(psTeams, Seed)
  
  library(operators)
  seriesTracker <- NA
  for(i in 1:nrow(postseason)){
    if(isTRUE(postseason$series[i] %!in% seriesTracker)){
      seriesTracker <- c(seriesTracker, postseason$series[i])
    }
    else{}
  }
  devtools::unload("operators")
  seriesTracker <- na.omit(seriesTracker)[1:15]
  Round <- c(rep(1,8),rep(2,4),rep(3,2),4)
  seriesTracker <- as.data.frame(cbind(seriesTracker,Round))
  seriesTracker$seriesTracker <- as.character(seriesTracker$seriesTracker)
  seriesTracker$Round <- as.integer(seriesTracker$Round)
  for(i in 1:nrow(postseason)){
    postseason$Round[i] <- seriesTracker$Round[which(seriesTracker$seriesTracker == postseason$series[i])]
  }
  
  
  #Getting number of games in each round
  numGames <- count(postseason, Round)
  numGames <- rbind(numGames, nrow(postseason)); numGames$Round[5] <- "Tot"
  
  #Number of OT's in each round
  numOT <- as.data.frame(count(postseason, Round, OT))
  numOT$OT <- as.character(numOT$OT)
  numOT <- filter(numOT, OT == "OT")
  numOT <- numOT[,c(1,3)]
  numOT$Round <- as.character(numOT$Round)
  complete <- data.frame(Round = c(1:4, "Tot"), n = rep(0,5))
  numOT <- union(numOT, complete) %>% 
    group_by(Round) %>%
    slice(1L) %>%
    ungroup(.)
  numOT <- arrange(numOT, Round)
  numOT$n[5] <- sum(numOT$n)

  
  #Getting number of long series in each round
  seriesGames <- as.data.frame(count(postseason, series))
  Round <- rep(NA,15)
  seriesGames <- cbind(seriesGames, Round)
  for(i in 1:15){
    seriesGames$Round[i] <- as.integer(seriesTracker$Round[which(seriesGames$series[i]==seriesTracker$seriesTracker)])
  }
  numLongSeries <- data.frame(as.character(c(1:4, "Tot")),rep(0,5))
  colnames(numLongSeries) <- c("Round", "n")
  for(i in 1:4){
    for(j in 1:15){
      if(isTRUE(numLongSeries$Round[i]==seriesGames$Round[j] & seriesGames$n[j] > 5)){
        numLongSeries$n[i] <- numLongSeries$n[i] + 1
        numLongSeries$n[5] <- numLongSeries$n[5] + 1
      }else{}
    }
  }
  
  #Getting Number of Close Games
  numClose <- data.frame(as.character(c(1:4, "Tot")),rep(0,5))
  colnames(numClose) <- c("Round", "n")
  closeInd <- which(abs(postseason$VisPts-postseason$HomePts) <= 3)
  numClose$n[5] <- length(closeInd)
  for(i in 1:length(closeInd)){
    j <- closeInd[i]
    numClose$n[postseason$Round[j]] <- numClose$n[postseason$Round[j]] + 1
  }
  
  #Getting Number of Upsets
  VisSeed <- rep(0,nrow(postseason))
  HomeSeed <- rep(0, nrow(postseason))
  Upset <- rep(0,nrow(postseason))
  postseason <- cbind(postseason,VisSeed,HomeSeed,Upset)
  for(i in 1:nrow(postseason)){
    postseason$VisSeed[i] <- psTeams$Seed[which(psTeams$Team == postseason$Vis[i])]
    postseason$HomeSeed[i] <- psTeams$Seed[which(psTeams$Team == postseason$Home[i])]
    if(isTRUE(postseason$HomePts[i] > postseason$VisPts[i] & postseason$HomeSeed[i] > postseason$VisSeed[i])){
      postseason$Upset[i] <- 1
    }
    else if(isTRUE(postseason$HomePts[i] < postseason$VisPts[i] & postseason$HomeSeed[i] < postseason$VisSeed[i])){
      postseason$Upset[i] <- 1
    }
    else{}
  }
  numUpset <- data.frame(as.character(c(1:4, "Tot")),rep(0,5))
  colnames(numUpset) <- c("Round", "n")
  upsetInd <- which(postseason$Upset == 1)
  numUpset$n[5] <- length(upsetInd)
  for(i in 1:length(upsetInd)){
    j <- upsetInd[i]
    numUpset$n[postseason$Round[j]] <- numUpset$n[postseason$Round[j]] + 1
  }
  
  #Getting percentages
  numOT$n <- numOT$n/numGames$n
  numLongSeries$n <- numLongSeries$n/c(8,4,2,1,15)
  numClose$n <- numClose$n/numGames$n
  numUpset$n <- numUpset$n/numGames$n
  
  return(c(numOT, numLongSeries, numClose, numUpset))
}


rebuild_metric <- function(metrics){
  met <- data.frame(metrics[1],metrics[2])
  met[,1] <- as.character(met[,1])
  met[,2] <- as.numeric(met[,2])
  return(met)
}

max_year <- 2019
vals <- 1
for(y in 2010:max_year){
  postseason <- getPostseason(y)
  metrics <- getMets(postseason, y)
  if(isTRUE(vals == 1)){
    assign(paste0(y, "_OT"), rebuild_metric(metrics[c(1,2)]))
  }
  else if(isTRUE(vals == 2)){
    assign(paste0(y, "_longSeries"), rebuild_metric(metrics[c(3,4)]))
  }
  else if(isTRUE(vals == 3)){
    assign(paste0(y, "_Close"), rebuild_metric(metrics[c(5,6)]))
  }
  else{
    assign(paste0(y, "_Upset"), rebuild_metric(metrics[c(7,8)]))
  }
  vals <- vals+1
}



