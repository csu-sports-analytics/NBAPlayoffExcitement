library(RCurl)
library(XML)
library(tidyverse)
set.seed(100)
#Using data from the 2018/19 and 2019/20 seasons
years <- c(2019:2020)

#Creating complete schedule
games <- data.frame()

for(i in 1:length(years)){
  #Establishing which months in each other years NBA games were played
  if(years[i]==2019){
    months <- c("october","november","december","january","february","march","april","may","june")
  }
  else if(years[i]==2020){
    months <- c("october","november","december","january", "february","march")
  }
  else{}
  for(j in 1:length(months)){
    #Creating URL and gathering table
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",years[i],"_games-",months[j],".html")
    url_new <- getURL(url)
    table <- readHTMLTable(url_new)
    month_sched <- table$schedule
    #Playoffs begin in April and there is a row that is empty that must be removed
    if(months[j]=="april"){
      month_sched <- month_sched[-(which(month_sched$Date=="Playoffs")),]
    }
    #Changing formats of the data and consolidating into only the data that we need
    month_sched$Date <- as.Date(month_sched$Date, "%a, %b %d,%Y")
    month_sched$Opponent <- as.character(month_sched$`Visitor/Neutral`)
    month_sched$OppPts <- as.numeric(as.character(month_sched[[4]]))
    month_sched$Team <- as.character(month_sched$`Home/Neutral`)
    month_sched$TeamPts <- as.numeric(as.character(month_sched[[6]]))
    month_sched <- month_sched[,c(1,13,14,11,12)]
    games <- rbind(games,month_sched)
  }
}

allTeams <- unique(games$Team)

#Adding PtsDiff to games
games <- games %>%
  mutate(PtsDiff = TeamPts-OppPts)

#Days since the game for weights for model (relative to when playoffs would have started)
games$DaysSince <- as.numeric(as.Date("2020-4-19")-games$Date)

#Which season the game was in, also for weighting model
#1 if 2018/19, 3 if 2019/20 so that games from this season are weighted heavier
#but last season is still accounted for
games <- games %>%
  mutate(., Season = if_else(Date < as.Date("2019-10-21"), 1, 3))

#Weight games based on how long ago they were
games <- games %>%
  mutate(., Game_Weight = Season*exp(-games$DaysSince/max(games$DaysSince)))

#Adding "H" for location
games <- games %>%
  mutate(., Location = "H")

#Cleaning data
games <- games %>%
  select(., Team, Opponent, PtsDiff, Location, Game_Weight)

#Adding games from away teams' perspectives
games_away <- games %>%
  select(., Opponent, Team, PtsDiff, Location, Game_Weight) %>%
  #Inverting PtsDiff
  mutate(., PtsDiff = -1*PtsDiff) %>%
  #Changing Location from H to A
  mutate(., Location = "A")

#Renaming columns
names(games_away)=c("Team", "Opponent", "PtsDiff", "Location", "Game_Weight")

#Combine home and away
games <- rbind(games, games_away)

games <- rbind(games, games_away)

#Making linear model to predict team score
lm.nba <- lm(PtsDiff ~ Team + Opponent + Location,
             data = games,
             weights = Game_Weight)
#Getting expected scores for each game
games$exp_pts_diff <- predict(lm.nba, newdata = games, type = "response")

#GLM to find prob of winning giving expected points spread
games$win <- ifelse(games$PtsDiff > 0,1,0)
glm.spread <- glm(win ~ exp_pts_diff, data = games, family = "binomial")

#Updating gameSim to use 2019/20 model alongside gameSim
gameSim <- function(high, low, game){
  #Higher seed is home
  if(isTRUE(game<3 | game == 5 | game == 7)){
    gamedf <- data.frame("Team" = high, "Opponent" = low, "Location" = "H", stringsAsFactors = F)
  }else{
    #Lower seed is home
    gamedf <- data.frame("Team" = high, "Opponent" = low, "Location" = "A", stringsAsFactors = F)
  }
  #Predicting game points difference
  gamedf$exp_pts_diff <- as.numeric(predict(lm.nba, newdata = gamedf, type = "response")[[1]])
  
  #Odds of points high seed winning given pts diff
  win_prob <- predict.glm(glm.spread, newdata = gamedf, type = "response")[[1]]
  
  #Returns TRUE or FALSE based on win probability
  #If TRUE, the higher seed won, if FALSE, the lower seed won
  return(c(rbernoulli(1,win_prob), win_prob, gamedf$exp_pts_diff))
}

#### Current Format ####
S <- 10000
playoffSim1 <- list()
for(s in 1:S){
  #Gathering teams that would make the playoffs if the season ended on March 11, 2020
  r1 <- data.frame("Seed" = 1:16, 
                   "Team" = c("Milwaukee Bucks", "Los Angeles Lakers", "Toronto Raptors",
                              "Los Angeles Clippers", "Boston Celtics", "Denver Nuggets",
                              "Utah Jazz", "Miami Heat", "Oklahoma City Thunder",
                              "Houston Rockets", "Indiana Pacers", "Philadelphia 76ers", 
                              "Dallas Mavericks","Memphis Grizzlies", "Brooklyn Nets", 
                              "Orlando Magic"),
                   "Conf" = c("E", "W", "E", "W", "E", "W", "W","E","W",
                              "W", "E", "E", "W", "W", "E","E"), 
                   stringsAsFactors = FALSE)
  #Seeding each team based on conference
  r1 <- r1 %>%
    group_by(., Conf) %>%
    mutate(., ConfSeed = row_number())
  
  r1results <- r1SimCurr(r1)
  #Reconstructing sim results
  west1 <- data.frame(r1results[1],r1results[2],r1results[3],r1results[4], stringsAsFactors = FALSE)
  east1 <- data.frame(r1results[5],r1results[6],r1results[7],r1results[8], stringsAsFactors = FALSE)
  playoffSim1 <- c(playoffSim1,west1,east1,r1results[9],r1results[10],r1results[11],r1results[12])
  
  
  r2 <- data.frame("Seed" = rep(NA,8), 
                   "Team" = c("", "", "",
                              "", "", "",
                              "", ""),
                   "Conf" = c("", "", "", "", "", "", "",""), 
                   "ConfSeed" = rep(NA,8),
                   stringsAsFactors = FALSE)
  #Taking series winners from west first round and advancing them
  for(i in 1:4){
    if(west1$highW[i]>west1$lowW[i]){
      r2$Team[i] <- west1$high[i]
      r2$Conf[i] <- "W"
      r2$Seed[i] <- r1$Seed[which(r1$Team==west1$high[i])]
      r2$ConfSeed[i] <- r1$ConfSeed[which(r1$Team==west1$high[i])]
    }
    else{
      r2$Team[i] <- west1$low[i]
      r2$Conf[i] <- "W"
      r2$Seed[i] <- r1$Seed[which(r1$Team==west1$low[i])]
      r2$ConfSeed[i] <- r1$ConfSeed[which(r1$Team==west1$low[i])]
    }
  }
  #Taking series winners from east first round and advancing them
  for(i in 1:4){
    if(isTRUE(east1$highW[i]>east1$lowW[i])){
      r2$Team[i+4] <- east1$high[i]
      r2$Conf[i+4] <- "E"
      r2$Seed[i+4] <- r1$Seed[which(r1$Team==east1$high[i])]
      r2$ConfSeed[i+4] <- r1$ConfSeed[which(r1$Team==east1$high[i])]
    }
    else{
      r2$Team[i+4] <- east1$low[i]
      r2$Conf[i+4] <- "E"
      r2$Seed[i+4] <- r1$Seed[which(r1$Team==east1$low[i])]
      r2$ConfSeed[i+4] <- r1$ConfSeed[which(r1$Team==east1$low[i])]
    }
  }
  
  r2results <- r2SimCurr(r2)
  #Reconstructing sim results
  west2 <- data.frame(r2results[1],r2results[2],r2results[3],r2results[4], stringsAsFactors = FALSE)
  east2 <- data.frame(r2results[5],r2results[6],r2results[7],r2results[8], stringsAsFactors = FALSE)
  playoffSim1 <- c(playoffSim1,west2,east2,r2results[9],r2results[10],r2results[11],r2results[12])
  
  r3 <- data.frame("Seed" = rep(NA,4), 
                   "Team" = c("", "", "",""),
                   "Conf" = c("", "", "", ""), 
                   "ConfSeed" = rep(NA,4),
                   stringsAsFactors = FALSE)
  #Taking series winners from west first round and advancing them
  for(i in 1:2){
    if(isTRUE(west2$highW[i]>west2$lowW[i])){
      r3$Team[i] <- west2$high[i]
      r3$Conf[i] <- "W"
      r3$Seed[i] <- r1$Seed[which(r1$Team==west2$high[i])]
      r3$ConfSeed[i] <- r1$ConfSeed[which(r1$Team==west2$high[i])]
    }
    else{
      r3$Team[i] <- west2$low[i]
      r3$Conf[i] <- "W"
      r3$Seed[i] <- r1$Seed[which(r1$Team==west2$low[i])]
      r3$ConfSeed[i] <- r1$ConfSeed[which(r1$Team==west2$low[i])]
    }
  }
  #Taking series winners from east first round and advancing them
  for(i in 1:2){
    if(isTRUE(east2$highW[i]>east2$lowW[i])){
      r3$Team[i+2] <- east2$high[i]
      r3$Conf[i+2] <- "E"
      r3$Seed[i+2] <- r1$Seed[which(r1$Team==east2$high[i])]
      r3$ConfSeed[i+2] <- r1$ConfSeed[which(r1$Team==east2$high[i])]
    }
    else{
      r3$Team[i+2] <- east2$low[i]
      r3$Conf[i+2] <- "E"
      r3$Seed[i+2] <- r1$Seed[which(r1$Team==east2$low[i])]
      r3$ConfSeed[i+2] <- r1$ConfSeed[which(r1$Team==east2$low[i])]
    }
  }
  
  r3results <- r3SimCurr(r3)
  #Reconstructing sim results
  west3 <- data.frame(r3results[1],r3results[2],r3results[3],r3results[4], stringsAsFactors = FALSE)
  east3 <- data.frame(r3results[5],r3results[6],r3results[7],r3results[8], stringsAsFactors = FALSE)
  playoffSim1 <- c(playoffSim1,west3,east3,r3results[9],r3results[10],r3results[11],r1results[12])
  
  
  finals <- data.frame("Seed" = rep(NA,2), 
                       "Team" = c("", ""),
                       "Conf" = c("", ""), 
                       "ConfSeed" = rep(NA,2),
                       stringsAsFactors = FALSE)
  #Taking series winner from west and advancing them
  i <- 1
  if(isTRUE(west3$highW[i]>west3$lowW[i])){
    finals$Team[i] <- west3$high[i]
    finals$Conf[i] <- "W"
    finals$Seed[i] <- r1$Seed[which(r1$Team==west3$high[i])]
    finals$ConfSeed[i] <- r1$ConfSeed[which(r1$Team==west3$high[i])]
  }else{
    finals$Team[i] <- west3$low[i]
    finals$Conf[i] <- "W"
    finals$Seed[i] <- r1$Seed[which(r1$Team==west3$low[i])]
    finals$ConfSeed[i] <- r1$ConfSeed[which(r1$Team==west3$low[i])]
  }
  
  #Taking series winners from east first round and advancing them
  i <- 1
  if(isTRUE(east3$highW[i]>east3$lowW[i])){
    finals$Team[i+1] <- east3$high[i]
    finals$Conf[i+1] <- "E"
    finals$Seed[i+1] <- r1$Seed[which(r1$Team==east3$high[i])]
    finals$ConfSeed[i+1] <- r1$ConfSeed[which(r1$Team==east3$high[i])]
  }else{
    finals$Team[i+1] <- east3$low[i]
    finals$Conf[i+1] <- "E"
    finals$Seed[i+1] <- r1$Seed[which(r1$Team==east3$low[i])]
    finals$ConfSeed[i+1] <- r1$ConfSeed[which(r1$Team==east3$low[i])]
  }
  finals <- finals %>%
    arrange(.,Seed)
  
  finalsresults <- finalsSimCurr(finals)
  #Reconstructing sim results
  finals_mu <- data.frame(finalsresults[1],finalsresults[2],finalsresults[3],finalsresults[4], stringsAsFactors = FALSE)
  playoffSim1 <- c(playoffSim1,finals_mu,finalsresults[5],finalsresults[6],finalsresults[7],finalsresults[8],finalsresults[9])
}



#Figuring out the indexing for highOTprob series, upsets, number of 6/7 game series
upsetIndex1 <- rep(NA,(4*S))
s <- 1
#Inital locations of upsets in playoffSim list
upsetIndex1[1] <- 10;upsetIndex1[2] <- 22; upsetIndex1[3] <- 34; upsetIndex1[4] <- 42
for(i in 5:(4*S)){
  #Each sim is 45 elements long in this format
  upsetIndex1[i] <- upsetIndex1[i-4]+45
}

#Number of games in each round with high probability of OT
highOT1 <- rep(NA,length(upsetIndex1))
s <- 1
for(i in 1:length(upsetIndex1)){
  otIndex1 <- upsetIndex1[i]-1
  highOT1[s] <- playoffSim1[[otIndex1]]
  s <- s + 1
}


#Number of upsets in a round divided by total number of games
upsets1 <- rep(NA,length(upsetIndex1))
s <- 1
for(i in 1:length(upsetIndex1)){
  upsets1[s] <- playoffSim1[[upsetIndex1[i]]]
  s <- s + 1
}


#Number of series that went to 6 or 7 games
longSeries1 <- rep(NA,length(upsetIndex1))
s <- 1
for(i in 1:length(upsetIndex1)){
  g6g7Index1 <- upsetIndex1[i]+1
  longSeries1[s] <- playoffSim1[[g6g7Index1]]
  s <- s + 1
}

#Champions
champs1 <- rep(NA,S)
s <- 1
for(i in 1:S){
  #Champions are crowned every 41st entry in the playoffSim1 list
  champIndex1 <- 45 * i
  champs1[i] <- playoffSim1[[champIndex1]]
}
champs1 <- data.frame(champs1)
champsfreq1 <- data.frame(table(champs1))

#Getting colors of teams for plot
library(teamcolors)
champscol1 <- intersect(allTeams, champsfreq1$champs1)
primcolors1 <- gather(data.frame(lapply(sort(champscol1), team_pal))[1,])$value

#### 8W8E Format ####
#Doing 20000 series simulations
S <- 10000
playoffSim2 <- list()
for(s in 1:S){
  #Gathering teams that would make the playoffs if the season ended on March 11, 2020
  r1 <- data.frame("Seed" = 1:16, 
                   "Team" = c("Milwaukee Bucks", "Los Angeles Lakers", "Toronto Raptors",
                              "Los Angeles Clippers", "Boston Celtics", "Denver Nuggets",
                              "Utah Jazz", "Miami Heat", "Oklahoma City Thunder",
                              "Houston Rockets", "Indiana Pacers", "Philadelphia 76ers", 
                              "Dallas Mavericks","Memphis Grizzlies", "Brooklyn Nets", 
                              "Orlando Magic"),
                   "Conf" = c("E", "W", "E", "W", "E", "W", "W","E","W",
                              "W", "E", "E", "W", "W", "E","E"), 
                   stringsAsFactors = FALSE)
  
  r1results <- r1Sim88(r1)
  #Reconstructing sim results
  round1 <- data.frame(r1results[1],r1results[2],r1results[3],r1results[4], stringsAsFactors = FALSE)
  playoffSim2 <- c(playoffSim2,round1,r1results[5],r1results[6],r1results[7],r1results[8])
  
  
  r2 <- data.frame("Seed" = rep(NA,8), 
                   "Team" = c("", "", "",
                              "", "", "",
                              "", ""),
                   stringsAsFactors = FALSE)
  #Taking series winners from first round and advancing them
  for(i in 1:8){
    if(round1$highW[i]>round1$lowW[i]){
      r2$Team[i] <- round1$high[i]
      r2$Seed[i] <- r1$Seed[which(r1$Team==round1$high[i])]
    }
    else{
      r2$Team[i] <- round1$low[i]
      r2$Seed[i] <- r1$Seed[which(r1$Team==round1$low[i])]
    }
  }
  for(i in 1:4){
    if(r2$Seed[i] > r2$Seed[nrow(r2)-i+1]){
      tmp <- r2[i,]
      r2[i,] <- r2[nrow(r2)-i+1,]
      r2[nrow(r2)-i+1,] <- tmp
    }
    else{
    }
  }
  
  r2results <- r2Sim88(r2)
  #Reconstructing sim results
  round2 <- data.frame(r2results[1],r2results[2],r2results[3],r2results[4], stringsAsFactors = FALSE)
  playoffSim2 <- c(playoffSim2,round2,r2results[5],r2results[6],r2results[7],r2results[8])
  
  
  r3 <- data.frame("Seed" = rep(NA,4), 
                   "Team" = c("", "", "",""),
                   stringsAsFactors = FALSE)
  #Taking series winners from second round and advancing them
  for(i in 1:4){
    if(isTRUE(round2$highW[i]>round2$lowW[i])){
      r3$Team[i] <- round2$high[i]
      r3$Seed[i] <- r1$Seed[which(r1$Team==round2$high[i])]
    }
    else{
      r3$Team[i] <- round2$low[i]
      r3$Seed[i] <- r1$Seed[which(r1$Team==round2$low[i])]
    }
  }
  for(i in 1:2){
    if(r3$Seed[i] > r3$Seed[nrow(r3)-i+1]){
      tmp <- r3[i,]
      r3[i,] <- r3[nrow(r3)-i+1,]
      r3[nrow(r3)-i+1,] <- tmp
    }
    else{
    }
  }
  
  r3results <- r3Sim88(r3)
  #Reconstructing sim results
  round3 <- data.frame(r3results[1],r3results[2],r3results[3],r3results[4], stringsAsFactors = FALSE)
  playoffSim2 <- c(playoffSim2,round3,r3results[5],r3results[6],r3results[7],r1results[8])
  
  finals <- data.frame("Seed" = rep(NA,2), 
                       "Team" = c("", ""),
                       stringsAsFactors = FALSE)
  #Taking series winners and advancing them
  for(i in 1:2){
    if(isTRUE(round3$highW[i]>round3$lowW[i])){
      finals$Team[i] <- round3$high[i]
      finals$Seed[i] <- r1$Seed[which(r1$Team==round3$high[i])]
    }else{
      finals$Team[i] <- round3$low[i]
      finals$Seed[i] <- r1$Seed[which(r1$Team==round3$low[i])]
    }
  }
  if(finals$Seed[1] > finals$Seed[2]){
    tmp <- finals[1,]
    finals[1,] <- finals[2,]
    finals[2,] <- tmp
  }
  
  finalsresults <- finalsSim88(finals)
  #Reconstructing sim results
  finals_mu <- data.frame(finalsresults[1],finalsresults[2],finalsresults[3],finalsresults[4], stringsAsFactors = FALSE)
  playoffSim2 <- c(playoffSim2,finals_mu,finalsresults[5],finalsresults[6],finalsresults[7],finalsresults[8],finalsresults[9])
}


#Figuring out the indexing for highOTprob series, upsets, number of 6/7 game series
upsetIndex2 <- rep(NA,(4*S))
s <- 1
#Inital locations of upsets in playoffSim list
upsetIndex2[1] <- 6;upsetIndex2[2] <- 14; upsetIndex2[3] <- 22; upsetIndex2[4] <- 30
for(i in 5:(4*S)){
  #Each sim is 33 elements long in this format
  upsetIndex2[i] <- upsetIndex2[i-4]+33
}

#Number of games in each round with high probability of OT
highOT2 <- rep(NA,length(upsetIndex2))
s <- 1
for(i in 1:length(upsetIndex2)){
  otIndex2 <- upsetIndex2[i]-1
  highOT2[s] <- playoffSim2[[otIndex2]]
  s <- s + 1
}


#Number of upsets in a round divided by total number of games
upsets2 <- rep(NA,length(upsetIndex2))
s <- 1
for(i in 1:length(upsetIndex2)){
  upsets2[s] <- playoffSim2[[upsetIndex2[i]]]
  s <- s + 1
}


#Number of series that went to 6 or 7 games
longSeries2 <- rep(NA,length(upsetIndex2))
s <- 1
for(i in 1:length(upsetIndex2)){
  g6g7Index2 <- upsetIndex2[i]+1
  longSeries2[s] <- playoffSim2[[g6g7Index2]]
  s <- s + 1
}

#Champions
champs2 <- rep(NA,S)
s <- 1
for(i in 1:S){
  #Champions are crowned every 33rd entry in the playoffSim2 list
  champIndex2 <- 33 * i
  champs2[i] <- playoffSim2[[champIndex2]]
}
champs2 <- data.frame(champs2)
champsfreq2 <- data.frame(table(champs2))

#Getting colors of teams for plot
library(teamcolors)
champscol2 <- intersect(allTeams, champsfreq2$champs2)
primcolors2 <- gather(data.frame(lapply(sort(champscol2), team_pal))[1,])$value




