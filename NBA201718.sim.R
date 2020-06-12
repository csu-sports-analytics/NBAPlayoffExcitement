#Must get scores from 2016/17 and 2017/18 because this is the last time this
#scenario would be applicable

#Using data from the 2016/17 and 2017/18 seasons
years <- c(2017:2018)

#Creating complete schedule
games <- data.frame()


for(i in 1:length(years)){
  #Establishing which months in each other years NBA games were played
  if(years[i]==2017){
    months <- c("october","november","december","january","february","march","april","may","june")
  }
  else if(years[i]==2018){
    months <- c("october","november","december","january", "february","march","april")
  }
  else{}
  for(j in 1:length(months)){
    #Creating URL and gathering table
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",years[i],"_games-",months[j],".html")
    url_new <- getURL(url)
    table <- readHTMLTable(url_new)
    month_sched <- table$schedule
    #Playoffs begin in April and there is a row that is empty that must be removed
    if(years[i]==2018 & months[j]=="april"){
      month_sched <- month_sched[-c(which(month_sched$Date=="Playoffs"):nrow(month_sched)),]
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

teams <- unique(games$Team)[1:30]

#Adding PtsDiff to games
games <- games %>%
  mutate(PtsDiff = TeamPts-OppPts)

#Days since the game for weights for model (relative to when playoffs started in 2018)
games$DaysSince <- as.numeric(as.Date("2018-4-14")-games$Date)
#Which season the game was in, also for weighting model
#1 if 2016/17, 3 if 2017/28 so that games from this season are weighted heavier
#but last season is still accounted for
games <- games %>%
  mutate(Season = if_else(Date < as.Date("2017-10-16"), as.numeric(1), as.numeric(3)))
#Weight games based on how long ago they were
games <- games %>%
  mutate(., Game_Weight = games$Season*exp(-games$DaysSince/games$DaysSince[1]))
#Adding "H" for location
games <- games %>%
  mutate(., Location = "H")
#Cleaning data
games <- games %>%
  select(., Team, Opponent, PtsDiff, Location, Game_Weight)
#Adding games from away teams' perspectives
games_away <- games %>%
  select(., Opponent, Team, PtsDiff, Location, Game_Weight) %>%
  mutate(., PtsDiff = -1*PtsDiff) %>%
  mutate(., Location = "A")
names(games_away)=c("Team", "Opponent", "PtsDiff", "Location", "Game_Weight")
games <- rbind(games, games_away)


#Making linear model to predict team score
lm.nba.old <- lm(PtsDiff ~ Team + Opponent + Location, 
                 data = games,
                 weights = Game_Weight)

#GLM to find prob of winning giving expected points spread
games <- games %>%
  select(., Team, Opponent, PtsDiff, Location)
games$win <- ifelse(games$PtsDiff > 0,1,0)
games$exp_pts_diff <- predict(lm.nba.old, newdata = games, type = "response")
glm.spread.old <- glm(win ~ exp_pts_diff, data = games, family = "binomial")

#Updating gameSim to use 2017/18 model alongside gameSim
gameSim <- function(high, low, game){
  #Higher seed is home
  if(isTRUE(game<3 | game == 5 | game == 7)){
    gamedf <- data.frame("Team" = high, "Opponent" = low, "Location" = "H", stringsAsFactors = F)
  }else{
    #Lower seed is home
    gamedf <- data.frame("Team" = high, "Opponent" = low, "Location" = "A", stringsAsFactors = F)
  }
  #Predicting game points difference
  gamedf$exp_pts_diff <- as.numeric(as.character(unlist(predict.lm(lm.nba.old, newdata = gamedf, se.fit = TRUE, type = "response")[[1]])))
  
  #Odds of points high seed winning given pts diff
  win_prob <- predict.glm(glm.spread.old, newdata = gamedf, type = "response")
  
  #Returns TRUE or FALSE based on win probability
  #If TRUE, the higher seed won, if FALSE, the lower seed won
  return(c(rbernoulli(1,win_prob), win_prob, gamedf$exp_pts_diff))
}

teams1718 <- data.frame("Seed" = 1:16, 
           "Team" = c("Houston Rockets", "Toronto Raptors", "Golden State Warriors",
                      "Boston Celtics", "Philadelphia 76ers", "Cleveland Cavaliers",
                      "Portland Trail Blazers", "Oklahoma City Thunder", "Indiana Pacers", 
                      "Utah Jazz", "New Orleans Pelicans", "San Antonio Spurs",
                      "Minnesota Timberwolves", "Miami Heat", 
                      "Milwaukee Bucks", "Washington Wizards"),
           "Conf" = c("W", "E", "W", "E", "E", "E", "W","W","E",
                      "W", "W", "W", "W", "E", "E","E"), 
           stringsAsFactors = FALSE)


## Current Format
#Doing 20000 series simulations
S <- 100
playoffSim1718Curr <- list()
for(s in 1:S){
  #Gathering teams that would made the playoffs
  r1 <- data.frame("Seed" = 1:16, 
                   "Team" = c("Houston Rockets", "Toronto Raptors", "Golden State Warriors",
                              "Boston Celtics", "Philadelphia 76ers", "Cleveland Cavaliers",
                              "Portland Trail Blazers", "Oklahoma City Thunder", "Indiana Pacers", 
                              "Utah Jazz", "New Orleans Pelicans", "San Antonio Spurs",
                              "Minnesota Timberwolves", "Miami Heat", 
                              "Milwaukee Bucks", "Washington Wizards"),
                   "Conf" = c("W", "E", "W", "E", "E", "E", "W","W","E",
                              "W", "W", "W", "W", "E", "E","E"), 
                   stringsAsFactors = FALSE)
  #Seeding each team based on conference
  r1 <- r1 %>%
    group_by(., Conf) %>%
    mutate(ConfSeed = row_number())
  
  r1results <- r1SimCurr(r1)
  #Reconstructing sim results
  west1 <- data.frame(r1results[1],r1results[2],r1results[3],r1results[4], stringsAsFactors = FALSE)
  east1 <- data.frame(r1results[5],r1results[6],r1results[7],r1results[8], stringsAsFactors = FALSE)
  playoffSim1718Curr <- c(playoffSim1718Curr,west1,east1,r1results[9],r1results[10],r1results[11],r1results[12])
  
  
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
  playoffSim1718Curr <- c(playoffSim1718Curr,west2,east2,r2results[9],r2results[10],r2results[11],r2results[12])
  
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
  playoffSim1718Curr <- c(playoffSim1718Curr,west3,east3,r3results[9],r3results[10],r3results[11],r1results[12])
  
  
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
  playoffSim1718Curr <- c(playoffSim1718Curr,finals_mu,finalsresults[5],finalsresults[6],finalsresults[7],finalsresults[8],finalsresults[9])
}


#Figuring out the indexing for highOTprob series, upsets, number of 6/7 game series and close games
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
  highOT1[s] <- playoffSim1718Curr[[otIndex1]]
  s <- s + 1
}


#Number of upsets in a round divided by total number of games
upsets1 <- rep(NA,length(upsetIndex1))
s <- 1
for(i in 1:length(upsetIndex1)){
  upsets1[s] <- playoffSim1718Curr[[upsetIndex1[i]]]
  s <- s + 1
}


#Number of series that went to 6 or 7 games
longSeries1 <- rep(NA,length(upsetIndex1))
s <- 1
for(i in 1:length(upsetIndex1)){
  g6g7Index1 <- upsetIndex1[i]+1
  longSeries1[s] <- playoffSim1718Curr[[g6g7Index1]]
  s <- s + 1
}

#Number of series that went to 6 or 7 games
closeGames1 <- rep(NA,length(upsetIndex1))
s <- 1
for(i in 1:length(upsetIndex1)){
  closeIndex1 <- upsetIndex1[i]+2
  closeGames1[s] <- playoffSim1718Curr[[closeIndex1]]
  s <- s + 1
}

#Champions
champs1 <- rep(NA,S)
s <- 1
for(i in 1:S){
  #Champions are crowned every 41st entry in the playoffSim1718Curr, list
  champIndex1 <- 45 * i
  champs1[i] <- playoffSim1718Curr[[champIndex1]]
}
champs1 <- data.frame(champs1)
champsfreq1 <- data.frame(table(champs1))

#Getting colors of teams for plot
library(teamcolors)
allTeams <- unique(games$Team)
champscol1 <- intersect(allTeams, champsfreq1$champs1)
primcolors1 <- gather(data.frame(lapply(sort(champscol1), team_pal))[1,])$value


# Current format champions plot
ggplot(data = champsfreq1, aes(x = champs1, y = Freq)) + geom_bar(stat = "identity", aes(fill = champs1)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors1) + geom_text(aes(label=Freq, vjust = -.25))+
  labs(title = "Frequency of Simulated Championships (20,000 sims)", x = "Team", y = "Frequency", subtitle = "Current Playoff Format for 2017/18 Season")










#8 West, 8 East Format
#Doing 20000 series simulations
S <- 100
playoffSim17188W8E <- list()
for(s in 1:S){
  #Gathering teams that made the playoffs
  r1 <- data.frame("Seed" = 1:16, 
                   "Team" = c("Houston Rockets", "Toronto Raptors", "Golden State Warriors",
                              "Boston Celtics", "Philadelphia 76ers", "Cleveland Cavaliers",
                              "Portland Trail Blazers", "Oklahoma City Thunder", "Indiana Pacers", 
                              "Utah Jazz", "New Orleans Pelicans", "San Antonio Spurs",
                              "Minnesota Timberwolves", "Miami Heat", 
                              "Milwaukee Bucks", "Washington Wizards"),
                   "Conf" = c("W", "E", "W", "E", "E", "E", "W","W","E",
                              "W", "W", "W", "W", "E", "E","E"), 
                   stringsAsFactors = FALSE)
  
  r1results <- r1Sim88(r1)
  #Reconstructing sim results
  round1 <- data.frame(r1results[1],r1results[2],r1results[3],r1results[4], stringsAsFactors = FALSE)
  playoffSim17188W8E <- c(playoffSim17188W8E,round1,r1results[5],r1results[6],r1results[7],r1results[8])
  
  
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
  playoffSim17188W8E <- c(playoffSim17188W8E,round2,r2results[5],r2results[6],r2results[7],r2results[8])
  
  
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
  playoffSim17188W8E <- c(playoffSim17188W8E,round3,r3results[5],r3results[6],r3results[7],r1results[8])
  
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
  playoffSim17188W8E <- c(playoffSim17188W8E,finals_mu,finalsresults[5],finalsresults[6],finalsresults[7],finalsresults[8],finalsresults[9])
}





#Figuring out the indexing for highOTprob series, upsets, number of 6/7 game series and close games
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
  highOT2[s] <- playoffSim17188W8E[[otIndex2]]
  s <- s + 1
}


#Number of upsets in a round divided by total number of games
upsets2 <- rep(NA,length(upsetIndex2))
s <- 1
for(i in 1:length(upsetIndex2)){
  upsets2[s] <- playoffSim17188W8E[[upsetIndex2[i]]]
  s <- s + 1
}


#Number of series that went to 6 or 7 games
longSeries2 <- rep(NA,length(upsetIndex2))
s <- 1
for(i in 1:length(upsetIndex2)){
  g6g7Index2 <- upsetIndex2[i]+1
  longSeries2[s] <- playoffSim17188W8E[[g6g7Index2]]
  s <- s + 1
}

#Number of series that went to 6 or 7 games
closeGames2 <- rep(NA,length(upsetIndex2))
s <- 1
for(i in 1:length(upsetIndex2)){
  closeIndex2 <- upsetIndex2[i]+2
  closeGames2[s] <- playoffSim17188W8E[[closeIndex2]]
  s <- s + 1
}

#Champions
champs2 <- rep(NA,S)
s <- 1
for(i in 1:S){
  #Champions are crowned every 33rd entry in the playoffSim17188W8E list
  champIndex2 <- 33 * i
  champs2[i] <- playoffSim17188W8E[[champIndex2]]
}
champs2 <- data.frame(champs2)
champsfreq2 <- data.frame(table(champs2))

#Getting colors of teams for plot
library(teamcolors)
champscol2 <- intersect(allTeams, champsfreq2$champs2)
primcolors2 <- gather(data.frame(lapply(sort(champscol2), team_pal))[1,])$value
# 8 West, 8 East
ggplot(data = champsfreq2, aes(x = champs2, y = Freq)) + geom_bar(stat = "identity", aes(fill = champs2)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors2) + geom_text(aes(label=Freq, vjust = -.25))+
  labs(title = "Frequency of Simulated Championships (20,000 sims)", x = "Team", y = "Frequency", subtitle = "8 West and 8 East, Conference-less Playoff Format  for 2017/18 Season")






#Doing 20000 series simulations
S <- 100
playoffSim171816 <- list()
for(s in 1:S){
  #Gathering teams that would make the playoffs if the top 16 teams in the NBA were chosen
  r1 <- data.frame("Seed" = 1:16, 
                   "Team" = c("Houston Rockets", "Toronto Raptors", "Golden State Warriors",
                              "Boston Celtics", "Philadelphia 76ers", "Cleveland Cavaliers",
                              "Portland Trail Blazers", "Oklahoma City Thunder", "Indiana Pacers", 
                              "Utah Jazz", "New Orleans Pelicans", "San Antonio Spurs",
                              "Minnesota Timberwolves", "Denver Nuggets",
                              "Miami Heat", "Milwaukee Bucks"),
                   "Conf" = c("W", "E", "W", "E", "E", "E", "W","W","E",
                              "W", "W", "W", "W", "W", "E","E"), 
                   stringsAsFactors = FALSE)
  
  r1results <- r1Sim16(r1)
  #Reconstructing sim results
  round1 <- data.frame(r1results[1],r1results[2],r1results[3],r1results[4], stringsAsFactors = FALSE)
  playoffSim171816 <- c(playoffSim171816,round1,r1results[5],r1results[6],r1results[7],r1results[8])
  
  
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
  
  r2results <- r2Sim16(r2)
  #Reconstructing sim results
  round2 <- data.frame(r2results[1],r2results[2],r2results[3],r2results[4], stringsAsFactors = FALSE)
  playoffSim171816 <- c(playoffSim171816,round2,r2results[5],r2results[6],r2results[7],r2results[8])
  
  
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
  
  r3results <- r3Sim16(r3)
  #Reconstructing sim results
  round3 <- data.frame(r3results[1],r3results[2],r3results[3],r3results[4], stringsAsFactors = FALSE)
  playoffSim171816 <- c(playoffSim171816,round3,r3results[5],r3results[6],r3results[7],r1results[8])
  
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
  
  finalsresults <- finalsSim16(finals)
  #Reconstructing sim results
  finals_mu <- data.frame(finalsresults[1],finalsresults[2],finalsresults[3],finalsresults[4], stringsAsFactors = FALSE)
  playoffSim171816 <- c(playoffSim171816,finals_mu,finalsresults[5],finalsresults[6],finalsresults[7],finalsresults[8],finalsresults[9])
}




#Figuring out the indexing for highOTprob series, upsets, number of 6/7 game series and close games
upsetIndex3 <- rep(NA,(4*S))
s <- 1
#Inital locations of upsets in playoffSim list
upsetIndex3[1] <- 6;upsetIndex3[2] <- 14; upsetIndex3[3] <- 22; upsetIndex3[4] <- 30
for(i in 5:(4*S)){
  #Each sim is 37 elements long in this format
  upsetIndex3[i] <- upsetIndex3[i-4]+33
}

#Number of games in each round with high probability of OT
highOT3 <- rep(NA,length(upsetIndex3))
s <- 1
for(i in 1:length(upsetIndex3)){
  otIndex3 <- upsetIndex3[i]-1
  highOT3[s] <- playoffSim171816[[otIndex3]]
  s <- s + 1
}


#Number of upsets in a round divided by total number of games
upsets3 <- rep(NA,length(upsetIndex3))
s <- 1
for(i in 1:length(upsetIndex3)){
  upsets3[s] <- playoffSim171816[[upsetIndex3[i]]]
  s <- s + 1
}


#Number of series that went to 6 or 7 games
longSeries3 <- rep(NA,length(upsetIndex3))
s <- 1
for(i in 1:length(upsetIndex3)){
  g6g7Index3 <- upsetIndex3[i]+1
  longSeries3[s] <- playoffSim171816[[g6g7Index3]]
  s <- s + 1
}

#Number of series that went to 6 or 7 games
closeGames3 <- rep(NA,length(upsetIndex3))
s <- 1
for(i in 1:length(upsetIndex3)){
  closeIndex3 <- upsetIndex3[i]+2
  closeGames3[s] <- playoffSim171816[[closeIndex3]]
  s <- s + 1
}

#Champions
champs3 <- rep(NA,S)
s <- 1
for(i in 1:S){
  #Champions are crowned every 33rd entry in the playoffSim171816 list
  champIndex3 <- 33 * i
  champs3[i] <- playoffSim171816[[champIndex3]]
}
champs3 <- data.frame(champs3)
champsfreq3 <- data.frame(table(champs3))

#Getting colors of teams for plot
library(teamcolors)
champscol3 <- intersect(allTeams, champsfreq3$champs3)
primcolors3 <- gather(data.frame(lapply(sort(champscol3), team_pal))[1,])$value

# 16 team
ggplot(data = champsfreq3, aes(x = champs3, y = Freq)) + geom_bar(stat = "identity", aes(fill = champs3)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors3) + geom_text(aes(label=Freq, vjust = -.25))+
  labs(title = "Frequency of Simulated Championships (20,000 sims)", x = "Team", y = "Frequency", subtitle = "16 Total Teams, Conference-less Playoff Format for 2017/18 Season")


#Scraping real results and creating dataframe with counts of events
url <- getURL("https://www.basketball-reference.com/playoffs/NBA_2018_games.html")
real1718 <- as.data.frame(readHTMLTable(url))
real1718 <- real1718[,c(3,5,4,6,8)]
colnames(real1718) <- c("Vis","Home","VisPts","HomePts","OT")
#Creating unique series identifiers
real1718 <- real1718 %>%
  unite(series, 1:2, remove=FALSE)
real1718$series <- real1718$series %>% 
  str_split(., '_') %>% 
  lapply(., 'sort') %>%  
  lapply(., 'paste', collapse=' ') %>% 
  unlist(.)

real1718$Vis <- as.character(real1718$Vis)
real1718$Home <- as.character(real1718$Home)
real1718$VisPts <- as.numeric(as.character(real1718$VisPts))
real1718$HomePts <- as.numeric(as.character(real1718$HomePts))
real1718$OT <- as.character(real1718$OT)

install.packages("operators")
library(operators)
seriesTracker <- NA
for(i in 1:nrow(real1718)){
  if(isTRUE(real1718$series[i] %!in% seriesTracker)){
    seriesTracker <- c(seriesTracker, real1718$series[i])
  }
  else{}
}
seriesTracker <- na.omit(seriesTracker)[1:15]
Round <- c(rep(1,8),rep(2,4),rep(3,2),4)
seriesTracker <- as.data.frame(cbind(seriesTracker,Round))
seriesTracker$seriesTracker <- as.character(seriesTracker$seriesTracker)
seriesTracker$Round <- as.integer(seriesTracker$Round)
for(i in 1:nrow(real1718)){
  real1718$Round[i] <- seriesTracker$Round[which(seriesTracker$seriesTracker == real1718$series[i])]
}


#Getting number of games in each round
numGames <- count(real1718, Round)
numGames <- rbind(numGames, nrow(real1718)); numGames$Round[5] <- "Tot"

#Number of OT's in each round
numOT <- as.data.frame(count(real1718, Round, OT))
numOT$OT <- as.character(numOT$OT)
numOT <- filter(numOT, OT == "OT")
numOT <- numOT[,c(1,3)]
numOT <- rbind(numOT,c(NA,0),c(NA,0))
numOT$Round[c(4,5)] <- c("3","Tot")
numOT$n[5] <- sum(numOT$n)
numOT <- arrange(numOT, Round)

#Getting number of long series in each round
seriesGames <- as.data.frame(count(real1718, series))
Round <- rep(NA,15)
seriesGames <- cbind(seriesGames, Round)
for(i in 1:15){
  seriesGames$Round[i] <- as.integer(seriesTracker$Round[which(seriesGames$series[i]==seriesTracker$seriesTracker)])
}
numLongSeries <- data.frame(c(1:4, "Tot"),rep(0,5))
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
numClose <- data.frame(c(1:4, "Tot"),rep(0,5))
colnames(numClose) <- c("Round", "n")
closeInd <- which(abs(real1718$VisPts-real1718$HomePts) <= 3)
numClose$n[5] <- length(closeInd)
for(i in 1:length(closeInd)){
  j <- closeInd[i]
  numClose$n[real1718$Round[j]] <- numClose$n[real1718$Round[j]] + 1
}

#Getting Number of Upsets
VisSeed <- rep(0,nrow(real1718))
HomeSeed <- rep(0, nrow(real1718))
Upset <- rep(0,nrow(real1718))
real1718 <- cbind(real1718,VisSeed,HomeSeed,Upset)
for(i in 1:nrow(real1718)){
  real1718$VisSeed[i] <- which(teams1718$Team == real1718$Vis[i])
  real1718$HomeSeed[i] <- which(teams1718$Team == real1718$Home[i])
  if(real1718$HomePts[i] > real1718$VisPts[i] & real1718$HomeSeed[i] > real1718$VisSeed[i]){
    real1718$Upset[i] <- 1
  }
  else if(real1718$HomePts[i] < real1718$VisPts[i] & real1718$HomeSeed[i] < real1718$VisSeed[i]){
    real1718$Upset[i] <- 1
  }
  else{}
}
numUpset <- data.frame(c(1:4, "Tot"),rep(0,5))
colnames(numUpset) <- c("Round", "n")
upsetInd <- which(real1718$Upset == 1)
numUpset$n[5] <- length(upsetInd)
for(i in 1:length(upsetInd)){
  j <- upsetInd[i]
  numUpset$n[real1718$Round[j]] <- numUpset$n[real1718$Round[j]] + 1
}

#Getting percentages
numOT$n <- numOT$n/numGames$n
numLongSeries$n <- numLongSeries$n/c(8,4,2,1,15)
numClose$n <- numClose$n/numGames$n
numUpset$n <- numUpset$n/numGames$n


devtools::unload("operators")
#Function that runs Tukey's HSD and gives data their groups
tukey_label <- function(df, hsd){
  df$Group <- NA
  for(i in 1:nrow(hsd$groups)){
    df$Group[which(rownames(hsd$groups)[i]==paste0(df$Format, ":",df$Round))] <- as.character(hsd$groups[i,2])
  }
  return(df)
}




#High Overtime for by rounds
#Round 1
highOT1r1 <- data.frame(Count = highOT1[seq(1, length(highOT1), 4)]) %>% mutate(., Round = 1, Format = "Current")
highOT2r1 <- data.frame(Count = highOT2[seq(1, length(highOT2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
highOT3r1 <- data.frame(Count = highOT3[seq(1, length(highOT3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
highOT1r2 <- data.frame(Count = highOT1[seq(2, length(highOT1), 4)]) %>% mutate(., Round = 2, Format = "Current")
highOT2r2 <- data.frame(Count = highOT2[seq(2, length(highOT2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
highOT3r2 <- data.frame(Count = highOT3[seq(2, length(highOT3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
highOT1r3 <- data.frame(Count = highOT1[seq(3, length(highOT1), 4)]) %>% mutate(., Round = 3, Format = "Current")
highOT2r3 <- data.frame(Count = highOT2[seq(3, length(highOT2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
highOT3r3 <- data.frame(Count = highOT3[seq(3, length(highOT3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
highOT1r4 <- data.frame(Count = highOT1[seq(4, length(highOT1), 4)]) %>% mutate(., Round = 4, Format = "Current")
highOT2r4 <- data.frame(Count = highOT2[seq(4, length(highOT2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
highOT3r4 <- data.frame(Count = highOT3[seq(4, length(highOT3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
highOT <- rbind(highOT1r1,highOT1r2,highOT1r3,highOT1r4,
                highOT2r1,highOT2r2,highOT2r3,highOT2r4,
                highOT3r1,highOT3r2,highOT3r3,highOT3r4)

#High OT HSD Tests (Separated by rounds)
library(agricolae)
lm.OT <- lm(Count ~ Format + Round, data = highOT)
HSD.OT <- HSD.test(lm.OT, c("Format", "Round")); HSD.OT$groups
highOT <- tukey_label(highOT, HSD.OT)
highOT <- highOT %>%
  mutate(., FR = paste0(Format,Round)) %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = highOT, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Games With High Probability of Overtime",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format", y = "# of Games with High OT Prob/Total # of Games Played") +
  geom_text(data=highOT, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = numOT[1:4,], aes(yintercept = n), col = "red", linetype = "dashed")

#High OT HSD Tests (Not separated by rounds)
highOT1 <- data.frame(Count = highOT1) %>% mutate(., Format = "Current")
highOT2 <- data.frame(Count = highOT2) %>% mutate(., Format = "8W8E")
highOT3 <- data.frame(Count = highOT3) %>% mutate(., Format = "16TOT")
highOTf <- rbind(highOT1,highOT2,highOT3)
lm.OTf <- lm(Count ~ Format, data = highOTf)
HSD.OTf <- HSD.test(lm.OTf, "Format"); HSD.OTf$groups
highOTf <- highOTf %>%
  mutate(., Group = if_else(Format == "Current", HSD.OTf$groups[1,2],
                            if_else(Format == "8W8E", HSD.OTf$groups[2,2], HSD.OTf$groups[3,2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = highOTf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Games With High Probability of Overtime",
       subtitle = "Across All Rounds",
       x = "Playoff Format", y = "# of Games with High OT Prob/Total # of Games Played") +
  geom_text(data=highOTf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1))+
  geom_hline(data = numOT[5,], aes(yintercept = n), col = "red", linetype = "dashed")






#Upset Ratio by rounds
#Round 1
upsets1r1 <- data.frame(Count = upsets1[seq(1, length(upsets1), 4)]) %>% mutate(., Round = 1, Format = "Current")
upsets2r1 <- data.frame(Count = upsets2[seq(1, length(upsets2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
upsets3r1 <- data.frame(Count = upsets3[seq(1, length(upsets3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
upsets1r2 <- data.frame(Count = upsets1[seq(2, length(upsets1), 4)]) %>% mutate(., Round = 2, Format = "Current")
upsets2r2 <- data.frame(Count = upsets2[seq(2, length(upsets2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
upsets3r2 <- data.frame(Count = upsets3[seq(2, length(upsets3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
upsets1r3 <- data.frame(Count = upsets1[seq(3, length(upsets1), 4)]) %>% mutate(., Round = 3, Format = "Current")
upsets2r3 <- data.frame(Count = upsets2[seq(3, length(upsets2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
upsets3r3 <- data.frame(Count = upsets3[seq(3, length(upsets3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
upsets1r4 <- data.frame(Count = upsets1[seq(4, length(upsets1), 4)]) %>% mutate(., Round = 4, Format = "Current")
upsets2r4 <- data.frame(Count = upsets2[seq(4, length(upsets2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
upsets3r4 <- data.frame(Count = upsets3[seq(4, length(upsets3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
upsets <- rbind(upsets1r1,upsets1r2,upsets1r3,upsets1r4,
                upsets2r1,upsets2r2,upsets2r3,upsets2r4,
                upsets3r1,upsets3r2,upsets3r3,upsets3r4)

#Upsets HSD Tests (Separated by rounds)
lm.upsets <- lm(Count ~ Format + Round, data = upsets)
library(agricolae)
HSD.upsets <- HSD.test(lm.upsets, c("Format", "Round")); HSD.upsets$groups
upsets <- tukey_label(upsets, HSD.upsets)
upsets <- upsets %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = upsets, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Upsets (Lower Seed Beat Higher Seed)",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format", y = "# of Upsets/Total # of Games Played") + 
  geom_text(data=upsets, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = numUpset[1:4,], aes(yintercept = n), col = "red", linetype = "dashed")

#Upsets HSD Tests (Not separated by rounds)
upsets1 <- data.frame(Count = upsets1) %>% mutate(., Format = "Current")
upsets2 <- data.frame(Count = upsets2) %>% mutate(., Format = "8W8E")
upsets3 <- data.frame(Count = upsets3) %>% mutate(., Format = "16TOT")
upsetsf <- rbind(upsets1,upsets2,upsets3)
lm.upsetsf <- lm(Count ~ Format, data = upsetsf)
HSD.upsetsf <- HSD.test(lm.upsetsf, "Format"); HSD.upsetsf$groups
upsetsf <- upsetsf %>%
  mutate(., Group = if_else(Format == "Current", HSD.upsetsf$groups[2,2],
                            if_else(Format == "8W8E", HSD.upsetsf$groups[3,2], HSD.upsetsf$groups[1,2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = upsetsf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Upsets (Lower Seed Beat Higher Seed)",
       subtitle = "Across All Rounds",
       x = "Playoff Format", y = "# of Upsets/Total # of Games Played") +
  geom_text(data=upsetsf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = numUpset[5,], aes(yintercept = n), col = "red", linetype = "dashed")




#Long series by rounds
#Round 1
longSeries1r1 <- data.frame(Count = longSeries1[seq(1, length(longSeries1), 4)]) %>% mutate(., Round = 1, Format = "Current")
longSeries2r1 <- data.frame(Count = longSeries2[seq(1, length(longSeries2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
longSeries3r1 <- data.frame(Count = longSeries3[seq(1, length(longSeries3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
longSeries1r2 <- data.frame(Count = longSeries1[seq(2, length(longSeries1), 4)]) %>% mutate(., Round = 2, Format = "Current")
longSeries2r2 <- data.frame(Count = longSeries2[seq(2, length(longSeries2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
longSeries3r2 <- data.frame(Count = longSeries3[seq(2, length(longSeries3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
longSeries1r3 <- data.frame(Count = longSeries1[seq(3, length(longSeries1), 4)]) %>% mutate(., Round = 3, Format = "Current")
longSeries2r3 <- data.frame(Count = longSeries2[seq(3, length(longSeries2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
longSeries3r3 <- data.frame(Count = longSeries3[seq(3, length(longSeries3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
longSeries1r4 <- data.frame(Count = longSeries1[seq(4, length(longSeries1), 4)]) %>% mutate(., Round = 4, Format = "Current")
longSeries2r4 <- data.frame(Count = longSeries2[seq(4, length(longSeries2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
longSeries3r4 <- data.frame(Count = longSeries3[seq(4, length(longSeries3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
longSeries <- rbind(longSeries1r1,longSeries1r2,longSeries1r3,longSeries1r4,
                    longSeries2r1,longSeries2r2,longSeries2r3,longSeries2r4,
                    longSeries3r1,longSeries3r2,longSeries3r3,longSeries3r4)

#Long Series HSD Tests (Separated by rounds)
lm.longSeries <- lm(Count ~ Format + Round, data = longSeries)
HSD.longSeries <- HSD.test(lm.longSeries, c("Format", "Round")); HSD.longSeries$groups
longSeries <- tukey_label(longSeries, HSD.longSeries)
longSeries <- longSeries %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, as.integer(Count), as.integer(NaN))) %>%
  ungroup(.)

ggplot(data = longSeries, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Long Series (6 and 7 Games)",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format", y = "# of Long Series/Total # of Series Played") +
  geom_text(data=longSeries, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = numLongSeries[1:4,], aes(yintercept = n), col = "red", linetype = "dashed")

#longSeries HSD Tests (Not separated by rounds)
longSeries1 <- data.frame(Count = longSeries1) %>% mutate(., Format = "Current")
longSeries2 <- data.frame(Count = longSeries2) %>% mutate(., Format = "8W8E")
longSeries3 <- data.frame(Count = longSeries3) %>% mutate(., Format = "16TOT")
longSeriesf <- rbind(longSeries1,longSeries2,longSeries3)
lm.longSeriesf <- lm(Count ~ Format, data = longSeriesf)
HSD.longSeriesf <- HSD.test(lm.longSeriesf, "Format"); HSD.longSeriesf$groups
longSeriesf <- longSeriesf %>%
  mutate(., Group = if_else(Format == "Current", HSD.longSeriesf$groups[2,2],
                            if_else(Format == "8W8E", HSD.longSeriesf$groups[1,2], HSD.longSeriesf$groups[3,2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = longSeriesf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Long Series (6 and 7 Games)",
       subtitle = "Across All Rounds",
       x = "Playoff Format", y = "# of Long Series/Total # of Series Played") +
  geom_text(data=longSeriesf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  geom_hline(data = numLongSeries[5,], aes(yintercept = n), col = "red", linetype = "dashed")




#Close games by rounds
#Round 1
closeGames1r1 <- data.frame(Count = closeGames1[seq(1, length(closeGames1), 4)]) %>% mutate(., Round = 1, Format = "Current")
closeGames2r1 <- data.frame(Count = closeGames2[seq(1, length(closeGames2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
closeGames3r1 <- data.frame(Count = closeGames3[seq(1, length(closeGames3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
closeGames1r2 <- data.frame(Count = closeGames1[seq(2, length(closeGames1), 4)]) %>% mutate(., Round = 2, Format = "Current")
closeGames2r2 <- data.frame(Count = closeGames2[seq(2, length(closeGames2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
closeGames3r2 <- data.frame(Count = closeGames3[seq(2, length(closeGames3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
closeGames1r3 <- data.frame(Count = closeGames1[seq(3, length(closeGames1), 4)]) %>% mutate(., Round = 3, Format = "Current")
closeGames2r3 <- data.frame(Count = closeGames2[seq(3, length(closeGames2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
closeGames3r3 <- data.frame(Count = closeGames3[seq(3, length(closeGames3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
closeGames1r4 <- data.frame(Count = closeGames1[seq(4, length(closeGames1), 4)]) %>% mutate(., Round = 4, Format = "Current")
closeGames2r4 <- data.frame(Count = closeGames2[seq(4, length(closeGames2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
closeGames3r4 <- data.frame(Count = closeGames3[seq(4, length(closeGames3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
closeGames <- rbind(closeGames1r1,closeGames1r2,closeGames1r3,closeGames1r4,
                    closeGames2r1,closeGames2r2,closeGames2r3,closeGames2r4,
                    closeGames3r1,closeGames3r2,closeGames3r3,closeGames3r4)

#Close Games HSD Tests (Separated by rounds)
lm.close <- lm(Count ~ Format + Round, data = closeGames)
HSD.close <- HSD.test(lm.close, c("Format", "Round")); HSD.close$groups
closeGames <- tukey_label(closeGames, HSD.close)
closeGames <- closeGames %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, as.integer(Count), as.integer(NaN))) %>%
  ungroup(.)

ggplot(data = closeGames, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Close Games (Predicted Pts Diff < 3)",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format", y = "# of Close Games/Total # of Games Played") +
  geom_text(data=closeGames, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = numClose[1:4,], aes(yintercept = n), col = "red", linetype = "dashed")

#longSeries HSD Tests (Not separated by rounds)
closeGames1 <- data.frame(Count = closeGames1) %>% mutate(., Format = "Current")
closeGames2 <- data.frame(Count = closeGames2) %>% mutate(., Format = "8W8E")
closeGames3 <- data.frame(Count = closeGames3) %>% mutate(., Format = "16TOT")
closeGamesf <- rbind(closeGames1,closeGames2,closeGames3)
lm.closef <- lm(Count ~ Format, data = closeGamesf)
HSD.closef <- HSD.test(lm.closef, "Format"); HSD.closef$groups
closeGamesf <- closeGamesf %>%
  mutate(., Group = if_else(Format == "Current", HSD.closef$groups[3,2],
                            if_else(Format == "8W8E", HSD.closef$groups[2,2], HSD.closef$groups[1,2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = closeGamesf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Close Games (Predicted Pts Diff < 3)",
       subtitle = "Across All Rounds",
       x = "Playoff Format", y = "# of Close Games/Total # of Games Played") +
  geom_text(data=closeGamesf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = numClose[5,], aes(yintercept = n), col = "red", linetype = "dashed")



#Just exploring the values that expected points differences take on
closeTestInd <- which(abs(real1718$VisPts-real1718$HomePts)<=3)
closeTest <- data.frame(high = rep(NA,length(closeTestInd)), low = rep(NA,length(closeTestInd)), game = rep(NA,length(closeTestInd)))
for(i in 1:length(closeTestInd)){
  ind <- closeTestInd[i]
  home <- real1718$Home[ind]
  vis <- real1718$Vis[ind]
  if(isTRUE(real1718$HomeSeed[ind] > real1718$VisSeed[ind])){
    closeTest$high[i] <- vis
    closeTest$low[i] <- home
    closeTest$game[i] <- 3
  }
  else{
    closeTest$high[i] <- home
    closeTest$low[i] <- vis
    closeTest$game[i] <- 1
  }
}
real1718[closeTestInd,]
summary(abs(gameSim(closeTest$high, closeTest$low, closeTest$game)[23:33]))




closeTest2 <- data.frame(high = rep(NA,82), low = rep(NA,82), game = rep(NA,82))
for(i in 1:82){
  home <- real1718$Home[i]
  vis <- real1718$Vis[i]
  if(isTRUE(real1718$HomeSeed[i] > real1718$VisSeed[i])){
    closeTest2$high[i] <- vis
    closeTest2$low[i] <- home
    closeTest2$game[i] <- 3
  }
  else{
    closeTest2$high[i] <- home
    closeTest2$low[i] <- vis
    closeTest2$game[i] <- 1
  }
}
summary(abs(gameSim(closeTest2$high, closeTest2$low, closeTest2$game)[165:246]))
unique(sort(abs(gameSim(closeTest2$high, closeTest2$low, closeTest2$game)[165:246])))
sum(abs(gameSim(closeTest2$high, closeTest2$low, closeTest2$game)[165:246])<= 3)
closeTest2[1,3]
gameSim(closeTest2[2,1], closeTest2[2,2], closeTest2[2,3])
