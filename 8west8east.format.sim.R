#Every series is best of 7 (2-2-1-1-1) so this will take the higher seed and give them
#home court advantage
gameSim <- function(high, low, game){
  #Higher seed is home
  if(isTRUE(game<3 | game == 5 | game == 7)){
    gamedf <- data.frame("Team" = high, "Opponent" = low, "Location" = "H", stringsAsFactors = F)
  }else{
    #Lower seed is home
    gamedf <- data.frame("Team" = high, "Opponent" = low, "Location" = "A", stringsAsFactors = F)
  }
  #Predicting game points difference
  gamedf$exp_pts_diff <- as.numeric(as.character(unlist(predict.lm(lm.nba, newdata = gamedf, se.fit = TRUE, type = "response")[[1]])))
  
  #Odds of points high seed winning given pts diff
  win_prob <- predict.glm(glm.spread, newdata = gamedf, type = "response")
  
  #Returns TRUE or FALSE based on win probability
  #If TRUE, the higher seed won, if FALSE, the lower seed won
  return(c(rbernoulli(1,win_prob), win_prob, gamedf$exp_pts_diff))
}


#Simulating First Round of Playoffs
r1Sim <- function(r1){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up first round matchups
  round1 <- data.frame(matrix(ncol=4,nrow=8))
  colnames(round1) <- c("high", "low", "highW", "lowW")
  for(i in 1:8){
    round1$high[i] <- r1$Team[i]
    round1$highW[i] <- 0
    round1$low[i] <- r1$Team[nrow(r1)-i+1]
    round1$lowW[i] <- 0
  }
  #There are 8 first round series
  for(s in 1:8){
    #Initializing values
    high <- round1$high[s]
    highW <- 0
    low <- round1$low[s]
    lowW <- 0
    #Series are best of 7
    for(g in 1:7){
      #Continue loop until a team has 4 wins
      if(highW != 4 & lowW != 4){
        outcome <- gameSim(high,low,g)
        numGame <- numGame+1
        #If outcome of sim is true, high team won the game
        if(isTRUE(outcome[[1]] == TRUE)){
          highW <- highW+1
        }else{
          #If outcome of sim is false, low team won the game
          lowW <- lowW+1
          numUpset <- numUpset +1
        }
        round1$highW[s] <- highW
        round1$lowW[s] <- lowW
        #If win prob is between .47 and .53 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.03){
          numHighOT <- numHighOT+1
        }
        else{}
        # If predicted pts diff is less than 3, this is very close, one possession game
        # that is likely going down to the wire
        if(abs(outcome[[3]])<3){
          numClose <- numClose + 1
        }
        else{}
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
      }
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(round1$highW+round1$lowW>5)
  return(c(round1,numHighOT,numUpset/numGame,numg6g7,numClose))
}


#Simulating Second Round of Playoffs
r2Sim <- function(r2){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up second round matchups
  round2 <- data.frame(matrix(ncol=4,nrow=4))
  colnames(round2) <- c("high", "low", "highW", "lowW")
  for(i in 1:4){
    round2$high[i] <- r2$Team[i]
    round2$highW[i] <- 0
    round2$low[i] <- r2$Team[nrow(r2)-i+1]
    round2$lowW[i] <- 0
  }
  #There are 4 second round series
  for(s in 1:4){
    #Initializing values
    high <- round2$high[s]
    highW <- 0
    low <- round2$low[s]
    lowW <- 0
    #Series are best of 7
    for(g in 1:7){
      #Continue loop until a team has 4 wins
      if(highW != 4 & lowW != 4){
        outcome <- gameSim(high,low,g)
        numGame <- numGame+1
        #If outcome of sim is true, high team won the game
        if(isTRUE(outcome[[1]] == TRUE)){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
          numUpset <- numUpset +1
        }
        round2$highW[s] <- highW
        round2$lowW[s] <- lowW
        #If win prob is between .47 and .53 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.03){
          numHighOT <- numHighOT+1
        }
        else{}
        # If predicted pts diff is less than 3, this is very close, one possession game
        # that is likely going down to the wire
        if(abs(outcome[[3]])<3){
          numClose <- numClose + 1
        }
        else{}
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
      }
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(round2$highW+round2$lowW>5)
  return(c(round2,numHighOT,numUpset/numGame,numg6g7,numClose))
}


#Simulating Third Round of Playoffs
r3Sim <- function(r3){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose3 <- 0
  #Setting up third round matchups
  round3 <- data.frame(matrix(ncol=4,nrow=2))
  colnames(round3) <- c("high", "low", "highW", "lowW")
  #There is only one matchup in each conference
  for(i in 1:2){
    round3$high[i] <- r3$Team[i]
    round3$highW[i] <- 0
    round3$low[i] <- r3$Team[nrow(r3)-i+1]
    round3$lowW[i] <- 0
  }
  #There are 2 third round series
  for(s in 1:2){
    #Initializing values
    high <- round3$high[s]
    highW <- 0
    low <- round3$low[s]
    lowW <- 0
    #Series are best of 7
    for(g in 1:7){
      #Continue loop until a team has 4 wins
      if(highW != 4 & lowW != 4){
        outcome <- gameSim(high,low,g)
        numGame <- numGame+1
        #If outcome of sim is true, high team won the game
        if(isTRUE(outcome[[1]] == TRUE)){
          highW <- highW+1
        }else{
          #If outcome of sim is false, low team won the game
          lowW <- lowW+1
          numUpset <- numUpset +1
        }
        round3$highW[s] <- highW
        round3$lowW[s] <- lowW
        #If win prob is between .47 and .53 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.03){
          numHighOT <- numHighOT+1
        }else{}
        # If predicted pts diff is less than 3, this is very close, one possession game
        # that is likely going down to the wire
        if(isTRUE(abs(outcome[[3]])<3)){
          numClose3 <- numClose3 + 1
        }else{}
      }else{
        #Once a team has reached 4 wins, store final totals in the df
      }
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(round3$highW+round3$lowW>5)
  return(c(round3,numHighOT,numUpset/numGame,numg6g7,numClose3))
}


#Simulating Third Round of Playoffs
finalsSim <- function(finals){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up finals matchup
  finals_mu <- data.frame(matrix(ncol=4,nrow=1))
  colnames(finals_mu) <- c("high", "low", "highW", "lowW")
  i <- 1
  finals_mu$high[i] <- finals$Team[i]
  finals_mu$highW[i] <- 0
  finals_mu$low[i] <- finals$Team[nrow(finals)-i+1]
  finals_mu$lowW[i] <- 0
  #There is 1 finals series
  s <- 1
  #Initializing values
  high <- finals_mu$high[s]
  highW <- 0
  low <- finals_mu$low[s]
  lowW <- 0
  #Series are best of 7
  for(g in 1:7){
    #Continue loop until a team has 4 wins
    if(highW != 4 & lowW != 4){
      outcome <- gameSim(high,low,g)
      numGame <- numGame+1
      #If outcome of sim is true, high team won the game
      if(isTRUE(outcome[[1]] == TRUE)){
        highW <- highW+1
      }
      #If outcome of sim is false, low team won the game
      else{
        lowW <- lowW+1
        numUpset <- numUpset +1
      }
      finals_mu$highW[s] <- highW
      finals_mu$lowW[s] <- lowW
      #If win prob is between .47 and .53 we can say there is a decent chance the game
      #goes to OT
      if(abs(outcome[[2]]-.5)<.03){
        numHighOT <- numHighOT+1
      }
      else{}
      # If predicted pts diff is less than 3, this is very close, one possession game
      # that is likely going down to the wire
      if(abs(outcome[[3]])<3){
        numClose <- numClose + 1
      }
      else{}
    }
    #Once a team has reached 4 wins, store final totals in the df
    else{
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(finals_mu$highW+finals_mu$lowW>5)
  if(finals_mu$highW > finals_mu$lowW){
    champion <- finals_mu$high
  }
  else{
    champion <- finals_mu$low
  }
  return(c(finals_mu,numHighOT,numUpset/numGame,numg6g7,numClose,champion))
}




#Doing 20000 series simulations
S <- 100
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
  
  r1results <- r1Sim(r1)
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
  
  r2results <- r2Sim(r2)
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
  
  r3results <- r3Sim(r3)
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
  
  finalsresults <- finalsSim(finals)
  #Reconstructing sim results
  finals_mu <- data.frame(finalsresults[1],finalsresults[2],finalsresults[3],finalsresults[4], stringsAsFactors = FALSE)
  playoffSim2 <- c(playoffSim2,finals_mu,finalsresults[5],finalsresults[6],finalsresults[7],finalsresults[8],finalsresults[9])
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

#Number of series that went to 6 or 7 games
closeGames2 <- rep(NA,length(upsetIndex2))
s <- 1
for(i in 1:length(upsetIndex2)){
  closeIndex2 <- upsetIndex2[i]+2
  closeGames2[s] <- playoffSim2[[closeIndex2]]
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
champscol2 <- intersect(r1$Team, champsfreq2$champs2)
primcolors2 <- gather(data.frame(lapply(sort(champscol2), team_pal))[1,])$value


