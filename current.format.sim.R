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
r1SimCurr <- function(r1){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up first round matchups for the west
  west <- filter(r1, Conf=="W")
  west1 <- data.frame(matrix(ncol=4,nrow=4))
  colnames(west1) <- c("high", "low", "highW", "lowW")
  for(i in 1:4){
    west1$high[i] <- west$Team[i]
    west1$highW[i] <- 0
    west1$low[i] <- west$Team[nrow(west)-i+1]
    west1$lowW[i] <- 0
  }
  #There are 4 first round series in the west
  for(s in 1:4){
    #Initializing values
    high <- west1$high[s]
    highW <- 0
    low <- west1$low[s]
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
        west1$highW[s] <- highW
        west1$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.02){
          numHighOT <- numHighOT+1
        }
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
  
  #Setting up first round matchups for the east
  east <- filter(r1, Conf=="E")
  east1 <- data.frame(matrix(ncol=4,nrow=4))
  colnames(east1) <- c("high", "low", "highW", "lowW")
  for(i in 1:4){
    east1$high[i] <- east$Team[i]
    east1$highW[i] <- 0
    east1$low[i] <- east$Team[nrow(east)-i+1]
    east1$lowW[i] <- 0
  }
  #There are 4 first round series in the east
  for(s in 1:4){
    #Initializing values
    high <- east1$high[s]
    highW <- 0
    low <- east1$low[s]
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
        east1$highW[s] <- highW
        east1$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(isTRUE(abs(outcome[[2]]-.5)<.02)){
          numHighOT <- numHighOT+1
        }
        else{}
        # If predicted pts diff is less than 3, this is very close, one possession game
        # that is likely going down to the wire
        if(isTRUE(abs(outcome[[3]])<3)){
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
  numg6g7 <- sum(west1$highW+west1$lowW>5) + sum(east1$highW+east1$lowW>5)
  return(c(west1,east1,numHighOT/numGame,numUpset/numGame,numg6g7/8,numClose/numGame))
}


#Simulating Second Round of Playoffs
r2SimCurr <- function(r2){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up second round matchups for the west
  west <- filter(r2, Conf=="W")
  west2 <- data.frame(matrix(ncol=4,nrow=2))
  colnames(west2) <- c("high", "low", "highW", "lowW")
  for(i in 1:2){
    west2$high[i] <- west$Team[i]
    west2$highW[i] <- 0
    west2$low[i] <- west$Team[nrow(west)-i+1]
    west2$lowW[i] <- 0
  }
  #There are 2 second round series in the west
  for(s in 1:2){
    #Initializing values
    high <- west2$high[s]
    highW <- 0
    low <- west2$low[s]
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
        west2$highW[s] <- highW
        west2$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.02){
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
  
  #Setting up second round matchups for the east
  east <- filter(r2, Conf=="E")
  east2 <- data.frame(matrix(ncol=4,nrow=2))
  colnames(east2) <- c("high", "low", "highW", "lowW")
  for(i in 1:2){
    east2$high[i] <- east$Team[i]
    east2$highW[i] <- 0
    east2$low[i] <- east$Team[nrow(east)-i+1]
    east2$lowW[i] <- 0
  }
  #There are 2 second round series in the west
  for(s in 1:2){
    #Initializing values
    high <- east2$high[s]
    highW <- 0
    low <- east2$low[s]
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
        east2$highW[s] <- highW
        east2$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.02){
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
  numg6g7 <- sum(west2$highW+west2$lowW>5) + sum(east2$highW+east2$lowW>5)
  return(c(west2,east2,numHighOT/numGame,numUpset/numGame,numg6g7/4,numClose/numGame))
}


#Simulating Third Round of Playoffs
r3SimCurr <- function(r3){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose3 <- 0
  #Setting up third round matchups for the west
  west <- filter(r3, Conf=="W")
  west3 <- data.frame(matrix(ncol=4,nrow=1))
  colnames(west3) <- c("high", "low", "highW", "lowW")
  #There is only one matchup in each conference
  i <- 1
  west3$high[i] <- west$Team[i]
  west3$highW[i] <- 0
  west3$low[i] <- west$Team[nrow(west)-i+1]
  west3$lowW[i] <- 0
  #There is 1 third round series in the west
  s <- 1
  #Initializing values
  high <- west3$high[s]
  highW <- 0
  low <- west3$low[s]
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
      west3$highW[s] <- highW
      west3$lowW[s] <- lowW
      #If win prob is between .48 and .52 we can say there is a decent chance the game
      #goes to OT
      if(abs(outcome[[2]]-.5)<.02){
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
  
  #Setting up third round matchups for the east
  east <- filter(r3, Conf=="E")
  east3 <- data.frame(matrix(ncol=4,nrow=1))
  colnames(east3) <- c("high", "low", "highW", "lowW")
  #There is only one matchup in each conference
  i <- 1
  east3$high[i] <- east$Team[i]
  east3$highW[i] <- 0
  east3$low[i] <- east$Team[nrow(east)-i+1]
  east3$lowW[i] <- 0
  #There is 1 third round series in the west
  s <- 1
  #Initializing values
  high <- east3$high[s]
  highW <- 0
  low <- east3$low[s]
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
      east3$highW[s] <- highW
      east3$lowW[s] <- lowW
      #If win prob is between .48 and .52 we can say there is a decent chance the game
      #goes to OT
      if(abs(outcome[[2]]-.5)<.02){
        numHighOT <- numHighOT+1
      }
      else{}
      # If predicted pts diff is less than 3, this is very close, one possession game
      # that is likely going down to the wire
      if(abs(outcome[[3]])<3){
        numClose3 <- numClose3 + 1
      }
      else{}
    }
    #Once a team has reached 4 wins, store final totals in the df
    else{
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(west3$highW+west3$lowW>5) + sum(east3$highW+east3$lowW>5)
  return(c(west3,east3,numHighOT/numGame,numUpset/numGame,numg6g7/2,numClose3/numGame))
}


#Simulating Third Round of Playoffs
finalsSimCurr <- function(finals){
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
      #If win prob is between .48 and .52 we can say there is a decent chance the game
      #goes to OT
      if(abs(outcome[[2]]-.5)<.02){
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
  return(c(finals_mu,numHighOT/numGame,numUpset/numGame,numg6g7,numClose/numGame,champion))
}




#Doing 20000 series simulations
S <- 20000
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

#Number of series that went to 6 or 7 games
closeGames1 <- rep(NA,length(upsetIndex1))
s <- 1
for(i in 1:length(upsetIndex1)){
  closeIndex1 <- upsetIndex1[i]+2
  closeGames1[s] <- playoffSim1[[closeIndex1]]
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

  
