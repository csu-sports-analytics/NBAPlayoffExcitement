#Inverts game perspective from home team to away team
invert <- function(data, TeamPts = F) {
  data <- mutate(data, Tmp = Team, Team = Opponent, Opponent = Tmp)
  data$Tmp[data$Location == "H"] <- "A"
  data$Tmp[data$Location == "A"] <- "H"
  data$Tmp[data$Location == "N"] <- "N"
  data$Location <- data$Tmp
  if(TeamPts) {
    TmpPts <- data$TeamPts
    data$TeamPts <- data$OppPts
    data$OppPts <- TmpPts
  }
  return(select(data,-Tmp))
}

#Every series is best of 7 (2-2-1-1-1) so this will take the higher seed and give them
#home court advantage

#We expect that the range of points for an NBA is between 50 and 185, so we can limit the points matrix
min_pts <- 50
max_pts <- 185
gameSim <- function(high, low, game){
  #Higher seed is home
  if((game<3 | game == 5 | game == 7)){
    gamedf <- data.frame("Team" = high, "Opponent" = low, "Location" = "H", stringsAsFactors = F)
    gametmp <- invert(gamedf)
    gamedf <- rbind(gamedf,gametmp)
  }
  #Lower seed is home
  else{
    gamedf <- data.frame("Team" = low, "Opponent" = high, "Location" = "H", stringsAsFactors = F)
    gametmp <- invert(gamedf)
    gamedf <- rbind(gamedf, gametmp)
  }
  #Predicting game score
  pts_pred <- predict(glm.nba, newdata = gamedf, type = "response")
  high_pred <- round(pts_pred[1]); low_pred <- round(pts_pred[2])
  #SD of teams
  high_sd <- sd(filter(games, Team == high)$TeamPts)
  low_sd <- sd(filter(games, Team == low)$TeamPts)
  
  #Odds of points being acheived
  pts_matrix <- dnorm(min_pts:max_pts, high_pred, high_sd) %o% dnorm(min_pts:max_pts, low_pred, low_sd)
  
  #Home teams win OT about 56% of the time, so we can reasonably add that to win_prob
  #Instead of simulating OT
  #https://www.scapps.org/jems/index.php/1/article/view/898
  ot_prod <- sum(diag(pts_matrix))
  win_prob <- sum(pts_matrix[lower.tri(pts_matrix)]) + (.56*ot_prod)
  
  #Returns TRUE or FALSE based on win probability
  #If TRUE, the higher seed won, if FALSE, the lower seed won
  return(rbernoulli(1,win_prob))
}

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

#Simulating First Round of Playoffs
r1Sim <- function(r1){
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
        #If outcome of sim is true, high team won the game
        if(outcome == TRUE){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
        }
        west1$highW[s] <- highW
        west1$lowW[s] <- lowW
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
        #If outcome of sim is true, high team won the game
        if(outcome == TRUE){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
        }
        east1$highW[s] <- highW
        east1$lowW[s] <- lowW
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
      }
    }
  }
  return(c(west1,east1))
}
r1results <- r1Sim(r1)
#Reconstructing sim results
west1 <- data.frame(r1results[1],r1results[2],r1results[3],r1results[4], stringsAsFactors = FALSE)
east1 <- data.frame(r1results[5],r1results[6],r1results[7],r1results[8], stringsAsFactors = FALSE)



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
r2 <- r2 %>%
  arrange(.,Seed)

#Simulating Second Round of Playoffs
r2Sim <- function(r2){
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
        #If outcome of sim is true, high team won the game
        if(outcome == TRUE){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
        }
        west2$highW[s] <- highW
        west2$lowW[s] <- lowW
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
        #If outcome of sim is true, high team won the game
        if(outcome == TRUE){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
        }
        east2$highW[s] <- highW
        east2$lowW[s] <- lowW
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
      }
    }
  }
  return(c(west2,east2))
}
r2results <- r2Sim(r2)
#Reconstructing sim results
west2 <- data.frame(r2results[1],r2results[2],r2results[3],r2results[4], stringsAsFactors = FALSE)
east2 <- data.frame(r2results[5],r2results[6],r2results[7],r2results[8], stringsAsFactors = FALSE)


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
r3 <- r3 %>%
  arrange(.,Seed)

#Simulating Third Round of Playoffs
r3Sim <- function(r3){
  #Setting up third round matchups for the west
  west <- filter(r3, Conf=="W")
  west3 <- data.frame(matrix(ncol=4,nrow=1))
  colnames(west3) <- c("high", "low", "highW", "lowW")
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
      #If outcome of sim is true, high team won the game
      if(outcome == TRUE){
        highW <- highW+1
      }
      #If outcome of sim is false, low team won the game
      else{
        lowW <- lowW+1
      }
      west3$highW[s] <- highW
      west3$lowW[s] <- lowW
    }
    #Once a team has reached 4 wins, store final totals in the df
    else{
    }
  }
  
  #Setting up third round matchups for the east
  east <- filter(r3, Conf=="E")
  east3 <- data.frame(matrix(ncol=4,nrow=1))
  colnames(east3) <- c("high", "low", "highW", "lowW")
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
      #If outcome of sim is true, high team won the game
      if(outcome == TRUE){
        highW <- highW+1
      }
      #If outcome of sim is false, low team won the game
      else{
        lowW <- lowW+1
      }
      east3$highW[s] <- highW
      east3$lowW[s] <- lowW
    }
    #Once a team has reached 4 wins, store final totals in the df
    else{
    }
  }
  return(c(west3,east3))
}
r3results <- r3Sim(r3)
#Reconstructing sim results
west3 <- data.frame(r3results[1],r3results[2],r3results[3],r3results[4], stringsAsFactors = FALSE)
east3 <- data.frame(r3results[5],r3results[6],r3results[7],r3results[8], stringsAsFactors = FALSE)



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

#Simulating Third Round of Playoffs
finalsSim <- function(finals){
  #Setting up third round matchups for the west
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
      #If outcome of sim is true, high team won the game
      if(outcome == TRUE){
        highW <- highW+1
      }
      #If outcome of sim is false, low team won the game
      else{
        lowW <- lowW+1
      }
      finals_mu$highW[s] <- highW
      finals_mu$lowW[s] <- lowW
    }
    #Once a team has reached 4 wins, store final totals in the df
    else{
    }
  }
  return(finals_mu)
}
finalsresults <- finalsSim(finals)
#Reconstructing sim results
finals_mu <- data.frame(finalsresults[1],finalsresults[2],finalsresults[3],finalsresults[4], stringsAsFactors = FALSE)
finals_mu

#Doing 20 series simulations
S <- 1000
playoffSim <- list()
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
  
  r1results <- r1Sim(r1)
  #Reconstructing sim results
  west1 <- data.frame(r1results[1],r1results[2],r1results[3],r1results[4], stringsAsFactors = FALSE)
  east1 <- data.frame(r1results[5],r1results[6],r1results[7],r1results[8], stringsAsFactors = FALSE)
  playoffSim <- c(playoffSim,west1,east1)
  
  
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
  r2 <- r2 %>%
    arrange(.,Seed)
  
  r2results <- r2Sim(r2)
  #Reconstructing sim results
  west2 <- data.frame(r2results[1],r2results[2],r2results[3],r2results[4], stringsAsFactors = FALSE)
  east2 <- data.frame(r2results[5],r2results[6],r2results[7],r2results[8], stringsAsFactors = FALSE)
  playoffSim <- c(playoffSim,west2,east2)
  
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
  r3 <- r3 %>%
    arrange(.,Seed)
  
  r3results <- r3Sim(r3)
  #Reconstructing sim results
  west3 <- data.frame(r3results[1],r3results[2],r3results[3],r3results[4], stringsAsFactors = FALSE)
  east3 <- data.frame(r3results[5],r3results[6],r3results[7],r3results[8], stringsAsFactors = FALSE)
  playoffSim <- c(playoffSim,west3,east3)
  
  
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
  
  finalsresults <- finalsSim(finals)
  #Reconstructing sim results
  finals_mu <- data.frame(finalsresults[1],finalsresults[2],finalsresults[3],finalsresults[4], stringsAsFactors = FALSE)
  playoffSim <- c(playoffSim,finals_mu)
}

