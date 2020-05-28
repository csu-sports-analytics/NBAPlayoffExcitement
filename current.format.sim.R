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
firstround <- data.frame("Seed" = 1:16, 
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
firstround <- firstround %>%
  group_by(., Conf) %>%
  mutate(., ConfSeed = row_number())

#Simulating First Round of Playoffs
firstroundSim <- function(firstround){
  #Setting up first round matchups for the west
  west <- filter(firstround, Conf=="W")
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
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
        west1$highW[s] <- highW
        west1$lowW[s] <- lowW
      }
    }
  }
  
  #Setting up first round matchups for the east
  east <- filter(firstround, Conf=="E")
  east1 <- data.frame(matrix(ncol=4,nrow=4))
  colnames(east1) <- c("high", "low", "highW", "lowW")
  for(i in 1:4){
    east1$high[i] <- east$Team[i]
    east1$highW[i] <- 0
    east1$low[i] <- east$Team[nrow(west)-i+1]
    east1$lowW[i] <- 0
  }
  #There are 4 first round series in the west
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
          print(c(high, highW))
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
          print(c(low, lowW))
        }
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
        east1$highW[s] <- highW
        east1$lowW[s] <- lowW
      }
    }
  }
  return(c(west1,east1))
}
firstroundSim(firstround)

