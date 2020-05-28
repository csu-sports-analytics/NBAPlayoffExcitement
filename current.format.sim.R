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
  pts_pred <- predict(glm.nba, newdata = gamedf, type = "response")
  high_pred <- pts_pred[1]; low_pred <- pts_pred[2]
  high_sd <- sd(filter(games, Team == high)$TeamPts)
  low_sd <- sd(filter(games, Team == low)$TeamPts)
  pts_matrix <- dnorm(0:max_pts, high_pred, high_sd) %o% dnorm(0:max_pts, low_pred, low_sd)
  win_prob <- sum(pts_matrix[lower.tri(pts_matrix)])
  loss_prod <- sum(pts_matrix[upper.tri(pts_matrix)])
  return(win_prob)
}

playoff_teams <- data.frame("Seed" = 1:16, 
                            "Team" = c("Milwaukee Bucks", "Los Angeles Lakers", "Toronto Raptors",
                                       "Los Angeles Clippers", "Boston Celtics", "Denver Nuggets",
                                       "Utah Jazz", " Miami Heat", "Oklahoma City Thunder",
                                       "Houston Rockets", "Indiana Pacers", "Philadelphia 76ers", 
                                       "Dallas Mavericks","Memphis Grizzlies", "Brooklyn Nets", 
                                       "Orlando Magic"),
                            "Conf" = c("E", "W", "E", "W", "E", "W", "W","E","W",
                                       "W", "E", "E", "W", "W", "E","E"))

