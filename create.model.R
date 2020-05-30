
#Days since the game for weights for model
games$DaysSince <- as.numeric(Sys.Date()-games$Date)
#Which season the game was in, also for weighting model
#1 if 2018/19, 3 if 2019/20 so that games from this season are weighted heavier
#but last season is still accounted for
games <- games %>%
  mutate(., Season = if_else(Date < as.Date("2019-10-21"), 1, 3))
#Adding "H" for location
games <- games %>%
  mutate(., Location = "H")
#Adding games from away teams' perspectives
games_away <- games %>%
  select(., Date, Opponent, OppPts, Team, TeamPts,DaysSince, Season) %>%
  mutate(., Location = "A")
names(games_away)=c("Date", "Team", "TeamPts", "Opponent", "OppPts", "TeamElo", "OpponentElo", "DaysSince", "Season", "Location")
games <- rbind(games, games_away)
#Weight games based on how long ago they were
games <- games %>%
  mutate(., Game_Weight = Season*exp(-games$DaysSince/max(games$DaysSince)))




#Making linear model to predict team score
glm.nba <- lm(TeamPts ~ Team + Opponent + TeamElo + OpponentElo + Location, 
               data = games,
               family = "gaussian",
               weights = Game_Weight)

#Model Diagnostic Plots
qqnorm(glm.nba$residuals)
qqline(glm.nba$residuals)
plot(glm.nba$residuals ~ glm.nba$fitted.values)

#Testing model against actual game results
model_games <- games %>%
  select(., Team, Opponent, Location)
model_game_ExpPts <- as.numeric(as.character(unlist(predict.glm(glm.nba, newdata = model_games, se.fit = TRUE, type = "response")[[1]])))
model_game_SE <- as.numeric(as.character(unlist(predict.glm(glm.nba, newdata = model_games, se.fit = TRUE, type = "response")[2])))
model_game_Pts <- rep(NA, nrow(model_games))
for(i in 1:nrow(model_games)){
  model_game_Pts[i] <- round(rnorm(1,model_game_ExpPts[i],model_game_SE[i]))
}
model_games <- cbind(model_games,model_game_Pts)

plot(y=model_games$model_game_Pts,x=games$TeamPts)

#Checking what proportion of games did the simulation predict the winner
wl_games <- rep(NA, nrow(games)/2)
for(i in 1:(nrow(games)/2)){
  if(isTRUE(games$TeamPts[i]>(games$TeamPts[i+(nrow(games)/2)]))){
    wl_games[i] <- "W"
  }
  else{
    wl_games[i] <- "L"
  }
}

wl_model <- rep(NA, nrow(model_games)/2)
for(i in 1:(nrow(model_games)/2)){
  if(isTRUE(model_game_TeamPts[i]>(model_game_TeamPts[i+(nrow(games)/2)]))){
    wl_model[i] <- "W"
  }
  else{
    wl_model[i] <- "L"
  }
}

count <- 0
for(i in 1:(nrow(model_games)/2)){
  if(isTRUE(wl_games[i]==wl_model[i])){
    count <- count+1
  }
  else{
  }
}
count/length(wl_model)
