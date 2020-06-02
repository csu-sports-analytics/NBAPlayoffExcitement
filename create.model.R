
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
  mutate(., PtsDiff = -1*PtsDiff) %>%
  mutate(., Location = "A")
names(games_away)=c("Team", "Opponent", "PtsDiff", "Location", "Game_Weight")
games <- rbind(games, games_away)
#Some games were played at neutral sites
games$Location[1681] <- "N"
games$Location[1692] <- "N"
games$Location[1981] <- "N"
games$Location[1681+(length(games$Location)/2)] <- "N"
games$Location[1692+(length(games$Location)/2)] <- "N"
games$Location[1981+(length(games$Location)/2)] <- "N"



#Making linear model to predict team score
lm.nba <- lm(PtsDiff ~ Team + Opponent + Location, 
               data = games,
               weights = Game_Weight)

#GLM wto find prob of winning giving expected points spread
games <- games %>%
  select(., Team, Opponent, PtsDiff, Location)
games$win <- ifelse(games$PtsDiff > 0,1,0)
games$exp_pts_diff <- predict(lm.nba, newdata = games)
glm.spread <- glm(win ~ exp_pts_diff, data = games, family = "binomial")

#With Elo
# lm.nba <- lm(TeamPts ~ Team + Opponent + TeamElo + OpponentElo + Location, 
#                data = games,
#                family = "gaussian",
#                weights = Game_Weight)

#Model Diagnostic Plots
qqnorm(lm.nba$residuals)
qqline(lm.nba$residuals)
plot(lm.nba$residuals ~ lm.nba$fitted.values)

# #Testing model against actual game results
# model_games <- games %>%
#   select(., Team, Opponent,Location)
# model_games$win = ifelse(games$PtsDiff > 0,1,0)
# model_games$exp_pts_diff <- as.numeric(as.character(unlist(predict.lm(lm.nba, newdata = model_games, se.fit = TRUE, type = "response")[[1]])))
# glm.spread <- glm(win ~ exp_pts_diff, data = model_games, family = "binomial")
# model_games$wlprob <- predict.glm(glm.spread, newdata = model_games, type = "response")

# model_game_SE <- as.numeric(as.character(unlist(predict.lm(lm.nba, newdata = model_games, se.fit = TRUE, type = "response")[2])))
# model_game_Pts <- rep(NA, nrow(model_games))
# for(i in 1:nrow(model_games)){
#   model_game_Pts[i] <- round(rnorm(1,model_game_ExpPts[i],model_game_SE[i]))
# }
# model_games <- cbind(model_games,model_game_Pts)
# 
# plot(y=model_games$model_game_Pts,x=games$TeamPts)
# 
# #Checking what proportion of games did the simulation predict the winner
# wl_games <- rep(NA, nrow(games))
# wl_games <- mutate(games, wl = if_else(PtsDiff > 0, "W", "L"))
# 
# wl_model <- rep(NA, nrow(model_games))
# for(i in 1:(nrow(model_games))){
#   if(isTRUE(model_game_Pts[i]>0)){
#     wl_model[i] <- "W"
#   }
#   else{
#     wl_model[i] <- "L"
#   }
# }

