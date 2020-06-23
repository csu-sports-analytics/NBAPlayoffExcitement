



#Model Diagnostic Plots
qqnorm(lm.nba$residuals)
qqline(lm.nba$residuals)
plot(lm.nba$residuals ~ lm.nba$fitted.values)
plot(games$PtsDiff, games$exp_pts_diff)


# #Testing model against actual game results
# model_games <- games %>%
#   select(., Team, Opponent,Location,PtsDiff)
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

mean(abs(games$exp_pts_diff)<=4)
mean(abs(games$PtsDiff)<=4)
mean(games$exp_pts_diff > 0 & games$win == 1) + mean(games$exp_pts_diff < 0 & games$win == 0)
summary(lm.nba)
