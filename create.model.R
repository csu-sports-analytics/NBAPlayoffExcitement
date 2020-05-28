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
names(games_away)=c("Date", "Team", "TeamPts", "Opponent", "OppPts", "DaysSince", "Season", "Location")
games <- rbind(games, games_away)
#Weight games based on how long ago they were
games <- games %>%
  mutate(., Game_Weight = Season*exp(-games$DaysSince/max(games$DaysSince)))

#Making linear model to predict team score
glm.nba <- glm(TeamPts ~ Team + Opponent + Location, 
               data = games,
               family = "gaussian",
               weights = Game_Weight)

