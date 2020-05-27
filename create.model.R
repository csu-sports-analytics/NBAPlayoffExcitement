#Days since the game for weights for model
games$DaysSince <- as.numeric(Sys.Date()-games$Date)
#Which season the game was in, also for weighting model
#1 if 2018/19, 0 if 2019/20
games <- games %>%
  mutate(., season = if_else(Date < as.Date("2019-10-21"), 1, 0))