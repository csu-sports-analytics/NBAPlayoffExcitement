install.packages("RCurl")
library(RCurl)
install.packages("XML")
library(XML)
install.packages("tidyverse")
library(tidyverse)
install.packages("stringr")
library(stringr)
install.packages("teamcolors")
#Using data from the 2018/19 and 2019/20 seasons
years <- c(2019:2020)

#Creating complete schedule
games <- data.frame()


for(i in 1:length(years)){
  #Establishing which months in each other years NBA games were played
  if(years[i]==2019){
    months <- c("october","november","december","january","february","march","april","may","june")
  }
  else if(years[i]==2020){
    months <- c("october","november","december","january", "february","march")
  }
  else{}
  for(j in 1:length(months)){
    #Creating URL and gathering table
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",years[i],"_games-",months[j],".html")
    url_new <- getURL(url)
    table <- readHTMLTable(url_new)
    month_sched <- table$schedule
    #Playoffs begin in April and there is a row that is empty that must be removed
    if(months[j]=="april"){
      month_sched <- month_sched[-(which(month_sched$Date=="Playoffs")),]
    }
    #Changing formats of the data and consolidating into only the data that we need
    month_sched$Date <- as.Date(month_sched$Date, "%a, %b %d,%Y")
    month_sched$Opponent <- as.character(month_sched$`Visitor/Neutral`)
    month_sched$OppPts <- as.numeric(as.character(month_sched[[4]]))
    month_sched$Team <- as.character(month_sched$`Home/Neutral`)
    month_sched$TeamPts <- as.numeric(as.character(month_sched[[6]]))
    month_sched <- month_sched[,c(1,13,14,11,12)]
    games <- rbind(games,month_sched)
  }
}

#Adding PtsDiff to games
games <- games %>%
  mutate(PtsDiff = TeamPts-OppPts)

allTeams = unique(games$Team)[1:30]










# #Scraping elo from FiveThirtyEight
# url_elo <- getURL("https://projects.fivethirtyeight.com/2020-nba-predictions/")
# elo_table <- readHTMLTable(url_elo)
# elo_table <- elo_table$`standings-table`
# elo_table$`Current rating` <- as.numeric(as.character(elo_table$`Current rating`))
# elo_table$Team <- as.character(elo_table$Team)
# elo_table <- elo_table[,-c(2,3)]
# #Removing record from the teams
# elo_table$Team <- substr(elo_table$Team,1,nchar(elo_table$Team)-5)
# 
# #Adding current elo to game scores
# TeamElo <- rep(NA,nrow(games))
# OpponentElo <- rep(NA,nrow(games))
# games <- cbind(games,TeamElo,OpponentElo)
# for(i in 1:nrow(games)){
#   games$TeamElo[i] <- elo_table$`Current rating`[which(word(elo_table$Team,-1) == word(games$Team[i],-1))]
#   games$OpponentElo[i] <- elo_table$`Current rating`[which(word(elo_table$Team,-1) == word(games$Opponent[i],-1))]
# }
