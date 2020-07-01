library(RCurl)
library(XML)
library(tidyverse)
#### Scraper to get different playoffs from basketball reference ####
getPostseason <- function(year){
  url <- paste0("https://www.basketball-reference.com/playoffs/NBA_",year,"_games.html")
  newurl <- getURL(url)
  postseason <- as.data.frame(readHTMLTable(newurl))
  postseason <- postseason[,c(3,5,4,6,8)]
  colnames(postseason) <- c("Vis","Home","VisPts","HomePts","OT")
  #Creating unique series identifiers
  postseason <- postseason %>%
    unite(., series, 1:2, remove=FALSE)
  postseason$series <- postseason$series %>% 
    str_split(., '_') %>% 
    lapply(., 'sort') %>%  
    lapply(., 'paste', collapse=' ') %>% 
    unlist(.)
  postseason$VisPts <- as.integer(as.character(postseason$VisPts))
  postseason$HomePts <- as.integer(as.character(postseason$HomePts))
  postseason$Vis <- as.character(postseason$Vis)
  postseason$Home <- as.character(postseason$Home)
  return(postseason)
}

#### Gathers the metrics ####
getMets <- function(postseason, year){
  #Getting standings for seeding
  url <- paste0("https://www.espn.com/nba/standings/_/season/",year,"/group/league")
  newurl <- getURL(url)
  psTeams <- as.data.frame(readHTMLTable(newurl)) %>%
    select(Var.1)
  psTeams$Var.1 <- as.character(psTeams$Var.1)
  if(year < 2016){
    for(i in 1:30){
      if(isTRUE(str_detect(psTeams$Var.1[i],"--"))){
      }
      else{
        psTeams$Var.1[i] <- paste0("e --",psTeams$Var.1[i])
      }
    }
  }
  psTeams <- unlist(str_split(psTeams$Var.1, pattern = "--"))
  psTeams <- data.frame(PS = psTeams[seq(1,length(psTeams),2)], 
                           Teams = psTeams[seq(2,length(psTeams),2)])
  #ESPN has Clippers as "LA" while Basketball-Reference has it as "Los Angeles"
  psTeams <- psTeams %>%
    mutate(., Teams = ifelse(str_detect(Teams, "LACLA"),"LACLos Angeles Clippers",as.character(Teams)))
  #Removing unwanted abbreviations
  teams1 <- str_extract(psTeams$Teams, '[A-Z][a-z]+')
  teams2 <- word(psTeams$Teams,2,sep = " ")
  teams3 <- word(psTeams$Teams,3,sep = " ")
  teams <- paste(teams1, teams2, teams3, sep = " ")
  psTeams$Teams <- unlist(str_remove(teams, " NA"))
  psTeams$PS <- as.character(psTeams$PS)
  #Finding which seed belongs to which team
  Seed <- rep(0,30)
  j <- 0
  for(i in 1:30){
    if(isTRUE(psTeams$PS[i] != "e ")){
      j <- j+1
      Seed[i] <- j
    }
    else{
      Seed[i] <- 0
    }
  }
  #Combining seeding with teams
  psTeams <- cbind(psTeams, Seed)
  
  library(operators)
  seriesTracker <- NA
  for(i in 1:nrow(postseason)){
    if(isTRUE(postseason$series[i] %!in% seriesTracker)){
      seriesTracker <- c(seriesTracker, postseason$series[i])
    }
    else{}
  }
  devtools::unload("operators")
  seriesTracker <- na.omit(seriesTracker)[1:15]
  Round <- c(rep(1,8),rep(2,4),rep(3,2),4)
  seriesTracker <- as.data.frame(cbind(seriesTracker,Round))
  seriesTracker$seriesTracker <- as.character(seriesTracker$seriesTracker)
  seriesTracker$Round <- as.integer(seriesTracker$Round)
  for(i in 1:nrow(postseason)){
    postseason$Round[i] <- seriesTracker$Round[which(seriesTracker$seriesTracker == postseason$series[i])]
  }
  
  #Getting number of games in each round
  numGames <- count(postseason, Round)
  numGames <- rbind(numGames, nrow(postseason)); numGames$Round[5] <- "Tot"
  
  #Number of OT's in each round
  numOT <- as.data.frame(count(postseason, Round, OT))
  numOT$OT <- as.character(numOT$OT)
  numOT <- filter(numOT, OT == "OT")
  numOT <- numOT[,c(1,3)]
  numOT$Round <- as.character(numOT$Round)
  complete <- data.frame(Round = c(1:4, "Tot"), n = rep(0,5))
  numOT <- union(numOT, complete) %>% 
    group_by(Round) %>%
    slice(1L) %>%
    ungroup(.)
  numOT <- arrange(numOT, Round)
  numOT$n[5] <- sum(numOT$n)

  #Getting number of long series in each round
  seriesGames <- as.data.frame(count(postseason, series))
  Round <- rep(NA,15)
  seriesGames <- cbind(seriesGames, Round)
  for(i in 1:15){
    seriesGames$Round[i] <- as.integer(seriesTracker$Round[which(seriesGames$series[i]==seriesTracker$seriesTracker)])
  }
  numLongSeries <- data.frame(as.character(c(1:4, "Tot")),rep(0,5))
  colnames(numLongSeries) <- c("Round", "n")
  for(i in 1:4){
    for(j in 1:15){
      if(isTRUE(numLongSeries$Round[i]==seriesGames$Round[j] & seriesGames$n[j] > 5)){
        numLongSeries$n[i] <- numLongSeries$n[i] + 1
        numLongSeries$n[5] <- numLongSeries$n[5] + 1
      }else{}
    }
  }
  
  #Getting Number of Close Games
  numClose <- data.frame(as.character(c(1:4, "Tot")),rep(0,5))
  colnames(numClose) <- c("Round", "n")
  closeInd <- which(abs(postseason$VisPts-postseason$HomePts) <= 3)
  numClose$n[5] <- length(closeInd)
  for(i in 1:length(closeInd)){
    j <- closeInd[i]
    numClose$n[postseason$Round[j]] <- numClose$n[postseason$Round[j]] + 1
  }
  
  #Getting Number of Upsets
  VisSeed <- rep(0,nrow(postseason))
  HomeSeed <- rep(0, nrow(postseason))
  Upset <- rep(0,nrow(postseason))
  postseason <- cbind(postseason,VisSeed,HomeSeed,Upset)
  for(i in 1:nrow(postseason)){
    postseason$VisSeed[i] <- psTeams$Seed[which(psTeams$Team == postseason$Vis[i])]
    postseason$HomeSeed[i] <- psTeams$Seed[which(psTeams$Team == postseason$Home[i])]
    if(isTRUE(postseason$HomePts[i] > postseason$VisPts[i] & postseason$HomeSeed[i] > postseason$VisSeed[i])){
      postseason$Upset[i] <- 1
    }
    else if(isTRUE(postseason$HomePts[i] < postseason$VisPts[i] & postseason$HomeSeed[i] < postseason$VisSeed[i])){
      postseason$Upset[i] <- 1
    }
    else{}
  }
  numUpset <- data.frame(as.character(c(1:4, "Tot")),rep(0,5))
  colnames(numUpset) <- c("Round", "n")
  upsetInd <- which(postseason$Upset == 1)
  numUpset$n[5] <- length(upsetInd)
  for(i in 1:length(upsetInd)){
    j <- upsetInd[i]
    numUpset$n[postseason$Round[j]] <- numUpset$n[postseason$Round[j]] + 1
  }
  
  #Getting percentages
  numOT$n <- numOT$n/numGames$n
  numLongSeries$n <- numLongSeries$n/c(8,4,2,1,15)
  numClose$n <- numClose$n/numGames$n
  numUpset$n <- numUpset$n/numGames$n
  
  return(c(numOT, numLongSeries, numClose, numUpset, numGames))
}

##### Function that takes in the list format and rebuilds them into dataframes ####
rebuild_metric <- function(metrics, year){
  met <- data.frame(metrics[1],metrics[2])
  met[,1] <- as.character(met[,1])
  met[,2] <- as.numeric(met[,2])
  met[,3] <- as.character(year)
  return(met)
}

#### Goes year-by-year and gathers postseasons, metrics and then rebuilds and saves them ####
min_year <- 2010
max_year <- 2019
for(y in min_year:max_year){
  postseason <- getPostseason(y)
  metrics <- getMets(postseason, y)
  assign(paste0(y, "_OT"), rebuild_metric(metrics[c(1,2)],y))
  assign(paste0(y, "_longSeries"), rebuild_metric(metrics[c(3,4)],y))
  assign(paste0(y, "_Close"), rebuild_metric(metrics[c(5,6)],y))
  assign(paste0(y, "_Upset"), rebuild_metric(metrics[c(7,8)],y))
  assign(paste0(y, "_Games"), rebuild_metric(metrics[c(9,10)],y))
}

#### Gathering yearly metrics into one dataframe for each metric ####
gatherMet <- function(metric){
  #Creates an empty dataframe and gives it a metric specific name
  assign(paste0("past_", metric), data.frame())
  #Adds each years metric table to overall dataframe
  for(y in min_year:max_year){
    assign(paste0("past_", metric),rbind(get(paste0("past_", metric)),get(paste0(y,"_",metric))))
  }
  return(get(paste0("past_", metric)))
}

#### Getting each metric and sorting them by round to make plotting easier ####
#Also giving top 3 results in each round a Year to make labelling easier
past_Close <- gatherMet("Close") %>% 
  arrange(desc(n)) %>% 
  group_by(Round) %>% 
  mutate(., Year = if_else(row_number()<4, as.numeric(V3), NaN)) %>%
  ungroup(.) %>%
  arrange(Round)
past_Upset <- gatherMet("Upset") %>% 
  arrange(desc(n)) %>% 
  group_by(Round) %>% 
  mutate(., Year = if_else(row_number()<4, as.numeric(V3), NaN)) %>%
  ungroup(.) %>%
  arrange(Round)
past_longSeries <- gatherMet("longSeries") %>% 
  arrange(desc(n)) %>% 
  group_by(Round) %>% 
  mutate(., Year = if_else(row_number()<4, as.numeric(V3), NaN)) %>%
  ungroup(.) %>%
  arrange(Round)
past_OT <- gatherMet("OT") %>% 
  arrange(desc(n)) %>% 
  group_by(Round) %>% 
  mutate(., Year = if_else(row_number()<4, as.numeric(V3), NaN)) %>%
  ungroup(.) %>%
  arrange(Round)

tukey_label <- function(df, hsd){
  df$Group <- NA
  for(i in 1:nrow(hsd$groups)){
    df$Group[which(rownames(hsd$groups)[i]==paste0(df$Format, ":",df$Round))] <- as.character(hsd$groups[i,2])
  }
  return(df)
}

#### High Overtime for by rounds ####
#Round 1
highOT1r1 <- data.frame(Count = highOT1[seq(1, length(highOT1), 4)]) %>% mutate(., Round = 1, Format = "Current")
highOT2r1 <- data.frame(Count = highOT2[seq(1, length(highOT2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
highOT3r1 <- data.frame(Count = highOT3[seq(1, length(highOT3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
highOT1r2 <- data.frame(Count = highOT1[seq(2, length(highOT1), 4)]) %>% mutate(., Round = 2, Format = "Current")
highOT2r2 <- data.frame(Count = highOT2[seq(2, length(highOT2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
highOT3r2 <- data.frame(Count = highOT3[seq(2, length(highOT3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
highOT1r3 <- data.frame(Count = highOT1[seq(3, length(highOT1), 4)]) %>% mutate(., Round = 3, Format = "Current")
highOT2r3 <- data.frame(Count = highOT2[seq(3, length(highOT2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
highOT3r3 <- data.frame(Count = highOT3[seq(3, length(highOT3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
highOT1r4 <- data.frame(Count = highOT1[seq(4, length(highOT1), 4)]) %>% mutate(., Round = 4, Format = "Current")
highOT2r4 <- data.frame(Count = highOT2[seq(4, length(highOT2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
highOT3r4 <- data.frame(Count = highOT3[seq(4, length(highOT3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
highOT <- rbind(highOT1r1,highOT1r2,highOT1r3,highOT1r4,
                highOT2r1,highOT2r2,highOT2r3,highOT2r4,
                highOT3r1,highOT3r2,highOT3r3,highOT3r4)

#### High OT HSD Tests (Separated by rounds) ####
library(agricolae)
lm.OT <- lm(Count ~ Format + Round, data = highOT)
HSD.OT <- HSD.test(lm.OT, c("Format", "Round")); HSD.OT$groups
highOT <- tukey_label(highOT, HSD.OT)
highOT <- highOT %>%
  mutate(., FR = paste0(Format,Round)) %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)


#### High OT HSD Tests (Not separated by rounds) ####
highOT1 <- data.frame(Count = highOT1) %>% mutate(., Format = "Current")
highOT2 <- data.frame(Count = highOT2) %>% mutate(., Format = "8W8E")
highOT3 <- data.frame(Count = highOT3) %>% mutate(., Format = "16TOT")
highOTf <- rbind(highOT1,highOT2,highOT3)
lm.OTf <- lm(Count ~ Format, data = highOTf)
HSD.OTf <- HSD.test(lm.OTf, "Format"); HSD.OTf$groups
highOTf <- highOTf %>%
  mutate(., Group = case_when(Format == "Current" ~ as.character(HSD.OTf$groups[which(rownames(HSD.OTf$groups) == "Current"),2]),
                              Format == "8W8E" ~ as.character(HSD.OTf$groups[which(rownames(HSD.OTf$groups) == "8W8E"),2]), 
                              Format == "16TOT" ~ as.character(HSD.OTf$groups[which(rownames(HSD.OTf$groups) == "16TOT"),2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)



#### Upset Ratio by rounds ####
#Round 1
upsets1r1 <- data.frame(Count = upsets1[seq(1, length(upsets1), 4)]) %>% mutate(., Round = 1, Format = "Current")
upsets2r1 <- data.frame(Count = upsets2[seq(1, length(upsets2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
upsets3r1 <- data.frame(Count = upsets3[seq(1, length(upsets3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
upsets1r2 <- data.frame(Count = upsets1[seq(2, length(upsets1), 4)]) %>% mutate(., Round = 2, Format = "Current")
upsets2r2 <- data.frame(Count = upsets2[seq(2, length(upsets2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
upsets3r2 <- data.frame(Count = upsets3[seq(2, length(upsets3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
upsets1r3 <- data.frame(Count = upsets1[seq(3, length(upsets1), 4)]) %>% mutate(., Round = 3, Format = "Current")
upsets2r3 <- data.frame(Count = upsets2[seq(3, length(upsets2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
upsets3r3 <- data.frame(Count = upsets3[seq(3, length(upsets3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
upsets1r4 <- data.frame(Count = upsets1[seq(4, length(upsets1), 4)]) %>% mutate(., Round = 4, Format = "Current")
upsets2r4 <- data.frame(Count = upsets2[seq(4, length(upsets2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
upsets3r4 <- data.frame(Count = upsets3[seq(4, length(upsets3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
upsets <- rbind(upsets1r1,upsets1r2,upsets1r3,upsets1r4,
                upsets2r1,upsets2r2,upsets2r3,upsets2r4,
                upsets3r1,upsets3r2,upsets3r3,upsets3r4)

#### Upsets HSD Tests (Separated by rounds) ####
lm.upsets <- lm(Count ~ Format + Round, data = upsets)
library(agricolae)
HSD.upsets <- HSD.test(lm.upsets, c("Format", "Round")); HSD.upsets$groups
upsets <- tukey_label(upsets, HSD.upsets)
upsets <- upsets %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

#### Upsets HSD Tests (Not separated by rounds) ####
upsets1 <- data.frame(Count = upsets1) %>% mutate(., Format = "Current")
upsets2 <- data.frame(Count = upsets2) %>% mutate(., Format = "8W8E")
upsets3 <- data.frame(Count = upsets3) %>% mutate(., Format = "16TOT")
upsetsf <- rbind(upsets1,upsets2,upsets3)
lm.upsetsf <- lm(Count ~ Format, data = upsetsf)
HSD.upsetsf <- HSD.test(lm.upsetsf, "Format"); HSD.upsetsf$groups
upsetsf <- upsetsf %>%
  mutate(., Group = case_when(Format == "Current" ~ as.character(HSD.upsetsf$groups[which(rownames(HSD.upsetsf$groups) == "Current"),2]),
                              Format == "8W8E" ~ as.character(HSD.upsetsf$groups[which(rownames(HSD.upsetsf$groups) == "8W8E"),2]), 
                              Format == "16TOT" ~ as.character(HSD.upsetsf$groups[which(rownames(HSD.upsetsf$groups) == "16TOT"),2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)


#### Long series by rounds ####
#Round 1
longSeries1r1 <- data.frame(Count = longSeries1[seq(1, length(longSeries1), 4)]) %>% mutate(., Round = 1, Format = "Current")
longSeries2r1 <- data.frame(Count = longSeries2[seq(1, length(longSeries2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
longSeries3r1 <- data.frame(Count = longSeries3[seq(1, length(longSeries3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
longSeries1r2 <- data.frame(Count = longSeries1[seq(2, length(longSeries1), 4)]) %>% mutate(., Round = 2, Format = "Current")
longSeries2r2 <- data.frame(Count = longSeries2[seq(2, length(longSeries2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
longSeries3r2 <- data.frame(Count = longSeries3[seq(2, length(longSeries3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
longSeries1r3 <- data.frame(Count = longSeries1[seq(3, length(longSeries1), 4)]) %>% mutate(., Round = 3, Format = "Current")
longSeries2r3 <- data.frame(Count = longSeries2[seq(3, length(longSeries2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
longSeries3r3 <- data.frame(Count = longSeries3[seq(3, length(longSeries3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
longSeries1r4 <- data.frame(Count = longSeries1[seq(4, length(longSeries1), 4)]) %>% mutate(., Round = 4, Format = "Current")
longSeries2r4 <- data.frame(Count = longSeries2[seq(4, length(longSeries2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
longSeries3r4 <- data.frame(Count = longSeries3[seq(4, length(longSeries3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
longSeries <- rbind(longSeries1r1,longSeries1r2,longSeries1r3,longSeries1r4,
                    longSeries2r1,longSeries2r2,longSeries2r3,longSeries2r4,
                    longSeries3r1,longSeries3r2,longSeries3r3,longSeries3r4)

#### Long Series HSD Tests (Separated by rounds) ####
lm.longSeries <- lm(Count ~ Format + Round, data = longSeries)
HSD.longSeries <- HSD.test(lm.longSeries, c("Format", "Round")); HSD.longSeries$groups
longSeries <- tukey_label(longSeries, HSD.longSeries)
longSeries <- longSeries %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, as.integer(Count), as.integer(NaN))) %>%
  ungroup(.)


#### Long Series HSD Tests (Not separated by rounds) ####
longSeries1 <- data.frame(Count = longSeries1) %>% mutate(., Format = "Current")
longSeries2 <- data.frame(Count = longSeries2) %>% mutate(., Format = "8W8E")
longSeries3 <- data.frame(Count = longSeries3) %>% mutate(., Format = "16TOT")
longSeriesf <- rbind(longSeries1,longSeries2,longSeries3)
lm.longSeriesf <- lm(Count ~ Format, data = longSeriesf)
HSD.longSeriesf <- HSD.test(lm.longSeriesf, "Format"); HSD.longSeriesf$groups
longSeriesf <- longSeriesf %>%
  mutate(., Group = case_when(Format == "Current" ~ as.character(HSD.longSeriesf$groups[which(rownames(HSD.longSeriesf$groups) == "Current"),2]),
                              Format == "8W8E" ~ as.character(HSD.longSeriesf$groups[which(rownames(HSD.longSeriesf$groups) == "8W8E"),2]), 
                              Format == "16TOT" ~ as.character(HSD.longSeriesf$groups[which(rownames(HSD.longSeriesf$groups) == "16TOT"),2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)




#### NOT WORRIED ABOUT CLOSE GAMES ####
# #Close games by rounds
# #Round 1
# closeGames1r1 <- data.frame(Count = closeGames1[seq(1, length(closeGames1), 4)]) %>% mutate(., Round = 1, Format = "Current")
# closeGames2r1 <- data.frame(Count = closeGames2[seq(1, length(closeGames2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
# closeGames3r1 <- data.frame(Count = closeGames3[seq(1, length(closeGames3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
# #Round 2
# closeGames1r2 <- data.frame(Count = closeGames1[seq(2, length(closeGames1), 4)]) %>% mutate(., Round = 2, Format = "Current")
# closeGames2r2 <- data.frame(Count = closeGames2[seq(2, length(closeGames2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
# closeGames3r2 <- data.frame(Count = closeGames3[seq(2, length(closeGames3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
# #Round 3
# closeGames1r3 <- data.frame(Count = closeGames1[seq(3, length(closeGames1), 4)]) %>% mutate(., Round = 3, Format = "Current")
# closeGames2r3 <- data.frame(Count = closeGames2[seq(3, length(closeGames2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
# closeGames3r3 <- data.frame(Count = closeGames3[seq(3, length(closeGames3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
# #Finals
# closeGames1r4 <- data.frame(Count = closeGames1[seq(4, length(closeGames1), 4)]) %>% mutate(., Round = 4, Format = "Current")
# closeGames2r4 <- data.frame(Count = closeGames2[seq(4, length(closeGames2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
# closeGames3r4 <- data.frame(Count = closeGames3[seq(4, length(closeGames3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
# closeGames <- rbind(closeGames1r1,closeGames1r2,closeGames1r3,closeGames1r4,
#                     closeGames2r1,closeGames2r2,closeGames2r3,closeGames2r4,
#                     closeGames3r1,closeGames3r2,closeGames3r3,closeGames3r4)
# 
# #Close Games HSD Tests (Separated by rounds)
# lm.close <- lm(Count ~ Format + Round, data = closeGames)
# HSD.close <- HSD.test(lm.close, c("Format", "Round")); HSD.close$groups
# closeGames <- tukey_label(closeGames, HSD.close)
# closeGames <- closeGames %>%
#   group_by(Format, Round) %>%
#   arrange(., desc(Count)) %>%
#   mutate(., max_group = if_else(row_number()==1, as.integer(Count), as.integer(NaN))) %>%
#   ungroup(.)
# 
# ggplot(data = closeGames, aes(x = Format, y = Count)) +
#   geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
#   theme_minimal() + 
#   labs(title = "Percentage of Close Games (Predicted Pts Diff < 3)",
#        subtitle = "Separated by Playoff Rounds (4 = Finals)",
#        x = "Playoff Format", y = "# of Close Games/Total # of Games Played") +
#   geom_text(data=closeGames, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
#   facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
#   geom_hline(data = past_Close[1:40,], aes(yintercept = n, color = V3), linetype = "dashed") +
#   geom_label_repel(data=past_Close[1:40,], aes(x = 4, y=n, label = Year, color = V3), 
#                    force = 10, size = 2.5, direction = "y", box.padding = 1) +
#   theme(legend.position = "none")
# 
# #Close Games HSD Tests (Not separated by rounds)
# closeGames1 <- data.frame(Count = closeGames1) %>% mutate(., Format = "Current")
# closeGames2 <- data.frame(Count = closeGames2) %>% mutate(., Format = "8W8E")
# closeGames3 <- data.frame(Count = closeGames3) %>% mutate(., Format = "16TOT")
# closeGamesf <- rbind(closeGames1,closeGames2,closeGames3)
# lm.closef <- lm(Count ~ Format, data = closeGamesf)
# HSD.closef <- HSD.test(lm.closef, "Format"); HSD.closef$groups
# closeGamesf <- closeGamesf %>%
#   mutate(., Group = if_else(Format == "Current", HSD.closef$groups[3,2],
#                             if_else(Format == "8W8E", HSD.closef$groups[2,2], HSD.closef$groups[1,2]))) %>%
#   group_by(Format) %>%
#   arrange(., desc(Count)) %>%
#   mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
#   ungroup(.)
# 
# ggplot(data = closeGamesf, aes(x = Format, y = Count)) +
#   geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
#   theme_minimal() + 
#   labs(title = "Percentage of Close Games (Predicted Pts Diff < 3)",
#        subtitle = "Across All Rounds",
#        x = "Playoff Format", y = "# of Close Games/Total # of Games Played") +
#   geom_text(data=closeGamesf, aes(y = max_group, label = closeGamesf$Group, vjust = -.75, col = closeGamesf$Group), show.legend = FALSE) +
#   scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
#   geom_hline(data = past_Close[41:50,], aes(yintercept = n, color = V3), linetype = "dashed") +
#   geom_label_repel(data=past_Close[41:50,], aes(x = 4, y=n, label = Year, color = V3), 
#                    force = 10, size = 2.5, direction = "y", box.padding = 1) +
#   theme(legend.position = "none")
# 
# 
# 
