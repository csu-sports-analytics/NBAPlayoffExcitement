library(tidyverse)
library(stringr)
library(teamcolors)

#### Simulating First Round of Playoffs ####
r1SimCurr <- function(r1){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
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
        numGame <- numGame+1
        #If outcome of sim is true, high team won the game
        if(isTRUE(outcome[[1]] == TRUE)){
          highW <- highW+1
        }else{
          #If outcome of sim is false, low team won the game
          lowW <- lowW+1
          numUpset <- numUpset +1
        }
        west1$highW[s] <- highW
        west1$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.02){
          numHighOT <- numHighOT+1
        }
        # If predicted pts diff is less than 4, this is very close, one possession game
        # that is likely going down to the wire
        if(abs(outcome[[3]])<=4){
          numClose <- numClose + 1
        }
        else{}
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
        numGame <- numGame+1
        #If outcome of sim is true, high team won the game
        if(isTRUE(outcome[[1]] == TRUE)){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
          numUpset <- numUpset +1
        }
        east1$highW[s] <- highW
        east1$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(isTRUE(abs(outcome[[2]]-.5)<.02)){
          numHighOT <- numHighOT+1
        }
        else{}
        # If predicted pts diff is less than 4, this is very close, one possession game
        # that is likely going down to the wire
        if(isTRUE(abs(outcome[[3]])<=4)){
          numClose <- numClose + 1
        }
        else{}
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
      }
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(west1$highW+west1$lowW>5) + sum(east1$highW+east1$lowW>5)
  return(c(west1,east1,numHighOT/numGame,numUpset/numGame,numg6g7/8,numClose/numGame))
}


#### Simulating Second Round of Playoffs ####
r2SimCurr <- function(r2){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
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
        numGame <- numGame+1
        #If outcome of sim is true, high team won the game
        if(isTRUE(outcome[[1]] == TRUE)){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
          numUpset <- numUpset +1
        }
        west2$highW[s] <- highW
        west2$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.02){
          numHighOT <- numHighOT+1
        }
        else{}
        # If predicted pts diff is less than 4, this is very close, one possession game
        # that is likely going down to the wire
        if(abs(outcome[[3]])<=4){
          numClose <- numClose + 1
        }
        else{}
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
        numGame <- numGame+1
        #If outcome of sim is true, high team won the game
        if(isTRUE(outcome[[1]] == TRUE)){
          highW <- highW+1
        }
        #If outcome of sim is false, low team won the game
        else{
          lowW <- lowW+1
          numUpset <- numUpset +1
        }
        east2$highW[s] <- highW
        east2$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.02){
          numHighOT <- numHighOT+1
        }
        else{}
        # If predicted pts diff is less than 4, this is very close, one possession game
        # that is likely going down to the wire
        if(abs(outcome[[3]])<=4){
          numClose <- numClose + 1
        }
        else{}
      }
      #Once a team has reached 4 wins, store final totals in the df
      else{
      }
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(west2$highW+west2$lowW>5) + sum(east2$highW+east2$lowW>5)
  return(c(west2,east2,numHighOT/numGame,numUpset/numGame,numg6g7/4,numClose/numGame))
}


#### Simulating Third Round of Playoffs ####
r3SimCurr <- function(r3){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose3 <- 0
  #Setting up third round matchups for the west
  west <- filter(r3, Conf=="W")
  west3 <- data.frame(matrix(ncol=4,nrow=1))
  colnames(west3) <- c("high", "low", "highW", "lowW")
  #There is only one matchup in each conference
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
      numGame <- numGame+1
      #If outcome of sim is true, high team won the game
      if(isTRUE(outcome[[1]] == TRUE)){
        highW <- highW+1
      }else{
        #If outcome of sim is false, low team won the game
        lowW <- lowW+1
        numUpset <- numUpset +1
      }
      west3$highW[s] <- highW
      west3$lowW[s] <- lowW
      #If win prob is between .48 and .52 we can say there is a decent chance the game
      #goes to OT
      if(abs(outcome[[2]]-.5)<.02){
        numHighOT <- numHighOT+1
      }else{}
      # If predicted pts diff is less than 4, this is very close, one possession game
      # that is likely going down to the wire
      if(isTRUE(abs(outcome[[3]])<=4)){
        numClose3 <- numClose3 + 1
      }else{}
    }else{
      #Once a team has reached 4 wins, store final totals in the df
    }
  }
  
  #Setting up third round matchups for the east
  east <- filter(r3, Conf=="E")
  east3 <- data.frame(matrix(ncol=4,nrow=1))
  colnames(east3) <- c("high", "low", "highW", "lowW")
  #There is only one matchup in each conference
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
      numGame <- numGame+1
      #If outcome of sim is true, high team won the game
      if(isTRUE(outcome[[1]] == TRUE)){
        highW <- highW+1
      }
      #If outcome of sim is false, low team won the game
      else{
        lowW <- lowW+1
        numUpset <- numUpset +1
      }
      east3$highW[s] <- highW
      east3$lowW[s] <- lowW
      #If win prob is between .48 and .52 we can say there is a decent chance the game
      #goes to OT
      if(abs(outcome[[2]]-.5)<.02){
        numHighOT <- numHighOT+1
      }
      else{}
      # If predicted pts diff is less than 4, this is very close, one possession game
      # that is likely going down to the wire
      if(abs(outcome[[3]])<=4){
        numClose3 <- numClose3 + 1
      }
      else{}
    }
    #Once a team has reached 4 wins, store final totals in the df
    else{
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(west3$highW+west3$lowW>5) + sum(east3$highW+east3$lowW>5)
  return(c(west3,east3,numHighOT/numGame,numUpset/numGame,numg6g7/2,numClose3/numGame))
}


#### Simulating Third Round of Playoffs ####
finalsSimCurr <- function(finals){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up finals matchup
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
      numGame <- numGame+1
      #If outcome of sim is true, high team won the game
      if(isTRUE(outcome[[1]] == TRUE)){
        highW <- highW+1
      }
      #If outcome of sim is false, low team won the game
      else{
        lowW <- lowW+1
        numUpset <- numUpset +1
      }
      finals_mu$highW[s] <- highW
      finals_mu$lowW[s] <- lowW
      #If win prob is between .48 and .52 we can say there is a decent chance the game
      #goes to OT
      if(abs(outcome[[2]]-.5)<.02){
        numHighOT <- numHighOT+1
      }
      else{}
      # If predicted pts diff is less than 4, this is very close, one possession game
      # that is likely going down to the wire
      if(abs(outcome[[3]])<=4){
        numClose <- numClose + 1
      }
      else{}
    }
    #Once a team has reached 4 wins, store final totals in the df
    else{
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(finals_mu$highW+finals_mu$lowW>5)
  if(finals_mu$highW > finals_mu$lowW){
    champion <- finals_mu$high
  }
  else{
    champion <- finals_mu$low
  }
  return(c(finals_mu,numHighOT/numGame,numUpset/numGame,numg6g7,numClose/numGame,champion))
}