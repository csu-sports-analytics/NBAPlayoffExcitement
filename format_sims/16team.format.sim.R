#### Simulating First Round of Playoffs ####
r1Sim16 <- function(r1){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up first round matchups
  round1 <- data.frame(matrix(ncol=4,nrow=8))
  colnames(round1) <- c("high", "low", "highW", "lowW")
  for(i in 1:8){
    round1$high[i] <- r1$Team[i]
    round1$highW[i] <- 0
    round1$low[i] <- r1$Team[nrow(r1)-i+1]
    round1$lowW[i] <- 0
  }
  #There are 8 first round series
  for(s in 1:8){
    #Initializing values
    high <- round1$high[s]
    highW <- 0
    low <- round1$low[s]
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
        round1$highW[s] <- highW
        round1$lowW[s] <- lowW
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
  numg6g7 <- sum(round1$highW+round1$lowW>5)
  return(c(round1,numHighOT/numGame,numUpset/numGame,numg6g7/8,numClose/numGame))
}


#### Simulating Second Round of Playoffs ####
r2Sim16 <- function(r2){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up second round matchups
  round2 <- data.frame(matrix(ncol=4,nrow=4))
  colnames(round2) <- c("high", "low", "highW", "lowW")
  for(i in 1:4){
    round2$high[i] <- r2$Team[i]
    round2$highW[i] <- 0
    round2$low[i] <- r2$Team[nrow(r2)-i+1]
    round2$lowW[i] <- 0
  }
  #There are 4 second round series
  for(s in 1:4){
    #Initializing values
    high <- round2$high[s]
    highW <- 0
    low <- round2$low[s]
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
        round2$highW[s] <- highW
        round2$lowW[s] <- lowW
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
  numg6g7 <- sum(round2$highW+round2$lowW>5)
  return(c(round2,numHighOT/numGame,numUpset/numGame,numg6g7/4,numClose/numGame))
}


#### Simulating Third Round of Playoffs ####
r3Sim16 <- function(r3){
  numUpset <- 0
  numGame <- 0
  numHighOT <- 0
  numg6g7 <- 0
  numClose <- 0
  #Setting up third round matchups
  round3 <- data.frame(matrix(ncol=4,nrow=2))
  colnames(round3) <- c("high", "low", "highW", "lowW")
  #There is only one matchup in each conference
  for(i in 1:2){
    round3$high[i] <- r3$Team[i]
    round3$highW[i] <- 0
    round3$low[i] <- r3$Team[nrow(r3)-i+1]
    round3$lowW[i] <- 0
  }
  #There are 2 third round series
  for(s in 1:2){
    #Initializing values
    high <- round3$high[s]
    highW <- 0
    low <- round3$low[s]
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
        round3$highW[s] <- highW
        round3$lowW[s] <- lowW
        #If win prob is between .48 and .52 we can say there is a decent chance the game
        #goes to OT
        if(abs(outcome[[2]]-.5)<.02){
          numHighOT <- numHighOT+1
        }else{}
        # If predicted pts diff is less than 4, this is very close, one possession game
        # that is likely going down to the wire
        if(isTRUE(abs(outcome[[3]])<=4)){
          numClose <- numClose + 1
        }else{}
      }else{
        #Once a team has reached 4 wins, store final totals in the df
      }
    }
  }
  #Counting how many series went to games 6 or 7
  numg6g7 <- sum(round3$highW+round3$lowW>5)
  return(c(round3,numHighOT/numGame,numUpset/numGame,numg6g7/2,numClose/numGame))
}


#### Simulating Third Round of Playoffs ####
finalsSim16 <- function(finals){
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