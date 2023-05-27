
get_similarity <- function(RatingDiff, DaysSinceLastGame) {
  
  RatingDiff + (maxDaysLimit - DaysSinceLastGame)^1.4
}

expand.grid.unique <- function(x, y, include.equals=FALSE){
  x <- unique(x)
  y <- unique(y)
  
  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  
  do.call(rbind, lapply(seq_along(x), g))
}

get_all_matchups <- function(names1, names2) {
  expand.grid.unique(names1, names2) %>%
    as.data.frame() %>%
    rename(Player1 = V1, Player2 = V2) %>%
    left_join(playersThisDiv, by = c("Player1" = "Name")) %>%
    rename(Rating1 = Rating) %>%
    left_join(playersThisDiv, by = c("Player2" = "Name")) %>%
    rename(Rating2 = Rating) %>%
    mutate(No540 = coalesce(No_540_timeslot.x, No_540_timeslot.y)) %>% # If either can't play at this time slot
    mutate(No620 = coalesce(No_620_timeslot.x, No_620_timeslot.y)) %>% # If either can't play at this time slot
    mutate(No820 = coalesce(No_820_timeslot.x, No_820_timeslot.y)) %>% # Basically an OR
    mutate(Division = coalesce(Division.x, Division.y)) %>%
    replace_na(list(No540 = 0, No620 = 0, No820 = 0)) %>%
    select(-c(ends_with(".x"), ends_with(".y"))) %>%
    mutate(RatingDiff = abs(Rating1 - Rating2)) 
}

add_dates <- function(dfr, allDates) {
  dfr %>%
    mutate(Date = c(allDates, allDates[c(3,4,5,2)], allDates[c(5,1,4)], allDates[c(2, 1)], allDates[3]))
}

add_dates_sample <- function(sch, preSchedGames, allDates) {
  startAgain = TRUE
  while(startAgain){
    availDates = expand.grid(Player = playersThisDiv$Name, Date = allDates) %>% as.data.frame() %>%
      anti_join(select(preSchedGames, Player2, Date), by = c("Player" = "Player2", "Date" = "Date")) %>%
      anti_join(select(preSchedGames, Player1, Date), by = c("Player" = "Player1", "Date" = "Date"))
    
    ## Add dates
    sch$Date = allDates[1]
    for(i in 1:nrow(sch)){
      startAgain = FALSE
      thisDate = availDates %>% 
        filter(Player %in% c(sch$Player1[i], sch$Player2[i])) %>% 
        group_by(Date) %>% tally() %>% 
        filter(n==2) 
      
      if(nrow(thisDate) == 0){
        startAgain = TRUE
        break
      }
      thisDate = thisDate %>% sample_n(1) %>% pull(Date)
      
      sch$Date[i] = thisDate
      ind = which(availDates$Date == thisDate & availDates$Player %in% c(sch$Player1[i], sch$Player2[i]))
      availDates = availDates[-ind,]
      rownames(availDates) <- NULL
    }
  } # END while
  sch
}

get_bad_times <- function(row) {
  row %>% select(starts_with("No")) %>% 
    pivot_longer(everything(), names_to = "Heading", values_to = "IsBad") %>%
    inner_join(timeLookup, by = "Heading") %>%
    filter(IsBad == 1) %>%
    pull(Time)
}

get_court <- function(thisDate, allDateTimes, badTimes) {
  dd = allDateTimes %>% 
    mutate(Time = as.character(Time)) %>% 
    filter(Date == thisDate) %>%
    filter(!Time %in% badTimes) %>%
    group_by(Time) %>%
    tally() %>%
    filter(n == max(n)) %>%
    sample_n(1)
  
  allDateTimes %>% 
    mutate(Time = as.character(Time)) %>% 
    filter(Date == thisDate) %>% 
    filter(Time == dd$Time) %>%
    filter(Court == max(Court))
}

add_times_and_courts <- function(sch, allDateTimes) {
  for(i in 1:nrow(sch)){
    badTimes = get_bad_times(sch[i,]) # We won't consider any of these times
    thisCourt = get_court(sch$Date[i], allDateTimes, badTimes)
    
    sch$Time[i] = thisCourt$Time
    sch$Court[i] = thisCourt$Court
    
    ind = which(allDateTimes$Date == sch$Date[i] & allDateTimes$Time == thisCourt$Time & allDateTimes$Court == thisCourt$Court)
    allDateTimes = allDateTimes[-ind,]
    rownames(allDateTimes) <- NULL
  }
  sch
}

is_time_bad <- function(row, thisTime) {
  # TODO: Create dictionary instead
  if(row$No540 && thisTime == allTimes[1]) return(TRUE)
  if(row$No620 && thisTime == allTimes[2]) return(TRUE)
  if(row$No820 && thisTime == allTimes[5]) return(TRUE)
  
  FALSE
}

get_paired_players <- function(sch, pairedDiv) {
  sch %>% 
    filter(Division == pairedDiv) %>% 
    select(Player1, Player2) %>% 
    pivot_longer(cols = everything(), values_to = "Player") %>% 
    filter(Player != "bye") %>% 
    pull(Player) %>% 
    unique()
}

add_bye_players <- function(sch, addedPlayers, div, pairedDiv) {
  sch %>%
    left_join(addedPlayers, by = c("Player1", "Player2", "Division")) %>%
    mutate(Division = ifelse(Player2 == "bye" & Division == pairedDiv, paste(div, pairedDiv), Division)) %>%
    mutate(Player2 = ifelse(Player2 == "bye" & Division == paste(div, pairedDiv), OtherDivPlayer, Player2)) %>%
    select(-OtherDivPlayer)
}
