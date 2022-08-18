numWeeksToCreate = 5
startDate = as.Date('2022-07-26')

library(tidyverse)
# library(rvest)
library(jsonlite)
library(TouRnament)
options(dplyr.summarise.inform=F)

#### DATA PREP ####

## Simple input file
raw = read.csv("Squash-Input-RA-TNCL-2022-Summer.csv")

inNames = raw %>%
  rename(Level = "X") %>%
  pivot_longer(starts_with("X"), names_to = "Team", values_to = "Player") %>%
  mutate(Team = sub("X", "", Team)) %>%
  filter(Player != "")

## From Rankenstein directly
url <- "https://rankenstein.ca/api.pl?action=rankings&club=RA"
table <- url %>% fromJSON() 

# TODO: Get rankings and make levels

#### FUNCTIONS ####

roundrobin_custom <- function(dfr, numWeeks) {
  generate_schedule(dfr, numWeeks) %>%
    pivot_longer(starts_with("Week"), names_to = "Matchday", values_to = "GameID") %>%
    mutate(Matchday = as.numeric(sub("Week", "", Matchday))) %>%
    group_by(Matchday, GameID) %>%
    summarize(
      Home = first(Player)
      , Away = last(Player)
    ) %>% 
    select(-GameID)
} 

is_week_bad <- function(inNames) {
  inNames %>%
    group_by(GameID) %>%
    summarize(
      EqualLevel = all(Level == first(Level)) # TODO: Faster aggregation function
      , DifferentTeam = !all(Team == first(Team))
    ) %>%
    summarize(
      allLevelsGood = all(EqualLevel)
      , allTeamsGood = all(DifferentTeam)
    ) %>%
    mutate(AnyBad = !(allLevelsGood & allTeamsGood)) %>%
    pull(AnyBad)
}

is_schedule_bad <- function(dfr) {

  players = dfr$Player
  isBad = FALSE

  # for(i in 1:numWeeks) dfr[i + nCols] = generate_week(inNames)
  long = dfr %>%
    pivot_longer(cols = starts_with("Week"), names_to = "Week", values_to = "ID") %>%
    mutate(Week = sub("Week", "", Week))

  for(pl in players){
    numInLevel = dfr %>% filter(Level == dfr$Level[dfr$Player == pl]) %>% nrow()

    opponents = long %>%
      filter(Player == pl) %>%
      select(Week, ID) %>%
      inner_join(long, by = c("Week", "ID")) %>%
      filter(Player != pl) %>%
      group_by(Player) %>%
      tally()

    if(nrow(opponents) < (numInLevel - 1)){
      cat("\nSchedule does not afford each player a game against all opponents in level", dfr$Level[dfr$Player == pl])
      isBad = TRUE
      break
    }

    if(max(opponents$n) > min(opponents$n) + 1){
      cat("\nSchedule allocates uneven game counts in level", dfr$Level[dfr$Player == pl])
      isBad = TRUE
      break
    }
  }
  isBad
}

generate_week <- function(inNames) {
  # Level must match
  # Team cannot match
  # Only one game (per week)

  ## TODO: Does not work for odd-numbered players in a level (as one would have to play someone from another level)

  nGames = if(nrow(inNames) %% 2 == 0) nrow(inNames)/2 else (nrow(inNames) - 1 / 2)
  gameIDs = rep(1:nGames, each=2)
  isBad = TRUE
  numLevels = length(unique(inNames$Level))
  levelNames = unique(inNames$Level)

  inNames$GameID = NA # Create space so not done within loop

  while(isBad){
    for(lev in levelNames){
      inNames$GameID[inNames$Level == lev] = sample(gameIDs[which(inNames$Level == lev)])
    }

    isBad = is_week_bad(inNames)
  }

  inNames$GameID
}

generate_schedule <- function(dfr, numWeeks) {
  # Should play everyone if possible
  # Shouldn't play N+2 games against one opponent if only N games against another
  # numWeeks = 9

  nCols = ncol(dfr) # Info columns coming in, could change in future

  ## Create new column for each week
  dfrWide = data.frame(
    Week = paste0("Week", 1:numWeeks)
    ,Value = NA
  ) %>%
    pivot_wider(names_from = Week, values_from = Value) %>%
    cbind(dfr, .)
  isBad = TRUE

  while(isBad){
    ## Generate a random schedule for all weeks
    for(i in 1:numWeeks) dfrWide[i + nCols] = generate_week(dfrWide)

    isBad = is_schedule_bad(dfrWide)
  }
  
  dfrWide
}

make_schedule_for_level <- function(dfr, numWeeks = 5) {
  ## Will only do two full cycles at a time 
  # dfr = dd[[1]]
  # dfr = dd[[5]]
  # numWeeks = numWeeksToCreate
  
  # n = nrow(dfr)
  # combs = choose(n, 2)
  
  thisLevel = dfr %>% pull(Level) %>% unique()
  stopifnot(length(thisLevel) == 1)
  
  ## Add extra players in case there are less than 5 (required for roundrobin function to work properly)
  # while(nrow(dfr) < 6) dfr = dfr %>% add_row(Level = "E", Player = letters[nrow(dfr) + 1])
  if(nrow(dfr) < 6){
    res = roundrobin_custom(dfr, numWeeks) %>% mutate(Level = thisLevel)
  } else {
    res = dfr %>% 
      pull(Player) %>%
      roundrobin(second_round=T,match_free=TRUE,randomize=TRUE) %>%
      filter(Matchday <= numWeeks) %>%
      mutate(Level = thisLevel)
  }
  
  res
}

#### MAINLINE ####

DaysDF = data.frame(
  MatchDate = seq(startDate, by = 7, length.out = numWeeksToCreate)
  , Matchday = 1:numWeeksToCreate
)

scheduleDF = inNames %>% 
  group_by(Level) %>% 
  group_split() %>%
  lapply(make_schedule_for_level, numWeeks = numWeeksToCreate) %>%
  reduce(bind_rows) %>%
  select(Level, Matchday, Home, Away) %>%
  inner_join(DaysDF, by = "Matchday") %>%
  select(-Matchday) %>%
  arrange(MatchDate)

write_csv(scheduleDF, paste0("Squash-Schedule-RA-TNCL-2022-Summer-Starting-", startDate, ".csv"))
