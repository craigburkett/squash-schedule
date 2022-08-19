maxDaysLimit = 90

library(tidyverse)
library(magrittr)
library(jsonlite)

source("Helpers.R")

options(dplyr.summarise.inform=F)

outDir = "Output/"
if(!dir.exists(outDir)) dir.create(outDir)

todaysDate = Sys.Date()
startDateStr = gsub("-", "", as.character(todaysDate - maxDaysLimit - 30))

#### DATA PREP ####

## Names and Ranks from Rankenstein directly
url <- "https://rankenstein.ca/api.pl?action=rankings" # &club=RA
rankings <- url %>% fromJSON(flatten = T) %>% extract2("rankings") %>% select(starts_with("player"), rating) %>% rename(Name = player.name, ID = player.id)

## Simple input file - join to ratings online
players = read.csv("Squash-Input-RA-TNCL-2022-Fall.csv") %>%
  left_join(rankings, by = "Name") %>%
  mutate(rating = ifelse(is.na(rating), RatingEstimate, rating)) %>%
  select(Name, rating) %>%
  arrange(Name)

# TODO: Some kind of error checking on input names, if I can't find them in Rankenstein, with a message at least

#### MAINLINE ####
ourPlayers = pull(players, Name)

## Get previous played games and date of match
url <- paste0("https://rankenstein.ca/api.pl?action=results&start=", startDateStr) # &club=RA

results <- url %>% 
  fromJSON(flatten = T) %>% 
  select(loserName, winnerName, winnerId, loserId, date) %>% # About 15s to pull all data; <1s with 4 month restriction
  filter(loserName %in% ourPlayers & winnerName %in% ourPlayers)

## Each pairing and the number of days since their last match
summ = results %>%
  mutate(
     PlayerA = ifelse(winnerName < loserName, winnerName, loserName)
    ,PlayerB = ifelse(winnerName > loserName, winnerName, loserName)
    ) %>%
  select(PlayerA, PlayerB, date) %>%
  distinct() %>%
  group_by(PlayerA, PlayerB) %>%
  summarize(DaysSinceLastGame = min(todaysDate - as.Date(date)))

fullSumm = summ %>%
  rename(Opponent = PlayerB, Player = PlayerA) %>%
  select(Player, Opponent, DaysSinceLastGame) %>%
  bind_rows(rename(summ, Opponent = PlayerA, Player = PlayerB))

allPairings = expand.grid(Opponent = ourPlayers, Player = ourPlayers) %>%
  left_join(fullSumm, by = c("Player", "Opponent")) %>%
  left_join(players, by = c("Player" = "Name")) %>%
  rename(PlayerRating = rating) %>%
  left_join(players, by = c("Opponent" = "Name")) %>%
  rename(OpponentRating = rating) %>%
  mutate(RatingDiff = abs(PlayerRating - OpponentRating)) %>%
  filter(Player != Opponent) %>%
  replace_na(list(DaysSinceLastGame = maxDaysLimit)) %>%
  # TODO: Opponents since last game instead? Not symmetrical though ...
  mutate(DaysSinceLastGame = ifelse(DaysSinceLastGame > maxDaysLimit, maxDaysLimit, DaysSinceLastGame)) %>%
  # TODO: Get court time preferences and consider in scoring function
  mutate(Similarity = get_similarity(RatingDiff, DaysSinceLastGame))

## Loop through players starting from the top, and take the best opponent one at a time
# TODO: Consider all players simultaneously
workingPairings = allPairings
loopPlayers = allPairings %>% select(Player, PlayerRating) %>% distinct() %>% arrange(desc(PlayerRating)) %>% pull(Player)
while(length(loopPlayers) > 1){
  player = loopPlayers[1]
  thisPairing = workingPairings %>% filter(Player == player) %>% filter(Similarity == min(Similarity))
  opponent = thisPairing %>% pull(Opponent)
  
  playerRating = players$rating[players$Name == player]
  opponentRating = players$rating[players$Name == opponent]
  daysSinceLastMatch = thisPairing %>% pull(DaysSinceLastGame)
  if(daysSinceLastMatch == maxDaysLimit) daysSinceLastMatch = paste(maxDaysLimit, "or more")
  
  cat(paste0("\nMatch created: ", player, " (", playerRating,") \tvs ", opponent, " (", opponentRating,") \t- ", daysSinceLastMatch, " days since last match"))
  loopPlayers = setdiff(loopPlayers, c(player, opponent))
  workingPairings = filter(workingPairings, Opponent != opponent, Opponent != player)
}

# TODO: Output schedule however
# write_csv(scheduleDF, paste0(outDir, "Squash-Schedule-RA-TNCL-2022-Summer-Starting-", startDate, ".csv"))
