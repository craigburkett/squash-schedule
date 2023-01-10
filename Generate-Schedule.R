maxDaysLimit = 90
RATING_DEFAULT = 6500
club = "RA"
league = "TNCL"
year = "2023"
season = "Winter"

library(tidyverse)
library(magrittr)
library(jsonlite)

source("Helpers.R")

options(dplyr.summarise.inform=F)
todaysDate = Sys.Date()
startDateStr = gsub("-", "", as.character(todaysDate - maxDaysLimit - 30))

fileString = paste(club, league, year, season, sep = "-")
outDir = "Output/"
if(!dir.exists(outDir)) dir.create(outDir)
outFile = paste0(outDir, "Squash-Schedule-", fileString, "-", todaysDate, ".csv")
data.frame(Home=character(), HomeRating=integer(), Away=character(), AwayRating=integer(), DaysSinceLastMatch=character()) %>%
  write.csv(outFile, row.names=F)

#### DATA PREP ####
# if(!file.exists(paste0("Squash-Input-", fileString, ".csv"))) stop("Input file does not exist.")

## Names and Ranks from Rankenstein directly
url <- "https://rankenstein.ca/api.pl?action=rankings" # &club=RA
rankings <- url %>% fromJSON(flatten = T) %>% extract2("rankings") %>% select(starts_with("player"), rating) %>% rename(Name = player.name, ID = player.id)

## Simple input file - join to ratings online
players = read.csv(file.choose()) %>%
# players = read.csv(paste0("Squash-Input-", fileString, ".csv")) %>%
  mutate(Name = paste(First, Last)) %>%
  left_join(rankings, by = "Name") %>%
  # Estimate from input file takes priority, then Rankenstein rating (could be old or egregiously wrong), then default value is neither available
  mutate(rating = ifelse(!is.na(RatingEstimate), RatingEstimate, ifelse(!is.na(rating), rating, RATING_DEFAULT))) %>%
  filter(is.na(Absent) | Absent == 0)

tt = players %>% filter(is.na(ID))
message("Names not found in Rankenstein:\n\n")
message(paste0(capture.output(tt), collapse = '\n'))

players = players %>%
  select(Name, rating) %>%
  arrange(Name)

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
while(length(loopPlayers) > 0){
  
  player = loopPlayers[1]
  
  # IF there's only one player left, just assign them to play a spare
  if(length(loopPlayers) == 1){
    opponent = "<SPARE>"
    cat(paste0("\nMatch created: ", player, " (", playerRating,") \tvs ", opponent))
    thisRow = data.frame(Home = player, HomeRating = playerRating, Away = opponent, AwayRating = "", DaysSinceLastMatch = "")
    write.table(thisRow, file = outFile, append = T, row.names = F, col.names = F, sep = ",")
    break
  } 
  
  ## Find best match for current player
  thisPairing = workingPairings %>% filter(Player == player) %>% filter(Similarity == min(Similarity)) %>% slice(1)
  opponent = thisPairing %>% pull(Opponent)
  
  ## Get player ratings and days since last match
  playerRating = players$rating[players$Name == player]
  opponentRating = players$rating[players$Name == opponent]
  daysSinceLastMatch = thisPairing %>% pull(DaysSinceLastGame)
  
  if(daysSinceLastMatch == maxDaysLimit) daysSinceLastMatch = paste(maxDaysLimit, "or more")
  
  ## Display match (names, ratings, days since last game) on console and write to file
  cat(paste0("\nMatch created: ", player, " (", playerRating,") \tvs ", opponent, " (", opponentRating,") \t- ", daysSinceLastMatch, " days since last match"))
  thisRow = data.frame(Home = player, HomeRating = playerRating, Away = opponent, AwayRating = opponentRating, DaysSinceLastMatch = daysSinceLastMatch)
  write.table(thisRow, file = outFile, append = T, row.names = F, col.names = F, sep = ",")
  
  ## Remove players from pool
  loopPlayers = setdiff(loopPlayers, c(player, opponent))
  workingPairings = filter(workingPairings, Opponent != opponent, Opponent != player)
}
# TODO: Last two players might have just played, should look back and swap if this is the case
