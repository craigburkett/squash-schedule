RATING_DEFAULT = 6500
maxDaysLimit = 183
club = "WSQ"
year = "2023"
season = "Winter"

library(tidyverse)
library(magrittr)
library(jsonlite)

source("Helpers.R")

options(dplyr.summarise.inform=F)
todaysDate = Sys.Date()
startDateStr = gsub("-", "", as.character(todaysDate - maxDaysLimit))

fileString = paste(club, year, season, sep = "-")
outDir = "Output/"
if(!dir.exists(outDir)) dir.create(outDir)
outFile = paste0(outDir, "Squash-Schedule-", fileString, "-", todaysDate, ".csv")

#### DATA PREP ####

## Names and Ranks from Rankenstein directly
url <- "https://rankenstein.ca/api.pl?action=rankings" # Only gets 'active' players, unfortunately
# url <- "https://rankenstein.ca/api.pl?action=rankings&club=WSQ"
# url <- "https://rankenstein.ca/api.pl?action=rankings&club=WSQ&status=INACTIVE"

rankings <- url %>% fromJSON(flatten = T) %>% extract2("rankings") %>% select(starts_with("player"), rating) %>% rename(Name = player.name, ID = player.id)

## Simple input file - join to ratings online
players = read.csv(file.choose()) %>%
  # players = read.csv(paste0("Squash-Input-", fileString, ".csv")) %>%
  mutate(Name = paste(First, Last)) %>%
  left_join(rankings, by = "Name") %>%
  # Estimate from input file takes priority, then Rankenstein rating (could be old or egregiously wrong), then default value if neither available
  mutate(rating = ifelse(!is.na(RatingEstimate), RatingEstimate, ifelse(!is.na(rating), rating, RATING_DEFAULT))) %>%
  filter(is.na(Absent) | Absent == 0)

tt = players %>% filter(is.na(ID))
message("Names not found in Rankenstein:\n\n")
message(paste0(capture.output(tt), collapse = '\n'))

# players %>% select(Name, Division, rating) %>% arrange(Division, desc(rating))

#### MAINLINE ####
playersSub = players %>%
  select(Name, rating, starts_with("No_")) %>%
  arrange(Name)

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
  summarize(DaysSinceLastGame = min(todaysDate - as.Date(date)) %>% as.numeric())

## Both ways
fullSumm = summ %>%
  rename(Opponent = PlayerB, Player = PlayerA) %>%
  select(Player, Opponent, DaysSinceLastGame) %>%
  bind_rows(rename(summ, Opponent = PlayerA, Player = PlayerB))

## Assign games one Division at a time, for 5 weeks
for(div in unique(players$Division)){
  playersThisDiv = filter(players, Division == div)
  
  allPairings = expand.grid.unique(playersThisDiv$Name, playersThisDiv$Name) %>%
    as.data.frame() %>%
    rename(Opponent = V1, Player = V2) %>%
    left_join(fullSumm, by = c("Player", "Opponent")) %>%
    left_join(playersSub, by = c("Player" = "Name")) %>%
    rename(PlayerRating = rating) %>%
    left_join(playersSub, by = c("Opponent" = "Name")) %>%
    rename(OpponentRating = rating) %>%
    mutate(NoEarly = coalesce(No_early_timeslot.x, No_early_timeslot.y)) %>% # If either can't play at this time slot
    mutate(NoLate  = coalesce(No_late_timeslot.x, No_late_timeslot.y)) %>% # Basically an OR
    select(-c(ends_with(".x"), ends_with(".y"))) %>%
    mutate(RatingDiff = abs(PlayerRating - OpponentRating)) %>%
    replace_na(list(DaysSinceLastGame = maxDaysLimit)) %>%
    mutate(DaysSinceLastGame = ifelse(DaysSinceLastGame > maxDaysLimit, maxDaysLimit, DaysSinceLastGame))
  
  
}
  
  # if(daysSinceLastMatch == maxDaysLimit) daysSinceLastMatch = paste(maxDaysLimit, "or more")
  
  ## Display match (names, ratings, days since last game) on console and write to file
  # cat(paste0("\nMatch created: ", player, " (", playerRating,") \tvs ", opponent, " (", opponentRating,") \t- ", daysSinceLastMatch, " days since last match"))
  # thisRow = data.frame(Home = player, HomeRating = playerRating, Away = opponent, AwayRating = opponentRating, DaysSinceLastMatch = daysSinceLastMatch)
  # write.table(thisRow, file = outFile, append = T, row.names = F, col.names = F, sep = ",")
  
  