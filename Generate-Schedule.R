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
# url <- "https://rankenstein.ca/api.pl?action=rankings&club=WSQ"
# url <- "https://rankenstein.ca/api.pl?action=rankings&club=WSQ&status=INACTIVE"

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

# players %>% select(Name, Division, rating) %>% arrange(Division, desc(rating))

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
  summarize(DaysSinceLastGame = min(todaysDate - as.Date(date)) %>% as.numeric())

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
# 
# Match created: Assad Khan (8198) 	        vs Jason Salares (7985) 	          - 90 or more days since last match
# Match created: Andrew Papp-Castari (7986)	vs Walter Dicesare (7783) 	        - 90 or more days since last match
# Match created: Toby Kemp (7955) 	        vs Tracey Sly (7707) 	              - 90 or more days since last match
# Match created: Joe Gallant (7697) 	      vs Jason Kim (7595) 	              - 90 or more days since last match
# Match created: Rahmat Shahidi (7579) 	    vs Nick Zhang (7540) 	              - 90 or more days since last match
# Match created: Julie MacElwee (7499) 	    vs Sharat Gupta (7493) 	            - 90 or more days since last match
# Match created: Helen Kennedy (7494) 	    vs Hassan Sarbishaei (7489) 	      - 90 or more days since last match
# Match created: Lorraine Tetreault (7439) 	vs Clive Dsousa (7370) 	            - 90 or more days since last match
# Match created: Lisa MacElwee (7402) 	    vs James Harley (7296) 	            - 90 or more days since last match
# Match created: Sam Goolamallee (7364) 	  vs Dan Chow (7280) 	                - 90 or more days since last match
# Match created: Lyndsay Gardiner (7363) 	  vs Andre Lamprecht (7311) 	        - 90 or more days since last match
# Match created: Brent Powell (7273) 	      vs Matt Delaurier (7124) 	          - 90 or more days since last match
# Match created: Bernard St.Denis (7266) 	  vs Richard Swift (7006) 	          - 90 or more days since last match
# Match created: Craig Burkett (6754) 	    vs Kevin Gray (6692) 	              - 90 or more days since last match
# Match created: Shawn Dawson (6669) 	      vs Detlef Dransch (6606) 	          - 90 or more days since last match
# Match created: Ammar Gupta (6500) 	      vs Tom Montor (6498) 	              - 90 or more days since last match
# Match created: Anthony Wright (6491) 	    vs Paula Piilonen (6309) 	          - 90 or more days since last match
# Match created: Matt Taylor (6456) 	      vs Steven Charlebois-Page (6303) 	  - 90 or more days since last match
# Match created: Francis Mcquaid (6300) 	  vs Ray Nazarzai (6288) 	            - 90 or more days since last match
# Match created: Aadi Dua (6284) 	          vs Richard Katan (6272) 	          - 90 or more days since last match
# Match created: Doug Herbert (6245) 	      vs Syed Hassan (6218) 	            - 90 or more days since last match
# Match created: Tim Ambery (6235) 	        vs Peter Crawley (6232) 	          - 90 or more days since last match
# Match created: Eric Mia (6200) 	          vs Ben Tomka (6187) 	              - 90 or more days since last match
# Match created: Shreyans Shah (6200) 	    vs <SPARE>


