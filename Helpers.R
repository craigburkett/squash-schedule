
get_similarity <- function(RatingDiff, DaysSinceLastGame) {
  RatingDiff + (maxDaysLimit - DaysSinceLastGame)^1.4
}
