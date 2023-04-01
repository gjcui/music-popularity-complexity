# Load packages
library(dplyr)
library(tidyverse)

#Original billboard rank set
billboard <- read_csv("data/billboard.csv")






#Sets for rank average calculations
rankValue <- billboard
rankWeeks <- billboard

#keep only certain variables
keptValue <- c("date", "rank", "song", "artist")
keptWeeks <- c("song", "artist", "weeks_on_board")
rankValue <- rankValue[keptValue]
rankWeeks <- rankWeeks[keptWeeks]

#keep only the highest (true) value of weeks on board
rankWeeks <- rankWeeks[order(rankWeeks$song, -rankWeeks$weeks_on_board),]
rankWeeks <- rankWeeks[ !duplicated(rankWeeks[c("song","artist")]), ] #this ensures songs with same names by different artists won't be merged into 1

#change rank value so more popular has higher value
rankValue$rank <- (101 - rankValue$rank)

#add per week rank to find overall popularity (grouping by artist and song so songs with same names by different artists won't be merged)
rankValue <- rankValue %>%
  group_by(song, artist) %>%
  reframe(rank = sum(rank))














