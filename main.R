# Load packages
library(dplyr)
library(tidyverse)
library(readr)
library(stringr)
library(readxl)

#Original billboard rank sets
billboard <- read_csv("data/billboard.csv")
spotify <- read_csv("data/SpotifyAudioFeaturesApril2019.csv")






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

#sort by name and artist (prepare for join)
rankWeeks <- rankWeeks[order(rankWeeks$song, rankWeeks$artist),]
rankValue <- rankValue[order(rankValue$song, rankValue$artist),]

#merge weeks and value frames (inner join, both name and artist need to match)
rank <- rankValue %>% inner_join(rankWeeks, by=c("song","artist"))






#Set spotify cleaning
spotify <- subset(spotify, select = -c(track_id, popularity))
spotify <- spotify[order(spotify$track_name, spotify$artist_name),]
spotify <- rename(spotify, song = 'track_name', artist = 'artist_name')





#Prepare new set for use for chord parser utility
chords <- subset(rank, select = c(song, artist))

#assign a number id to each song for use of the chord parser utility
chords$observation <- 1:nrow(chords)
chords$observation <- (chords$observation - 1)

#reorder and remove variable name of observation id
chords <- chords[, c(3, 2, 1)]
chords <- rename(chords, Artists = "artist", Name = "song")
colnames(chords)[1] <-""

#remove uncaught commas
# change to factor line not needed      chords[c('Artists','Name')] <- lapply(chords[c('Artists','Name')], as.factor)
chords$Artists <- gsub(",","",chords$Artists)
chords$Name <- gsub(",","",chords$Name)

#Export chords as csv file for chord parser utility
write.csv(chords, "data\\scrapeSongs.csv", row.names = FALSE, quote = FALSE)

#Export string of artist names
artistNames <- as.data.frame(t(chords))
artistNames <- artistNames[2,]
write.csv(artistNames, "data\\artistNames.csv", row.names = FALSE, quote = TRUE)

#Export rank set
write.csv(rank, "data\\rank.csv", row.names = FALSE, quote = TRUE)





