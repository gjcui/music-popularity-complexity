# Load packages
library(dplyr)
library(tidyverse)
library(readr)
library(stringr)
library(readxl)
library(tidyr)

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

#Prepare for fuzzy join
library(stringdist)
#standardize cases and spaces
rank$artist <- gsub("[^[:alnum:][:space:]]", "", tolower(trimws(rank$artist)))
rank$song <- gsub("[^[:alnum:][:space:]]", "", tolower(trimws(rank$song)))
spotify$artist <- gsub("[^[:alnum:][:space:]]", "", tolower(trimws(spotify$artist)))
spotify$song <- gsub("[^[:alnum:][:space:]]", "", tolower(trimws(spotify$song)))

# Removing unwanted words from the "artist" variable in the "spotify" dataset
spotify$artist <- gsub("(?i)\\s*(feat(uring)?|and)\\s.*", "", spotify$artist)
spotify$artist <- gsub("\\[radio edit\\]|\\[dub\\]", "", spotify$artist)

# Removing unwanted words from the "artist" variable in the "rank" dataset
rank$artist <- gsub("(?i)\\s*(feat(uring)?|and)\\s.*", "", rank$artist)
rank$artist <- gsub("\\[radio edit\\]|\\[dub\\]", "", rank$artist)

#remove common words that are not a part of a song
remove_words <- c("\\[radio edit\\]", "\\[dub\\]", "remix", "version", "radio edit", "acoustic")
rank$artist <- gsub(paste0("\\b(", paste(remove_words, collapse = "|"), ")\\b"), "", rank$artist)
rank$song <- gsub(paste0("\\b(", paste(remove_words, collapse = "|"), ")\\b"), "", rank$song)
spotify$artist <- gsub(paste0("\\b(", paste(remove_words, collapse = "|"), ")\\b"), "", spotify$artist)
spotify$song <- gsub(paste0("\\b(", paste(remove_words, collapse = "|"), ")\\b"), "", spotify$song)

#remove missing obs
rank <- rank[complete.cases(rank$song, rank$artist),]
spotify <- spotify[complete.cases(spotify$song, spotify$artist),]
rank <- rank[complete.cases(trimws(rank$song)) & complete.cases(trimws(rank$artist)),]
spotify <- spotify[complete.cases(trimws(spotify$song)) & complete.cases(trimws(spotify$artist)),]

# Remove extra spaces in spotify dataset
spotify$song <- str_replace_all(spotify$song, "\\s+", " ")
spotify$artist <- str_replace_all(spotify$artist, "\\s+", " ")

# Remove extra spaces in rank dataset
rank$song <- str_replace_all(rank$song, "\\s+", " ")
rank$artist <- str_replace_all(rank$artist, "\\s+", " ")



joined_data <- inner_join(rank, spotify, by = c("song", "artist"))

















#join spotify and rank
song_attributes <- inner_join(rank, spotify, by = c("artist" = "artist", "song" = "song"))


#----------------------------------------------------------------------------------------------------------------------------------------------
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






