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

spotify$artist <- trimws(spotify$artist)
spotify$song <- trimws(spotify$song)

rank$artist <- trimws(rank$artist)
rank$song <- trimws(rank$song)




#join sets (inner)
joined_data <- inner_join(rank, spotify, by = c("song", "artist"))














library(caret)


# Split the dataset into training and testing sets
set.seed(123)
training_rows <- createDataPartition(joined_data$rank, p = 0.7, list = FALSE)
training_set <- joined_data[training_rows, ]
testing_set <- joined_data[-training_rows, ]

# Train a linear model
modelAA <- lm(rank ~ key + mode + time_signature + tempo + loudness + duration_ms, data = training_set)

# View the summary of the model
summary(modelAA)
AIC(modelAA)

# Make predictions on the testing set
predictions <- predict(modelAA, newdata = testing_set)

# Evaluate the performance of the model using RMSE
rmse <- sqrt(mean((testing_set$rank - predictions)^2))
rmse

# Create scatterplots of the predictor variables against popularity
library(ggplot2)
ggplot(joined_data, aes(x = mode, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm")

library(ggplot2)
ggplot(joined_data, aes(x = instrumentalness, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm")

library(ggplot2)
ggplot(joined_data, aes(x = duration_ms, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm")












# Train full linear model with all variables
modelAB <- lm(rank ~ key + mode + time_signature + tempo + loudness + duration_ms + acousticness + danceability + energy + instrumentalness + liveness + speechiness + valence, data = training_set)

# View the summary of the model
summary(modelAB)
AIC(modelAB)

# Make predictions on the testing set
predictions <- predict(modelAB, newdata = testing_set)

# Evaluate the performance of the model using RMSE
rmse <- sqrt(mean((testing_set$rank - predictions)^2))
rmse





# Train a linear model
modelHC <- lm(rank ~ duration_ms + energy + speechiness + valence, data = training_set)

# View the summary of the model
summary(modelHC)
AIC(modelHC)

# Make predictions on the testing set
predictions <- predict(modelHC, newdata = testing_set)

# Evaluate the performance of the model using RMSE
rmse <- sqrt(mean((testing_set$rank - predictions)^2))
rmse

# Create scatterplots of the predictor variables against popularity
library(ggplot2)
ggplot(joined_data, aes(x = energy, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm")

library(ggplot2)
ggplot(joined_data, aes(x = speechiness, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm")

library(ggplot2)
ggplot(joined_data, aes(x = duration_ms, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm")

library(ggplot2)
ggplot(joined_data, aes(x = valence, y = rank)) +
  geom_point() +
  geom_smooth(method = "lm")


