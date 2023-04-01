# Load packages
library(dplyr)
library(tidyverse)


#Original billboard rank set
originalSet <- read_csv("P:/QAC/econ300/STUDENTS/gcui01/Code/charts.csv")

#Set for rank average calculations
rankValue <- originalSet


#clean observations in ranked 
valueKept <- c("date", "rank", "song", "peak-rank", "weeks-on-board")
rankValue <- rankValue[valueKept]


#Merge rows with same song names
rankValue <- rankValue %>%
  group_by(song) %>%
  summarize(rank = sum(rank), `weeks-on-board` = `weeks-on-board`)









