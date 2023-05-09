library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(reshape2)


# Set up text
text <- "Yeah I've been tryna call I've been on my own for long enough Maybe you can show me how to love, maybe I'm going through withdrawals You don't even have to do too much You can turn me on with just a touch, baby I look around and Sin City's cold and empty (oh) No one's around to judge me (oh) I can't see clearly when you're gone I said, ooh, I'm blinded by the lights No, I can't sleep until I feel your touch I said, ooh, I'm drowning in the night Oh, when I'm like this, you're the one I trust (Hey, hey, hey) I'm running out of time 'Cause I can see the sun light up the sky So I hit the road in overdrive, baby, oh The city's cold and empty (oh) No one's around to judge me (oh) I can't see clearly when you're gone I said, ooh, I'm blinded by the lights No, I can't sleep until I feel your touch I said, ooh, I'm drowning in the night Oh, when I'm like this, you're the one I trust I'm just walking by to let you know (by to let you know) I can never say it on the phone (say it on the phone) Will never let you go this time (ooh) I said, ooh, I'm blinded by the lights No, I can't sleep until I feel your touch (Hey, hey, hey) I said, ooh, I'm blinded by the lights No, I can't sleep until I feel your touch"



# Define function to calculate Levenshtein distance
levenshtein_distance <- function(x, y) {
  n <- nchar(x)
  m <- nchar(y)
  d <- matrix(0, n + 1, m + 1)
  for (i in 1:n) {
    d[i + 1, 1] <- i
  }
  for (j in 1:m) {
    d[1, j + 1] <- j
  }
  for (j in 1:m) {
    for (i in 1:n) {
      if (substr(x, i, i) == substr(y, j, j)) {
        d[i + 1, j + 1] <- d[i, j]
      } else {
        d[i + 1, j + 1] <- min(d[i, j + 1] + 1, d[i + 1, j] + 1, d[i, j] + 1)
      }
    }
  }
  return(d[n + 1, m + 1])
}




# Calculate self-similarity matrix using Levenshtein distance
segments <- strsplit(text, " ")[[1]]
n <- length(segments)
similarity_matrix <- matrix(0, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    distance <- levenshtein_distance(segments[i], segments[j])
    similarity_matrix[i, j] <- 1 / (1 + distance)
  }
}

# Generate heatmap of self-similarity matrix
ggplot(data = melt(similarity_matrix), aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Segments", y = "Segments", title = "Self-Similarity Matrix")



self_similarity <- sum(similarity_matrix) / (n*n)

print(self_similarity)










