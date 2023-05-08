# Import genius
from lyricsgenius import Genius

import api_key
genius = Genius(api_key.client_token)

#Song csv command file
import csv

# Open the CSV file for reading
with open('scrapeSongs.csv', 'r') as csvfile:
    # Create a CSV reader object
    reader = csv.reader(csvfile)

    # Iterate through the rows of the file
    for row in reader:
        # Extract the values from the row
        numID = row[0]
        songArtist = row[1]
        songName = row[2]

        # Call your command with the extracted parameters

        artist = genius.search_artist("Andy Shauf", max_songs=3, sort="title")









