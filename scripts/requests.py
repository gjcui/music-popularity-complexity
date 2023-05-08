import api_key

import lyricsgenius

genius = lyricsgenius.Genius(api_key.client_token)

# Song csv command file
import csv

# Open the CSV file for reading and the output CSV file for writing
with open('scrapeSongs.csv', 'r') as csvfile, open('output.csv', 'w', newline='') as outfile:
    # Create CSV reader and writer objects
    reader = csv.reader(csvfile)
    writer = csv.writer(outfile)

    # Write header row to output CSV file
    writer.writerow(['numID', 'songArtist', 'songName', 'popularity', 'complexity'])

    # Iterate through the rows of the input CSV file
    for row in reader:
        # Extract the values from the row
        numID = row[0]
        songArtist = row[1]
        songName = row[2]

        # Call your command with the extracted parameters
        try:
            artist = search.search_artist(songArtist, max_songs=3, sort=songName)
            popularity = artist.popularity
            complexity = artist.get('raw_data', {}).get('artist', {}).get('stats', {}).get('hot', {}).get('familiarity', 'NA')
        except:
            # If an error occurs, print "NA" for each field
            popularity = 'NA'
            complexity = 'NA'
            print(f"Error processing row: {numID}, {songArtist}, {songName}")

        # Write the extracted values and any additional data to the output CSV file
        writer.writerow([numID, songArtist, songName, popularity, complexity])
