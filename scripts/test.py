import lyricsgenius

# Set up the Genius API client
genius = lyricsgenius.Genius('<insert your API key here>')

# Prompt the user for the artist and song title
artist_name = input("Enter the name of the artist: ")
song_title = input("Enter the name of the song: ")

# Search for the song and get the lyrics
song = genius.search_song(song_title, artist_name)
if song:
    lyrics = song.lyrics
    print(lyrics)
else:
    print("Sorry, couldn't find lyrics for that song.")
