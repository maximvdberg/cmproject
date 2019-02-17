library(tidyverse)
library(spotifyr)

# Remove unnecessary data received from the Spotify API, and
# select which columns are usefull to us.
remove_junk <- function(data) {
    select(data,Genre=playlist_name,
           PlaylistLength=playlist_num_tracks,
           Artist=artist_name,
           Album=album_name,
           Track=track_name,
           Date=track_added_at,
           Popularity=track_popularity,
           Danceability=danceability,
           Energy=energy,
           Key=key,
           Loudness=loudness,
           Mode=mode,
           Speechiness=speechiness,
           Acousticness=acousticness,
           Instrumentalness=instrumentalness,
           Liveness=liveness,
           Valence=valence,
           Tempo=tempo,
           Duration=duration_ms,
           TimeSignature=time_signature,
           KeyMode=key_mode)
}

# Get playlist data
future_funk <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","4a0xb2zui3hIPll7CMgeSu"))
kawaii_future_bass <- remove_junk( get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","75OfhBfc4tnQ8MFdiPiMcx"))
futurepop <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","1TBQdi8VdYsvruWv1W5HjB"))
future_ambient <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2dZ7eWcGRtuyseKY5QNZoP"))
future_garage <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2IgZ50kclGP2tNVx7mu9vL"))

all_genres <- rbind(future_funk,kawaii_future_bass,futurepop,future_ambient,future_garage)



# Make some plots
ggplot(all_genres,aes(x=Genre,size=Liveness,color=Energy)) + geom_jitter(aes(y=Valence)) + geom_bar(aes(x=Genre,y=Valence),alpha=0.3,stat="summary")
ggsave("scatter.png")
ggplot(all_genres,aes(color=Genre)) + geom_freqpoly(binwidth=0.15,aes(x=Danceability),size=1)
ggsave("danceability.png")
ggplot(all_genres,aes(color=Genre)) + geom_freqpoly(binwidth=0.15,aes(x=Valence),size=1)
ggsave("valence.png")
ggplot(all_genres,aes(color=Genre)) + geom_freqpoly(binwidth=0.15,aes(x=Energy),size=1)
ggsave("energy.png")
