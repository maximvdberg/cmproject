

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
