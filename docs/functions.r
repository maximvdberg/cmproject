#----- SOME FUNCTIONS -----#

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


get_analysis <- function(id) {
    # return(get_tidy_audio_analysis(id) %>%
    #        select(segments) %>%
    #        unnest(segments) %>%
    #        select(start, duration, pitches))
    return(get_tidy_audio_analysis(id) %>%
    compmus_align(bars, segments) %>%
    select(bars) %>% unnest(bars) %>%
    mutate(
        pitches =
            map(segments,
                compmus_summarise, pitches,
                method = 'rms', norm = 'euclidean')) %>%
    mutate(
        timbre =
            map(segments,
                compmus_summarise, timbre,
                method = 'mean')))
}

plot_chroma <- function(data) {
    return(data %>%
        mutate(pitches = map(pitches, compmus_normalise, 'chebyshev')) %>%
        compmus_gather_chroma %>%
        ggplot(
            aes(
                x = start + duration / 2,
                width = duration,
                y = pitch_class,
                fill = value)) +
        geom_tile() +
        labs(x = 'Time (s)', y = NULL, fill = 'Magnitude') +
        theme_minimal())
}

plot_cesptro <- function(data) {
    return(data %>%
        compmus_gather_timbre %>%
        ggplot(
            aes(
                x = start + duration / 2,
                width = duration,
                y = basis,
                fill = value)) +
        geom_tile() +
        labs(x = 'Time (s)', y = NULL, fill = 'Magnitude') +
        scale_fill_viridis_c(option = 'E') +
        theme_classic())
}


plot_ssm <- function(data) {
    return(data %>%
        compmus_self_similarity(timbre, 'cosine') %>%
        ggplot(
            aes(
                x = xstart + xduration / 2,
                width = xduration,
                y = ystart + yduration / 2,
                height = yduration,
                fill = d)) +
        geom_tile() +
        coord_fixed() +
        scale_fill_viridis_c(option = 'E', guide = 'none') +
        theme_classic() +
        labs(x = '', y = ''))
}



# -------- Load Data --------- #

if (!file.exists("data/ff.rds") {
    future_funk <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","4a0xb2zui3hIPll7CMgeSu"))
    writeRDS(future_funk, "data/ff.rds")
} else {
    future_funk <- readRDS("data/ff.rds")
}

if (!file.exists("data/kfb.rds") {
    kawaii_future_bass <- remove_junk( get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","75OfhBfc4tnQ8MFdiPiMcx"))
    writeRDS(kawaii_future_bass, "data/kfb.rds")
} else {
    kawaii_future_bass <- readRDS("data/kfb.rds")
}

if (!file.exists("data/fp.rds") {
    futurepop <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","1TBQdi8VdYsvruWv1W5HjB"))
    writeRDS(futurepop, "data/fp.rds")
} else {
    futurepop <- readRDS("data/fp.rds")
}

if (!file.exists("data/fa.rds") {
    future_ambient <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2dZ7eWcGRtuyseKY5QNZoP"))
    writeRDS(future_ambient, "data/fa.rds")
} else {
    future_ambient <- readRDS("data/fa.rds")
}

if (!file.exists("data/fg.rds") {
    future_garage <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2IgZ50kclGP2tNVx7mu9vL"))
    writeRDS(future_garage, "data/fg.rds")
} else {
    future_garage <- readRDS("data/fg.rds")
}
