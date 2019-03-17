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

circshift <- function(v, n) {if (n == 0) v else c(tail(v, n), head(v, -n))}

    # C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
    c(1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
    c(1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
    c(1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
    c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
    c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
    tribble(
        ~name  , ~template,
        'Gb:7'  , circshift(seventh_chord,  6),
        'Gb:maj', circshift(major_chord,    6),
        'Bb:min', circshift(minor_chord,   10),
        'Db:maj', circshift(major_chord,    1),
        'F:min' , circshift(minor_chord,    5),
        'Ab:7'  , circshift(seventh_chord,  8),
        'Ab:maj', circshift(major_chord,    8),
        'C:min' , circshift(minor_chord,    0),
        'Eb:7'  , circshift(seventh_chord,  3),
        'Eb:maj', circshift(major_chord,    3),
        'G:min' , circshift(minor_chord,    7),
        'Bb:7'  , circshift(seventh_chord, 10),
        'Bb:maj', circshift(major_chord,   10),
        'D:min' , circshift(minor_chord,    2),
        'F:7'   , circshift(seventh_chord,  5),
        'F:maj' , circshift(major_chord,    5),
        'A:min' , circshift(minor_chord,    9),
        'C:7'   , circshift(seventh_chord,  0),
        'C:maj' , circshift(major_chord,    0),
        'E:min' , circshift(minor_chord,    4),
        'G:7'   , circshift(seventh_chord,  7),
        'G:maj' , circshift(major_chord,    7),
        'B:min' , circshift(minor_chord,   11),
        'D:7'   , circshift(seventh_chord,  2),
        'D:maj' , circshift(major_chord,    2),
        'F#:min', circshift(minor_chord,    6),
        'A:7'   , circshift(seventh_chord,  9),
        'A:maj' , circshift(major_chord,    9),
        'C#:min', circshift(minor_chord,    1),
        'E:7'   , circshift(seventh_chord,  4),
        'E:maj' , circshift(major_chord,    4),
        'G#:min', circshift(minor_chord,    8),
        'B:7'   , circshift(seventh_chord, 11),
        'B:maj' , circshift(major_chord,   11),
        'D#:min', circshift(minor_chord,    3),
)

key_templates <-
    tribble(
        ~name    , ~template,
        'Gb:maj', circshift(major_key,  6),
        'Bb:min', circshift(minor_key, 10),
        'Db:maj', circshift(major_key,  1),
        'F:min' , circshift(minor_key,  5),
        'Ab:maj', circshift(major_key,  8),
        'C:min' , circshift(minor_key,  0),
        'Eb:maj', circshift(major_key,  3),
        'G:min' , circshift(minor_key,  7),
        'Bb:maj', circshift(major_key, 10),
        'D:min' , circshift(minor_key,  2),
        'F:maj' , circshift(major_key,  5),
        'A:min' , circshift(minor_key,  9),
        'C:maj' , circshift(major_key,  0),
        'E:min' , circshift(minor_key,  4),
        'G:maj' , circshift(major_key,  7),
        'B:min' , circshift(minor_key, 11),
        'D:maj' , circshift(major_key,  2),
        'F#:min', circshift(minor_key,  6),
        'A:maj' , circshift(major_key,  9),
        'C#:min', circshift(minor_key,  1),
        'E:maj' , circshift(major_key,  4),
        'G#:min', circshift(minor_key,  8),
        'B:maj' , circshift(major_key, 11),
        'D#:min', circshift(minor_key,  3))



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
                method = 'rms', norm = 'manhattan')) %>%
    mutate(
        timbre =
            map(segments,
                compmus_summarise, timbre,
                method = 'mean')))
}

plot_chroma <- function(data) {
    return(data %>%
        mutate(pitches = map(pitches, compmus_normalise, 'euclidean')) %>%
        compmus_gather_chroma %>%
        ggplot(
            aes(
                x = start + duration / 2,
                width = duration,
                y = pitch_class,
                fill = value)) +
        geom_tile() +
        labs(x = 'Time (s)', y = NULL, fill = 'Magnitude') +
        theme_minimal() +
        theme(aspect.ratio=2/4))
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
        theme_classic() +
        theme(aspect.ratio=2/4))
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

plot_chord <- function(data) {
    return(data %>%
        compmus_match_pitch_template(chord_templates, 'euclidean', 'manhattan') %>%
        ggplot(
            aes(x = start + duration / 2, width = duration, y = name, fill = d)) +
        geom_tile() +
        scale_fill_viridis_c(option = 'E', guide = 'none') +
        theme_minimal() +
        labs(x = 'Time (s)', y = '') +
        theme(aspect.ratio=4/4))
}





# -------- Load Data --------- #

# Playlist data
if (!file.exists("data/ff.rds")) {
    future_funk <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","4a0xb2zui3hIPll7CMgeSu"))
    saveRDS(future_funk, "data/ff.rds")
} else {
    future_funk <- readRDS("data/ff.rds")
}

if (!file.exists("data/kfb.rds")) {
    kawaii_future_bass <- remove_junk( get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","75OfhBfc4tnQ8MFdiPiMcx"))
    saveRDS(kawaii_future_bass, "data/kfb.rds")
} else {
    kawaii_future_bass <- readRDS("data/kfb.rds")
}

if (!file.exists("data/fp.rds")) {
    futurepop <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","1TBQdi8VdYsvruWv1W5HjB"))
    saveRDS(futurepop, "data/fp.rds")
} else {
    futurepop <- readRDS("data/fp.rds")
}

if (!file.exists("data/fa.rds")) {
    future_ambient <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2dZ7eWcGRtuyseKY5QNZoP"))
    saveRDS(future_ambient, "data/fa.rds")
} else {
    future_ambient <- readRDS("data/fa.rds")
}

if (!file.exists("data/fg.rds")) {
    future_garage <- remove_junk(get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2IgZ50kclGP2tNVx7mu9vL"))
    saveRDS(future_garage, "data/fg.rds")
} else {
    future_garage <- readRDS("data/fg.rds")
}


# Playlist audio feature
if (!file.exists("data/ff_full.rds")) {
    future_funk_full <- get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","4a0xb2zui3hIPll7CMgeSu") %>%
        add_audio_analysis
    saveRDS(future_funk_full, "data/ff_full.rds")
} else {
    future_funk_full <- readRDS("data/ff_full.rds")
}

if (!file.exists("data/kfb_full.rds")) {
    kawaii_future_bass_full <-  get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","75OfhBfc4tnQ8MFdiPiMcx") %>%
        add_audio_analysis
    saveRDS(kawaii_future_bass_full, "data/kfb_full.rds")
} else {
    kawaii_future_bass_full <- readRDS("data/kfb_full.rds")
}

if (!file.exists("data/fp_full.rds")) {
    futurepop_full <- get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","1TBQdi8VdYsvruWv1W5HjB") %>%
        add_audio_analysis
    saveRDS(futurepop_full, "data/fp_full.rds")
} else {
    futurepop_full <- readRDS("data/fp_full.rds")
}

if (!file.exists("data/fa_full.rds")) {
    future_ambient_full <- get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2dZ7eWcGRtuyseKY5QNZoP") %>%
        add_audio_analysis
    saveRDS(future_ambient_full, "data/fa_full.rds")
} else {
    future_ambient_full <- readRDS("data/fa_full.rds")
}

if (!file.exists("data/fg_full.rds")) {
    future_garage_full <- get_playlist_audio_features("bkd0b33gypt1ixtyg44x4y2ui","2IgZ50kclGP2tNVx7mu9vL") %>%
        add_audio_analysis
    saveRDS(future_garage_full, "data/fg_full.rds")
} else {
    future_garage_full <- readRDS("data/fg_full.rds")
}
