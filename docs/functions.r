

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

#' Get a tidy audio analysis from Spotify.
#'
#' spotifyr returns Spotify's audio analysis as a large list. This function
#' uses list columns to create a structure that works more richly within the
#' tidyverse.
get_tidy_audio_analysis <- function(track_uri, ...)
{
    get_track_audio_analysis(track_uri, ...) %>%
        list %>% transpose %>% as_tibble %>%
        mutate_at(vars(meta, track), . %>% map(as_tibble)) %>%
        unnest(meta, track) %>%
        select(
            analyzer_version,
            duration,
            contains('fade'),
            ends_with('confidence'),
            bars:segments) %>%
        mutate_at(
            vars(bars, beats, tatums, sections),
            . %>% map(bind_rows)) %>%
        mutate(
            segments =
                map(
                    segments,
                    . %>%
                        transpose %>% as_tibble %>%
                        unnest(.preserve = c(pitches, timbre)) %>%
                        mutate(
                            pitches =
                                map(
                                    pitches,
                                    . %>%
                                        flatten_dbl %>%
                                        set_names(
                                            c(
                                                'C', 'C#|Db', 'D', 'D#|Eb',
                                                'E', 'F', 'F#|Gb', 'G',
                                                'G#|Ab', 'A', 'A#|Bb', 'B'))),
                            timbre =
                                map(
                                    timbre,
                                    . %>%
                                        flatten_dbl %>%
                                        set_names(
                                            c(
                                                'c1', 'c2', 'c3', 'c4',
                                                'c5', 'c6', 'c7', 'c8',
                                                'c9', 'c10', 'c11', 'c12'))))))
}

#' Normalise vectors for Computational Musicology.
#'
#' We use a number of normalisation strategies in Computational Musicology.
#' This function brings them together into one place, along with common
#' alternative names.
compmus_normalise <- compmus_normalize <- function(v, method = "euclidean")
{
    ## Supported functions

    harmonic  <- function(v) v * sum(1 / abs(v))
    manhattan <- function(v) v / sum(abs(v))
    euclidean <- function(v) v / sqrt(sum(v^2))
    chebyshev <- function(v) v / max(abs(v))
    clr       <- function(v) {lv <- log(v); lv - mean(lv)}

    ## Method aliases

    METHODS <-
        list(
            harmonic  = harmonic,
            manhattan = manhattan,
            L1        = manhattan,
            euclidean = euclidean,
            L2        = euclidean,
            chebyshev = chebyshev,
            maximum   = chebyshev,
            aitchison = clr,
            clr       = clr)

    ## Function selection


    if (!is.na(i <- pmatch(method, names(METHODS))))
        METHODS[[i]](v)
    else
        stop('The method name is ambiguous or the method is unsupported.')
}

#' Compute pairwise distances for Computational Musicology in long format.
#'
#' We use a number of distance measures in Computational Musicology.
#' This function brings them together into one place, along with common
#' alternative names. It is designed for convenience, not speed.
compmus_long_distance <- function(xdat, ydat, feature, method = "euclidean")
{

    feature <- enquo(feature)

    ## Supported functions

    manhattan <- function(x, y) sum(abs(x - y))
    euclidean <- function(x, y) sqrt(sum((x - y) ^ 2))
    chebyshev <- function(x, y) max(abs(x - y))
    pearson   <- function(x, y) 1 - cor(x, y)
    cosine    <- function(x, y)
    {
        1 - sum(compmus_normalise(x, "euc") * compmus_normalise(y, "euc"))
    }
    angular   <- function(x, y) 2 * acos(1 - cosine(x, y)) / pi
    aitchison <- function(x, y)
    {
        euclidean(compmus_normalise(x, "clr"), compmus_normalise(y, "clr"))
    }

    ## Method aliases

    METHODS <-
        list(
            manhattan   = manhattan,
            cityblock   = manhattan,
            taxicab     = manhattan,
            L1          = manhattan,
            totvar      = manhattan,
            euclidean   = euclidean,
            L2          = euclidean,
            chebyshev   = chebyshev,
            maximum     = chebyshev,
            pearson     = pearson,
            correlation = pearson,
            cosine      = cosine,
            angular     = angular,
            aitchison   = aitchison)

    ## Function selection

    if (!is.na(i <- pmatch(method, names(METHODS))))
        bind_cols(
            crossing(
                xdat %>% select(xstart = start, xduration = duration),
                ydat %>% select(ystart = start, yduration = duration)),
            xdat %>% select(x = !!feature) %>%
                crossing(ydat %>% select(y = !!feature)) %>%
                transmute(d = map2_dbl(x, y, METHODS[[i]])))
    else
        stop('The method name is ambiguous or the method is unsupported.')
}

#' Gathers chroma vectors into long format.
#'
#' Gathers chroma vectors into long format for Computational Musicology.
compmus_gather_chroma <- function(data)
{
    data %>%
    mutate(pitches = map(pitches, bind_rows)) %>% unnest(pitches) %>%
    gather("pitch_class", "value", C:B) %>%
    mutate(pitch_class = fct_shift(factor(pitch_class), 3))
}


