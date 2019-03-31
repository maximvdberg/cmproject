

if (!file.exists("data/angela_media.rds")) {
    AAA50 <- get_playlist_audio_features('1160290630', '5TPQBc9opP7yIozTgJeDkT') %>%
        add_audio_analysis
    Indie50 <- get_playlist_audio_features('1160290630', '5a9w1bnzdP2Mb86BbYloXA') %>%
        add_audio_analysis
    Film50 <- get_playlist_audio_features('1160290630', '5Wh4OFc6FranJG3ozs7O6I') %>%
        add_audio_analysis

    Media <- AAA %>% mutate(playlist = "AAA") %>%
        rbind(Indie %>% mutate(playlist = "Indie"),
              Film %>% mutate(playlist = "Film"))

    saveRDS(Media, "data/angela_media.rds")
} else {
    Media <- readRDS("data/angela_media.rds")
}


if (!file.exists("data/angela.rds")) {

    media_full <-
        Media %>%
        mutate(playlist = factor(playlist)) %>%
        mutate(
         segments =
             map2(segments, key, compmus_c_transpose)) %>%
        mutate(
         pitches =
             map(segments,
                 compmus_summarise, pitches,
                 method = 'mean', norm = 'manhattan'),
         timbre =
             map(
                 segments,
                 compmus_summarise, timbre,
                 method = 'mean')) %>%
        mutate(pitches = map(pitches, compmus_normalise, 'clr')) %>%
        mutate_at(vars(pitches, timbre), map, bind_rows) %>%
        unnest(pitches, timbre)

    saveRDS(media_full, "data/angela.rds")
} else {
    media_full <- readRDS("data/angela.rds")
}


media50_juice <-
    recipe(track_name ~
               danceability +
               energy +
               loudness +
               speechiness +
               acousticness +
               instrumentalness +
               liveness +
               valence +
               tempo +
               duration_ms +
               C + `C#|Db` + D + `D#|Eb` +
               E + `F` + `F#|Gb` + G +
               `G#|Ab` + A + `A#|Bb` + B +
               c01 + c02 + c03 + c04 + c05 + c06 +
               c07 + c08 + c09 + c10 + c11 + c12,
           data = media50_full) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    # step_range(all_predictors()) %>%
    prep(media50_full %>% mutate(track_name = str_trunc(track_name, 20))) %>%
    juice %>%
    column_to_rownames('track_name')

future_dist <- dist(future_juice, method = 'euclidean')

protoclust(future_dist) %>% dendro_data %>% ggdendrogram
