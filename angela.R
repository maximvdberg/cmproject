

if (!file.exists("data/angela_media.rds")) {
    AAA <- get_playlist_audio_features('1160290630', '5TPQBc9opP7yIozTgJeDkT') %>%
        add_audio_analysis
    Indie <- get_playlist_audio_features('1160290630', '5a9w1bnzdP2Mb86BbYloXA') %>%
        add_audio_analysis
    Film <- get_playlist_audio_features('1160290630', '5Wh4OFc6FranJG3ozs7O6I') %>%
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
