---
title: "Spotify's Genres of the Future"
author: "Maxim van den Berg"
date: "24 February 2019"
output:
    flexdashboard::flex_dashboard:
        storyboard: true
        theme: lumen
---



```
## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.0     ✔ purrr   0.3.0
## ✔ tibble  2.0.1     ✔ dplyr   0.7.8
## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
## ✔ readr   1.3.1     ✔ forcats 0.3.0
```

```
## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

### An Introduction



Spotify has several curious genres within its database, including some which are characterized by the word *future*. Namely, the following selection:

- Futurepop
- Future Funk
- Kawaii Future Bass
- Future Ambient
- Future Garage

It is definitely not clear if these genres are connected by something more than just their name. We'll explore why these genres are called the way the are, and if a (strong) connection might actually tie these genres together. This will consist of exploring the aspects of the music within each genre first, after which we will properly compare the 5 genres by defining a suitable corpus and


```r
# TODO: ADD SOME SOUND EXAMPLES IF POSSIBLE
```

***

Something here too?



### A Future of Sounds, Rhythms and Timbres

## A Short Examination of the Genres
# Future Funk and Kawaii Bass
From exploring the genres and listening to some excerpts of their music, we can quickly suspect that the *future* description might mean something entirely different per genre. In the case of Future Funk and Kawaii Future Bass, it seems to refer to a futuristic pop-y sound, with a distinct electronic feel, although real (sampled) instruments and some vocals (often with light-hearted lyrics) now and then are common too, especially in Future Funk. Future Kawaii Bass sets itself apart from Future Funk with extensive use of *chiptune* sounds and strong upbeat rhythms.

# Futurepop and Future Garage
dark synths and rhythms, vocals, desperate lyrics, very minor

```r
# TODO: jeez i can barely listen to this, it is giving me instant depression
```

# Future Ambient
minimalistic, especially drum grooves. little instruments, long songs, little synths but a lot of samples, occasional real "smooth" instruments

## The Corpus

The collections of songs we'll analyze, our corpus, will naturally be a selection of songs from artist from each of these genres. Large names such as Snail's House (which has in fact been attributed to the pioneer of Kawaii Future Bass) will be of particular interest and multiple songs will be included, but some genres such as future ambient consist of mostly smaller artist, where a smaller and more varied selection of songs from different artist is more appropriate.
It could also be helpful to compare some of the outliers of the genre, but care must be taken to ensure those artist would actually belong to the genre, instead of their appearance being based solely on Spotify's automatic assignment. If not, it will be more useful to exclude them from the research, as they would negatively interfere with making valid comparisons between the genres.


```r
# TODO: table with corpus (might redefine the corpus if I have time)
```






### First Investigations




```
## No summary function supplied, defaulting to `mean_se()
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

***

An important finding in our prelimenary finding is
Here is a graph to show that
