## Spotify's genres of the future
*Maxim van den Berg*

### Introduction

Spotify has several curious genres within its database, including some which are charactized by the word *future*, namely the following 5:

* futurepop
* future funk
* kawaii future bass
* future ambient
* future garage

On first sight, these genres seem to be related at least somewhat. For example, the artist *Snail's House* is listed as both an future funk and a kawaii future bass artist. The other genres seem to fall in other categories however. Where future funk and kawaii future bass are generally more typical music, the others fall more on the side of experimental music. The Spotify API reports large differences in statistics between the genres, especially in terms of energy and danceability.

It is definitely not clear if these genres are connected by something more than just their name. I want to explore why these genres are called the way the are, and if a (strong) connection might actually tie these genres together.This will consist of exploring the aspects of the music within each genre first, after which the 5 genres can be properly compared. It might be insighfull to also look at their non-future counterparts (if those exist), and see how each future genre differs from them to discover if any pattern may exist.

The corpus will naturallly be a selection of songs from artist from each of these genres. Large names such as the aformentioned Snail's House will be of particular interest and mutliple songs will be included, but some genres such as future ambient consist of mostly smaller artist, where a smaller and more varied selection of songs from different artist is more appropiate.
It can also be helpfull to compare some of the outliers of the genre, but care must be taken to ensure those artist would actually belong to the genre, instead of their appearance being based solely on Spotify's automatic assignment. If not, it will be more usefull to exclude them from the research, as they would negatively interfere with making valid comparisons between the genres.


### Some Visualisation
For the following graphs, I used playlist largely based on playlists provided by the Every Noise at Once website (http://everynoise.com/), each 50 tracks long.

<img src="./scatter.png" width="800px"><br>

To explore the structure of the data, we begin by comparing some common features from the Spotify API. To me, valence seemed to be a logical choice for comparison, as I felt like the genres sounded most distict in this regard. The graph shows the average valence of the songs from each genre, as well as the value of each individual song in the form of a scatter plot. Additionally, the colour displays the energy value of the tracks, another feature I felt would provide contrast between the genres. Lastly, the size of each point gives an indication of the liveness of the track, as provided by Spotify.

The following graphs show the distribution of the values of three features (namely danceability, valence and energy) among the different genres. From this we can deduce in what value range the focus of the different genres lie, such as concentrated high energy of Futurepop, or the generally higher valence of Future Funk, Pop and Kawaii Bass.
<br>
<img src="./danceability.png" width="600px"><br>
<img src="./valence.png" width="600px"><br>
<img src="./energy.png" width="600px"><br>

