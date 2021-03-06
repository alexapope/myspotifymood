myspotifymood
================
Alexa Pope
2021-05-03

A package extending the capability of \[spotifyr\]
(<https://github.com/charlie86/spotifyr>) by measuring the valence
(happiness) of the tracks on the user’s specified playlist.
This README is derived from Matt Kearney’s excellent
[rtweet](https://github.com/mkearney/rtweet) documentation.

## Install

Install from GitHub with the following code:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("alexapope/myspotifymood")
```

First, set up a Dev account with Spotify to access their Web API here.
This will give you your Client ID and Client Secret. Once you have
those, you can pull your access token into R with
get\_spotify\_access\_token().

The easiest way to authenticate is to set your credentials to the System
Environment variables SPOTIFY\_CLIENT\_ID and SPOTIFY\_CLIENT\_SECRET.
The default arguments to get\_spotify\_access\_token() (and all other
functions in this package) will refer to those. Alternatively, you can
set them manually and make sure to explicitly refer to your access token
in each subsequent function call.This authentication process is directly
derived from Charlie Thompson’s exceptionally detailed \[spotifyr\]
(<https://github.com/charlie86/spotifyr>) authorization documentation.

## Usage

This is the function that is currently live for myspotifymood.

To begin, the user must first enter the following code, inserting their
keys where appropriate:

``` setup
my_spotify_mood = function(username = "", client_id = "", 
                           client_secret = "", playlists = ""){ 
  Sys.setenv(client_id)
  Sys.setenv(client_secret)
  access_token = get_spotify_access_token()
```

Currently, this must be done at the start of every session.

Next, the fun begins with <code>myspotifymood</code>.

Its first argument takes the Spotify user’s username and utilizes it to
extract the user’s necessary data.

The second and third argument take the user’s Spotify authentication
codes from Spotify API Developer (Client ID and Client Secret) in order
for the user to be able to utilize SpotifyR.One would have to make link
their Spotify account with Spotify Developer in order to retrieve these
codes.

The fourth argument determines the playlist id of what playlist the user
will be extracting data like tracks and track audio features from. This
will output a data frame of all of the user’s playlists and the user
must locate their chosen playlist’s id as one of the variables in the
data frame and input the code in lieu of the playlist argument.

``` r
## load my_spotify_mood
library(my_spotify_mood)

## Enter argument will be surrounded by quotation marks
my_spotify_mood(username, client_id, client_secret, playlists)

## In order to retrieve the playlist ID for the function 
playlists = spotifyr::get_user_playlists(username)

## Function results in a visualization of one's playlist track's valence.
```

Important potential variables in the playlists object are “name” the
playlist name, “id” the playlist id, and “uri” different iteration of
the playlist id. The uri is not necessary for this function so disregard
it.

The visualization, when the function is utilized, produces a column plot
that all the tracks on the extracted playlist and the valence scores of
the tracks with intervals of 0.25. The “x” legend to the right of the
plot illustrates the degree of valance based on the values accompanied
by lighter to darker shades of blue. The lighter and higher the color
and valence score is, the happier the is track is and viceversa for the
darker and lower valence scores.

This function wishes to help it’s user get a better understanding of
their own music taste in accordance with the already established package
by Charlie Thompson, \[spotifyr\]
(<https://github.com/charlie86/spotifyr>). This function cannot be
utilized if the user doesn’t have a Spotify account, and doesn’t go
through the authorization process.
