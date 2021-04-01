#' Title

#'

#' Description

#'

#' Details

#'

#' @author Kurt Wirth

#'

#' @param x A number.

#' @param y A number.

#'

#' @return The sum of \code{x}^2 and \code{y}^2

#'

#' @examples

#' add_squares(1, 2)

#' add_squares(1, -2)

#'

#' @export

# ESTABLISH CONNECTION SPOTIFY API

my_spotify_mood = function(x, y, z){
  "lexapopee" = x
  Sys.setenv("cd4f903eee184055825608bae2eb0cf2" = y)
  Sys.setenv("9cfd998fca23438bbc966299611b5168" = z)
  access_token = get_spotify_access_token()

  # GET sPECIFIC PLAYLIST AUDIO FEATURES

  personal_playlist = spotifyr::get_playlist_audio_features("spotify:playlist:7IjYJeoglgPd0fLXQs6qtm" = p)

  # GET PLAYLIST TRACKS VALENCE

  playlists = spotifyr::get_user_playlists("lexapopee") %>%
    playlist_track_valence = spotifyr::get_playlist_tracks(p)%>%
    spotifyr::get_playlist_audio_features(p)%>%
    group_by(track.name, valence)%>%
    filter(valence >= 0.1)%>%
    ggplot(aes(x = valence, y = track.name, fill = ..x..)) +
    geom_col()

  # GET PLAYLIST TRACKS POPULARITY

  playlist_track_popularity = p %>%
    group_by(track.name, valence) %>%
    filter(track.popularity >= 1) %>%
    ggplot(aes(x = track.popularity, y = track.name, fill = ..x..)) +
    geom_col()

  # GET TASTE QUADRANTS

  TasteQuadrants <- ggplot(data = personal_playlist, aes(x = valence, y = track.popularity, color = track.explicit)) +
    geom_jitter() +
    geom_vline(xintercept = 0.5) +
    geom_hline(yintercept = 0.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    annotate('text', 0.25 / 2, 0.95, label = "angry") +
    annotate('text', 1.75 / 2, 0.95, label = "happy") +
    annotate('text', 1.75 / 2, 0.05, label = "chill") +
    annotate('text', 0.25 / 2, 0.05, label = "sad") +
    labs(x= "valence", y= "popularity") +
    ggtitle("Your Music Taste in Quadrants", "Defining your music taste based on a specifc Spotify Playlist")
}
