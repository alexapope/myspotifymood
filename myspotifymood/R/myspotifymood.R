#' Visualizes Valence From Spotify
#'
#' A function that allows the user to retrieve the individual tracks valance
#' (happiness level) on a specific playlist.
#'
#' @author Alexa Pope
#'
#' @param username The Spotify username of the individual utilizing this
#' function.
#'
#' @param  client_id The user's Spotify Developer Client ID
#'
#' @param  client_secret The user's Spotify Developer Client Secret.
#'
#' @param  playlists The user's chosen playlist's id.
#'
#' @return A visualization of the valence levels of every track on a specific
#' playlist of the user's choosing.
#'
#' @examples
#' myspotifymood("billybob08", "cbfj2895nbdudnejf2", "2kjgkjbrgrslenfjkrnjsne",
#' "34IdsklfjbejQkbrsFG458HSf")
#'
#' @export

# ESTABLISH CONNECTION SPOTIFY API

my_spotify_mood = function(username, client_id, client_secret, playlists){
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
  access_token = spotifyr::get_spotify_access_token()

  # GET sPECIFIC PLAYLIST AUDIO FEATURES

  playlists = spotifyr::get_user_playlists(username)

  p = playlists

  # GET PLAYLIST TRACKS VALENCE

    playlist_track_valence = spotifyr::get_playlist_tracks(p) %>%
    spotifyr::get_playlist_audio_features(p) %>%
    group_by(track.name, valence) %>%
    filter(valence >= 0.1)

    playlist_track_valence %>% ggplot(aes(x = valence, y = track.name, fill = ..x..)) +
    geom_col()
}




