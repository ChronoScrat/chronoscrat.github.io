#' This file queries MusicBrainz's API for information regarding tracks, artists
#' and albums gotten from LastFM's weekly feed. We do this because, for some
#' reason, LastFM is able to correctly provide an MBID in the tracks feed, but
#' when we query this MBID through other endpoints, it fails to return the info.

#' This code is supposed to run after `lastfm.R`. If run from a R script using
#' the source function, it will inherit the `week_feed` data frame required.
#' If not, you should save the data as an RDS/RData and import it.


# Libraries

library(dplyr)
library(lubridate)
library(jsonlite)

# Load environment
## If the code is executed headless, the keys will be provided by the environment,
## else, they will be load into R via a .env file and the credentials JSON.

if(Sys.getenv("LASTFM_KEY") == ""){
  dotenv::load_dot_env()
}

if(Sys.getenv("CI") != "true"){
  bigQueryR::bqr_auth("auth.json")
} else{
  
  system('echo "$BIGQUERY_CREDS_64" | base64 --decode > auth-cred.json')
  
  bigQueryR::bqr_auth("auth-cred.json")
  
}

if(!exists("week_df")){
  week_df <- load("week_df.RData")
}


week_unique_tracks <- unique(week_df$track_mbid) %>% purrr::discard(is.na)

tracks_df <- bigQueryR::bqr_query(projectId = Sys.getenv("BQ_PROJECT"),
                                  datasetId = Sys.getenv("BQ_MUSIC_DATA"),
                                  query = sprintf("SELECT * FROM %s",
                                                  Sys.getenv("BQ_M_TRACKS_D")),
                                  useLegacySql = FALSE)

existing_tracks <- unique(tracks_df$tracks_mbid)

new_tracks <- setdiff(week_unique_tracks, existing_tracks)

final_tracks_df <- NULL

if(length(new_tracks) != 0){
  
  for(i in 1:length(new_tracks)){
    
    track_query <- read_json(paste0(
      "https://musicbrainz.org/ws/2/recording/",
      new_tracks[i],
      "?fmt=json&inc=artists+tags"
    ))
    
    # Normally, we should consider the problem of a track having more than two
    # artists. However, due to a problem with scrobbling Youtube Music, tracks
    # with more than two artists are returned with no MBID. I can postpone
    # dealing with this problem until then.
    
    # TODO Adapt code (and database) to allow more than one artist
    
    artist_name <- track_query$`artist-credit`[[1]]$artist$name
    artist_mbid <- track_query$`artist-credit`[[1]]$artist$id
    
    
    
  }
  
} else{
  print("Amazingly, no new tracks")
}