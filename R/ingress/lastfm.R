#' This file queries Last.FM's API weekly to get the data, then it feeds it to
#' a BigQuery table
#' 
#' This code is supposed to run on Mondays at 12AM, fetching the data from the previous week's
#' Monday up until Sunday.


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


# Set the time frame

week_end <- today()
week_start <- as_datetime(week_end - days(7))
week_start_utc <- as.numeric(week_start)


# Send initial request to LastFM's server. This will fetch the 200 most recent
# scrobbled tracks within the timeframe and page the rest.

week_tracks <- read_json(paste0(
  "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=NathanaelRolim&api_key=",
  Sys.getenv("LASTFM_KEY"),
  "&format=json&limit=200&from=",
  week_start_utc
), simplifyVector = TRUE)

# Initializing an empty variable is not needed in R, but it is good practice anyway
week_df <- NULL

# Number of pages in the initial request. If it is greater than 1, we should loop
# through all the pages and get the data. This will be all bound into the `week_df`
# dataframe.

num_pages <- as.numeric(week_tracks$recenttracks$`@attr`$totalPages)

if(num_pages == 1){
  week_df <- week_tracks[["recenttracks"]]$track
} else{
  
  for (i in 1:num_pages) {
    
    query <- read_json(paste0(
      "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=NathanaelRolim&api_key=",
      Sys.getenv("LASTFM_KEY"),
      "&format=json&limit=200&from=",
      week_start_utc,
      "&page=",
      i
    ), simplifyVector = TRUE)
    
    tmp_df <- query[["recenttracks"]]$track
    
    if(i == 1){
      week_df <- tmp_df
    } else{
      week_df <- bind_rows(week_df,tmp_df)
    }
    
    
  }
  
}

# Mutate the dataframe in order to exclude unwanted columns and unnest some dataframes;
# Finally, we should clean the dataframe and leave it ready for integration into
# the main database in GCP.


week_df <- week_df %>%
  select(-c(streamable,
            image))

week_df <- week_df %>%
  unnest(., keep_empty = TRUE) %>%
  rename(.,
         artist_mbid = mbid,
         artist_name = `#text`,
         track_mbid = mbid1,
         album_mbid = mbid2,
         album_name = `#text1`,
         track_name = name,
         date_uts = uts,
         date_text = `#text2`,
         track_url = url)

week_df <- week_df %>%
  mutate(date_uts = as.numeric(date_uts),
         date_full = as_datetime(date_uts),
         date_uts = as.integer(date_uts)) %>%
  mutate(across(where(is.character), ~ na_if(.,""))) %>%
  select(-c(date_text))


#Upload the data into the GCP

bigQueryR::bqr_upload_data(projectId = Sys.getenv("BQ_PROJECT"),
                           datasetId = Sys.getenv("BQ_MUSIC_DATA"),
                           tableId = Sys.getenv("BQ_M_TRACKS_F"),
                           upload_data = week_df,
                           create = "CREATE_NEVER",
                           writeDisposition = "WRITE_APPEND")

################################################################################

# Get all artists from the week's feed. In this case, we select the MBID because
# the artist's name field is entirely unreliable. The problem, however, is that
# not all artists have an associated MBID, but that is a risk we accept.

week_artists <- unique(week_df$artist_mbid) %>% purrr::discard(is.na)


# Now we query the existing artists database. This table exists so that we don't
# have to query LastFM's API every time.

artists_df <- bigQueryR::bqr_query(projectId = Sys.getenv("BQ_PROJECT"),
                                   datasetId = Sys.getenv("BQ_MUSIC_DATA"),
                                   query = sprintf("SELECT * FROM %s",
                                                   Sys.getenv("BQ_M_ARTISTS_D")),
                                   useLegacySql = FALSE)

# Now get all the *new* artists

existing_artists <- unique(artists_df$artist_mbid)
new_artists <- setdiff(week_artists, existing_artists)


# Loop through all the new artists and query their information. This will all be
# bound into the `final_artist_df` dataframe.

final_artist_df <- NULL


for(i in 1:length(new_artists)){
  artist_query <- read_json(paste0(
    "http://ws.audioscrobbler.com/2.0/?method=artist.getinfo&api_key=",
    Sys.getenv("LASTFM_KEY"),
    "&format=json&mbid=",
    new_artists[i]
  ))
  
  # There is a strange problem in LastFM's API in which it will correctly provide
  # the artist's MBID in the tracks feed, but is not able to find it in the 
  # `artist.getinfo` method. A solution would be to query MusicBrainz API directly
  # but I am not in the mood right now.
  
  # TODO Switch from LastFM to MusicBrainz API
  
  if(length(artist_query) == 1){
    
    artist_query <- artist_query$artist
    artist_query <- within(artist_query, rm(list = c("image",
                                                     "stats",
                                                     "similar",
                                                     "bio")))
    final_tmp <- as.data.frame(list(
      artist_mbid = artist_query$mbid,
      artist_name = artist_query$name,
      artist_url = artist_query$url,
      artist_main_style = artist_query$tags$tag[1]
    ))
    
    if(i == 1){
      final_artist_df <- final_tmp
    } else{
      final_artist_df <- bind_rows(final_artist_df,final_tmp)
    }
    
  } else{
    print("LastFM could not find artist's information even though it provided the artist MBID.")
  }
}


################################################################################

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
    
    
    
  }
  
} else{
  print("Amanzingly, no new tracks")
}