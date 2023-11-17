library(bigrquery)
library(dplyr)
library(jsonlite)
library(lubridate)
library(DBI)
library(tidyr)


if(Sys.getenv("CI") != "true"){
  dotenv::load_dot_env()
  bq_auth(path = "./auth.json")
} else{
  system('echo "$BIGQUERY_CREDS_64" | base64 --decode > auth-cred.json')
  bq_auth(path = "./auth-cred.json")
}

week_end <- today()
week_start <- as_datetime(week_end - days(7))
week_start_utc <- as.numeric(week_start)

conn <- dbConnect(
  bigquery(),
  project = Sys.getenv("BQ_PROJECT"),
  dataset = "music_history",
  billing = Sys.getenv("BQ_PROJECT")
)


tables <- dbListTables(conn)

if( !("tracks_feed" %in% tables) ){
  
  tracks <- read_json(paste0(
    "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=NathanaelRolim&api_key=",
    Sys.getenv("LASTFM_KEY"),
    "&format=json&limit=200&to=",
    week_start_utc
  ), simplifyVector = TRUE)
  
  num_pages <- as.numeric(tracks$recenttracks$`@attr`$totalPages)
  
  tracks_df <- NULL
  
  if(num_pages == 1){
    tracks_df <- tracks[["recenttracks"]]$track
  } else{
    
    for (i in 1:num_pages) {
      
      query <- read_json(paste0(
        "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=NathanaelRolim&api_key=",
        Sys.getenv("LASTFM_KEY"),
        "&format=json&limit=200&to=",
        week_start_utc,
        "&page=",
        i
      ), simplifyVector = TRUE)
      
      tmp_df <- query[["recenttracks"]]$track
      
      if(i == 1){
        tracks_df <- tmp_df
      } else{
        tracks_df <- bind_rows(tracks_df,tmp_df)
      }
      
      
    }
    
  }
  
  tracks_df <- tracks_df %>%
    select(-c(streamable,
              image))
  
  tracks_df <- tracks_df %>%
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
  
  tracks_df <- tracks_df %>%
    mutate(date_uts = as.numeric(date_uts),
           date_full = as_datetime(date_uts),
           date_uts = as.integer(date_uts)) %>%
    mutate(across(where(is.character), ~ na_if(.,""))) %>%
    select(-c(date_text))
  
  
  dbWriteTable(conn,
               "tracks_feed",
               tracks_df)
  
}