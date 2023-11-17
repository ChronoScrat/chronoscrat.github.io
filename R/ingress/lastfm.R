#' This file queries Last.FM's API weekly to get the data, then it feeds it to
#' a BigQuery table
#' 
#' This code is supposed to run on Mondays at 12AM, fetching the data from the previous week's
#' Monday up until Sunday.

# TODO Change code to handle scenario of no new tracks

# Libraries

library(dplyr)
library(lubridate)
library(jsonlite)
library(bigrquery)
library(DBI)

# Load environment
## If the code is executed headless, the keys will be provided by the environment,
## else, they will be load into R via a .env file and the credentials JSON.

if(Sys.getenv("CI") != "true"){
  dotenv::load_dot_env()
  bq_auth(path = "./auth.json")
} else{
  system('echo "$BIGQUERY_CREDS_64" | base64 --decode > auth-cred.json')
  bq_auth(path = "./auth-cred.json")
}

# Set the time frame

week_end <- today()
week_start <- as_datetime(week_end - days(7))
week_start_utc <- as.numeric(week_start)


# Establish connection with BigQuery

conn <- dbConnect(
  bigquery(),
  project = Sys.getenv("BQ_PROJECT"),
  dataset = "music_history",
  billing = Sys.getenv("BQ_PROJECT")
)


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

dbAppendTable(conn,"tracks_feed",tracks_df)

print("Finished importing tracks")