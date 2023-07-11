#' This file queries Last.FM's API weekly to get the data, then it feeds it to
#' a BigQuery table
#' 
#' This code is supposed to run on Sundays, fetching the data from that week's
#' Monday up until Sunday.


library(dplyr)
library(lubridate)
library(jsonlite)


if(Sys.getenv("LASTFM_KEY") == ""){
  dotenv::load_dot_env()
}


week_end <- today()
week_start <- as_datetime(week_end - days(6))
week_start_utc <- as.numeric(week_start)


week_tracks <- read_json(paste0(
  "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=NathanaelRolim&api_key=",
  Sys.getenv("LASTFM_KEY"),
  "&format=json&limit=200&from=",
  week_start_utc
), simplifyVector = TRUE)

week_df <- NULL

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


week_df <- week_df %>%
  select(-c(streamable,
            image))

week_df <- week_df %>%
  unnest(.,cols = c(artist,album,date), keep_empty = TRUE) %>%
  rename(.,
         artist_mbid = mbid,
         artist_name = `#text`,
         track_mbid = mbid1,
         album_mbid = mbid2,
         album_name = `#text1`,
         track_name = name,
         date_uts = uts,
         date_text = `#text2`)

week_df <- week_df %>%
  mutate(date_uts = as.numeric(date_uts),
         date_full = as_datetime(date_uts)) %>%
  mutate(across(where(is.character), ~ na_if(.,"")))

###############################################################################


week_artists <- unique(week_df$artist_mbid) %>% purrr::discard(is.na)


artists_df <- NULL #replace

existing_artists <- unique(artists_df$artist_mbid)
new_artists <- setdiff(week_artists, existing_artists)

final_artist_df <- NULL

if(length(new_artists) != 0){
  
  for(i in length(new_artists)){
    artist_query <- read_json(paste0(
      "http://ws.audioscrobbler.com/2.0/?method=artist.getinfo&api_key=",
      Sys.getenv("LASTFM_KEY"),
      "&format=json&mbid=",
      new_artists[i]
    ))
    
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
  }
  
} else{
  
  print("No new artists")
}
