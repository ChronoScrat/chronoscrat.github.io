#' Google's BigQuery allows free-tier users to store up to 5GB of data into Big-
#' Query tables, but there is a catch: unless you set a billing account for your
#' project (something I am not willing to do at this time), all tables have a
#' maximum life of only 60 days. After that, they are completely deleted.
#' 
#' This healthcheck verifies whether the tables required for this project exist,
#' and recreates them if they do not.

# LastFM and MusicBrainz

source('R/heathcheck/music.R')