library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)



if(Sys.getenv("CI") != "true"){
  dotenv::load_dot_env()
}

rmarkdown::render("index.Rmd")