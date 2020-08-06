library(lubridate)
library(logging)
library(DBI)
library(tidyverse)

# Downloading Script
download_covi19 <- function(start_date, end_date){
  for(i in seq(start_date, end_date, by= 'days')){
    dt      <- as_date(i)
    dt_str  <- format(dt, format="%m-%d-%Y")
    url_str <- sprintf(url, dt_str)
    filename <- sprintf('%s.csv', dt_str)
    if (! file.exists(filename)){
      tryCatch({
        loginfo(url_str, logger="event")
        download.file(url_str, filename)

        ## load csv into DB
        df <- read_csv(filename)
        df$Date <- dt_str
        con <- dbConnect(RSQLite::SQLite(), dbname = 'covid19.db')
        dbExecute(conn = con, sprintf("DELETE FROM covid19 WHERE Date = '%s';",dt_str))
        dbWriteTable(conn = con, name ='covid19', value = df, append=TRUE)
        dbDisconnect(conn = con)

      }, error = function(err) {
        logerror(url_str, logger="event")
        #print(url_str)
      })
    }
  }
}




url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv'

# Handling Logging
basicConfig()
addHandler(writeToFile, logger="event", file="covid19_event.log")


# Handling Arguments
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  end_date   <- today(tz="Asia/Taipei")
  start_date <- end_date - days(3)
  download_covi19(start_date, end_date)
} else if (length(args)==2) {
  start_date <- ymd(args[1])
  end_date   <- ymd(args[2])
  download_covi19(start_date, end_date)
} else{
  stop("Please input 0 argument or 2 arguments", call.=FALSE)
}

