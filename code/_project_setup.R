##################################################
## Project: covid-19
## Script purpose: define useful functions and load packages
## Date: 13-5-2020
## Author: David JPO'Sullivan
##################################################

library(tidyverse)
library(lubridate)
library(ggmap)
library(cowplot) # for nice plots
library(scales)
library(tokenizers)
library(rvest)
library(git2r)
# set a nice teams for all ggplots
theme_set(theme_cowplot())

# functions ---------------------------------------------------------------


# helper to create the url to search 
search_url <- function(road_id, date){
  paste0('https://www.nratrafficdata.ie/c2/tfhourdirection.asp?sgid=ZvyVmXU8jBt9PJE$c7UXt6&spid=', 
         road_id, '&reportdate=', date, '&enddate=', date)
}


# grab time series road_info_df fn 
grab_ts_table <- function(road_id, date){
  
  # where does the table live
  url <- search_url(road_id, date)
  
  # pull the table
  hourly_ts <- url %>% read_html() %>% 
    html_nodes(xpath = '//*[@id="gridTable"]') %>% # pull the table from path
    html_table() # its in a next list format
  
  # reformat the data
  hourly_ts <- hourly_ts[[1]][-c(25:34),] # the last rows are summaries
  names(hourly_ts)[1] <- c('time') # missing name, all other header are fine 
  
  # tidy the names of other vars
  names(hourly_ts) <- hourly_ts %>% names %>% str_replace_all(" ", "_") %>% tolower
  
  # make sure the cols are in the right formate
  # note: they seem to use '-' for missing entries
  hourly_ts <- hourly_ts %>% as_tibble %>% # convert to tibble
    mutate_all(~str_replace_all(string = ., "-", '0')) %>% # replace - with nothing
    mutate_at(vars(-time), ~as.numeric(.)) # convert all cols bar time to numeric
  
  # add in some information
  hourly_ts <- hourly_ts %>% # convert to a tibble 
    mutate(date = date, # add date and date_time cols
           date_time = parse_date_time(paste(date, time), orders = c('ymdHM')))
  hourly_ts$road_id <- road_id
  
  return(hourly_ts)
}


# plotting helpers --------------------------------------------------------

log_10_axis_text <- function(x)trans_format("log10", math_format(10^.x))
