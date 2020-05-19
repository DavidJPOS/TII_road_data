##################################################
## Project: TTI-road-counter-data
## Script purpose: create a script to update road data
## Date: 17-5-2020
## Author: David JPO'Sullivan
##################################################

rm(list = ls()) # tidy work space
gc()

source('./code/_project_setup.R')

pull(repo = getwd())

nra_combine_info <- read_csv('./data/nra_combine_info_full.csv')
offical_gps_df <- read_csv('./data/roads_offical_gps_locations.csv')

scraped_data_df <- 
  nra_combine_info %>% 
  mutate(road_id = str_extract(file_names, pattern = 'NRA_.{12}'),
         dates = str_extract(file_names, pattern = '2020.{6}'),
  ) %>% 
  select(road_id, dates)

# update data -------------------------------------------------------------

# create a list of files that I do have

date <- Sys.Date() # grab todays date
search_df <-
  expand.grid(
    # we want to go 120 back for each road
    dates = as.Date(date, format="%Y-%m-%d") - duration(1:7, 'days'),
    road_id = offical_gps_df$road_id,
    stringsAsFactors = FALSE) %>% 
  as_tibble %>% 
  # remove data that I have already collected
  filter(!(road_id %in% scraped_data_df$road_id & 
             dates %in% scraped_data_df$dates))
search_df$successful = FALSE # when we search do we get the time series

# run scrapper
# for each road
for(row_i in (1:nrow(search_df))){ # row_i = 1
  # use this to try all the url's
  # try to get the data
  hourly_ts <- try(grab_ts_table(search_df$road_id[row_i],  search_df$dates[row_i]), TRUE)
  
  # if everything was ok save to file
  if(inherits(hourly_ts, 'try-error') == FALSE){
    write_csv(x = hourly_ts,
              paste0('./data/ind_road_data/', search_df$road_id[row_i],'_',
                     search_df$dates[row_i],'.csv'))
    
    # the current road data to the timeseries
    # all_hourly_ts <- bind_rows(all_hourly_ts , hourly_ts)
    
    # if successful
    search_df$successful[row_i] <- TRUE
  }
  
  # do we need to slow it down for some reason?
  # Sys.sleep(5)
  
  if(row_i %% 10 == 0) {
    cat(sep = "", '###########################\n')
    cat(sep = "", 'Currently ', round((row_i/nrow(search_df))*100, 2),'% done.\n')
    cat(sep = "", 'Currently ', round((sum(search_df$successful[1:row_i])/row_i)*100, 2),'% of roads and dates successfully accessed!\n')
    cat(sep = "", '\n')
  }
}



#### update search df files
downloaded_files <- search_df %>% filter(successful != FALSE)

file_names <-
  list.files('./data/ind_road_data/', full.names = TRUE) %>%
  (function(x=.)(x[grepl(x, pattern = '.csv')]))

# save todays search in a file
zip::zipr(zipfile = paste0('./data/road_data_full_', date,'.zip'), files = file_names)
write_csv(x = search_df, path = paste0('./data/search_results_info_',date,'.csv'))

# summary file of what each file contain (handy for improving the scraper)
read_nra_df <- tibble(file_names = file_names, successful = NA,
                      no_col = NA, no_row = NA, no_numeric = NA,
                      no_na = NA)

col_defs <- cols_only(time = col_time(),
                      total = col_double(),
                      date = col_date(),
                      date_time = col_datetime(),
                      road_id = col_character())

# cycle through and collect files
all_hourly_ts <- tibble() # where we will store the combined data
for(row_i in 1:length(file_names)){ # row_i <- 1
  # for each file
  df <- try({ # see if we can successfully read it in and pull the cols
    read_csv(file_names[row_i], col_types = col_defs) %>%
      select(road_id, everything())
  })
  
  if(inherits(df, 'try-error') == TRUE){ # if this fails
    read_nra_df$successful[row_i] <- FALSE
  }else{
    # otherwise note down some information on what was read in
    read_nra_df$no_col[row_i] <- ncol(df)
    read_nra_df$no_row[row_i] <- nrow(df)
    read_nra_df$no_numeric[row_i] <- sum(lapply(df, class) == 'numeric')
    read_nra_df$no_na[row_i] <- sum(is.na(df) == TRUE)
    
    if(sum(is.na(df$total)) < 24){ # if we are all the days data dont write it
      read_nra_df$successful[row_i] <- TRUE
      all_hourly_ts <- bind_rows(all_hourly_ts, df)
    }
  }
  
  if(row_i %% 1000 == 0) {
    cat(sep = "", '###########################\n')
    cat(sep = "", 'Currently ', round((row_i/length(file_names))*100, 2),'% done.\n')
    cat(sep = "", '\n')
  }
}

write_csv(all_hourly_ts, './data/all_hourly_road_wGPS_today.csv')

# update the files that we have 
nra_combine_info_full <- bind_rows(nra_combine_info,read_nra_df) %>% 
  filter(successful == TRUE) %>% 
  distinct()
write_csv(x = nra_combine_info_full, './data/nra_combine_info_full.csv')


# add geolocation ---------------------------------------------------------

all_hourly_ts <- read_csv('./data/all_hourly_road_wGPS_today.csv')

# # add lat and lon before saving hourly data
offical_gps_df <- read_csv('./data/roads_offical_gps_locations.csv')
offical_gps_df <- offical_gps_df %>% select(-no_idea)

offical_gps_df <- offical_gps_df %>% 
  filter(complete.cases(.), 
         lat !=0 & lon != 0,
         !(short_name %in% c('test','998','M99 1999.9 N','Bluetooth Test Site')))

all_hourly_ts <-  # from the gps data frame
  left_join(x = all_hourly_ts, # we want to add lat and lon by road_id
            y = offical_gps_df %>% select(road_id, lat, lon),
            by = 'road_id')

# save the hourly data to file

all_hourly_ts %>% mutate_all(~is.na(.)) %>% 
  summarise_all(~sum(.)/n()) 

all_hourly_ts <- all_hourly_ts %>% 
  filter(is.na(lon) == FALSE , is.na(lat) == FALSE)

# aggregate to daily level ------------------------------------------------


all_daily_ts <- all_hourly_ts %>% 
  filter(complete.cases(.)) %>%
  group_by(road_id, date, lon, lat) %>% 
  summarise(daily_total = sum(total)) %>% 
  ungroup()

all_daily_ts %>% count(road_id) %>% count(n) %>% arrange(desc(nn))


write_csv(all_daily_ts, path = './data/all_daily_road_wGPS_today.csv')

# combine data ------------------------------------------------------------

# grab the old files
new_road_daily <- read_csv(file = './data/all_daily_road_wGPS_today.csv')
old_road_daily <- read_csv(file = './data/all_daily_road_wGPS.csv')

full_road_daily <- bind_rows(old_road_daily, new_road_daily)
write_csv(x = full_road_daily, path = './data/all_daily_road_wGPS.csv')

# grab the old files
new_road_hourly <- read_csv(file = './data/all_hourly_road_wGPS_today.csv')
old_road_hourly <- read_csv(file = './data/all_hourly_road_wGPS.csv')

full_road_hourly <- bind_rows(old_road_hourly, new_road_hourly)
write_csv(x = full_road_hourly, path = './data/all_hourly_road_wGPS.csv')
# combine the data together


# push to github ----------------------------------------------------------

add(repo = getwd(), path = "./data/all_hourly_road_wGPS.csv")
add(repo = getwd(), path = "./data/all_daily_road_wGPS.csv")
add(repo = getwd(), path = "./data/nra_combine_info_full.csv")
add(repo = getwd(), path = "./data/roads_offical_gps_locations.csv")
# Commit the file
try(commit( repo = getwd(), message = paste0("Update as at: ", Sys.time())))

push(object = getwd(), 
     credentials = cred_user_pass( 
       username = "davidjposullivan@gmail.com", 
       password = "pN4rE05tfaTP"  # BE CAREFUL!!
     ))



# tidy works space when finished -----------------

rm(list = ls()) # tidy work space
gc()
