##################################################
## Project: TII-road-counter-data
## Script purpose: load data and plot
## Date: 15-5-2020
## Author: David JPO'Sullivan
##################################################

rm(list = ls()) # tidy work space
gc()

# libraries, source files and data ---------------
source('./code/_project_setup.R')

# save the search results
# search_df <- read_csv('./data/search_results_info.csv')
read_nra_df <- read_csv('./data/nra_combine_info_full.csv')
all_hourly_ts <- read_csv('./data/all_hourly_road_wGPS.csv')
offical_gps_df <- read_csv('./data/roads_offical_gps_locations.csv')
all_daily_ts <- read_csv('./data/all_daily_road_wGPS.csv')

pull(repo = getwd())


# plot some data ----------------------------------------------------------

set.seed(1111)
sample_road_id <- all_hourly_ts %>% count(road_id) %>% sample_n(3)
sample_road_id

p1 <- all_hourly_ts %>% filter(road_id %in% sample_road_id$road_id) %>% 
  group_by(date, road_id) %>% 
  summarise(total = sum(total, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = total), color = road_id) + 
  geom_line(size = 1.3) +
  geom_vline(xintercept = 0, color = 'red', size = 2) + 
  scale_colour_viridis_d() + 
  facet_wrap(~road_id, scale = 'free')

p2 <- all_hourly_ts %>% filter(road_id %in% sample_road_id$road_id) %>% 
  group_by(road_id,date) %>% 
  summarise(total = sum(total, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = total, color = road_id)) + 
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, color = 'red', size = 2) + 
  scale_colour_viridis_d(end = 0.8) + 
  theme_classic(); p2

pg1 <- plot_grid(p1, p2, ncol = 1, nrow = 2) # similar behaviour at all scales
# maybe the behaviour is the same across all scales?
ggsave(filename = './plots/selected_roads_ts.png', plot = pg1)

# across all road what is the average number of cars
# might have some sampling bias in the last week (only scrapped part of a week)
p3 <- all_hourly_ts %>% 
  group_by(week = week(date_time)) %>% 
  summarise(total = sum(total, na.rm = TRUE)) %>% 
  ggplot(aes(x = week, y = total)) + 
  geom_line(size = 1.1) +
  # geom_hline(yintercept = 0, color = 'red', size = 2) + 
  scale_colour_viridis_d() + 
  theme_classic()
# p3

p4 <- all_hourly_ts %>%
  group_by(date) %>% 
  summarise(total = sum(total, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = total)) + 
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, color = 'red', size = 2) + 
  scale_colour_viridis_d()
p4

pg2 <- plot_grid(p3, p4, ncol = 1, nrow = 2)
ggsave(filename = './plots/all_roads_weekly_daily_ts.png', plot = pg2)

# beakdown data weekdays vs pre-post lockdown -----------------------------

weekday_ts <- 
  all_hourly_ts %>% 
  mutate(weekday = weekdays(date_time) %>% # break into weekdays and make it a factor
           factor(., levels = c('Monday', 'Tuesday','Wednesday', 
                                'Thursday', 'Friday', 'Saturday', 'Sunday')), # day of week
         lockdown = factor(date <= '2020-03-24', # are we pre or post lockdown?
                           levels = c(TRUE, FALSE),
                           labels = c('pre-lockdown', 'post-lockdown')
         )
  )

weekday_summ <- weekday_ts %>% 
  group_by(weekday, lockdown) %>%
  summarise(mean = mean(total, na.rm = TRUE),
            median = median(total, na.rm = TRUE))

p5 <- # what is the pre and post avg like? 
  weekday_summ %>% 
  ggplot(aes(x = weekday, y = mean)) + 
  geom_bar(stat = 'identity', aes(fill = lockdown), color = 'black', position = 'dodge') +
  scale_fill_viridis_d(begin = 0.2, end = 1) + 
  ylab(label = 'Average number of cars') +
  theme(axis.text.x = element_text(angle = -25))

p6 <- # what is the pre and post avg like? 
  weekday_summ %>% 
  ggplot(aes(x = weekday, y = median)) + 
  geom_bar(stat = 'identity', aes(fill = lockdown), color = 'black', position = 'dodge') +
  scale_fill_viridis_d(begin = 0.2, end = 1) + 
  ylab(label = 'Average number of cars') +
  theme(axis.text.x = element_text(angle = -25))

weekday_ts %>% 
  group_by(weekday, lockdown) %>%
  summarise(mean = mean(total, na.rm = TRUE)) %>%
  ggplot(aes(x = weekday, y = mean, color = lockdown)) + 
  scale_fill_viridis_d(begin = 0.2) + 
  geom_point(aes(fill = lockdown)) + 
  theme(axis.text.x = element_text(angle = -45))

ggsave(filename = './plots/days_of_week_breakdown.png', plot = p5)
# map data ----------------------------------------------------------------

# register_google(key = 'insert-your-api-key')
# gg_ireland <- ggmap( get_map("ireland", zoom=7) )

# write_rds(x = gg_ireland, path = './data/google_map_ireland.rds')
# gg_ireland <- read_rds(path = './data/google_map_ireland.rds')
# 
# p6 <- # map of ireland with markers
#   gg_ireland + 
#   geom_point(data = offical_gps_df %>% select(lon, lat) %>% distinct(), 
#              aes(x = lon, y = lat), color = 'black', size = 2) + 
#   geom_point(data = offical_gps_df %>% select(lon, lat) %>% distinct(), 
#              aes(x = lon, y = lat), color = 'red', size = 1.3) +
#   geom_point(data = all_hourly_ts %>% select(lon, lat) %>% distinct(), 
#              aes(x = lon, y = lat), col = 'green', size = 1) +
#   theme_map()
# p6
# 
# ggsave(filename = './plots/road_location_with_scraped_data3.png', plot = p6)

# update github -----------------------------------------------------------
getwd()
setwd('/home/rstudio/TII_road_data')
add(repo = getwd(), path = "./plots/selected_roads_ts.png")
add(repo = getwd(), path = "./plots/days_of_week_breakdown.png")
add(repo = getwd(), path = "./plots/all_roads_weekly_daily_ts.png")

# Commit the file
try(commit( repo = getwd(), message = paste0("Update as at: ", Sys.time())))
push(object = getwd(), 
     credentials = cred_user_pass( 
       username = "davidjposullivan@gmail.com", 
       password = "pN4rE05tfaTP"  # BE CAREFUL!!
     ))


cat('Finished Job', file = 'plotting.txt')
# tidy works space when finished -----------------

rm(list = ls()) # tidy work space
gc()
