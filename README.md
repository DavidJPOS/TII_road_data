# Road Traffic data for Ireland

Scrapped from the following website: [here](https://www.nratrafficdata.ie/c2/gmapbasic.asp?sgid=ZvyVmXU8jBt9PJE$c7UXt6).

This repo should, all going well, update automatically daily with the latest data.

The data folder has: 

* `all_daily_road_wGPS.csv`, which has the total through put of traffic for each counters. It also has GPS corrdinates for each roads.
* `all_hourly_road_wGPS.csv`, which is the same as above but for hourly data. 
* `nra_combine_info_full.csv`, which roads (and on which days) that data was successfully scrapped. 
* `road_offical_gps_locations.csv` the offical positions of the counter along with descriptions.
* `full_data_zipped` unfilter and mostly uncleaned data.

All variables are fairly self-explanatory. 

The plots folder has some summary plots of the data. 

Map of counter locations locations. Green have been sucessfully scrapped, fully red points have not (most of these are counter that are no longer in use).
![the map](https://github.com/DavidJPOS/TII_road_data/blob/master/plots/road_location_with_scraped_data3.png).

The daily traffic counts over the last few months:
![the time-series](https://github.com/DavidJPOS/TII_road_data/blob/master/plots/daily_road_traffic_wWeekDay.png).

The weekly traffic counts over the last few months for NI crossing roads:
![the time-series](https://github.com/DavidJPOS/TII_road_data/blob/master/plots//plots/weekly_NI_traffic.png).


