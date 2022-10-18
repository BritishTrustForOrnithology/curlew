##############################
#
#    WWRG Curlew movement data - tagged birds
#    Some example maps plotting WWRG Curlew movements
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="wwrg", workspace_version_date="wwrg")
package_details <- c("webshot", "sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate", "sf", "knitr", "leaflet", "shiny", "move", "leaflet.extras2", "geojsonsf", "tinytex") 
seed_number <- 1



# =======================    Read header source code   =================

# header code source:
# 1. sets working directories
# 2. loads required packages
# 3. prints session info to file
# 4. sets seed for reproducibility

# should run on either PC or Mac if using .Rproj
source(file.path("code/source_setup_code_rproj.R"))

# project directories created:
# parentwd = Git
# projectwd = eurasian_african_bird_migration_atlas
# codewd = directory containing code, functions, source, etc
# datawd = directory containing data
# outputwd = directory containing outputs and results (within the appropriate version date)
# workspacewd = directory containing workspace files (.rds, .rda, .RData; within the appropriate version date)
# topoutputwd = top level output directory
# topworkspacewd= top level workspace directory


# =======================    Logic controls   =================

filter_data <- FALSE
max_tide_height <- 10


# =======================    Load data   =================

today_date <- Sys.Date()

all_tags <- getMovebankData(
  study = "Wash 2021 - Eurasian Curlews"
) %>% as.data.frame

# all_tags <- read.csv(file.path(datawd, "wwrg_data",  "movebank_wwrg_curlew_20211026.csv"), header = TRUE, stringsAsFactors = FALSE)
all_tags$new_datetime <- as.POSIXct(strptime(all_tags$timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
all_tags$new_datetime_min <- format(all_tags$new_datetime,format='%Y-%m-%d %H:%M')

# add time since midnight
clocks <-  function(t){hour(t)*3600+minute(t)*60+second(t)}
all_tags$sec_since_midnight <- clocks(all_tags$new_datetime)

# add rough day / night variable (6am UTC to 6pm UTC = day)
all_tags <- all_tags %>% 
  mutate(day_night = ifelse(sec_since_midnight < 21600 | sec_since_midnight > 64800, "night", "day"))

migration_dates <- data.frame(
  stringsAsFactors = FALSE,
  local_identifier = c("213809_G/WFN(N0)G",
                       "213810_G/WFN(N2)G","213811_G/WFN(N4)G",
                       "213812_G/WFN(N6)G","213813_G/WFN(N7)G",
                       "213814_G/WFN(N5)G","213815_WFN(6N)/O",
                       "213816_G/WFN(N3)G",
                       "213817_G/WFN(NA)G","213818_WFN(4J)/O"),
  north_22 = c("2022-04-17","2022-04-13",
               "2022-04-25","2022-04-12","2022-04-20",
               "2022-04-14","2022-04-12",
               "2022-04-17","2022-04-18","2022-04-14"),
  south_22 = c("2022-07-01","2022-06-23",
               "2022-07-11","2022-07-07","2022-06-30",
               "2022-07-09","2022-06-22",NA,
               "2022-06-28","2022-06-29")
)


# Choose dates -----------------

first_date <- "2022-04-10"
# first_date <- min(all_tags$new_datetime) %>% as.Date()
last_date <- "2022-04-25"

all_tags_filtered <- all_tags %>% 
  filter(new_datetime >= first_date & new_datetime <= last_date) %>% 
  filter(event_id != 23271192487)

names(all_tags_filtered) <- names(all_tags_filtered) %>% 
  str_replace_all("[.]", "_")


# # Load & merge tide data -----------------
# 
# tide_dt <- read.csv(file.path(datawd, "wwrg_data", "Tides_Bulldog_Bcn_20211001_20211101_0.csv"), header = TRUE, stringsAsFactors = FALSE, skip = 1)[2:12] %>% 
#   rbind(., read.csv(file.path(datawd, "wwrg_data", "Tides_Bulldog_Bcn_20211101_20211201_0.csv"), header = TRUE, stringsAsFactors = FALSE, skip = 1)[2:12]) %>% 
#   rbind(., read.csv(file.path(datawd, "wwrg_data", "Tides_Bulldog_Bcn_20211201_20220101_0.csv"), header = TRUE, stringsAsFactors = FALSE, skip = 1)[2:12]) %>% 
#   rbind(., read.csv(file.path(datawd, "wwrg_data", "Tides_Bulldog_Bcn_20220101_20220201_0.csv"), header = TRUE, stringsAsFactors = FALSE, skip = 1)[2:12]) %>% 
#   rbind(., read.csv(file.path(datawd, "wwrg_data", "Tides_Bulldog_Bcn_20220201_20220301_0.csv"), header = TRUE, stringsAsFactors = FALSE, skip = 1)[2:12]) %>% 
#   rbind(., read.csv(file.path(datawd, "wwrg_data", "Tides_Bulldog_Bcn_20220301_20220401_0.csv"), header = TRUE, stringsAsFactors = FALSE, skip = 1)[2:12])
# names(tide_dt) <- c("site_name", "timestamp", "observed_m", "predicted_m", "surge_m", "msl_m", "residual_m", "sd_m", "status", "quality_percent", "quality_flag")
# tide_dt$new_datetime <- as.POSIXct(strptime(tide_dt$timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
# tide_dt$new_datetime_min <- format(tide_dt$new_datetime,format='%Y-%m-%d %H:%M')
# 
# tide_dt <- tide_dt %>% 
#   filter(new_datetime >= first_date & new_datetime <= last_date)
# 
# tag_tide_merged_dt <- merge(all_tags_filtered, tide_dt, by = "new_datetime_min", all.x = TRUE) %>%
#   filter(!is.na(observed_m))
# 
# if (filter_data) {
#   
#   tag_tide_merged_dt <- tag_tide_merged_dt %>%
#     filter(observed_m <= max_tide_height)
# }


# Clean tag data - generic  -----------------

# filter out low sat counts
# filter out altitudes that are unlikely
tags_cleaned <- all_tags_filtered %>% 
  filter(gps_satellite_count >= 4)


# Clean tag data - altitude  -----------------

# filter out low sat counts
# filter out altitudes that are unlikely
tags_cleaned_alt <- all_tags_filtered %>% 
  filter(gps_satellite_count >= 3) %>% 
  filter(height_above_msl >= 0 & height_above_msl <= 2000)


# Clean tag data - ground speed  -----------------

# filter out low sat counts
# filter out altitudes that are unlikely
tags_cleaned_speed <- all_tags_filtered %>% 
  filter(gps_satellite_count >= 3) %>% 
  filter(ground_speed > 0)




# Plot data -----------------

# 6NO - longitude spring 2022
orn_6no_spring_2022_lon <- ggplot(data = all_tags %>% 
                                    filter(local_identifier %in% "213815_WFN(6N)/O") %>% 
                                    filter(new_datetime >= "2022-04-12" & new_datetime <= "2022-04-14") %>% 
                                    filter(height_above_msl >= 0),
                                  # aes(x = new_datetime, y = height.above.msl)) +
                                  aes(x = new_datetime, y = location_long)) +
  geom_line() +
  scale_x_datetime(breaks = "1 hour") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "", y = "longitude")

# 6NO - height above MSL spring 2022
orn_6no_spring_2022_alt <- ggplot(data = all_tags %>% 
                                    filter(local_identifier %in% "213815_WFN(6N)/O") %>% 
                                    filter(new_datetime >= "2022-04-12" & new_datetime <= "2022-04-14") %>% 
                                    filter(height_above_msl >= 0),
                                  aes(x = new_datetime, y = height_above_msl)) +
  # aes(x = new_datetime, y = location.long)) +
  geom_line() +
  scale_x_datetime(breaks = "1 hour") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "Timestamp (UTC)", y = "height above msl")

# A3N
orn_6no_spring_2022_lon / orn_6no_spring_2022_alt + plot_annotation(title = "Curlew 6N/O spring migration 2022")

ggsave(
  filename = paste0("6NO_spring_2022.png"),
  path = outputwd,
  height = 210,
  width= 210,
  units = "mm"
)



# =======================    Produce R Markdown maps   =================

############# 
############# 
############# 

############# Add crosstalk data slider filter

############# 
############# 
############# 
############# 

# --------  Map of all tagged birds by individual  ----------

# webshot::install_phantomjs()
# webshot::is_phantomjs_installed()
# tinytex::install_tinytex()  # install TinyTeX

rmarkdown::render(
  input = file.path(codewd, "wwrg_curlew", "wwrg_curlew_leaflet_maps.Rmd"),
  output_file = paste0(today_date, "_all_tags_leaflet_map_", first_date, "_", last_date, ".htmlf"),
  output_dir = outputwd)


# --------  Map of all tagged birds by tide height  ----------


rmarkdown::render(
  input = file.path(codewd, "wwrg_curlew", "wwrg_curlew_leaflet_maps_tide_height.Rmd"),
  output_file = paste0(today_date, "_all_tags_leaflet_map_tide_height_", first_date, "_", last_date, ".html"),
  output_dir = outputwd)



# --------  Map of all tagged birds by flight height  ----------


rmarkdown::render(
  input = file.path(codewd, "wwrg_curlew", "wwrg_curlew_leaflet_maps_altitude.Rmd"),
  output_file = paste0(today_date, "_all_tags_leaflet_map_altitude_", first_date, "_", last_date, ".html"),
  output_dir = outputwd)


# --------  Map of all tagged birds by ground speed  ----------


rmarkdown::render(
  input = file.path(codewd, "wwrg_curlew", "wwrg_curlew_leaflet_maps_ground_speed.Rmd"),
  output_file = paste0(today_date, "_all_tags_leaflet_map_ground_speed_", first_date, "_", last_date, ".html"),
  output_dir = outputwd)



# --------  Map of all tagged birds by day / night  ----------


# Create normal map
rmarkdown::render(
  input = file.path(codewd, "wwrg_curlew", "wwrg_curlew_leaflet_maps_day-night.Rmd"),
  output_file = paste0(today_date, "_all_tags_leaflet_map_day-night_", first_date, "_", last_date, ".html"),
  output_dir = outputwd)



# # =======================    Produce MoveVis maps   =================
# 
# file_format <- "mp4"   # various formats available in MoveVis package, if you've got a long animation, gif file size is huge, mp4s are much smaller
# map_service <- "osm"   # choose which map service, I've used osm and mapbox (satellite imagery)
# map_style <- "watercolor" # choose map style (terrain vs satellite)
# confidential <- FALSE   # strips lat/lon axis labels from map
# 
# path_col <- "blue"
# tail_trace_col <- "skyblue"
# 
# tag_list <- c("210996")
# 
# for (tag in tag_list) {
#   
#   if (tag %in% c("210996")) {
#     site <- "Stanta 2021"
#     season <- "migration"
#   }
#   
#   # load data, convert datetimes
#   
#   bird_df <- read.csv(file.path(datawd, "tracking_data", grep(tag, dir(datawd), value=TRUE)), header=TRUE, stringsAsFactors = FALSE)[, 1:22]
#   bird_df$new_datetime <- as.POSIXct(strptime(paste(bird_df$UTC_date, bird_df$UTC_time), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
#   
#   # Subset to season
#   if (season %in% "breeding") {
#     bird_df <- bird_df %>% 
#       filter(new_datetime >= "2021-05-28 16:00:00" & new_datetime < "2021-07-10")
#     fix_rate <- 60     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
#     # set_extent <- extent(0.66072464, 0.87235630, 52.437923, 52.564039)
#     set_extent <- extent(0.75307846, 0.84895134, 52.443769, 52.518937)
#     
#   }
#   
#   if (season %in% "migration") {
#     bird_df <- bird_df %>% 
#       filter(new_datetime >= "2021-07-06" & new_datetime < "2021-07-09")
#     fix_rate <- 15     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
#   }
#   
#   if (season %in% "wintering") {
#     bird_df <- bird_df %>% 
#       filter(new_datetime >= "2021-07-09" & new_datetime < today_date)
#     fix_rate <- 60     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
#   }
#   
#   # subset to only relevant columns needed to plot movements
#   # filter to high satellite counts only (4+)
#   # convert to move object
#   bird_df_move <- bird_df %>% 
#     filter(satcount >= 4) %>% 
#     dplyr::select(device_id, new_datetime, Latitude, Longitude) %>% 
#     mutate(bird_id = paste0("tag_", device_id)) %>% 
#     df2move(
#       # proj = crs("+init=epsg:4326"),
#       proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#       x = "Longitude",
#       y = "Latitude",
#       time = "new_datetime",
#       track_id = "bird_id"
#     )
#   
#   # align move_data to a uniform time scale
#   m <- align_move(bird_df_move, res = fix_rate, unit = "mins")
#   
#   if (site %in% "Stanta 2021") {
#     
#     if (map_style %in% "satellite") {
#       
#       if (season %in% c("breeding", "wintering")) {
#         frames <- m %>% 
#           frames_spatial(path_colours = c("orangered"),
#                          map_service = "mapbox",
#                          map_type = map_style,
#                          map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg",
#                          tail_colour = "orange",
#                          tail_size = 1.2,
#                          trace_show = TRUE,
#                          trace_colour = "orange",
#                          # equidistant = TRUE
#                          ext = set_extent
#           )  %>% 
#           add_gg(gg = expr(list(
#             theme(
#               legend.position = "none")
#           ))) %>%  
#           add_labels(x = "Longitude", y = "Latitude") %>%
#           add_northarrow() %>%
#           add_scalebar() %>%
#           add_timestamps(m, type = "label") %>%
#           add_progress()
#         
#       } else {
#         
#         frames <- m %>% 
#           frames_spatial(path_colours = c("orangered"),
#                          map_service = "mapbox",
#                          map_type = map_style,
#                          map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg",
#                          tail_colour = "orange",
#                          tail_size = 1.2,
#                          trace_show = TRUE,
#                          trace_colour = "orange" #,
#                          # equidistant = TRUE
#           )  %>% 
#           add_gg(gg = expr(list(
#             theme(
#               legend.position = "none")
#           ))) %>%  
#           add_labels(x = "Longitude", y = "Latitude") %>%
#           # add_northarrow() %>%
#           add_scalebar(position = "bottomright") %>%
#           add_timestamps(m, type = "label") %>%
#           add_progress()
#         
#       }
#     }
#     
#     if (map_service %in% "osm") {
#       
#       if (confidential) {
#         
#         frames <- m %>% 
#           frames_spatial(path_colours = path_col,
#                          map_service = map_service,
#                          map_type = map_style,
#                          tail_colour = tail_trace_col,
#                          tail_size = 1.2,
#                          trace_show = TRUE,
#                          trace_colour = tail_trace_col #,
#                          # equidistant = TRUE
#           ) %>% 
#           add_gg(gg = expr(list(
#             theme(axis.text = element_blank(),
#                   axis.title = element_blank(),
#                   axis.ticks = element_blank(),
#                   legend.position = "none")
#           ))) %>% 
#           # add_labels(x = "Longitude", y = "Latitude") %>%
#           # add_northarrow() %>%
#           add_scalebar(position = "bottomright") %>%
#           add_timestamps(m, type = "label") %>%
#           add_progress()
#         
#       } else {
#         
#         frames <- m %>% 
#           frames_spatial(path_colours = path_col,
#                          map_service = map_service,
#                          map_type = map_style,
#                          tail_colour = tail_trace_col,
#                          tail_size = 1.2,
#                          trace_show = TRUE,
#                          trace_colour = tail_trace_col #,
#                          # equidistant = TRUE
#           ) %>%
#           add_gg(gg = expr(list(
#             theme(
#               legend.position = "none")
#           ))) %>% 
#           add_labels(x = "Longitude", y = "Latitude") %>%
#           # add_northarrow() %>%
#           add_scalebar(position = "bottomright") %>%
#           add_timestamps(m, type = "label") %>%
#           add_progress()
#       }
#       
#     }
#     
#   }
#   
#   
#   
#   frames[[1]]
#   
#   animate_frames(frames, 
#                  out_file = file.path(outputwd, paste0(tag, "_", site, "_", season, "_",  map_style, "_with-longer-tail_", today_date, ".", file_format)), fps = 20, overwrite = TRUE
#   )
#   
# }
# 
