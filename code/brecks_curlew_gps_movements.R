##############################
#
#    Brecks Curlew movement data - tagged birds
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="2022-07", workspace_version_date="2022-07")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate", "sf", "knitr", "leaflet", "shiny", "move", "leaflet.extras2", "geojsonsf") 
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


# =======================    ANIMATION   =================



# =======================    Load data   =================

today_date <- Sys.Date()

file_format <- "mp4"   # various formats available in MoveVis package, if you've got a long animation, gif file size is huge, mp4s are much smaller
map_service <- "osm"   # choose which map service, I've used osm and mapbox (satellite imagery)
map_style <- "watercolor" # choose map style (terrain vs satellite)
confidential <- FALSE   # strips lat/lon axis labels from map

path_col <- "blue"
tail_trace_col <- "skyblue"


# load movebank log details
source(file.path(codewd, "movebank_log.R"))

# get info out of movebank
mb_study_name <- searchMovebankStudies(x="Curlew Breckland", login=loginStored)
mb_study_id <- getMovebankID(mb_study_name, login=loginStored)
mb_study_animals <- getMovebankAnimals(study = mb_study_id, login=loginStored)
mb_individual_id <- mb_study_animals[grep("LG", mb_study_animals$animalName), "local_identifier"]

# automatic Movebank download - doesn't work if tags are redployed
all_tags <- getMovebankLocationData(
  study = mb_study_name,
  login = loginStored,
  sensorID = "GPS",
  animalName = mb_individual_id,
  removeDuplicatedTimestamps = TRUE
) %>% as_tibble

# # read in Movebank data as downloaded csv
# all_tags <- read.csv(file.path(datawd, "BTO_NE_Pensthorpe_WWT - Eurasian Curlews - headstarted.csv"), header = TRUE, stringsAsFactors = FALSE) %>% 
#   as_tibble

# replace all '.' from Movebank column names with '_'
names(all_tags) <- names(all_tags) %>% 
  str_replace_all("[.]", "_")

# merge metadata with tag data
all_tags <- all_tags %>% 
  mutate(new_datetime = as.POSIXct(strptime(timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))) %>% 
  mutate(new_datetime_min = format(new_datetime,format='%Y-%m-%d %H:%M')) %>% 
  mutate(plot_label = paste(mb_individual_id, sep="_")) %>% 
  mutate(plot_label = str_replace_all(plot_label, " ", "_"))

# # add time since midnight
# clocks <-  function(t){hour(t)*3600+minute(t)*60+second(t)}
# all_tags$sec_since_midnight <- clocks(all_tags$new_datetime)
# 
# # add rough day / night variable (6am UTC to 6pm UTC = day)
# all_tags <- all_tags %>% 
#   mutate(day_night = ifelse(sec_since_midnight < 21600 | sec_since_midnight > 64800, "night", "day"))


# Choose dates -----------------


first_date <- dmy("01-06-2022")
last_date <- ymd(today_date)


# =======================    Plot data   =================

# fix rate controls downsampling of data, in minutes
# Could set to 15 if you have high res data, but trade off in terms of long rendering time
fix_rate <- 60

# filter movement data to site, cohort, post-release date times
bird_df <- all_tags %>% 
  filter(new_datetime >= strptime(paste(first_date, "09:00:00"), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))

# subset to only relevant columns needed to plot movements
# filter to high satellite counts only (4+)
# convert to move object
bird_df_move <- bird_df %>% 
  filter(gps_satellite_count >= 3) %>% 
  dplyr::select(plot_label, new_datetime, location_lat, location_long) %>% 
  df2move(proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
          x = "location_long",
          y = "location_lat",
          time = "new_datetime",
          track_id = "plot_label"
  )

# align move_data to a uniform time scale
m <- align_move(bird_df_move, res = fix_rate, unit = "mins")

# set path colours
path_colours <- "blue"

# create spatial frames with a OpenStreetMap terrain map
frames <- frames_spatial(m, path_colours = path_colours,
                                  map_service = map_service, 
                                  map_type = map_style,
                                  map_token = ifelse(map_service == "mapbox", "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg", ""),
                                  alpha = 0.5,
                                  # equidistant = ifelse(site == "Ken Hill", TRUE, FALSE),
                                  
                                  path_legend = TRUE,

                                  # tail_size = 1.2,
                                  tail_length = 20 #,
                                  # trace_show = TRUE,
                                  # trace_colour = trace_colours
                                  
) %>%
  
  # add some customizations, such as axis labels
  # add_labels(x = "Longitude", y = "Latitude") %>% 
  # add_northarrow() %>%
  # add_scalebar() %>%
  add_timestamps(m, type = "label") %>%
  add_progress() %>% 
  add_gg(gg = expr(list(
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()
    ))))

bto_logo <- png::readPNG(file.path(datawd, "C1-BTO-master-logo-portrait-(no-strap).png"))
bto_rast <- grid::rasterGrob(bto_logo, interpolate=TRUE)

frames <- add_gg(frames,
                          gg = expr(list(
                            annotation_custom(bto_rast,
                                              xmin=max(m$x)+0.002, xmax= max(m$x) + 0.009,
                                              ymin= max(m$y)-0.005, ymax= max(m$y) + 0.008)
                          ))) %>% 
  add_gg(gg = expr(list(
    labs(caption="\u00A9 Sam Franks / British Trust for Ornithology",
         title = paste("Breckland Curlew", mb_individual_id))
  )))


# preview one of the frames, e.g. the 100th frame
frames[[100]]

animate_frames(frames, out_file = file.path(outputwd, paste0("Breckland_2022_", map_style, "_with-longer-tail_", today_date, ".", file_format)),
               # overwrite = TRUE,
               # fps = 10
)

#####################################################
#####################################################
#####################################################

# tag_list <- c("220903")
# 
# for (tag in tag_list) {
#   
#   if (tag %in% c("210996")) {
#     site <- "Stanta 2021"
#     season <- "migration"
#   }
#   
#   if (tag %in% c("220903")) {
#     site <- "Brettenham 2022 OY"
#     season <- "migration"
#   }
#   
#   # load data, convert datetimes
#   
#   bird_df <- read.csv(file.path(datawd, "tracking_data", grep(tag, dir(paste(datawd, "tracking_data", sep="/")), value=TRUE)), header=TRUE, stringsAsFactors = FALSE)[, 1:22]
#   bird_df$new_datetime <- as.POSIXct(strptime(paste(bird_df$UTC_date, bird_df$UTC_time), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
#   
#   # Subset to season
#   if (season %in% "breeding") {
#     bird_df <- bird_df %>% 
#       filter(new_datetime >= "2022-04-01 16:00:00" & new_datetime < "2022-07-12")
#     fix_rate <- 60     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
#     # set_extent <- extent(0.66072464, 0.87235630, 52.437923, 52.564039)
#     set_extent <- extent(0.75307846, 0.84895134, 52.443769, 52.518937)
#     
#   }
#   
#   if (season %in% "migration") {
#     bird_df <- bird_df %>% 
#       filter(new_datetime >= "2022-07-12" & new_datetime < "2022-07-17")
#     fix_rate <- 15     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
#   }
#   
#   if (season %in% "wintering") {
#     bird_df <- bird_df %>% 
#       filter(new_datetime >= "2022-07-16" & new_datetime < today_date)
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
#   if (site %in% "Brettenham 2022 OY") {
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
# 
# # =======================    LEAFLET MAP   =================
# 
# 
# # =======================    Load data   =================
# 
# today_date <- Sys.Date()
# 
# all_tags <- getMovebankData(
#   study = "Eurasian Curlew Breckland"
# ) %>% as.data.frame
# 
# # all_tags <- read.csv(file.path(datawd, "wwrg_data",  "movebank_wwrg_curlew_20211026.csv"), header = TRUE, stringsAsFactors = FALSE)
# all_tags$new_datetime <- as.POSIXct(strptime(all_tags$timestamp, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
# all_tags$new_datetime_min <- format(all_tags$new_datetime,format='%Y-%m-%d %H:%M')
# 
# # add time since midnight
# clocks <-  function(t){hour(t)*3600+minute(t)*60+second(t)}
# all_tags$sec_since_midnight <- clocks(all_tags$new_datetime)
# 
# # add rough day / night variable (6am UTC to 6pm UTC = day)
# all_tags <- all_tags %>% 
#   mutate(day_night = ifelse(sec_since_midnight < 21600 | sec_since_midnight > 64800, "night", "day"))
# 
# 
# # Choose dates -----------------
# 
# first_date <- "2022-06-15"
# # first_date <- min(all_tags$new_datetime) %>% as.Date()
# last_date <- today_date + 1
# 
# all_tags_filtered <- all_tags %>% 
#   filter(new_datetime >= first_date & new_datetime <= last_date)
# 
# names(all_tags_filtered) <- names(all_tags_filtered) %>% 
#   str_replace_all("[.]", "_")
# 
# 
# # Clean tag data - generic  -----------------
# 
# # filter out low sat counts
# # filter out altitudes that are unlikely
# tags_cleaned <- all_tags_filtered %>% 
#   filter(gps_satellite_count >= 1)
# 
# 
# 
# # --------  Map of all tagged birds by individual  ----------
# 
# rmarkdown::render(
#   input = file.path(codewd, "brecks_curlew_leaflet_maps.Rmd"),
#   output_file = paste0(today_date, "_all_tags_leaflet_map_", first_date, "_", last_date, ".html"),
#   output_dir = outputwd)

