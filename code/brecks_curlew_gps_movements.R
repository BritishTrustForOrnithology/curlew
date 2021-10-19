##############################
#
#    Brecks Curlew movement data - tagged birds
#
##############################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew", output_version_date="2021_headstarting", workspace_version_date="2021_headstarting")
package_details <- c("sf","tidyverse","patchwork","move","moveVis","RColorBrewer","viridisLite","rcartocolor","lubridate")
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


# =======================    Load data   =================

today_date <- Sys.Date()

file_format <- "mp4"   # various formats available in MoveVis package, if you've got a long animation, gif file size is huge, mp4s are much smaller
map_service <- "osm"   # choose which map service, I've used osm and mapbox (satellite imagery)
map_style <- "watercolor" # choose map style (terrain vs satellite)
confidential <- FALSE   # strips lat/lon axis labels from map

path_col <- "blue"
tail_trace_col <- "skyblue"

tag_list <- c("210996")

for (tag in tag_list) {
  
  if (tag %in% c("210996")) {
    site <- "Stanta 2021"
    season <- "migration"
  }
  
  # load data, convert datetimes
  
  bird_df <- read.csv(file.path(datawd, "tracking_data", grep(tag, dir(datawd), value=TRUE)), header=TRUE, stringsAsFactors = FALSE)[, 1:22]
  bird_df$new_datetime <- as.POSIXct(strptime(paste(bird_df$UTC_date, bird_df$UTC_time), format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
  
  # Subset to season
  if (season %in% "breeding") {
    bird_df <- bird_df %>% 
      filter(new_datetime >= "2021-05-28 16:00:00" & new_datetime < "2021-07-10")
    fix_rate <- 60     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
    # set_extent <- extent(0.66072464, 0.87235630, 52.437923, 52.564039)
    set_extent <- extent(0.75307846, 0.84895134, 52.443769, 52.518937)
    
  }
  
  if (season %in% "migration") {
    bird_df <- bird_df %>% 
      filter(new_datetime >= "2021-07-06" & new_datetime < "2021-07-09")
    fix_rate <- 15     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
  }
  
  if (season %in% "wintering") {
    bird_df <- bird_df %>% 
      filter(new_datetime >= "2021-07-09" & new_datetime < today_date)
    fix_rate <- 60     # controls downsampling of data, in minutes. Could set to 15 if you have high res data, but trade off in terms of rendering time
  }
  
  # subset to only relevant columns needed to plot movements
  # filter to high satellite counts only (4+)
  # convert to move object
  bird_df_move <- bird_df %>% 
    filter(satcount >= 4) %>% 
    dplyr::select(device_id, new_datetime, Latitude, Longitude) %>% 
    mutate(bird_id = paste0("tag_", device_id)) %>% 
    df2move(
      # proj = crs("+init=epsg:4326"),
      proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
      x = "Longitude",
      y = "Latitude",
      time = "new_datetime",
      track_id = "bird_id"
    )
  
  # align move_data to a uniform time scale
  m <- align_move(bird_df_move, res = fix_rate, unit = "mins")
  
  if (site %in% "Stanta 2021") {
    
    if (map_style %in% "satellite") {
      
      if (season %in% c("breeding", "wintering")) {
        frames <- m %>% 
          frames_spatial(path_colours = c("orangered"),
                         map_service = "mapbox",
                         map_type = map_style,
                         map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg",
                         tail_colour = "orange",
                         tail_size = 1.2,
                         trace_show = TRUE,
                         trace_colour = "orange",
                         # equidistant = TRUE
                         ext = set_extent
          )  %>% 
          add_gg(gg = expr(list(
            theme(
              legend.position = "none")
          ))) %>%  
          add_labels(x = "Longitude", y = "Latitude") %>%
          add_northarrow() %>%
          add_scalebar() %>%
          add_timestamps(m, type = "label") %>%
          add_progress()
        
      } else {
        
        frames <- m %>% 
          frames_spatial(path_colours = c("orangered"),
                         map_service = "mapbox",
                         map_type = map_style,
                         map_token = "pk.eyJ1Ijoic2ZyYW5rczgyIiwiYSI6ImNrcmZkb2QybDVla2wyb254Y2ptZnlqb3UifQ.YNOfGD1SeRMZJDur73Emyg",
                         tail_colour = "orange",
                         tail_size = 1.2,
                         trace_show = TRUE,
                         trace_colour = "orange" #,
                         # equidistant = TRUE
          )  %>% 
          add_gg(gg = expr(list(
            theme(
              legend.position = "none")
          ))) %>%  
          add_labels(x = "Longitude", y = "Latitude") %>%
          # add_northarrow() %>%
          add_scalebar(position = "bottomright") %>%
          add_timestamps(m, type = "label") %>%
          add_progress()
        
      }
    }
    
    if (map_service %in% "osm") {
      
      if (confidential) {
        
        frames <- m %>% 
          frames_spatial(path_colours = path_col,
                         map_service = map_service,
                         map_type = map_style,
                         tail_colour = tail_trace_col,
                         tail_size = 1.2,
                         trace_show = TRUE,
                         trace_colour = tail_trace_col #,
                         # equidistant = TRUE
          ) %>% 
          add_gg(gg = expr(list(
            theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "none")
          ))) %>% 
          # add_labels(x = "Longitude", y = "Latitude") %>%
          # add_northarrow() %>%
          add_scalebar(position = "bottomright") %>%
          add_timestamps(m, type = "label") %>%
          add_progress()
        
      } else {
        
        frames <- m %>% 
          frames_spatial(path_colours = path_col,
                         map_service = map_service,
                         map_type = map_style,
                         tail_colour = tail_trace_col,
                         tail_size = 1.2,
                         trace_show = TRUE,
                         trace_colour = tail_trace_col #,
                         # equidistant = TRUE
          ) %>%
          add_gg(gg = expr(list(
            theme(
              legend.position = "none")
          ))) %>% 
          add_labels(x = "Longitude", y = "Latitude") %>%
          # add_northarrow() %>%
          add_scalebar(position = "bottomright") %>%
          add_timestamps(m, type = "label") %>%
          add_progress()
      }
      
    }
    
  }
  
  
  
  frames[[1]]
  
  animate_frames(frames, 
                 out_file = file.path(outputwd, paste0(tag, "_", site, "_", season, "_",  map_style, "_with-longer-tail_", today_date, ".", file_format)), fps = 20, overwrite = TRUE
  )
  
}

