##############################
#
#    WWRG Curlews - data request GPS data
#    Preparing GPS data for data requests
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

giswd <- file.path("../../GIS/curlew/wwrg")
gis_1km_dir <- file.path("../../GIS/British Isles/National Grids/GB/")
dr_datawd <- file.path(datawd, "wwrg_data", "serviced_data_requests")
if (!dir.exists(dr_datawd)) dir.create(dr_datawd)


# =======================    Control statements   =================

dr_shp_name <- "sedgeford_solar_clipped_gps_points.shp"
dr_csv_export_name <- "2022-10_sedgeford_solar_gps_basic.csv"
dr_geo_export_name <- "2022-10_sedgeford_solar_gps_basic.gpkg"

# sedgeford solar grid refs
dr_gridrefs <- c("TF7033", "TF7133", "TF7233", "TF7034", "TF7134", "TF7234", "TF7035", "TF7135", "TF7235")


# =======================    Load data   =================

today_date <- Sys.Date()

# load GPS data
all_tags <- getMovebankData(
  study = "Wash 2021 - Eurasian Curlews"
) %>% as.data.frame

all_tags_filtered <- all_tags %>% 
  filter(gps_satellite_count >= 4) %>% 
  filter(event_id != 23271192487)

# =======================    Extract data using sf in R   =================

# convert movebank data to sf points object
all_tags_points <- sf::st_as_sf(all_tags_filtered,
                                coords = c("location_long",
                                           "location_lat"),
                                crs = 4326)

# load GB 1km square shapefile and filter to requested grid refs
GB001_clip <- st_read(file.path(gis_1km_dir, "GB001kmclip2land_corrected.shp")) %>% 
  filter(ONEKMREF %in% dr_gridrefs)

# transform GB 1km squares to WGS 84
GB001_clip_4326 <- st_transform(GB001_clip, crs = 4326)

# intersect gps data to requested grid refs
points_clipped <- st_intersection(all_tags_points, GB001_clip_4326)

# add required columns for export
dr_data_sf <- points_clipped %>% 
  arrange(timestamp) %>% 
  rename(gridref = ONEKMREF) %>% 
  mutate(species = "curlew") %>% 
  mutate(date = as.Date(timestamp)) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

# export csv
dr_data_sf %>% 
  st_drop_geometry() %>% 
  dplyr::select(species, date, lat, lon) %>% 
  write.csv(., file.path(dr_datawd, dr_csv_export_name), row.names = FALSE)

# export geo file
dr_data_sf %>% 
  dplyr::select(species, date, lat, lon) %>% 
  st_write(., file.path(dr_datawd, dr_geo_export_name), driver = "GPKG", append = FALSE)



  # =======================    Extract data using QGIS then tidy & export in R  =================
  
# Read in points shapefile clipped in QGIS -----------------

dr_data <- st_read(file.path(giswd, dr_shp_name)) %>% 
  dplyr::select(timestamp, geometry) %>% 
  mutate(species = "curlew") %>% 
  mutate(date = as.Date(timestamp)) %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])


# Basic data export -----------------

# export csv file
dr_data %>% 
  st_drop_geometry() %>% 
  arrange(timestamp) %>% 
  dplyr::select(-timestamp) %>% 
  write.csv(., file.path(dr_datawd, dr_csv_export_name), row.names = FALSE)

# export shp file
dr_data %>% 
  arrange(timestamp) %>% 
  dplyr::select(-timestamp) %>% 
  st_write(., file.path(dr_datawd, dr_geo_export_name), driver = "GPKG", append = FALSE)


# Basic data export - CR Marks -----------------

cr_dt <- read.csv(file.path(datawd, "wwrg_data", "cmarksSolar.csv")) %>% 
  mutate(species = "curlew") %>% 
  mutate(new_date = lubridate::dmy(date)) %>% 
  dplyr::select(-date) %>% 
  rename(date = new_date) %>% 
  dplyr::select(species, date, site, lat, lon) %>% 
  write.csv(., file.path(dr_datawd, "2022-10__sedgeford_solar_cr-marks_basic.csv"), row.names = FALSE)

