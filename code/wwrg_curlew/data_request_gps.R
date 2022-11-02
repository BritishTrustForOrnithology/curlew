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
dr_datawd <- file.path(datawd, "wwrg_data", "serviced_data_requests")
if (!dir.exists(dr_datawd)) dir.create(dr_datawd)


# =======================    Control statements   =================

dr_shp_name <- "sedgeford_solar_clipped_gps_points.shp"
dr_csv_export_name <- "2022-10_sedgeford_solar_gps_basic.csv"
dr_geo_export_name <- "2022-10_sedgeford_solar_gps_basic.gpkg"


# =======================    Load data   =================

today_date <- Sys.Date()

dr_data <- st_read(file.path(giswd, dr_shp_name)) %>% 
  dplyr::select(timestamp, geometry) %>% 
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

