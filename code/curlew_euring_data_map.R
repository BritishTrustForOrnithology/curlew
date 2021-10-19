#####################################################################################

# Eurasian Curlew map - EURING data

#######################################################################################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew")
package_details <- c("tidyverse","data.table","pryr", "sf", "mapview","leaflet","tmap","spData","RColorBrewer","viridisLite","rcartocolor","lubridate")
seed_number <- 1


# =================================  Determine system env  ================================

# LOCAL
if(.Platform$OS =='windows') {
  cluster <- FALSE
  Mac <- FALSE
}

# HPCBTO
if(.Platform$OS=='unix' & Sys.getenv('USER')=='samf') {
  cluster <- TRUE
  Mac <- FALSE
  Wales <- FALSE
}

# Mac
if(.Platform$OS=='unix' & Sys.getenv('USER')=='samantha') {
  cluster <- FALSE
  Mac <- TRUE
  Wales <- FALSE
}

# =======================    Read header source code   =================

# header code source:
# 1. sets working directories
# 2. loads required packages
# 3. prints session info to file
# 4. sets seed for reproducibility

# BTO cluster
if (cluster) source(paste("/users1/samf", "source_setup_code.R", sep="/"))

# either PC or Mac
if (!cluster) {
  if (!Mac) source(paste("C:/Users/samf/Documents/Git/source_code", "source_setup_code.R", sep="/"))
  if (Mac) source(paste("/Volumes/SAM250GB/BTO PC Documents/Git/source_code", "source_setup_code.R", sep="/"))
}

# project directories created:
# parentwd = Git
# projectwd = eurasian_african_bird_migration_atlas
# codewd = directory containing code, functions, source, etc
# datawd = directory containing data
# outputwd = directory containing outputs and results (within the appropriate version date)
# workspacewd = directory containing workspace files (.rds, .rda, .RData; within the appropriate version date)
# top_outputwd = top level output directory
# top_workspacewd= top level workspace directory


# =======================    Load data   =================

# ---------  csv number of nests per year since 2000  ---------

dt <- readRDS(file.path(datawd, "euring_2020-01_Numenius-arquata.rds"))

# filter to any rings with an encounter in the UK
dt_uk_eire <- dt[ring %in% (dt[encounter.country %in% c("United Kingdom", "Ireland"), ring] %>% unique), ]


# =======================    Create spatial data   =================

# ----  Set up ringing/recovery data as linestring objects  ----

# create a geometry column with a LINESTRING of the ringing and the recovery point (x = lon, y = lat)
# you can create and assign objects in j inside the curly braces, then pass them out of j and assign as columns in the data.table
# the last line of j (the 'return' value) needs to be a list
# using by=urn is a bit of a cheat, means that the st_linestring function is applied on each row (which has a unique id)
dt_uk_eire[, geometry := {
  geometry <- st_linestring(x = matrix(c(lon, ringing.lon, lat, ringing.lat), ncol = 2)) %>% st_sfc %>% st_sf
}, 
by = urn
]

# choose only the RECOVERY data for mapping of lines (ringing data won't show anything because the two points of the linestring are in the same place
# convert to sf object
dt_map_lines <- 
  dt_uk_eire[encounter.type == "recovery",] %>% 
  st_as_sf(., crs=4326)


# ----  Set up ringing/recovery data as point objects  ----

dt_map_points <- st_as_sf(dt_uk_eire, coords = c("lon","lat"), crs=4326)



#============================ Plot map  ===================================


# ----  Create map  ----


# -----  set coordinate space - ggplot  ------

set.xlimits <- function(dt_min_lon, dt_max_lon) {
  min_lon <- ifelse(dt_min_lon < 0, dt_min_lon*1.05, ifelse(dt_min_lon == 0, -0.1, dt_min_lon*0.95))
  max_lon <- ifelse(dt_max_lon < 0, dt_max_lon*0.95, ifelse(dt_max_lon == 0, 0.1, dt_max_lon*1.05))
  return(c(min_lon, max_lon))
}

set.ylimits <- function(dt_min_lat, dt_max_lat) {
  min_lat <- ifelse(dt_min_lat < 0, dt_min_lat*1.05, ifelse(dt_min_lat == 0, -0.1, dt_min_lat*0.95))
  max_lat <- ifelse(dt_max_lat < 0, dt_max_lat*0.95, ifelse(dt_max_lat == 0, 0.1, dt_max_lat*1.05))
  return(c(min_lat, max_lat))
}

# coordinate space of map
xlimits <- set.xlimits(dt_uk_eire[, min(lon)], dt_uk_eire[, max(lon)])
ylimits <- set.ylimits(dt_uk_eire[, min(lat)], dt_uk_eire[, max(lat)])

world_epsg4326 <- st_transform(world, crs = 4326)

# mapping points as geom_sf takes a LOT longer than mapping points as geom_point
# also when mapping as geom_sf, can't get the projection to line up properly with the base map even though both are specified as the same

# create base map
base_map <- ggplot() +
  geom_sf(data = world_epsg4326, fill = "grey90", color="grey90")


base_map +
  geom_sf(data = dt_map_points, color = "#326E75", size = 0.7, alpha = 0.7) +
  geom_sf(data = dt_map_lines, colour = "#326E75", size = 0.05, alpha=0.3) +
  coord_sf(xlim = xlimits, ylim = ylimits) +
  theme_bw() +
  theme(
    panel.border = element_rect(colour = "grey80"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
    ) +
  labs(title="Migratory connectivity of ringed UK Curlew")
  
ggsave(file.path(outputwd, "figures", "connectivity_UK_curlew.png"), device="png", width=20, height=15, units="cm")

