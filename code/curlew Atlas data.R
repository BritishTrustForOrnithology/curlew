##########################################
#
#        BRECKS CURLEW ATLAS DATA
#
##########################################


library(maptools)
library(reshape)
library(raster)
library(sp)
library(rgeos)
library(rgdal)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(devtools)
library(birdatlas)
library(tidyverse)
library(sf)
library(foreign)


# =================================  SET DIRECTORY STRUCTURE  ====================

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

#### SET DIRECTORY PATHS
# # Wales HPC cluster
# if (cluster) parentwd <- c("/home/samantha.franks/")

if (cluster) parentwd <- c("/users1/samf") # BTO cluster
if (!cluster) {
  if (!Mac) parentwd <- c("C:/Users/samf/Documents")
  if (Mac) parentwd <- c("/Volumes/SAM250GB/BTO PC Documents")
}

scriptswd <- paste(parentwd, "Git/curlew/scripts", sep="/")
datawd <- paste(parentwd, "Git/curlew/data", sep="/")
outputwd <- paste(parentwd, "Git/curlew/output", sep="/")
workspacewd <- paste(parentwd, "Git/curlew/workspaces", sep="/")

options(digits=6)

# ======================== LOAD FUNCTIONS ===========================

# # uses the birdatlas package on Github:
# # https://github.com/BritishTrustForOrnithology/birdatlas
# remotes::install_github("BritishTrustForOrnithology/birdatlas", auth_token = "42e2d1d35aa57a58fecc5715773fb994b707f6b4", quiet = FALSE)

library(birdatlas)

# # use the source file if not using BTO Atlas package installed using devtools from Github
# source(paste(parentwd, "Git/atlas_core_functions/include_all_functions.R", sep="/"), chdir=TRUE)


# # ======================== TTV EFFORT ==========================
# 
# # # load details of TTVs surveyed and create a tetrad column with tenkm and tetlet
# # allttv <- load.ttv.details()
# # allttv$tetrad <- paste(allttv$tenkm, allttv$tetlet, sep="")
# # allttv$month <- month(allttv$obsdt)
# # allttv.breedseason <- filter(allttv, month >= 4 & month <= 7)
# 
# # load shapefile of Brecks tetrads that are desired
# # Brecks tetrad shapefile created based on selecting all tetrads in a rectangular grid + 1 that overlap with the Breckland SPA
# GB2kmBrecks <- readOGR(paste(parentwd, "GIS/projects/curlew", sep="/"), "GB002km_Brecks")
# GB2kmBrecks@data$ttv_surveyed <- ifelse(GB2kmBrecks@data$TETRAD %in% allttv.breedseason$tetrad, 1, 0)
# GB2kmBrecks.surveyed <- subset(GB2kmBrecks, ttv_surveyed==1)
# writeOGR(GB2kmBrecks.surveyed, outputwd, layer="ttv effort Brecks", driver="ESRI Shapefile")


# =========================   LOAD & MANIPULATE DATA   ========================

# readme for protocol_id:
# AROV = roving record
# ATTV1 = 1st hour of TTV
# ATTV2 = 2nd hour of TTV
# BTC = casual Bird Track records
# BTL = Bird Track list

# # ---------------  Load entire Atlas dataset, and subset & save CU only  ------------
# 
# # load entire raw 2010 Atlas dataset
# # convert to data.table and rm/gc dataframe
# dat_all <- load_raw_data_2010()
# dt_all <- dat_all %>% as.data.table(.)
# rm(dat_all)
# gc()
# 
# # # read in Atlas CU data which was extracted by Lucy
# # dat0 <- read.csv(paste(datawd, "tetrad_raw_data_for_CU_Brecks.csv", sep="/"), header=TRUE)
# 
# # subset to CU only, remove unnecessary columns and create tetrad field
# dt_CU <- dt_all[speccode == 203,]
# dt_CU[, user_id := NULL]
# dt_CU[, tetrad := paste0(tenkm, tetlet)]
# 
# # save dt_CU as its own dataset
# saveRDS(dt_CU, file.path(datawd, "all_atlas_2010_curlew.rds"))


# ---------------  Load all CU Atlas dataset, subset to study tetrads  ------------

# load RDS of dt_CU
dt_CU <- readRDS(file.path(datawd, "all_atlas_2010_curlew.rds"))

# load Breckland study area tetrads from GB002 shapefile
brecks_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_Brecks.dbf")) %>% as.data.table
brecks_tetrads[, `:=` (FID_1 = NULL,
                       LAND = NULL)]
setnames(brecks_tetrads, "TETRAD", "tetrad")
brecks_tetrads[, area := "brecks"]


# # load Wild Sands study area tetrads from GB002 shapefile
# wildsands_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_WildSands.dbf")) %>% as.data.table
# wildsands_tetrads[, `:=` (FID_1 = NULL,
#                        LAND = NULL)]
# setnames(wildsands_tetrads, "TETRAD", "tetrad")
# wildsands_tetrads[, area := "wild sands"]

# load Wild Sands EXTENDED study area tetrads from GB002 shapefile
wildsands_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_WildSands_extended.dbf")) %>% as.data.table
wildsands_tetrads[, `:=` (FID_1 = NULL,
                          LAND = NULL)]
setnames(wildsands_tetrads, "TETRAD", "tetrad")
wildsands_tetrads[, area := "wild sands"]

# combine Brecks / Wild Sands into single 'study area' object list of tetrads
study_tetrads <- list(brecks_tetrads, wildsands_tetrads) %>% rbindlist

# subset dt_CU by study areas, remove any tetrads with NA for speccode, select only April through July, Breeding season records, and category possible, probable and confirmed breeding
dt_CU_study_tetrads <- dt_CU[study_tetrads, on = "tetrad"][!is.na(speccode), ][obsmonth >= 4 & obsmonth <= 7,][season =="B" & cat %in% c(1, 2, 3),]

# create table showing number of obs in each breeding category status for each tetrad
tetrad_cat <- as.matrix(table(dt_CU_study_tetrads$tetrad, dt_CU_study_tetrads$cat))
colnames(tetrad_cat) <- c("possible","probable","confirmed")

# create new dataset with tetrad and numbers of instances of each breeding category status in each tetrad
tetrad_cat <- dt_CU_study_tetrads[,.N, .(tetrad, cat)] %>% dcast(., tetrad ~ cat, value.var = "N")
setnames(tetrad_cat, c("tetrad", "1", "2", "3"), c("tetrad", "possible", "probable", "confirmed"))

# create a new variable showing highest breeding evidence for a tetrad over the course of the Atlas (can be from any type of Atlas record, TTV1, TTV2, roving record or Bird Track evidence, in any year)
tetrad_cat[, breedevidence := ifelse(!is.na(confirmed), "confirmed", 
                                     ifelse(!is.na(probable), "probable", "possible")
                                     )]

# add study area to dataset of tetrads and highest breeding status
study_tetrad_area_lookup <- dt_CU_study_tetrads[,.N, .(tetrad, area)][, .(tetrad, area)]
tetrad_cat[study_tetrad_area_lookup, area := i.area]


# ---------------  Write breeding evidence status to shapefile attribute table  ------------

# Merge breeding evidence status dt to Brecks and Wild Sands shapefile attribute table
GB2kmBrecks <- st_read(file.path(parentwd, "GIS/projects/curlew/GB002km_Brecks.shp"), stringsAsFactors = FALSE) %>% select(., tetrad = TETRAD)
GB2kmBrecks_CU <- GB2kmBrecks %>% 
  merge(., tetrad_cat[area == "brecks",], by = "tetrad")
st_write(GB2kmBrecks_CU, 
         dsn = file.path(outputwd, "shapefiles"),
         layer = "CU_tetrads_breeding_evidence_Brecks",
         driver = "ESRI Shapefile",
         update = TRUE)


GB2kmWildSands <- st_read(file.path(parentwd, "GIS/projects/curlew/GB002km_WildSands_extended.shp"), stringsAsFactors = FALSE) %>% select(., tetrad = TETRAD)
GB2kmWildSands_CU <- GB2kmWildSands %>% 
  merge(., tetrad_cat[area == "wild sands",], by = "tetrad")
st_write(GB2kmWildSands_CU, 
         dsn = file.path(outputwd, "shapefiles"),
         layer = "CU_tetrads_breeding_evidence_WildSands_extended",
         driver = "ESRI Shapefile",
         update = TRUE)


