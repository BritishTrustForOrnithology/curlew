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
# devtools::install_github('BritishTrustForOrnithology/birdatlas', build_vignettes = TRUE)

library(birdatlas)

# # use the source file if not using BTO Atlas package installed using devtools from Github
# source(paste(parentwd, "Git/atlas_core_functions/include_all_functions.R", sep="/"), chdir=TRUE)


# ======================== TTV EFFORT ==========================

# # load details of TTVs surveyed and create a tetrad column with tenkm and tetlet
# allttv <- load.ttv.details()
# allttv$tetrad <- paste(allttv$tenkm, allttv$tetlet, sep="")
# allttv$month <- month(allttv$obsdt)
# allttv.breedseason <- filter(allttv, month >= 4 & month <= 7)

# load shapefile of Brecks tetrads that are desired
# Brecks tetrad shapefile created based on selecting all tetrads in a rectangular grid + 1 that overlap with the Breckland SPA
GB2kmBrecks <- readOGR(paste(parentwd, "GIS/projects/curlew", sep="/"), "GB002km_Brecks")
GB2kmBrecks@data$ttv_surveyed <- ifelse(GB2kmBrecks@data$TETRAD %in% allttv.breedseason$tetrad, 1, 0)
GB2kmBrecks.surveyed <- subset(GB2kmBrecks, ttv_surveyed==1)
writeOGR(GB2kmBrecks.surveyed, outputwd, layer="ttv effort Brecks", driver="ESRI Shapefile")


# =========================   LOAD & MANIPULATE DATA   ========================

# readme for protocol_id:
# AROV = roving record
# ATTV1 = 1st hour of TTV
# ATTV2 = 2nd hour of TTV
# BTC = casual Bird Track records
# BTL = Bird Track list

# load entire raw 2010 Atlas dataset
# convert to data.table and rm/gc dataframe
dat_all <- load_raw_data_2010()
dt_all <- dat_all %>% as.data.table(.)
rm(dat_all)
gc()

# # read in Atlas CU data which was extracted by Lucy
# dat0 <- read.csv(paste(datawd, "tetrad_raw_data_for_CU_Brecks.csv", sep="/"), header=TRUE)

# subset to CU only, remove unnecessary columns and create tetrad field
dt_CU <- dt_all[speccode == 203,]
dt_CU[, user_id := NULL]
dt_CU[, tetrad := paste0(tenkm, tetlet)]

# load Breckland study area tetrads from GB002 shapefile
brecks_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_Brecks.dbf")) %>% as.data.table
brecks_tetrads[, `:=` (FID_1 = NULL,
                       LAND = NULL)]
setnames(brecks_tetrads, "TETRAD", "tetrad")
brecks_tetrads[, area := "brecks"]


# load Wild Sands study area tetrads from GB002 shapefile
wildsands_tetrads <- foreign::read.dbf(file.path(parentwd, "GIS/projects/curlew", "GB002km_WildSands.dbf")) %>% as.data.table
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
tetrad_cat[, breedevidence := ifelse(confirmed >= 1, "confirmed", 
                                     ifelse(probable >= 1, "probable", "possible")
                                     )]

#####################################
#####################################
#####################################
#####################################
#####################################






tetrad_cat2 <- as.data.table(tetrad_cat)
  data.frame(tetrad=levels(dat1$tetrad), possible=tetrad.cat[,"possible"], probably=tetrad.cat[,"probable"], confirmed=tetrad.cat[,"confirmed"])
rownames(tetrad.cat2) <- 1:nrow(tetrad.cat2)

# create a new variable showing highest breeding evidence for a tetrad over the course of the Atlas (can be from any type of Atlas record, TTV1, TTV2, roving record or Bird Track evidence, in any year)
tetrad.cat2$breedevidence <- with(tetrad.cat2, ifelse(confirmed >= 1, "confirmed", ifelse(probably >= 1, "probable", "possible")))

# Load GB tetrad grid and subset by tetrads which are in Brecks CU dataset and add breeding code status to shapefile attribute table
GB2kmgrid <- readOGR(paste(parentwd, "GIS/British Isles/National Grids/GB", sep="/"), "GB002kmgrid")
GB2kmgrid.sub <- subset(GB2kmgrid, GB2kmgrid$TETRAD %in% levels(tetrad.cat2$tetrad))

GB2kmgrid.sub@data <- rename(GB2kmgrid.sub@data, tetrad=TETRAD)
GB2kmgrid.sub@data <- merge(GB2kmgrid.sub@data, tetrad.cat2, by="tetrad")

# write shapefile
writeOGR(GB2kmgrid.sub, outputwd, layer="CU tetrads_breeding evidence", driver="ESRI Shapefile")

# # turn raw curlew data (not summarised by tetrad across the whole Atlas period) into a shapefile
# GB2kmgrid.sub <- subset(GB2kmgrid, GB2kmgrid$TETRAD %in% levels(dat1$tetrad))
# 
# GB2kmgrid.sub@data <- rename(GB2kmgrid.sub@data, tetrad=TETRAD)
# GB2kmgrid.sub@data <- merge(GB2kmgrid.sub@data, dat1, by="tetrad")
