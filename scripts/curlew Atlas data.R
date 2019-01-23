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

source(paste(parentwd, "Git/atlas_core_functions/include_all_functions.R", sep="/"), chdir=TRUE)


# ======================== TTV EFFORT ==========================

# load details of TTVs surveyed and create a tetrad column with tenkm and tetlet
allttv <- load.ttv.details()
allttv$tetrad <- paste(allttv$tenkm, allttv$tetlet, sep="")
allttv$month <- month(allttv$obsdt)
allttv.breedseason <- filter(allttv, month >= 4 & month <= 7)

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

# read in Atlas CU data which was extracted by Lucy
dat0 <- read.csv(paste(datawd, "tetrad_raw_data_for_CU_Brecks.csv", sep="/"), header=TRUE)
dat0$tetrad <- paste(dat0$tenkm, dat0$tetrad_id, sep="")

# order by tetrad and remove duplicated rows
dat1 <- dat0[order(dat0$tetrad),]
dat1 <- dat1[-anyDuplicated(dat1),] # removes duplicate row 60
dat1$tetrad <- as.factor(dat1$tetrad)

# create table showing number of obs in each breeding category status for each tetrad
tetrad.cat <- as.matrix(table(dat1$tetrad, dat1$cat))
colnames(tetrad.cat) <- c("possible","probable","confirmed")

# create new dataframe with tetrad and numbers of instances of each breeding category status in each tetrad
tetrad.cat2 <- data.frame(tetrad=levels(dat1$tetrad), possible=tetrad.cat[,"possible"], probably=tetrad.cat[,"probable"], confirmed=tetrad.cat[,"confirmed"])
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
