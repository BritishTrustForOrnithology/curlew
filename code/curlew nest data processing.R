############################################################
#
#        NRS DATA for CURLEW
#
############################################################

# adapted from code from SPACE2 project
# Sam Franks
# 31 October 2016

# This code reads in derived NRS data files (.dv) and creates a cumulative curve of FED for each species
# Threshold FED dates at various levels are calculated

library(mgcv)
library(MASS)
library(reshape)
library(ggplot2)
library(grid)
library(colorRamps)
library(plyr)
library(sp)
library(rgdal)

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
datawd <- paste(parentwd, "Git/curlew/data/", sep="/")
outputwd <- paste(parentwd, "Git/curlew/output", sep="/")
workspacewd <- paste(parentwd, "Git/curlew/workspaces", sep="/")

opar <- par()

keep.obj <- c(ls(),"keep.obj")

source(paste(parentwd, "Git/functions/lsos function.R", sep="/")) # lsos() function

#--------------------------------------------------------------------------------------------------

dvfile <- read.table(paste(datawd, "readme dv file.csv", sep="/"), sep=",", header=TRUE) # derived from X:\Nest Records Unit\NRS\4 NRS output file spec\2 DV and ND file formats column start and end read in points

# newfile <- read.table(paste(datawd, "readme new file.csv", sep="/"), sep=",", header=TRUE) # derived from X:\Nest Records Unit\NRS\4 NRS output file spec\1 Dot new file format\

setwd(datawd)

dat <- list.files()[grep("dv", list.files())] # choose only the .dv files from this folder (folder also contains .nd files)

spp <- substr(dat,1,5) # species name
print(spp)
specieslist[[a]] <- spp

d1 <- read.fwf("curlenew.dv", widths=(dvfile$End-dvfile$Start+1), col.names=dvfile$Variable) # read in fixed width Fortran output file

#============================ DATASET PREPARATION ===================================

#---------- CLEAN DATASET -----------

###--- 1. Generate FEG ---###

#   FEGDIF=MAXFEG-MINFEG;
#   FEGMID=MINFEG + (FEGDIF/2);
#   FEGMID=ROUND(FEGMID,1);
#   IF IERROR NE 0 THEN DELETE;
#   IF FEGDIF > 10 THEN DELETE;

d1$FEGDIF <- d1$MAXFEG - d1$MINFEG
d1$FEGMID <- round(d1$MINFEG + d1$FEGDIF/2)


sum.nests <- table(d1$IYEA) %>% data.frame
names(sum.nests) <- c("year","no.nests")
sum.nests$year <- as.character(sum.nests$year) %>% as.numeric(sum.nests$year)

jpeg(paste(outputwd, "number of curlew nests submitted to NRS per year.jpg", sep="/"), res=300, width=12, height=8, units="in")
plot(no.nests~year, filter(sum.nests, year>=1970), type="l")
par(new=T)
plot(no.nests~year, filter(sum.nests, year>=1970), xaxt="n", yaxt="n", xlab="", ylab="")
title(main="Number of curlew nests submitted to Nest Record Scheme per year since 1970")
abline(v=2001, lty=2, col="red")
dev.off()




###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################




d2 <- subset(d1, IERROR==0)
d2 <- subset(d2, FEGDIF<=10) # Exclude nests where FEGDIF is greater than 10 days

d2 <- subset(d2, FEGMID<275) # Exclude nests after 1 Oct

###--- 2. Generate rough eastings/northings ---###

# Use either 100km grid refs (preferentially, if grid ref data is supplied) 
# If no grid ref data is supplied, then use 4 letter county code data and match with county centroid easting/northing
# If neither grid ref nor county code supplied, then remove record

GB100 <- read.table(paste(parentwd, "GIS/British Isles/National Grids/gridref lookups/GB100kmgid_eastnorth.txt", sep="/"), header=TRUE, sep="\t", na.strings="") # make sure to define <NA> values as blanks otherwise it thinks grid square "NA" is <NA>!
GB100 <- rename(GB100, c("gridref"="GB100"))

county <- read.table(paste(parentwd, "GIS/British Isles/Counties/county centroids.csv", sep="/"), header=TRUE, sep=",")
county <- rename(county, c("county"="IPL1", "E"="easting", "N"="northing"))

# 100km grid ref of nest
d2$GB100 <- substr(d2$IGRI.1.,1,2)

# merge GB100 and nest dataset
# nests without gridrefs, or erroneous gridrefs will be omitted
d2.2 <- merge(d2, GB100, by="GB100", all.x=TRUE)

# nests with grid refs / no grid refs dummy variable
d2.2$grid <- d2.2$land
d2.2$grid[which(is.na(d2.2$grid))] <- 0

# remove unnecessary columns
d2.3 <- d2.2[,-(which(names(d2.2) %in% c("land")))]

# nests with NO grid refs
nogrid <- subset(d2.3, grid==0)
grid <- subset(d2.3, grid==1)

# remove easting/northing columns from no grid
# merge no grid data with county code data
# data with no county data or erroneous county data will be omitted
nogrid2 <- nogrid[,-which(names(nogrid) %in% c("easting","northing"))]
nogrid3 <- merge(nogrid2, county, by="IPL1")
nogrid3 <- nogrid3[,-which(names(nogrid3) %in% c("lat","lon"))]

# order nogrid dataset columns the same as grid dataset columns
nogrid4 <- nogrid3[,names(grid)]

# round eastings/northings to nearest 10000
nogrid4$easting <- round(nogrid4$easting, -5)
nogrid4$northing <- round(nogrid4$northing, -5)

# put no gridref and gridref datasets back together
d2.3 <- rbind(grid,nogrid4)

###--- 3. Exclude species with too few nests in too many years ---###

d3 <- d2.3

d3 <- subset(d2.3, IYEA >= 1983 & IYEA <= 2010)
d3 <- droplevels(d3)

# Skip/exclude species with < 20 nests per year in > 70% of years
samplesize <- ddply(.data=d3, .(IYEA), summarize, n=length(FEGMID))
spp.smallnests <- samplesize[which(samplesize$n <= 20),"IYEA"]
if (length(spp.smallnests)/nrow(samplesize) > 0.7) {next}

#---------- VISUALIZE DATASET - FEG DENSITY PLOTS per YEAR, ALL YEARS -----------

#   # histogram & density plot all nests, all years combined together
#   ggplot(d2, aes(x=FEGMID)) + 
#     geom_histogram(aes(y=..density..), colour="black", fill="white") +    # Histogram with density instead of count on y-axis
#     geom_density(alpha=.2, fill="#FF6666")

###--- Plot FED density distributions by year, with sample sizes - ALL YEARS ---###

#   ggplot(d3, aes(x=FEGMID)) + geom_density() + facet_wrap(~IYEA, ncol=10) + geom_text(data=samplesize, aes(x=max(d3$FEGMID)-50, y=0.04, label=paste("n =", samplesize$n)), colour="black", cex=5, inherit.aes=FALSE, parse=FALSE)
#   
#   setwd(paste(outputwd, "FED distributions by year all years", sep="/"))
#   ggsave(paste(spp, "FED distributions by year all years.jpg"), height=12, width=15, units="in")

#------------------- MORE DATASET CLEANING ----------------------------

###--- Exclude years which have < 10 nests ---###
spp.enoughnests <- samplesize[-which(samplesize$n <= 10),"IYEA"]
d4 <- subset(d3, grepl(paste(spp.enoughnests, collapse="|"), d3$IYEA))
d4 <- droplevels(d4)

###--- Exclude outlier nests (early and late) # may not need to do this depending on the method selected for calculating the FED metric


#---------- VISUALIZE DATASET - FEG DENSITY PLOTS per YEAR, ONLY YEARS with > 10 nests/year -----------

# density plots for FEG, each year individually, with sample sizes

samplesize <- ddply(.data=d4, .(IYEA), summarize, n=length(FEGMID))

###--- Plot FED density distributions by year, with sample sizes - YEARS with large N ---###

#   ggplot(d4, aes(x=FEGMID)) + geom_density() + facet_wrap(~IYEA, ncol=10) + geom_text(data=samplesize, aes(x=max(d3$FEGMID)-50, y=0.035, label=paste("n =", samplesize$n)), colour="black", cex=5, inherit.aes=FALSE, parse=FALSE)
#   
#   setwd(paste(outputwd, "FED distributions by year", sep="/"))
#   ggsave(paste(spp, "FED distributions by year.jpg"), height=12, width=15, units="in") 
#   
#---------- ADD ANY OTHER CLEANING VARIABLES -------------

d5 <- d4

bird.modeldata[[a]] <- data.frame(spp, d5)

}


#----------  SAVE DATASET OBJECT AS RDS FILES  ------------

names(bird.modeldata) <- specieslist

# remove the species with no data by re-assigning a NULL list element (will drop them from the list)
bird.modeldata[c("leswh","treec","wilti")] <- NULL

saveRDS(bird.modeldata, file=paste(workspacewd, "bird phenology processed raw data.rds", sep="/"))



