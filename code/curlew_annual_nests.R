#######################################################################################
#
#   Project: Eurasian African Bird Migration Atlas
#   BTO project code: IAR01

#   Description: Data inspection and error correction

#   Name: Samantha Franks
#   Start date: 10/01/2019
#   Notable update dates:
#   Manuscript title (if applicable): n/a
#   Publication details (if applicable): n/a
#
#######################################################################################


# ======================   Variables to pass to setup code source  ===========

# Header code to install and load packages, set directory paths, set seed, capture session info

# project_details <- list(project_name, output_version_name, workspace_version_name)
# package_details <- c("package name 1", "package name 2")

project_details <- list(project_name="curlew")
package_details <- c("tidyverse","data.table","pryr")
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

dt <- fread(file.path(datawd, "curlew_nrs_totals_2019.csv")) %>% setnames(names(.), c("year", "N"))


# ---------  dv curlew file  ---------

dvfile <- fread(file.path(datawd, "readme dv file.csv")) # derived from X:\Nest Records Unit\NRS\4 NRS output file spec\2 DV and ND file formats column start and end read in points

dt_dv <- read.fwf(file.path(datawd, "curlenew.dv"), widths=(dvfile$End-dvfile$Start+1), col.names=dvfile$Variable) %>% data.table # read in fixed width Fortran output file


#============================ Clean data ===================================


#-------   1. Generate FEG  &  & table of -------

#   FEGDIF=MAXFEG-MINFEG;
#   FEGMID=MINFEG + (FEGDIF/2);
#   FEGMID=ROUND(FEGMID,1);
#   IF IERROR NE 0 THEN DELETE;
#   IF FEGDIF > 10 THEN DELETE;

dt_dv$FEGDIF <- dt_dv$MAXFEG - dt_dv$MINFEG
dt_dv$FEGMID <- round(dt_dv$MINFEG + dt_dv$FEGDIF/2)

sum_nests_pre_2000 <- dt_dv[,.N, IYEA] %>% setnames(names(.), c("year", "N")) %>% .[order(year),] %>% .[year < 2000,]

#============================ Plot pre- and post-2000 data  ===================================

dt_all <- rbind(sum_nests_pre_2000, dt)

if (!dir.exists(file.path(outputwd, "figures"))) dir.create(file.path(outputwd, "figures"))

ggplot(dt_all, aes(year, N)) + 
  geom_point(colour="cyan4", size=3) + 
  geom_line(colour="cyan4") + 
  geom_vline(xintercept=2001, linetype=2, colour="grey20") +
  labs(x = "Year", y = "Total no. of nests", title="Number of curlew nests submitted to the BTO Nest Record Scheme per year") +
  scale_x_continuous(breaks = seq(1940, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 150, 20)) +
  theme_bw()
ggsave(file.path(outputwd, "figures", "curlew_nrs_annual_totals_2018.png"), device="png", width=25, height=12, units="cm")

