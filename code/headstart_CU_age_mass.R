##############################
#
#    NE103: Headstarted Curlew age vs biometrics
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


today_date <- format(Sys.Date(), "%d-%b-%Y")

# Load data
# dt <- read.csv(file.path("headstart_CU_age_mass.csv"), header = TRUE, stringsAsFactors = FALSE)
dt <- read.csv(file.path("data", "headstart_curlew_2021_biometrics_20210928.csv"), header = TRUE, stringsAsFactors = FALSE) %>% filter(flag_id != "2T")
dt <- dt %>% 
  mutate(cohort_num = as.factor(cohort_num))


# =======================    Figures - 2021 Pensthorpe birds   =================


# 2021 data from Pensthorpe
curve_age_mass <- ggplot(data = dt) +
  geom_point(aes(y = weight, x = age, colour = cohort_num)) +
  geom_smooth(aes(y = weight, x = age, fill = cohort_num, colour = cohort_num)) +
  labs(x = "Age", y = "Weight (g)", title = "Curve of weight vs age per cohort group")

ggsave(paste0("age_weight_per_cohort_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")

# Wing vs age
curve_age_wing <- ggplot(data = dt) +
  geom_point(aes(y = wing, x = age, colour = cohort_num)) +
  geom_smooth(aes(y = wing, x = age, fill = cohort_num, colour = cohort_num)) +
  labs(x = "Age", y = "Wing length (mm)", title = "Curve of wing length vs age per cohort group")

ggsave(paste0("age_wing_per_cohort_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")


# Weight vs wing
curve_weight_wing <- ggplot(data = dt) +
  geom_point(aes(y = weight, x = wing, colour = cohort_num)) +
  geom_smooth(aes(y = weight, x = wing, fill = cohort_num, colour = cohort_num)) +
  labs(x = "Wing length (mm)", y = "Weight (g)", title = "Curve of weight vs wing length per cohort group")

ggsave(paste0("weight_wing_per_cohort_", today_date, ".png"), device = "png", path = outputwd, width = 30, height = 20, units = "cm")


# =======================    Figures - 2019 WWT birds  =================


# Plot age vs weight
curve_age_mass <- ggplot(data = dt) +
  geom_point(aes(y = last_mass, x = age_last_mass)) +
  geom_smooth(aes(y = last_mass, x = age_last_mass)) +
  labs(x = "Age at last mass", y = "Lass mass (g)", title = "Curve of last mass vs age at last mass")


age_violin <- ggplot() +
  geom_violin(data = dt,
              aes(y = age_released,
                  x = "Released")) +
  geom_jitter(data = dt,
              aes(y = age_released,
                  x = "Released"),
              height = 0, width = 0.1) +
  geom_violin(data = dt,
              aes(y = age_last_mass,
                  x = "Last mass")) +
  geom_jitter(data = dt,
              aes(y = age_last_mass,
                  x = "Last mass"),
              height = 0, width = 0.1) +
  labs(x = "", y = "Age", title = "Age of all birds when mass last measured, and on release")

mass_violin <- ggplot(data = dt %>% filter(age_last_mass >= 43),
                      aes(y = last_mass,
                          x = "")) +
  geom_violin() +
  geom_jitter(height = 0, width = 0.1) +
  theme(axis.ticks = element_blank()) +
  labs(x = "", y = "Last mass (g)", title = "Mass of all birds measured when >= 43 days old\n(minimum age when birds released)")

# Put plots together and save
((curve_age_mass / age_violin) | mass_violin) +
  plot_annotation(tag_levels = 'A')

ggsave("age_mass_plots.png", device = "png", path = outputwd, width = 30, height = 20, units = "cm")

# Calculate descriptive stats
dt %>% filter(age_last_mass >= 43) %>% summarise(mean(last_mass))
dt %>% filter(age_last_mass >= 43) %>% summarise(range(last_mass))
dt %>% summarise(range(age_last_mass))


  
