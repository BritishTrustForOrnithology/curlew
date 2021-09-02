##############################
#
#    NE103: WWT headstarted Curlew age vs mass
#
##############################

library(tidyverse)
library(patchwork)


# sexing results
df <- data.frame(sex = c("F", "M", "U"), totals = c(27+43, 20+35, 3+1))
df <- rbind(c(27+43), c(20+35))
dimnames(df) <- list(sex = c("F","M"), totals = c("observed"))
xsq <- chisq.test(df)


# Set working directory
# setwd("~/Dropbox/BTO-Projects/Curlew and waders/NE103/Methods & fieldwork/permissions")

# Load data
# dt <- read.csv(file.path("headstart_CU_age_mass.csv"), header = TRUE, stringsAsFactors = FALSE)
dt <- read.csv(file.path("data/headstart_curlew_2021_biometrics.csv"), header = TRUE, stringsAsFactors = FALSE) %>% filter(flag_id != "2T")
dt <- dt %>% 
  mutate(cohort_num = as.factor(cohort_num))

# 2021 data from Pensthorpe
curve_age_mass <- ggplot(data = dt) +
  geom_point(aes(y = weight, x = age, colour = cohort_num)) +
  geom_smooth(aes(y = weight, x = age, fill = cohort_num, colour = cohort_num), span = 1.2) +
  geom_text(aes(y = weight, x = age, label = flag_id)) +
  labs(x = "Age", y = "Weight (g)", title = "Curve of weight vs age per cohort group")

ggsave("output/figures/age_weight_per_cohort_with-smooth_with-flags.png", device = "png", width = 30, height = 20, units = "cm")

curve_age_mass <- ggplot(data = dt) +
  geom_point(aes(y = weight, x = age, colour = cohort_num)) +
  # geom_smooth(aes(y = weight, x = age, fill = cohort_num, colour = cohort_num)) +
  labs(x = "Age", y = "Weight (g)", title = "Curve of weight vs age per cohort group")

ggsave("output/figures/age_weight_per_cohort_no-smooth.png", device = "png", width = 30, height = 20, units = "cm")

# Wing vs age
curve_age_wing <- ggplot(data = dt) +
  geom_point(aes(y = wing, x = age, colour = cohort_num)) +
  geom_smooth(aes(y = wing, x = age, fill = cohort_num, colour = cohort_num), span = 1.2) +
  labs(x = "Age", y = "Wing length (mm)", title = "Curve of wing length vs age per cohort group")

ggsave("output/figures/age_wing_per_cohort_with-smooth.png", device = "png", width = 30, height = 20, units = "cm")


# Weight vs wing
curve_weight_wing <- ggplot(data = dt) +
  geom_point(aes(y = weight, x = wing, colour = cohort_num)) +
  geom_smooth(aes(y = weight, x = wing, fill = cohort_num, colour = cohort_num), span = 1.2) +
  labs(x = "Wing length (mm)", y = "Weight (g)", title = "Curve of weight vs wing length per cohort group")

ggsave("output/figures/weight_wing_per_cohort_with-smooth.png", device = "png", width = 30, height = 20, units = "cm")


#####################################
#####################################
#####################################


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

ggsave("age_mass_plots.png", device = "png", width = 30, height = 20, units = "cm")

# Calculate descriptive stats
dt %>% filter(age_last_mass >= 43) %>% summarise(mean(last_mass))
dt %>% filter(age_last_mass >= 43) %>% summarise(range(last_mass))
dt %>% summarise(range(age_last_mass))


  
