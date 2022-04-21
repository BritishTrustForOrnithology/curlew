# Colour-ring requirements Brecks 2022

list_of_packages <- c("tidyverse")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, library, character.only=TRUE)

dt <- read.csv(file.path("data","Colour ring master list - Curlew.csv"), stringsAsFactors = FALSE)
dt <- dt %>% 
  rename(allocation = U.Used..A.Allocated..N.Not.used..D.DO.NOT.USE) %>% 
  rename(group = Priority.group.for.making)

dt_sub <- dt %>% 
  filter(allocation == "A - BRECKS") %>% 
  filter(group %in% c(1,2))


colour_list <- list()

# LA1
colour_list[[1]] <- dt_sub %>% 
  group_by(allocation, LA1) %>% 
  tally() %>% 
  rename(colour = LA1)


# LB1
colour_list[[3]] <- dt_sub %>%
  group_by(allocation, LB1) %>% 
  tally() %>% 
  rename(colour = LB1)

# LB2
colour_list[[4]] <- dt_sub %>%
  group_by(allocation, LB2) %>% 
  tally() %>% 
  rename(colour = LB2)

# RA1
colour_list[[5]] <- dt_sub %>%
  group_by(allocation, RA1) %>% 
  tally() %>% 
  rename(colour = RA1)

# RA2
colour_list[[6]] <- dt_sub %>%
  group_by(allocation, RA2) %>% 
  tally() %>% 
  rename(colour = RA2)

colour_list_order <- colour_list %>% 
  do.call(rbind,.) %>% 
  group_by(colour) %>% 
  summarise(sum_colour = sum(n)) %>% 
  data.frame

colour_list_order

# in stock
# Yellow 20
# White 20
# Green 10
# Black 10
# Blue 15
# Orange 15

in_stock <- data.frame(
  colour = c("Y","W","G","N","B","O","R"),
  in_stock = c(10,21,11,11,16,16,12)
)

colour_list_order <- merge(colour_list_order, in_stock, on = "colour", all.x = TRUE) %>% 
  mutate(in_stock = ifelse(is.na(in_stock), 0, in_stock)) %>% 
  mutate(to_order = sum_colour - in_stock) %>% 
  mutate(to_order = ifelse(to_order < 0, 0, to_order))

colour_list_order

  
  
  
  

