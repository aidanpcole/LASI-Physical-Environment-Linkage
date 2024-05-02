###############################################################################
# Purpose: Find PDS shops within LASI-DAD buffers/Mean Phys Env Measure by HHID 
# Written by: Hunter Green and Aidan Cole
# R version: 4.3.2
###############################################################################

# Options
rm(list = ls())
cat("\014")


# Load libraries
library(stars) #sf: Sys.setenv("PROJ_LIB"=""); https://github.com/r-spatial/sf/issues/2302
library(tidyverse)
library(terra)
library(sf)
library(splitstackshape)
library(geojsonsf)
library(lidaRtRee)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(kableExtra)
library(stargazer)
library(car)



#pacman::p_load(data.table, openxlsx, haven, readr, dplyr, magrittr, stringr,
#labelled, scales, gridExtra, grid, ggplot2, lubridate,
#simputation, VIM, psych, lavaan, patchwork)




mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}


###############################################################################
# Load LASI-DAD coordinates
###############################################################################


mumbai_coor <- read.csv("C:/Users/Aidan/Desktop/mumbai_phys.csv") %>%
  mutate(city = "Mumbai")
delhi_coor <- read.csv("C:/Users/Aidan/Desktop/delhi_filt_phys.csv") %>%
  mutate(city = "Delhi")
kolkata_coor <- read.csv("C:/Users/Aidan/Desktop/kolkata_phys.csv") %>%
  mutate(city = "Kolkata")
chennai_coor <- read.csv("C:/Users/Aidan/Desktop/chennai_plz.csv") %>%
  mutate(city = "Chennai")

#chennai_phys has all hhids within boundary even those that aren't supposed to be there

chennai_final <- chennai_coor %>% drop_na()

mumbai_delhi <- rbind(mumbai_coor,delhi_coor)
mumbai_delhi_kolkata <- rbind(mumbai_delhi,kolkata_coor)
gimme_all_those_cities <- rbind(mumbai_delhi_kolkata,chennai_final) 

# this is lasi_ind2_all, how different from lasi_data? 
# has ssuid and hhid 
resp_data <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/LASI Physical Environment Data Linkage/Data/lasi_respondent.csv") %>%
  select(matches("hhid|ssuid"))
# this is the interviewer observation dataset 
# has just hhid
int_data <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/LASI Physical Environment Data Linkage/Data/lasi_comm.csv") %>% 
  select(matches("r2obstrffc|r2obstree|r2obspdstn|r2obssdwlk|r2obsgrnsp|r2obsneigh|r2obsroad|r2obstrash|r2obsstore|r2obsabnhs|r2obsdmg|r2obsbldcstn|r2obsrdsp|hhid"))
# community survey 
# has just ssuid
comm_data <- read.csv("C:/Users/Aidan/Downloads/lasi_comm2.csv") %>% 
  select(matches("uc002|uc013|uc017b_d|ssuid|hhid|state"))
# metro cities data, probably won't use
#census_data <- read.csv("C:/Users/Aidan/Downloads/india_census.csv") %>%
#  select(matches("ucen|ssuid|hhid|state"))
# this is harmonized lasi
# has just hhid
lasi_data <- read.csv("c:/Users/Aidan/Downloads/lasi_data.csv") %>% 
  select(matches("hhid|ssuid|state|r1caste|hh1ctot|raeducl|radadeducl|ramomeducl|raliterate|r1mstat|raeducl"))

no2_data <- read.csv("C:/Users/Aidan/Downloads/lasi_exposome_imp_hh.csv") %>%
  mutate(pm25_vd_2020_sum = pm25_vd_2020_01 + pm25_vd_2020_02 + pm25_vd_2020_03 + pm25_vd_2020_04 + pm25_vd_2020_05 + pm25_vd_2020_06 + pm25_vd_2020_07 + pm25_vd_2020_08 + pm25_vd_2020_09 + pm25_vd_2020_10 + pm25_vd_2020_11 + pm25_vd_2020_12) %>%
  mutate(pm25_vd_2020 = mean(pm25_vd_2020_sum,na.rm=TRUE)) %>%
  dplyr::select(hhid,no2_gbd_2020_i,pm25_vd_2020)



# just for the means 
pm_means <- no2_data %>% 
  dplyr::select(hhid, pm25_vd_2020_01, pm25_vd_2020_02, pm25_vd_2020_03, pm25_vd_2020_04, pm25_vd_2020_05, pm25_vd_2020_06, pm25_vd_2020_07, pm25_vd_2020_08, pm25_vd_2020_09, pm25_vd_2020_10, pm25_vd_2020_11, pm25_vd_2020_12)

pm_means$pm25_vd_2020 <- rowMeans(subset(pm_means, select = c(pm25_vd_2020_01, pm25_vd_2020_02, pm25_vd_2020_03, pm25_vd_2020_04, pm25_vd_2020_05, pm25_vd_2020_06, pm25_vd_2020_07, pm25_vd_2020_08, pm25_vd_2020_09, pm25_vd_2020_10, pm25_vd_2020_11, pm25_vd_2020_12)), na.rm = TRUE) 


z$mean <- rowMeans(subset(z, select = c(x, y)), na.rm = TRUE)


# lasi_hh2_all data 
# has hhid and ssuid
#hhid_data <- read.csv("C:/Users/Aidan/Downloads/india_hhids.csv")

no2_mumbai <- st_read("C:/Users/Aidan/Desktop/dummygeoboy/no2_mumbai.geojson") %>%
  mutate(mean_no = grid_code) %>%
  dplyr::select(hhid,mean_no) 

no2_delhi <- st_read("C:/Users/Aidan/Desktop/dummygeoboy/delhi_no.geojson") %>%
  mutate(mean_no = grid_code) %>%
  dplyr::select(hhid,mean_no) 

no2_chennai <- st_read("C:/Users/Aidan/Desktop/dummygeoboy/no2_chennai.geojson") %>%
  mutate(mean_no = grid_code) %>%
  dplyr::select(hhid,mean_no) 

no2_kolkata <- st_read("C:/Users/Aidan/Desktop/dummygeoboy/no2_kolkata.geojson") %>%
  mutate(mean_no = grid_code) %>%
  dplyr::select(hhid,mean_no) 

no2_chennai$hhid <- as.numeric(no2_chennai$hhid)
no2_delhi$hhid <- as.numeric(no2_delhi$hhid)

mum_w_no <- mumbai_coor %>% inner_join(no2_mumbai)
del_w_no <- delhi_coor %>% inner_join(no2_delhi)
che_w_no <- chennai_coor %>% inner_join(no2_chennai)
kol_w_no <- kolkata_coor %>% inner_join(no2_kolkata)


# if want to combine all no2 to do regression across cities 
mum_del <- rbind(veh_mum,veh_del)
mum_del_kol <- rbind(mum_del,veh_kol)
gimme_all_those_no2 <- rbind(mum_del_kol,veh_che) 

# do regression for all four cities individually and then do one with all four cities 
# sum number of vehicles for each city, filter variables to just sum of vehicles and no2 quantile 

veh_mum <- mum_w_no %>%
  mutate(vehicles = trains+motorcycles+cars+buses+trucks) 

veh_del <- del_w_no %>%
  mutate(vehicles = trains+motorcycles+cars+buses+trucks) 

veh_che <- che_w_no %>%
  mutate(vehicles = trains+motorcycles+cars+buses+trucks) 

veh_kol <- kol_w_no %>%
  mutate(vehicles = trains+motorcycles+cars+buses+trucks) 


# join env to H LASI (hhid to hhid)
lasi_cities <- gimme_z_cent_cities %>% inner_join(lasi_data) 
# join with interviewer observations (hhid to hhid)
lasi_int_cities <- lasi_cities %>% inner_join(int_data)
# join with ssuid (hhid to hhid and ssuid)
lasi_ssuids <- lasi_int_cities %>% inner_join(resp_data)
# join with community survey (ssuid to ssuid)
cities_hh_ss <- lasi_ssuids %>% inner_join(comm_data)

# JUST WANT HH
int_phys <- gimme_z_cent_cities %>% inner_join(int_data)
lasi_int_phys <- int_phys %>% inner_join(lasi_data)


no_phys <- gimme_z_cent_cities %>% inner_join(no2_data)
pm_phys <- gimme_z_cent_cities %>% inner_join(pm_means)




resp_cities <- gimme_all_those_cities %>% inner_join(resp_data)
int_cities <- gimme_all_those_cities %>% inner_join(int_data)
resp_comm_cities <- resp_cities %>% inner_join(comm_data, by='ssuid')

resp_int_comm_cities <- resp_comm_cities %>% inner_join(int_data, by ='hhid')

census_resp_comm_cities <- resp_comm_cities %>% inner_join(census_data, by='ssuid')

lasi_resp_int_comm_cities <- resp_int_comm_cities %>% inner_join(lasi_data) 

final_merge <- lasi_census_resp_comm_cities %>% inner_join(hhid_data)



filt_comm_mum <- mumbai_comm %>% 
  dplyr::select(sidewalk_per,people,r2obssdwlk,r2obspdstn) %>%
  mutate(sidewalk_condition = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obssdwlk)),
         foot_traffic = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obspdstn))) %>%
  drop_na()

filt_comm_del <- delhi_comm %>% 
  dplyr::select(sidewalk_per,people,r2obssdwlk,r2obspdstn) %>%
  mutate(sidewalk_condition = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obssdwlk)),
         foot_traffic = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obspdstn))) %>%
  drop_na()

filt_comm_kol <- kolkata_comm %>% 
  dplyr::select(sidewalk_per,people,r2obssdwlk,r2obspdstn) %>%
  mutate(sidewalk_condition = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obssdwlk)),
         foot_traffic = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obspdstn))) %>%
  drop_na()

filt_comm_che <- chennai_comm %>% 
  dplyr::select(sidewalk_per,people,r2obssdwlk,r2obspdstn) %>%
  mutate(sidewalk_condition = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obssdwlk)),
         foot_traffic = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obspdstn))) %>%
  drop_na()

filt_comm_mum <- mumbai_comm %>% 
  dplyr::select(gvi_per,r2obstree) %>%
  mutate(tree_observation = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obstree))) %>%
  drop_na()

filt_comm_del <- delhi_comm %>% 
  dplyr::select(gvi_per,r2obstree) %>%
  mutate(tree_observation = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obstree))) %>%
  drop_na()

filt_comm_kol <- kolkata_comm %>% 
  dplyr::select(gvi_per,r2obstree) %>%
  mutate(tree_observation = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obstree))) %>%
  drop_na()

filt_comm_che <- chennai_comm %>% 
  dplyr::select(gvi_per,r2obstree) %>%
  mutate(tree_observation = as.numeric(gsub("\\D*(\\d+).*", "\\1",r2obstree))) %>%
  drop_na()

final_mumbai <- filt_comm_mum %>%
  dplyr::select(sidewalk_per,people,sidewalk_condition,foot_traffic)

final_delhi <- filt_comm_del %>%
  dplyr::select(sidewalk_per,people,sidewalk_condition,foot_traffic)

final_kolkata <- filt_comm_kol %>%
  dplyr::select(sidewalk_per,people,sidewalk_condition,foot_traffic)

final_chennai <- filt_comm_che %>%
  dplyr::select(sidewalk_per,people,sidewalk_condition,foot_traffic)

ggcorrplot(
  round(cor(final_chennai), 1), 
  p.mat = cor_pmat(final_chennai),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 



model <- lm(people_gt ~ sidewalk_condition + foot_traffic, data = filt_comm_che)


model <- lm(vehicles ~ no_quant, data = veh_del)



for_maps <- gimme_all_those_cities %>%
  mutate(per_na = num_na/num_ints)


boxplot(x ~ gimme_all_those_cities$city, gimme_all_those_cities$cars_gt,
        col = c("#BEF7FF", "#a0dcff", "#82c2ff", "#458CFF"))


vehicles <- gimme_all_those_no2$vehicles
city <- gimme_all_those_no2$city
boxplot(vehicles ~ city, col = c("#BEF7FF", "#a0dcff", "#82c2ff", "#458CFF"))




mumbai_final <- mumbai_coor

hist(mumbai_final$img_ins, 
     main = "Mean Image Instance", 
     xlab = "Mean Image Instance")

hist(mumbai_final$img_seg, 
     main = "Mean Image Segmentation", 
     xlab = "Mean Image Segmentation")


delhi_final <- delhi_coor

hist(delhi_final$img_ins, 
     main = "Mean Image Instance", 
     xlab = "Mean Image Instance")

hist(delhi_final$img_seg, 
     main = "Mean Image Segmentation", 
     xlab = "Mean Image Segmentation")


kolkata_final <- kolkata_coor

hist(kolkata_final$img_ins, 
     main = "Mean Image Instance", 
     xlab = "Mean Image Instance")

hist(kolkata_final$img_seg, 
     main = "Mean Image Segmentation", 
     xlab = "Mean Image Segmentation")

chennai_final <- chennai_coor %>%
  drop_na()

hist(chennai_final$img_ins, 
     main = "Mean Image Instance", 
     xlab = "Mean Image Instance")

hist(chennai_final$img_seg, 
     main = "Mean Image Segmentation", 
     xlab = "Mean Image Segmentation")



gimme_all_those_cities2 <- resp_comm_cities




#### Center around the mean for percentage variables
# variables: gvi_per, sidewalk_per, building_per, sky_per, road_per, wall_per, fence_per
gimme_centered_cities <- gimme_all_those_no2 %>%
  mutate(cent_gvi = gimme_all_those_no2$gvi_per - mean(gimme_all_those_no2$gvi_per),
         cent_sidewalk = gimme_all_those_no2$sidewalk_per - mean(gimme_all_those_no2$sidewalk_per),
         cent_building = gimme_all_those_no2$building_per - mean(gimme_all_those_no2$building_per),
         cent_sky = gimme_all_those_no2$sky_per - mean(gimme_all_those_no2$sky_per),
         cent_road = gimme_all_those_no2$road_per - mean(gimme_all_those_no2$road_per),
         cent_wall = gimme_all_those_no2$wall_per - mean(gimme_all_those_no2$wall_per),
         cent_fence = gimme_all_those_no2$fence_per - mean(gimme_all_those_no2$fence_per))

gimme_centered_cities <- gimme_all_those_cities %>%
  mutate(cent_gvi = gvi_per - mean(gvi_per,na.rm=TRUE),
         cent_sidewalk = sidewalk_per - mean(sidewalk_per,na.rm=TRUE),
         cent_building = building_per - mean(building_per,na.rm=TRUE),
         cent_sky = sky_per - mean(sky_per,na.rm=TRUE),
         cent_road = road_per - mean(road_per,na.rm=TRUE),
         cent_wall = wall_per - mean(wall_per,na.rm=TRUE),
         cent_fence = fence_per - mean(fence_per,na.rm=TRUE))

#### Create Z-scores for count variables
# variables: people, riders, people_gt, trains, motorcycles, bikes, cars, buses, trucks, cars_gt
gimme_z_cent_cities <- gimme_centered_cities %>%
  mutate(cent_people = people - mean(people,na.rm=TRUE),
         cent_riders = riders - mean(riders,na.rm=TRUE),
         cent_people_gt = people_gt - mean(people_gt,na.rm=TRUE),
         cent_trains = trains - mean(trains,na.rm=TRUE),
         cent_motorcycles = motorcycles - mean(motorcycles,na.rm=TRUE),
         cent_bikes = bikes - mean(bikes,na.rm=TRUE),
         cent_cars = cars - mean(cars,na.rm=TRUE), 
         cent_buses = buses - mean(buses,na.rm=TRUE),
         cent_trucks = trucks - mean(trucks,na.rm=TRUE),
         cent_cars_gt = cars_gt - mean(cars_gt,na.rm=TRUE)) %>%
  mutate(z_people = cent_people/sd(people,na.rm=TRUE),
         z_riders = cent_riders/sd(riders,na.rm=TRUE),
         z_people_gt = cent_people_gt/sd(people_gt,na.rm=TRUE),
         z_trains = cent_trains/sd(trains,na.rm=TRUE),
         z_motorcycles = cent_motorcycles/sd(motorcycles,na.rm=TRUE),
         z_bikes = cent_bikes/sd(bikes,na.rm=TRUE),
         z_cars = cent_cars/sd(cars,na.rm=TRUE),
         z_buses = cent_buses/sd(buses,na.rm=TRUE),
         z_trucks = cent_trucks/sd(buses,na.rm=TRUE),
         z_cars_gt = cent_cars_gt/sd(cars_gt,na.rm=TRUE))



### computed mean centering and z score for all studies, can filter to just one study and export to csv again 

gimme_z_cent <- gimme_z_cent_cities %>%
  mutate(z_vehicles = z_trains+z_motorcycles+z_cars+z_buses+z_trucks)

filt_cits <- gimme_z_cent %>%
  dplyr::select(hhid,vehicles,no_quant,city) 


del_gimme <- gimme_all_those_no2 %>%
  filter(city == 'Mumbai')

kableExtra::kable(x = broom::tidy(model), format = "pipe")


model <- lm(no_quant ~ vehicles, data = del_gimme)




## Using out = 'Lm_1.doc' will export the results to a word doc.

stargazer(model, type='text', digits = 2, title = 'Mumbai: Mean NO2 vs Sum of Vehicles', style = 'qje', out = 'model.doc')

no_reg <- pm_phys %>% 
  mutate(vehicles = trains+motorcycles+cars+buses+trucks,
         pm25 = pm25_vd_2020)



mumbai_gimme <- no_reg %>%
  filter(city == "Mumbai")

mumbai_model <- lm(vehicles ~ pm25, data = mumbai_gimme)


delhi_gimme <- no_reg %>%
  filter(city == "Delhi")

delhi_model <- lm(vehicles ~ pm25, data = delhi_gimme)


kolkata_gimme <- no_reg %>%
  filter(city == "Kolkata")

kolkata_model <- lm(vehicles ~ pm25, data = kolkata_gimme)

chennai_gimme <- no_reg %>%
  filter(city == "Chennai")

chennai_model <- lm(vehicles ~ pm25, data = chennai_gimme)



comm_cities8 <- lasi_int_phys %>%
  mutate(buildings = building_per,
         obs_stores = r2obsstore,
         obs_construction = r2obsbldcstn)




comm_cities7 <- lasi_int_phys %>%
  mutate(per_na = num_na/num_ints,
         con_quant = factor(ntile(hh1ctot,5)),
         education = factor(raeducl))

comm_cities6 <- resp_int_comm_cities %>%
  filter(uc002 < 998) %>%
  mutate(sidewalk_yn = uc002,
         sidewalk_con = as.factor(r2obssdwlk)) 


comm_cities5 <- lasi_int_phys %>%
  mutate(pedestrians = people,
         foot_traffic = factor(r2obspdstn))

# used to be in mutate parks = uc017b_d
comm_cities4 <- lasi_int_phys %>% 
#  filter(uc017b_d <= 30) %>%
  mutate(obs_greenspace = r2obsgrnsp, 
         trees = r2obstree, 
         greenspace = gvi_per)

# int_light = factor(uc013)
comm_cities3 <- lasi_int_phys %>% 
  mutate(obs_light = r2obs)

comm_cities2 <- comm_cities %>%
  mutate(vehicles = trains+motorcycles+cars+buses+trucks,
         veh_volume = factor(r2obstrffc)) 




mumbai_gimme <- comm_cities8 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(buildings ~ obs_stores + obs_construction, data = mumbai_gimme)


delhi_gimme <- comm_cities8 %>%
  filter(city == "Delhi")

delhi_model <- lm(buildings ~ obs_stores + obs_construction, data = delhi_gimme)


kolkata_gimme <- comm_cities8 %>%
  filter(city == "Kolkata")

kolkata_model <- lm(buildings ~ obs_stores + obs_construction, data = kolkata_gimme)

chennai_gimme <- comm_cities8 %>%
  filter(city == "Chennai")

chennai_model <- lm(buildings ~ obs_stores + obs_construction, data = chennai_gimme)











mumbai_gimme <- comm_cities7 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(per_na ~ r1caste + con_quant + raliterate + ramomeducl + radadeducl + married, data = mumbai_gimme)



mumbai_gimme <- comm_cities7 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(per_na ~ education, data = mumbai_gimme)

delhi_gimme <- comm_cities7 %>%
  filter(city == "Delhi")

delhi_model <- lm(per_na ~ education, data = delhi_gimme)


kolkata_gimme <- comm_cities7 %>%
  filter(city == "Kolkata")

kolkata_model <- lm(per_na ~ education, data = kolkata_gimme)

chennai_gimme <- comm_cities7 %>%
  filter(city == "Chennai")

chennai_model <- lm(per_na ~ education, data = chennai_gimme)








mumbai_gimme <- comm_cities6 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(sidewalk_per ~ sidewalk_yn, data = mumbai_gimme)


delhi_gimme <- comm_cities6 %>%
  filter(city == "Delhi")

delhi_model <- lm(sidewalk_per ~ sidewalk_yn, data = delhi_gimme)


kolkata_gimme <- comm_cities6 %>%
  filter(city == "Kolkata")

kolkata_model <- lm(sidewalk_per ~ sidewalk_yn, data = kolkata_gimme)

chennai_gimme <- comm_cities6 %>%
  filter(city == "Chennai")

chennai_model <- lm(sidewalk_per ~ sidewalk_yn, data = chennai_gimme)


mumbai_gimme <- comm_cities5 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(pedestrians ~ foot_traffic, data = mumbai_gimme)


delhi_gimme <- comm_cities5 %>%
  filter(city == "Delhi")

delhi_model <- lm(pedestrians ~ foot_traffic, data = delhi_gimme)


kolkata_gimme <- comm_cities5 %>%
  filter(city == "Kolkata")

kolkata_model <- lm(pedestrians ~ foot_traffic, data = kolkata_gimme)

chennai_gimme <- comm_cities5 %>%
  filter(city == "Chennai")

chennai_model <- lm(pedestrians ~ foot_traffic, data = chennai_gimme)








mumbai_gimme <- comm_cities4 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(greenspace ~ trees, data = mumbai_gimme)


delhi_gimme <- comm_cities4 %>%
  filter(city == "Delhi")

delhi_model <- lm(greenspace ~ trees, data = delhi_gimme)


kolkata_gimme <- comm_cities4 %>%
  filter(city == "Kolkata")

kolkata_model <- lm(greenspace ~ trees, data = kolkata_gimme)

chennai_gimme <- comm_cities4 %>%
  filter(city == "Chennai")

chennai_model <- lm(greenspace ~ trees, data = chennai_gimme)






mumbai_gimme <- comm_cities3 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(light ~ int_light, data = mumbai_gimme)


delhi_gimme <- comm_cities3 %>%
  filter(city == "Delhi")

delhi_model <- lm(light ~ int_light, data = delhi_gimme)


kolkata_gimme <- comm_cities3 %>%
  filter(city == "Kolkata")

kolkata_model <- lm(light ~ int_light, data = kolkata_gimme)

chennai_gimme <- comm_cities3 %>%
  filter(city == "Chennai")

chennai_model <- lm(light ~ int_light, data = chennai_gimme)





mumbai_gimme <- comm_cities2 %>%
  filter(city == "Mumbai")

mumbai_model <- lm(vehicles ~ veh_volume, data = mumbai_gimme)


delhi_gimme <- comm_cities2 %>%
  filter(city == "Delhi")

delhi_model <- lm(vehicles ~ veh_volume, data = delhi_gimme)


kolkata_gimme <- comm_cities2 %>%
  filter(city == "Kolkata")

kolkata_model <- lm(vehicles ~ veh_volume, data = kolkata_gimme)

chennai_gimme <- comm_cities2 %>%
  filter(city == "Chennai")

chennai_model <- lm(vehicles ~ veh_volume, data = chennai_gimme)


stargazer(mumbai_model, delhi_model, kolkata_model, chennai_model, type='text', digits = 2, title = 'Cross-City Regression: Sum of Vehicles vs PM2.5 level', style = 'qje', column.labels = c('Mumbai', 'Delhi', 'Kolkata', 'Chennai'), out = 'c:/Users/Aidan/Desktop/veh_pm_reg.html')


## Stargazer 
## Combined output 
na.rm=TRUE


hist(mumbai_gimme$vehicles, 
     main = "Sum Vehicles, Mumbai", 
     xlab = "Sum Vehicles")


hist(delhi_gimme$vehicles, 
     main = "Sum Vehicles, Delhi", 
     xlab = "Sum Vehicles")

hist(kolkata_gimme$vehicles, 
     main = "Sum Vehicles, Kolkata", 
     xlab = "Sum Vehicles")

hist(chennai_gimme$vehicles, 
     main = "Sum Vehicles, Chennai", 
     xlab = "Sum Vehicles")


hist(mumbai_gimme$mean_no, 
     main = "Mean NO2, Mumbai", 
     xlab = "Mean NO2")

hist(delhi_gimme$mean_no, 
     main = "Mean NO2, Delhi", 
     xlab = "Mean NO2")

hist(kolkata_gimme$mean_no, 
     main = "Mean NO2, Kolkata", 
     xlab = "Mean NO2")

hist(chennai_gimme$mean_no, 
     main = "Mean NO2, Chennai", 
     xlab = "Mean NO2")


out = 'gimme.doc/html/latex'


scatterplot(num_na ~ cars, data = gimme_all_those_cities)





##########################################################################
### Author: Emma Nichols
### Date: 05/04/2023
### Project: IQCODE cross-national comparisons
### Purpose: Associations with informant characteristics
##########################################################################

rm(list = ls())

pacman::p_load(data.table, openxlsx, haven, readr, dplyr, magrittr, stringr,
               labelled, scales, gridExtra, grid, ggplot2, lubridate,
               simputation, VIM, psych, lavaan, patchwork)
date <- gsub("-", "_", Sys.Date())
set.seed(6541)

# SET OBJECTS -------------------------------------------------------------

dropbox_dir <- "C:/Users/emmanich/P2AGING Dropbox/Emma Nichols/"
dir <- paste0(dropbox_dir, "projects/iqcode_informants/")
harmonized_dir <- paste0(dropbox_dir, "Harmonized Data Files/")
elsahcap_dir <- paste0(dropbox_dir, "ELSA_HCAP/ToUpload/")
rawelsa_dir <- paste0(dropbox_dir, "H_ELSA/raw/Waves 0-9 - 31st ed/stata/stata13_se/")
rawdata_dir <- paste0(dir, "data/source/")
derived_dir <- paste0(dir, "data/derived/")
plot_dir <- paste0(dir, "paper/regressions/")
appendix_dir <- paste0(dir, "paper/appendix_figs/")

# GET DATA ----------------------------------------------------------------

full_data <- read_rds(paste0(derived_dir, "processed_data.rds"))

varlabel_dt <- data.table(var = c("relation_informant", "generation_informant", "caregiver_informant", "female_informant", "years_informant10", "freqcontact_informant"),
                          label = c("Relationship", "Generation", "Caregiver", "Women (informant)", "Years known (10)",  "Contact frequency"))

covlabel_dt <- data.table(covariate_set = paste0("covset", 0:2), 
                          covariate_label = c("No adjustment", "Demographics", "Demographics + cognition"))

studylabel_dt <- data.table(city = c("mumbai", "delhi", "kolkata", "chennai"),
                            city_label = c("Mumbai", "Delhi", "Kolkata", "Chennai"))

# SUBSET TO ONLY INFORMANT INTERVIEWS -------------------------------------

full_data <- full_data[informant == 1]

# RUN REGRESSIONS ---------------------------------------------------------

## add category to study
full_data[data == "lasidad", data := case_when(educ_isced == "No formal education" ~ "lasidad_noeduc",
                                               !educ_isced == "No formal education" ~ "lasidad_educ")]

## rescale age
full_data[, years_informant10 := years_informant/10]

covset0 <- c()
covset1 <- c("age", "female", "educ_collapsed")
covset2 <- c(covset1, "fgcp")

param_dt <- as.data.table(expand.grid(data = c("lasidad_educ", "lasidad_noeduc", "hrshcap", "elsahcap"), 
                                      var = c("relation_informant", "generation_informant", "caregiver_informant", "years_informant10", "freqcontact_informant"),
                                      adjustment = c("covset0", "covset1", "covset2")))
param_dt[, names(param_dt) := lapply(.SD, as.character), .SDcols = names(param_dt)]

create_formula <- function(exposure, covariates){
  if (is.null(covariates)){
    as.formula(paste0("iqcode_fullscore ~ ", exposure))
  } else {
    as.formula(paste0("iqcode_fullscore ~ ", exposure, " + ", paste(covariates, collapse = " + "))) 
  }
}

run_models <- function(row){
  print(row)
  
  ## get params
  e <- param_dt[, as.character(var)][row]
  c <- param_dt[, as.character(adjustment)][row]
  d <- param_dt[, as.character(data)][row]
  covs <- get(c)
  if (d == "lasidad_noeduc") covs <- covs[!covs == "educ_collapsed"]
  model_dt <- copy(full_data[data == d])
  
  ## run model
  model <- lm(create_formula(e,covs), data = model_dt)
  params <- parameters::model_parameters(model)
  
  ## save results
  param_select <- grepl(e, params$Parameter)
  result_dt <- data.table(exposure = e, covariate_set = c, city = d,
                          mean = params$Coefficient[param_select], lower = params$CI_low[param_select], upper = params$CI_high[param_select],
                          p = params$p[param_select])
  if (sum(param_select) > 1) result_dt[, category := params$Parameter[param_select]] else
    result_dt[, category := NA]
  
  return(result_dt)
}

result_dt <- rbindlist(lapply(1:nrow(param_dt), run_models))

# VISUALIZE RESULTS -------------------------------------------------------

graph_dt <- copy(result_dt)
graph_dt <- merge(graph_dt, varlabel_dt, by.x = "exposure", by.y = "var", sort = F)
graph_dt[, label := ifelse(is.na(category), label, paste0(label, "\n(", str_replace(category, exposure, ""), ")"))]
graph_dt <- merge(graph_dt, covlabel_dt, by = "covariate_set")
graph_dt <- merge(graph_dt, citylabel_dt, by = "city")
graph_dt[, label := factor(label, levels = graph_dt[city == "Delhi", unique(label)])]
graph_dt[, city_label := factor(city_label, levels = c("Kolkata", "Chennai", "Delhi", "Mumbai"))]
graph_dt[, covariate_label := factor(covariate_label, levels = c("No adjustment", "Demographics", "Demographics + cognition"))]

regression_gg <- ggplot(graph_dt, aes(x = label, y = mean, ymin = lower, ymax = upper, color = city_label)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(width = 0.1, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(name = "", values = c("#16324a", "#c96552", "#0CABA8", "#015958")) +
  scale_y_continuous(limits = c(-0.3,0.5), oob = oob_keep) +
  facet_wrap(~covariate_label, ncol = 1) +
  labs(x = "", y = "Difference in IQCODE score") +
  theme_bw() +
  theme(legend.position = "bottom", legend.margin = margin(-10,3,3,3, unit = "mm"), axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(plot_dir, "regression_results_", date, ".pdf"), plot = regression_gg, height = 9, width = 9)














