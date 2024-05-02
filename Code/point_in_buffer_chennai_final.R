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
# Load LASI coordinates
lasi_coor <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/H_LASI/Raw/Pollution/Environmental_Data/Data/Processed/LatLong_Clean/lasi_latlong_final.csv",  colClasses = c("hhid"="character"))
lasi_coor2 <- subset(lasi_coor, stateid %in% c("33.Tamil Nadu"))
lasi_coor3 <- lasi_coor %>%
  filter(lasi_coor$hhid == "128243301300200" | lasi_coor$hhid == "128243400680100" | lasi_coor$hhid == "128243400080200" | lasi_coor$hhid == "128243401020100" | lasi_coor$hhid == "128243400060100" | lasi_coor$hhid == "128243400340100" | lasi_coor$hhid == "128243400170100" | lasi_coor$hhid == "128243400090100" | lasi_coor$hhid == "128243401410100" | lasi_coor$hhid == "128243400590100" | lasi_coor$hhid == "128243301750300" | lasi_coor$hhid == "128243400490100" | lasi_coor$hhid == "128243401500100")
# need to get the ~20 households that aren't in Tamil Nadu but within the extent of Chennai data
lasi_coor_final <- rbind(lasi_coor2,lasi_coor3)
lasi_coor_sf <- st_as_sf(lasi_coor2, coords = c("long_final", "lat_final"), crs = "WGS84")
buffer <- st_buffer(lasi_coor_sf, dist = 500)

chennai_map_points <- st_buffer(chennai_map, dist = 1000)

###############################################################################
# Load PDS shop coordinates
###############################################################################
# Load PDS shop information
physenv <- st_read("C:/Users/Aidan/Desktop/chennai-combined-objects.geojson") %>%
  mutate(na_boy = ifelse((image_exist_instance=="No" | is.na(image_exist_instance)) & (image_exist_segment == "No" | is.na(image_exist_segment)),1,0))
  st_drop_geometry() %>%
  mutate(osmid = as.numeric(osmid))

physenv$center_point <- str_remove(physenv$center_point, pattern = "^POINT")
physenv$center_point <- gsub("[()]","",physenv$center_point)

physenv <- cSplit(physenv, "center_point", " ")
physenv_sf <- st_as_sf(physenv, coords = c("center_point_1", "center_point_2"), crs = "WGS84")%>%
  mutate(na_lala = ifelse((image_exist_instance=="No" | is.na(image_exist_instance)) & (image_exist_segment == "No" | is.na(image_exist_segment)),1,0))

chennai_map_no_na <- chennai_map %>%
  drop_na()

ggplot() +
  geom_sf(data=chennai_map_no_na,aes(fill=per_na))+
  scale_fill_gradient(low = "white", high = "red") +
  labs(title="Household buffer by percent NA", 
       subtitle="Chennai, India") +
  mapTheme()



###############################################################################
# Create buffers around LASI-DAD coordinates, identify shops within buffer,
#   summarize shop info
###############################################################################
join <- st_join(buffer, physenv) %>%
  group_by(hhid) %>%
  mutate(n = n()) 

join2 <- join %>%
  mutate(image_exist_inst = ifelse(image_exist_instance=="Yes",1,0),
         image_exist_seg = ifelse(image_exist_segment=="Yes",1,0),
         pole_exist = ifelse(pole_existence==1,1,0),
         traffic_light = ifelse(traffic_light_existence==1,1,0),
         traffic_sign = ifelse(traffic_sign_existence==1,1,0))


join_na <- join2 %>%
  dplyr::select(hhid,person,rider,train,truck,motorcycle,bus,car,bicycle,car_gt,person_gt,gvi,sidewalk_percent,building_percent,sky_percent,road_idx,wall_percent,fence_percent,image_exist_inst,image_exist_seg,pole_exist,traffic_light,traffic_sign,n) %>%
  mutate(na_boy = ifelse((image_exist_seg == 0 | is.na(image_exist_seg)) & (image_exist_inst == 0 | is.na(image_exist_inst)),1,0))


join3 <- join_na %>%
  summarise(num_ints = mean(n,na.rm=TRUE),
            people = mean(person,na.rm=TRUE),
            riders = mean(rider,na.rm=TRUE),
            people_gt = mean(person_gt,na.rm=TRUE),
            trains = mean(train,na.rm=TRUE),
            motorcycles = mean(motorcycle,na.rm=TRUE),
            bikes = mean(bicycle,na.rm=TRUE),
            cars = mean(car,na.rm=TRUE),
            buses = mean(bus,na.rm=TRUE),
            trucks = mean(truck,na.rm=TRUE),
            cars_gt = mean(car_gt,na.rm=TRUE),
            gvi_per = mean(gvi,na.rm=TRUE),
            sidewalk_per = mean(sidewalk_percent,na.rm=TRUE),
            building_per = mean(building_percent,na.rm=TRUE),
            sky_per = mean(sky_percent,na.rm=TRUE),
            road_per = mean(road_idx,na.rm=TRUE),
            wall_per = mean(wall_percent,na.rm=TRUE),
            fence_per = mean(fence_percent,na.rm=TRUE),
            img_ins = mean(image_exist_inst,na.rm=TRUE),
            img_seg = mean(image_exist_seg,na.rm=TRUE),
            pole_ex = mean(pole_exist,na.rm=TRUE),
            light_ex = mean(traffic_light,na.rm=TRUE),
            sign_ex = mean(traffic_sign,na.rm=TRUE),
            num_na = sum(na_boy))


new_DF <- join3[rowSums(is.na(join3)) > 0,] 

join4 <- join3

join4$is_na <- as.numeric(join4$hhid %in% new_DF$hhid)

join5 <- join4 %>%
  filter(is_na == 0) %>%
  dplyr::select(-is_na)

chennai_buffers <- join5
st_write(chennai_buffers,"C:/Users/Aidan/Desktop/dummygeoboy/chennai_buffer.geojson")


missed_list <- st_read("C:/Users/Aidan/Desktop/dummygeoboy/missing_hhids_chennai.geojson")

x <- missed_list$hhid

join6 <- join5 

missed_list$is_in <- as.numeric(missed_list$hhid %in% join6$hhid)


miss_coor <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/H_LASI/Raw/Pollution/Environmental_Data/Data/Processed/LatLong_Clean/lasi_latlong_final.csv",  colClasses = c("hhid"="character")) %>%
  filter(hhid==133214602860100)
miss_coor_sf <- st_as_sf(miss_coor, coords = c("long_final", "lat_final"), crs = "WGS84")
miss_buffer <- st_buffer(miss_coor_sf, dist = 500) %>%
  dplyr::select(hhid,geometry)

join5[nrow(join5) + 1,] <- list(as.character(133214600030100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214600550100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214600560100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214601070100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214601210100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

join5[nrow(join5) + 1,] <- list(as.character(133214602020100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214602410100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214602530200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214602730100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214602860100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

miss_coor <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/H_LASI/Raw/Pollution/Environmental_Data/Data/Processed/LatLong_Clean/lasi_latlong_final.csv",  colClasses = c("hhid"="character")) %>%
  filter(hhid==133214901660200)
miss_coor_sf <- st_as_sf(miss_coor, coords = c("long_final", "lat_final"), crs = "WGS84")
miss_buffer <- st_buffer(miss_coor_sf, dist = 500) %>%
  dplyr::select(hhid,geometry)

join5[nrow(join5) + 1,] <- list(as.character(133214603250100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214700860200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214700980100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214701200100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214800070100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

join5[nrow(join5) + 1,] <- list(as.character(133214800120100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214800740200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214801010100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214801100100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214901660200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

miss_coor <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/H_LASI/Raw/Pollution/Environmental_Data/Data/Processed/LatLong_Clean/lasi_latlong_final.csv",  colClasses = c("hhid"="character")) %>%
  filter(hhid==133219000260100)
miss_coor_sf <- st_as_sf(miss_coor, coords = c("long_final", "lat_final"), crs = "WGS84")
miss_buffer <- st_buffer(miss_coor_sf, dist = 500) %>%
  dplyr::select(hhid,geometry)

join5[nrow(join5) + 1,] <- list(as.character(133214902530100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214902690100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133214902820200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000080100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000110100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

join5[nrow(join5) + 1,] <- list(as.character(133219000180100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000210300),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000220500),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000240100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000260100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

miss_coor <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/H_LASI/Raw/Pollution/Environmental_Data/Data/Processed/LatLong_Clean/lasi_latlong_final.csv",  colClasses = c("hhid"="character")) %>%
  filter(hhid==133224000210100)
miss_coor_sf <- st_as_sf(miss_coor, coords = c("long_final", "lat_final"), crs = "WGS84")
miss_buffer <- st_buffer(miss_coor_sf, dist = 500) %>%
  dplyr::select(hhid,geometry)

join5[nrow(join5) + 1,] <- list(as.character(133219000510100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000630200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000680100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000830200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219000880100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

join5[nrow(join5) + 1,] <- list(as.character(133219000930200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219001040100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133219001130600),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133222700090100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133224000210100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

miss_coor <- read.csv("C:/Users/Aidan/P2AGING Dropbox/Aidan Cole/H_LASI/Raw/Pollution/Environmental_Data/Data/Processed/LatLong_Clean/lasi_latlong_final.csv",  colClasses = c("hhid"="character")) %>%
  filter(hhid==133226100101000)
miss_coor_sf <- st_as_sf(miss_coor, coords = c("long_final", "lat_final"), crs = "WGS84")
miss_buffer <- st_buffer(miss_coor_sf, dist = 500) %>%
  dplyr::select(hhid,geometry)

join5[nrow(join5) + 1,] <- list(as.character(128243301300200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243400680100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243400080200),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243401020100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243400060100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

join5[nrow(join5) + 1,] <- list(as.character(128243400340100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243400170100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243400090100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243401410100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243400590100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)

join5[nrow(join5) + 1,] <- list(as.character(128243301750300),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243400490100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(128243401500100),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)
join5[nrow(join5) + 1,] <- list(as.character(133226100101000),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,miss_buffer$geometry)


st_write(chennai_quarts, "C:/Users/Aidan/Desktop/dummygeoboy/chennai_quart.geojson")

PM25_QUANT <- PM25_CENS %>%
  mutate("2010 PM 2.5 Quantile (7)" = ntile(PM25_CENS$PM2010,7))
 
chennai_quarts <- chennai_map %>%
  mutate(quart_na = ntile(chennai_map$per_na,4))
         
chennai_map <- join5 %>%
  mutate(per_na = num_na/num_ints)

chennai_ppppppp <- join5 %>%
  st_drop_geometry()

write.csv(chennai_ppppppp, "C:/Users/Aidan/Desktop/chennai_plz.csv")

write.csv(join5, "C:/Users/Aidan/Desktop/chennai_w_phys.csv")


### graphs and figures 
chennai_final <- join5 
hist(chennai_final$num_ints)

chennai_final %>%
  ggplot(aes(x=num_ints)) + 
  geom_density(color = "red", fill = alpha("red",0.3)) + 
  theme_minimal() + 
  ggtitle("Distribution of Mean Intersections by HHID Buffer") + 
  ylab("Density")


y=mumbai_final$people_gt
x=rep(1,580) # arbitrarily, set ALL x=1
plot(x, y)

hist(chennai_final$num_na)
hist(chennai_final$people_gt)
hist(chennai_final$cars_gt)
hist(chennai_final$gvi_per)
hist(chennai_final$img_ins)
hist(chennai_final$img_seg)
hist(chennai_final$road_per)


chennai.sf <- chennai_final%>%
  mutate(hhid = as.numeric(chennai_final$hhid)) %>%
  dplyr::select(people,riders,trains,motorcycles,bikes,cars,buses,trucks,gvi_per,sidewalk_per,building_per,sky_per,road_per,wall_per,fence_per,pole_ex,light_ex,sign_ex,geometry)

numericVars <-
  select_if(st_drop_geometry(chennai.sf), is.numeric) %>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 


