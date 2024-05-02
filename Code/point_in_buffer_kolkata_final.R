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
lasi_coor <- subset(lasi_coor, stateid %in% c("19.West Bengal"))
lasi_coor_sf <- st_as_sf(lasi_coor, coords = c("long_final", "lat_final"), crs = "WGS84")
buffer <- st_buffer(lasi_coor_sf, dist = 500)

###############################################################################
# Load PDS shop coordinates
###############################################################################
# Load PDS shop information
physenv <- st_read("C:/Users/Aidan/Desktop/kolkata-combined-objects.geojson") %>%
  st_drop_geometry()

physenv$center_point <- str_remove(physenv$center_point, pattern = "^POINT")
physenv$center_point <- gsub("[()]","",physenv$center_point)

physenv <- cSplit(physenv, "center_point", " ")
physenv_sf <- st_as_sf(physenv, coords = c("center_point_1", "center_point_2"), crs = "WGS84")%>%
  mutate(na_lala = ifelse((image_exist_instance=="No" | is.na(image_exist_instance)) & (image_exist_segment == "No" | is.na(image_exist_segment)),1,0),
         na_boy = ifelse((image_exist_instance=="No" | is.na(image_exist_instance)) & (image_exist_segment == "Yes"),1,0))




ggplot() +
  geom_sf(data=st_union(tracts)) +
  geom_sf(data=kolkata_map,aes(fill=per_na),alpha=0.07)+
  scale_fill_gradient(low = "white", high = "red") +
  labs(title="Household buffer by percent NA", 
       subtitle="Kolkata, India") +
  mapTheme()


###############################################################################
# Create buffers around LASI-DAD coordinates, identify shops within buffer,
#   summarize shop info
###############################################################################
# Output is "long" by buffer (hhid), one row for each intersection
join <- st_join(buffer, physenv_sf) %>%
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
  mutate(na_boy = ifelse((image_exist_inst == 0 | is.na(image_exist_inst)) & (image_exist_seg == 0 | is.na(image_exist_seg)),1,0))


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

st_write(kolkata_quarts, "C:/Users/Aidan/Desktop/dummygeoboy/kolkata_quarts.geojson")

kolkata_quarts <- join5 %>%
  mutate(per_na = num_na/num_ints,
         quart_na = ntile(per_na,4))

kolkata_map <- join5 %>%
  mutate(per_na = num_na/num_ints)

kolkata_pppp <- join5 %>%
  st_drop_geometry()

write.csv(kolkata_pppp, "C:/Users/Aidan/Desktop/kolkata_phys.csv")

write.csv(join5, "C:/Users/Aidan/Desktop/kolkata_w_phys.csv")

### graphs and figures 
kolkata_final <- join5
hist(kolkata_final$num_ints)

kolkata_final %>%
  ggplot(aes(x=num_ints)) + 
  geom_density(color = "red", fill = alpha("red",0.3)) + 
  theme_minimal() + 
  ggtitle("Distribution of Mean Intersections by HHID Buffer") + 
  ylab("Density")


y=kolkata_final$people_gt
x=rep(1,748) # arbitrarily, set ALL x=1
plot(x, y)

hist(kolkata_final$num_na)
hist(kolkata_final$people_gt)
hist(kolkata_final$cars_gt)
hist(kolkata_final$gvi_per)
hist(kolkata_final$img_ins)
hist(kolkata_final$img_seg)
hist(kolkata_final$road_per)


kolkata.sf <- kolkata_final%>%
  mutate(hhid = as.numeric(kolkata_final$hhid)) %>%
  dplyr::select(people,riders,trains,motorcycles,bikes,cars,buses,trucks,gvi_per,sidewalk_per,building_per,sky_per,road_per,wall_per,fence_per,pole_ex,light_ex,sign_ex,geometry)

numericVars <-
  select_if(st_drop_geometry(kolkata.sf), is.numeric) %>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 



