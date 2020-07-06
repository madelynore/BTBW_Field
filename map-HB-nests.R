
library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)

## load in road data

road <- read_sf(file.path("basemap","hbef_roads")) # %>% st_transform(28356) # change EPSG if needed


## load in GPS points for nests

nests <- 
  file.path("gpx") %>% 
  # list files in directory
  list.files(".*gpx$", full.names = T) %>% 
  # read each file in
  lapply(read_sf, "waypoints") %>% 
  # turn list of data frames into one data frame
  do.call(rbind, .) # %>% st_transform(28356) # change EPSG if needed

  # # add lat/lon coordinates for use in labeling plots
  # mutate(
  #   lon = st_coordinates(st_transform(., 28356))[,1], 
  #   lat = st_coordinates(st_transform(., 28356))[,2]
  # )

## load in nest status data

status <- 
  "status-tracker.txt" %>% 
  read.delim(stringsAsFactors = F) %>% 
  # format dates as dates
  mutate_at("date", ymd) %>% 
  # select the most recent entry
  group_by(nestID) %>% 
  slice(which.max(date)) %>% 
  ungroup

## unite nest location and status data

data <- 
  nests %>%  
  left_join(status, by = c("name" = "nestID"))

## map 

ggplot() + 
  
  # map formatting
  theme_void() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    legend.position = "right"
  ) + 
  
  # roads
  geom_sf(data = road) + 

  # grid
  # geom_sf(data = grid) + 

  # nests
  geom_sf(data = data, 
          aes(shape = status), 
          show.legend = "point"
          ) #+ 
  
  # # zoom to nest points
  # coord_sf(
  #   xlim = c(st_bbox(nests)[1] - 20, st_bbox(nests)[3] + 20), 
  #   ylim = c(st_bbox(nests)[2] - 20, st_bbox(nests)[4] + 20)
  # )

## save the map

ggsave("statusMap.png", 
       width = 1024/171, height = 600/171, dpi = 600)
