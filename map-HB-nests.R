
library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)

## load in grid data

grid_main <- read_sf("basemap/HB Mid Grid.GPX") 
# %>% st_transform(28356) # change EPSG if needed
# save ranges of coordinates
x.range <- max(grid_main[,1]) - min(grid_main[,1])
y.range <- max(m[,2]) - min(m[,2])

# order by greatest range
if (x.range > y.range) {
  sort.id <- order(m[,1])
} else if (y.range > x.range) {
  sort.id <- order(m[,2])
} else if (y.range == x.range) {
  sort.id <- order(m[,2])
}

# creat lines by previous sorting and save them in the list
lines <- lapply(1:(length(sort.id)-1), function(i) {
  st_linestring(rbind(multipoints[sort.id[i],], multipoints[sort.id[i+1],]))
})

# plot results
plot(multipoints)
plot(lines[[1]], col = "orange", lwd = 2, add = TRUE)
plot(lines[[2]], col = "green", lwd = 2, add = TRUE)

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
  geom_sf(data = grid_main)  

  # grid
  # geom_sf(data = grid) + 

  # # nests
  # geom_sf(data = data, 
  #         aes(shape = status), 
  #         show.legend = "point"
  #         ) #+ 
  # 
  # # zoom to nest points
  # coord_sf(
  #   xlim = c(st_bbox(nests)[1] - 20, st_bbox(nests)[3] + 20), 
  #   ylim = c(st_bbox(nests)[2] - 20, st_bbox(nests)[4] + 20)
  # )

## save the map

ggsave("statusMap.png", 
       width = 1024/171, height = 600/171, dpi = 600)
