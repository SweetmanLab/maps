# Tutorial on making a simple publication quality map in R
# 09/30/2024
# Jon Sweetman, jfs6745@psu.edu
# based on https://bookdown.org/brianwood1/QDASS/simple-static-maps.html

#Load libraries
library(sf) #'simple features' package
library(ggplot2) # general purpose plotting
library(rnaturalearth) # map data
library(rnaturalearthdata)# map data
library(ggspatial) # scale bars and north arrow

theme_set(theme_bw())

world <- ne_countries(scale="medium", returnclass = "sf")
nepal <- ne_countries(country="nepal", type="countries", scale='large', returnclass = "sf")

#  A map of the world
ggplot(data = world) +
  geom_sf()

# A map of Nepal
ggplot(data = nepal) +
  geom_sf()

# adding titles and axis labels
ggplot(data = nepal) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Nepal")

#adding scale bar/north arrow
ggplot(data = nepal) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Nepal") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

# adding data 

city_data <- data.frame(city_name=c("Kathmandu", "Pokhara", "Birāṭnagar", "Tokha"))
city_data$lat <- c(27.7100, 28.2083, 26.4542, 27.2592)
city_data$lon <- c(85.3200, 83.9889, 87.2797,  85.3283)

ggplot(data = nepal) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Populations investigated in Nepal") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = city_data, mapping = aes(x = lon, y = lat), colour = "red") +
  geom_text(data = city_data, mapping=aes(x=lon, y=lat, label=city_name), nudge_y = 0.2, color="darkblue")
  


# Part 2: plotting data from a section of the US (Pennsylvania)
usa_states <- ne_states(country = "United States of America", returnclass = "sf")

state_points <- data.frame(state = usa_states$name,
                           st_coordinates(st_centroid(usa_states$geometry)))

ggplot(data = usa_states) + 
  geom_sf(fill= "gray") + 
  annotation_scale(location = "tr", width_hint = 0.5) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
    xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("USA all states") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue")) 

ggplot(data = usa_states) + 
  geom_sf(fill= "gray") + 
  annotation_scale(location = "bl", width_hint = 0.5) + 
  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-128, -60), ylim = c(22, 50), expand = FALSE) + #zoom in on a portion of the map
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Lower 48 states") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue")) 


ggplot(data = usa_states) + 
  geom_sf(fill= "gray") + 
  annotation_scale(location = "tr", width_hint = 0.5) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-81.5, -73.5), ylim = c(39, 43), expand = FALSE) + #zoom in on a portion of the map
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Pennsylvania") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue")) 
 
# Load Data
Ponddata <- read_csv("c://Rdata/MWard_Ponddata_2024.csv", col_names = TRUE)

ggplot(data = usa_states) + 
  geom_sf(fill= "gray") + 
  annotate(geom = "text", x = -77, y = 41.5, label = "Study Sites", fontface = "italic", color = "blue", size = 6) +
  annotation_scale(location = "tr", width_hint = 0.5) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-81.5, -73.5), ylim = c(39, 43), expand = FALSE) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Pennsylvania") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue")) +
  geom_point(data = Ponddata, mapping = aes(x=as.numeric(longitude), y=as.numeric(latitude), color = Ponddata$state_forest)) 
  

# zooming in on a section using coord_sf further
ggplot(data = usa_states) + 
  geom_sf(fill= "gray") + 
  annotation_scale(location = "tr", width_hint = 0.5) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-78.5, -76), ylim = c(40, 41.5), expand = FALSE) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Pennsylvania") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue")) +
  geom_point(data = Ponddata, mapping = aes(x=as.numeric(longitude), y=as.numeric(latitude), color = Ponddata$state_forest)) 

# Load a shapefile of state forests
forests <- sf::st_read("maps/forests/DCNR_BOF_StateForests202406.shp") #import shapefile of ecoregions (download/unzip first)
st_crs(forests) # displays info on coordinate reference system
st_bbox(forests) # shows bounding box of shape file

# Plot the shapefile
ggplot(data = forests) +
  geom_sf()

ggplot(data = usa_states) + 
  geom_sf(fill= "gray") + 
  geom_sf(data = forests) +   #adds state forest shapefile
  annotation_scale(location = "br", width_hint = 0.5) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-78.5, -76), ylim = c(40, 41.5), expand = FALSE) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Study Sites, Central PA") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue")) +
  geom_point(data = Ponddata, mapping = aes(x=as.numeric(longitude), y=as.numeric(latitude), color = Ponddata$state_forest)) + 
  theme(legend.title = element_blank()) 

# add some points
PAcities <- data.frame(cityname = c("State College", "Huntingdon"))
PAcities$lat <- c(40.7934, 40.4848)
PAcities$long <- c(-77.8600,-78.0103)

ggplot(data = usa_states) + 
  geom_sf(fill= "gray") + 
  geom_sf(data = forests) +   #adds state forest shapefile
  annotation_scale(location = "br", width_hint = 0.4) + 
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-78.5, -76), ylim = c(40, 41.5), expand = FALSE) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  ggtitle("Study Sites, Central PA") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "lightblue")) +
  geom_point(data = Ponddata, mapping = aes(x=as.numeric(longitude), y=as.numeric(latitude), color = Ponddata$state_forest)) + 
  geom_point(data = PAcities, mapping = aes(x = long, y = lat, color = "red")) +
  geom_text (data = PAcities, mapping = aes(x = long, y = lat, label = cityname), nudge_y = 0.05, color="darkblue") +
  theme(legend.position = "none")
