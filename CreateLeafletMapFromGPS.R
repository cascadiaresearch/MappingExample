#### Mapping Example ####
# Author: James Fahlbusch
# Date: 6/22/2020
# Cascadia Research
# Goldbogen Lab, Stanford University
#
# Description: This script imports generic GPS location data, plots and saves a map of locations with
# bathymetry, and creates and saves an interactive Leaflet Map
#
# NOTE: This script assumes that the data being imported has the following columns (see example csv file):
# "DateTimeUTC" - UTC Datetime in the format MM/DD/YYYY HH:MM:SS
# "Lat" - Latitude in decimal degrees
# "Long" - Longitude in decimal degrees
#
# Output: PNG plot of locations, HTML Leaflet Map of locations

## You should be able to run every line in this file, select the appropriate csv file with locations in it, and it will save the maps.

#### Load Packages ####
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
# Load Packages
pkgTest("tidyverse")
pkgTest("ggplot2")
pkgTest("lubridate")
pkgTest("leaflet")
pkgTest("htmlwidgets")
pkgTest("R.matlab")
pkgTest("rstudioapi")
pkgTest("rnaturalearth")
pkgTest("marmap")
pkgTest("metR")

# Set the Working Directory to the path of source file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)

#### Global Variables ####
# Set the Timezone of the data (GMT+7 for summertime in CA and WA)
tzOffset <- "Etc/GMT+7" 

#### Import GPS Data ####
#  select the SDA filter CSV file
filename <- file.choose()
setwd(dirname(filename)) # set the wd so files are saved with the deployment data
#Extract tag type and deployment id
depid <- basename(filename)
depid <- unlist(strsplit(depid,"[-]"))
depid <- paste(depid[[1]][1],depid[[2]][1],sep="-")
depid

GPS_data <- read_csv(filename)

# select only columns with datetime, lat, long, hit type, and comment
GPS_data <- GPS_data[,c("DateTimeUTC","Lat","Long")]
# change column names to more practical shorter names
colnames(GPS_data)[1:3] <- c("dt","lat","long")
# Create proper datetime objects (convert GMT to local)
dt <- as.POSIXct(GPS_data$dt, format="%m/%d/%Y %H:%M:%S", tz='GMT')
head(dt)
attr(dt, "tzone") # check that dt is in GMT
GPS_data$dt <- dt 
rm(dt) # clean up
# create a column with local time
GPS_data$dttz <- with_tz(GPS_data$dt, tzOffset)
attr(GPS_data$dttz, "tzone") 
# simple plot to see if the data looks ok
plot(GPS_data$long,GPS_data$lat)

#### Create a Map ####
world <- ne_countries(scale = "large", returnclass = "sf")
# Download Bathymetry data
lon1 = floor(min(GPS_data$long+2,na.rm=TRUE))
lon2 = ceiling(max(GPS_data$long-2,na.rm=TRUE)) 
lat1 = ceiling(max(GPS_data$lat+2,na.rm=TRUE))
lat2 = floor(min(GPS_data$lat-2,na.rm=TRUE))
b = getNOAA.bathy(lon1, lon2, lat1, lat2, resolution = 4)
rm(lat1,lat2,lon1,lon2)
# convert bathymetry to data frame
bf = fortify.bathy(b)

# Adjust this for short deployments in constrained area:
degBuffer <- .5 # Number degree buffer around plot

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(min(GPS_data$long,na.rm = TRUE)-degBuffer, max(GPS_data$long,na.rm = TRUE)+degBuffer), 
           ylim = c(min(GPS_data$lat,na.rm = TRUE)-degBuffer, max(GPS_data$lat,na.rm = TRUE)+degBuffer), expand = FALSE) +
  # add 200m contour
  geom_contour(data = bf,
               aes(x=x, y=y, z=z),
               #breaks=c(-100,-200,-300,-400),
               breaks=c(-200),
               size=c(0.4),
               colour="darkgrey", show.legend = FALSE) +
  #geom_text_contour(data = bf, aes(x=x, y=y,z = z),breaks=c(-100,-200,-300,-400),
  geom_text_contour(data = bf, aes(x=x, y=y,z = z),breaks=c(-200),
                    show.legend = FALSE, size = 2.2, alpha = .6, nudge_y = -.002) +
  geom_point(data=GPS_data,aes(x=as.numeric(as.character(long)),
                              y=as.numeric(as.character(lat))),
             alpha = 0.5, color= 'blue',size = 1) +
  geom_path(data=GPS_data,aes(x=as.numeric(as.character(long)),
                             y=as.numeric(as.character(lat))),
            alpha = 0.5, color= 'blue') +
  scale_alpha(guide = 'none') + 
  # annotation_scale(location = "bl", width_hint = 0.5) + 
  ggtitle(paste0(depid," GPS Track")) +
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(axis.title = element_text(face="bold", size=20),
        axis.text = element_text(face="bold", size=16),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = NULL)


ggsave(sprintf("%s_Map.png",depid), plot = last_plot(), device = "png",
       scale = 2, width = 7, height = 5, units = "in", dpi = 600, limitsize = F)




#### Create Leaflet Map #### 

# initiate the leaflet instance and store it to a variable
m = leaflet() %>%
  # Base groups
  addProviderTiles(providers$Esri.OceanBasemap, group = "Oceans (default)") %>%  
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  addPolylines(
    lng = GPS_data$long, 
    lat = GPS_data$lat,
    weight = 1.5,
    group = "GPS Track"
  ) %>% 
  addCircleMarkers( 
    lng = GPS_data$long, 
    lat = GPS_data$lat,
    popup = GPS_data$dttz, 
    radius = 2, 
    color = "black",
    stroke = FALSE, 
    fillOpacity = 0.75,
    #clusterOptions = markerClusterOptions(),
    group = "GPS Positions"
  )  %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Oceans (default)", "Toner", "OSM", "Toner Lite"),
    overlayGroups = c("GPS Positions","GPS Track"),
    options = layersControlOptions(collapsed = F),
    position = "topright"
  ) %>% 
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE) 
# we can "run"/compile the map, by running the printing it
m
# Save it!
saveWidget(m, file=paste0(depid,"-LeafletMap.html"))


