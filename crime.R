#libraries
library(sp) # for crs and sptransform
library(ggplot2)
library(dplyr) # for joins
library(tmap)
library(geoR) # for jitter
library(rgeos) #for gintersection
library(spatstat) # for ppp
library(osmar) # for node and ways
library(rgdal) # read shapefiles
library(maptools) # for as.owin


# load necessary data
load("boundaries/LondonLSOA")  # boundaries from: https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
load("boundaries/LondonWards")
projection <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
LondonWards <- spTransform(LondonWards, projection)
LondonLSOA <- spTransform(LondonLSOA, projection)
load("df.Rda") # cleaned crime df, original data from: https://data.police.uk/data/

# remove crime reports with no location from df
df <- df[!is.na(df$Longitude),]
df <- df[!is.na(df$Latitude),]

# plot barplot of crime type
par(mar=c(8, 4.1, 4.1, 2.1))
ylim <- c(0, 1.5*max(table(df$Crime.type)))
barplot <- barplot(table(df$Crime.type),las=2, ylab="Frequency", cex.names=0.6, cex.axis=0.6, cex.lab=0.6, col = rainbow(length(unique(df$Crime.type))), ylim = ylim)
text(x = barplot, y = table(df$Crime.type), label = table(df$Crime.type), pos = 3, cex = 0.6, col = "black")

# plot line chart crime x month
crime_count_by_month <-data.frame(table(df$Month, df$Crime.type)) 
names(crime_count_by_month) <- c("Month", "Crime.type","Freq")
crime_count_by_month$Month <- gsub("2017-", "", crime_count_by_month$Month)
ggplot(crime_count_by_month, aes(x = Month, y = Freq, colour = Crime.type, group=Crime.type), main="Crime Count")+
  geom_line()+
  geom_point()+
  scale_color_discrete(name = "Crime types")+
  labs(y = "Crime count") + 
  scale_x_discrete(labels = abbreviate)

# get stats
LSOA_stats <- matrix(c(1.0, 64.0, 127.0, 175.3, 211.0, 7329.0), ncol=1, byrow=TRUE)
rownames(LSOA_stats) <- c("Minimum","1st Quartile","Median","Mean","3rd Quartile","Maximum")
colnames(LSOA_stats) <- c("Value")
LSOA_stats

# get crime count by LSOA
LondonLSOA_transformed <- LondonLSOA
crimenum_by_LSOA <- data.frame(table(df$LSOA.name)) %>% na.omit()
crimenum_by_LSOA$Var1 <- as.character(crimenum_by_LSOA$Var1)
crimenum_by_LSOA <- crimenum_by_LSOA[!apply(crimenum_by_LSOA, 1, function(x) any(x=="")),] 

# get combined boundary + LSOA crime count data
LondonLSOA_transformed@data <- left_join(LondonLSOA_transformed@data, crimenum_by_LSOA, by=c("LSOA11NM"="Var1"))
LondonLSOA_transformed[is.na(LondonLSOA_transformed$Freq)]<- 0 #set Freq = 0 for LSOAs with no crimes


# get violence and sexual offences only
vs_df <- subset(df, Crime.type == 'Violence and sexual offences')
vs_df <- vs_df[!is.na(vs_df$Longitude),]
vs_df <- vs_df[!is.na(vs_df$Latitude),]
vs_LSOA <- LondonLSOA
vs_num_by_LSOA <- data.frame(table(vs_df$LSOA.name)) %>% na.omit()
vs_num_by_LSOA$Var1 <- as.character(vs_num_by_LSOA$Var1)
vs_num_by_LSOA <- vs_num_by_LSOA[!apply(vs_num_by_LSOA, 1, function(x) any(x=="")),]
vs_LSOA@data<- left_join(vs_LSOA@data, vs_num_by_LSOA, by=c("LSOA11NM"="Var1"))

# get census data
load("lsoa_census.Rda")
vs_census <-  vs_LSOA
vs_census@data <- left_join(vs_census@data, lsoa_census, by=c("LSOA11CD" = "Codes"))
vs_census$total_pop <- vs_census$X0.15 + vs_census$X16.29 + vs_census$X30.44 + vs_census$X45.64 + vs_census$X65.
# get VSO crime rate per 1000 people
vs_census$freq_pop <-  vs_census$Freq / vs_census$total_pop * 1000

# plot map
tmap_mode("view")
tm_shape(vs_census) +
  tm_polygons(col="Freq", palette="Blues", style = "jenks", border.col='transparent', title = "Crime counts / 1000 people", n=4) +
  tm_layout("2017 London Crime Count Map by LSOA", title.size=1) 


# get top 20 LSOAs
top_20 <-  data.frame(vs_census@data[order(-vs_census$freq_pop)[1:20], "Names"])
top_20$freq_pop <- vs_census@data[order(-vs_census$freq_pop)[1:20], "freq_pop"]
colnames(top_20) <- c('LSOA name','Crime Rate')
top_20

# get data for City of London 001F
COL001F_shp <- vs_LSOA[vs_LSOA$LSOA11NM == 'City of London 001F',]
COL001F_roads <- readOGR('boundaries/COL001F_roads/COL001F_roads.shp')
COL001F_ll <- vs_df[vs_df$LSOA.name == 'City of London 001F',]
COL001F_ll <- subset(COL001F_ll, select=c("Longitude", "Latitude"))

# crime locations are aggregated to certain fixed points to provide anonymity 
# so multiple crimes are recorded to have the same coordinates 
# Jitter them so that each crime has its own coordinates
unique_COL001F_ll <- unique(COL001F_ll)
COL001F_ll <- jitterDupCoords(COL001F_ll, max = 0.0001)
points <- SpatialPoints(COL001F_ll)
unique_points <- SpatialPoints(unique_COL001F_ll)

# set crs
wgs <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
proj4string(points) <- CRS(wgs)
proj4string(unique_points) <- CRS(wgs)
proj4string(COL001F_roads) <- CRS(wgs)

# get only roads and points within the LSOA boundary
roads <- gIntersection(COL001F_shp, COL001F_roads)
points <- gIntersection(COL001F_shp, points)
unique_points <- gIntersection(COL001F_shp, unique_points)

# manipulate data into suitable forms for buffering
for (i in 1:nrow(unique_points@coords)){
  rownames(unique_points@coords)[i] <- i
}
for (i in 1:nrow(points@coords)){
  rownames(points@coords)[i] <- i
}


# H1. VSO crimes are randomly distributed in the City of London 001F LSOA    

# plot map of COL001F
tmap_mode("plot")
tm_shape(roads)+
  tm_lines(col= 'grey', lwd=1.0)+ #
  tm_layout(legend.bg.color="white",
            title="City of London 001F Map",
            title.position = c("left", "top")) +
  tm_shape(unique_points)+
  tm_dots(size = 0.1, col = 'red', alpha = 1)

# Get ppp object
points_ll <-  data.frame(points@coords)
coordinates(points_ll) <- ~x+y
pts <- coordinates(points_ll)
COL001F_Owin <- as.owin(COL001F_shp)
COL001F_ppp <- ppp(pts[,1],pts[,2], window=COL001F_Owin)

# To test for H1, we employ the use of quadrat counting. If the spatial pattern is homogeneous, each quadrat should have roughly the same amount of intensity (points per area).

# test for homogeneity 
qX <- quadratcount(COL001F_ppp, nx = 8 , ny = 4)
COL001F_ppp <- as.ppp(COL001F_ppp)
plot(COL001F_ppp, pch=16, main = "VSO Quadrat Counts")
plot(qX, add = TRUE, lty=2, cex = 0, col = 'red') 

qt <- quadrat.test(COL001F_ppp, nx = 8, ny = 4)
qt

COL001F_den <- density(COL001F_ppp)
plot(COL001F_den)

# H2: The location of VSO crimes correlate with the location of entertainment outlets.    

# get COL001F osm data
COL001F_nodes_ways <- readRDS(file = "COL001F_nodes_ways.rds")

# create POIs
POIs_df <- data.frame()
POIs_category <- c('restaurant','pub', 'bar')
for (i in 1:length(POIs_category)){
  id <- find(COL001F_nodes_ways, node(tags(v == POIs_category[i])))
  poi <- subset(COL001F_nodes_ways, id)
  poi <- as_sp(poi, "points")
  poi <- spTransform(poi, wgs)
  poi <- gIntersection(poi, COL001F_shp)
  POIs_df <- rbind(POIs_df,poi@coords)
}
colnames(POIs_df) <- c('Longitude','Latitude')
POIs <-  SpatialPoints(POIs_df)
proj4string(POIs) <- wgs

# plot POIs against VSO crime locations

tm_shape(COL001F_shp)+
  tm_polygons(col= 'white', lwd=1.0)+ 
  tm_shape(points)+
  tm_dots(size = 0.1, col = 'red', alpha = 1, title = 'VSO')+
  tm_shape(POIs)+
  tm_dots(size = 0.1, col = 'blue', alpha = 1, title = 'Entertainment outlets')+
  tm_layout(
    title="VSO Crime & Entertainment Outlet Locations in London 001F",
    title.position = c("left", "top"),
    title.size = 0.8 ) +
  tm_add_legend(
    type = c("fill"),
    labels = c('VSO crimes','Entertainment Outlets'),
    col = c('red','blue'))


##### prepare data for kcross #####
# create joint pubs_points df
POIs_type <- rep('POIs', nrow(POIs@coords))
points_type <- rep('crime', nrow(points@coords))
POIs_df <- data.frame(lat = POIs$Latitude, lng = POIs$Longitude ,type = POIs_type)
points_df <- data.frame(lat = points$y, lng = points$x ,type = points_type)
POIs_points <- rbind(POIs_df, points_df)

# create ppp object for joint pubs_points df
coordinates(POIs_points) <- ~lng+lat
pts <- coordinates(POIs_points)
POIspoints_ppp <- ppp(pts[,1],pts[,2], window=COL001F_Owin, marks = POIs_points$type)

# kcross test
Kcross.inhom_monte_carlo <- envelope(POIspoints_ppp, fun = Kcross.inhom, nsim =100, i = 'POIs', j = 'crime')
plot(Kcross.inhom_monte_carlo, main = "Monte Carlo Simulation on Kcross.inhom")

### Clean data for buffer to work ####
tmerc <- "+proj=tmerc +lat_0=42.5 +lon_0=-72.5 +k=0.9999642857142857 +x_0=500000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
points <- spTransform(points, CRS(tmerc))
roads <- spTransform(roads, CRS(tmerc))

buffered_points <- gBuffer(points, width = 5, byid=TRUE)

points_on_roads <- gIntersects(roads,buffered_points, byid=TRUE)

