library(sf)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(leafem)
library(leaflet)
###################################
#define variables

#watershed path
watershed_path = "C:/Users/kevin.turnbow/GIS/watershed6.shp"
#attains dataframe path
attains_path ="C:/Users/kevin.turnbow/Desktop/at.csv"
#streams path
streams_path = ("C:/Users/kevin.turnbow/GIS/streams.shp")
#awqm dataframe path
awqm_path ="C:/Users/kevin.turnbow/Downloads/A_Habitatactivites.xlsx"
#stations path
stations_path ="C:/Users/kevin.turnbow/GIS/stations.shp"
#stream length minimum
stm_leng_min = 8
##################################

##################################
#input data

#read in the data from attains
at <- read.csv(attains_path)
#removed any records of Meeting Criteria
at<- subset(at, PARAM_STATUS_NAME != "Meeting Criteria")

#read Huc 8 of Illinois
water <- st_read(watershed_path)
#select the HUC 8 of interest
watershed <- subset(water, (name == "Lower Ohio"))
#watershed <- subset(water, (name == "Lower Ohio" | name == "Upper Mississippi-Salt"))

# read in the shapefile for streams
allstream <- st_read(streams_path)
allstream <- allstream %>%
  filter(Miles>stm_leng_min)
#convert the 3D shapefile to 2 dimensional
allstream <- st_zm(allstream, what='ZM')

# read in the shapefile for stations
station <- st_read(stations_path)
#read in the data from awqms of sampling dates
awqm <- read_excel(awqm_path)
#remove duplicate dates
awqm <-awqm[!duplicated(awqm$Activity_Start_Date), ]

###################################

###################################

#clip Ohio tribs from streams
tribs <- st_filter(allstream, watershed)
tribs

#count the number of records of PARAM_NAME per ASSESSMENT_UNIT_ID
b <- at %>%
group_by(ASSESSMENT_UNIT_ID) %>%
summarize(Counto_303d = length(unique(PARAM_NAME))) 
#rename the column names to match the shapefile column names
colnames(b) <- c('AUID','PARAM_NAME')

#merge data sets to combine the streams shapefile and the count of pollutants per stream
df_stream <- merge(b, allstream, by = "AUID", all = TRUE)
df_stream <- df_stream %>%
  filter(Miles>8)
st_as_sf(df_stream)  



#count the number of records of Activity_Start_Date per ASSESSMENT_UNIT_ID
c <- awqm %>%
group_by(Monitoring_Location_ID) %>%
summarize(Visits = length(unique(Activity_Start_Date)), Last_Visit = max(unique(Activity_Start_Date)))
#rename the column names to match the shapefile column names
colnames(c) <- c('Monitoring','visits','Last_visit')


#merge data sets to combine the stations shapefile the count of pollutants per stream
df_station <- merge(c, station, by = "Monitoring", all = TRUE) 
st_as_sf(df_station)


#export the merged data set to a shapefile
st_write(df_stream, "C:/Users/kevin.turnbow/GIS/stream5.shp")

#export the merged data set to a shapefile
st_write(df_station, "C:/Users/kevin.turnbow/GIS/stations5.shp")


######################################################################################################

df_streamj <- df_stream %>%
  #mutate(AUID = str_replace_all(AUID, "IL_", "")) %>%
  filter(str_starts(AUID, "IL_J"))

df_streamat <- df_stream %>%
  #mutate(AUID = str_replace_all(AUID, "IL_", "")) #%>%
  filter(str_starts(AUID, "IL_AT"))
df_streama <- df_stream %>%
  #mutate(AUID = str_replace_all(AUID, "IL_", "")) #%>%
  filter(str_starts(AUID, "IL_A"))


df_streama <- anti_join(df_streama, df_streamat)

random_row_indicesat<- sample(nrow(df_streamat), 11, replace = FALSE)
random_rowsat <- df_streamat[random_row_indicesat,]

random_row_indicesa<- sample(nrow(df_streama), 11, replace = FALSE)
random_rowsa <- df_streama[random_row_indicesa,]

random_row_indicesj<- sample(nrow(df_streamj), 12, replace = FALSE)
random_rowsj <- df_streamj[random_row_indicesj,]

#combine randomly selected rows
f<-rbind(random_rowsa,random_rowsj,random_rowsat)


#write.xlsx(f,"C:/Users/kevin.turnbow/Desktop/rand.xlsx")
st_write(f, "C:/Users/kevin.turnbow/GIS/rand6.shp", delete_layer = TRUE)
f <- st_read("C:/Users/kevin.turnbow/GIS/rand6.shp")
g <- st_read("C:/Users/kevin.turnbow/GIS/stations5.shp")

#match the coordinate systems between the stations and streams
g<-st_transform(g, crs = st_crs(f))

#create 100m buffer on streams for selecting stations
buffered_sf2 <- st_buffer(f, dist = 100)
#select stations within 100 meter of a stream line vector
stations_on_streams<-intersection_result <- st_intersection(buffered_sf2, g)
st_as_sf(stations_on_streams) 


# Create a basic Leaflet map

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  setView(lng = -98.58, lat = 39.82, zoom = 4)%>% # Set initial view to the US
  addPolygons(
    data=water,
    fillColor = "tan",
    fillOpacity = .9,
    color = "white",
    dashArray = "3"
    )%>%
  addPolylines(data = f)%>%
  addCircleMarkers(data =stations_on_streams,
                   radius = 5, # Small radius for a dot
                   color = "black", # Outline color
                   fillColor = "red", # Fill color
                   fillOpacity = 1, # Opacity of the fill
                   stroke = TRUE) 

m
