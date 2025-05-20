library(sf)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
#watershed path
watershed_path = "C:/Users/kevin.turnbow/GIS/watershed6.shp"
#attains dataframe path
attains_path ="C:/Users/kevin.turnbow/Desktop/at.csv"
#streams path
streams_path = ("C:/Users/kevin.turnbow/GIS/stream1.shp")
#awqm dataframe path
awqm_path ="C:/Users/kevin.turnbow/Downloads/A_Habitatactivites.xlsx"
#stations path
stations_path ="C:/Users/kevin.turnbow/GIS/stations.shp"
#stream length minimum
stm_leng_min = 8

#read Huc 8 of Illinois
water <- st_read(watershed_path)
ggplot(water) +
  geom_sf(color = "Blue")
#select the HUC 8 of interest
watershed <- subset(water, (name == "Lower Ohio" | name == "Upper Mississippi-Salt" | name == "Upper Mississippi-Merimec"))

ggplot(watershed) +
  geom_sf(color = "Blue")


#read in the data from attains
at <- read.csv(attains_path)

#removed any records of Meeting Criteria
at<- subset(at, PARAM_STATUS_NAME != "Meeting Criteria")

# read in the shapefile for streams
allstream <- st_read(streams_path)
allstream <- allstream %>%
  filter(Miles>stm_leng_min)
#convert the 3D shapefile to 2 dimensional
allstream <- st_zm(allstream, what='ZM')
st_write(allstream, "C:/Users/kevin.turnbow/GIS/output1.csv", layer_options = "GEOMETRY=AS_XY")

ggplot(allstream) +
  geom_sf(color = "Blue")

#clip Ohio tribs from streams
tribs <- st_filter(allstream, watershed)
tribs

ggplot(tribs) +
  geom_sf(color = "Blue")



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


#export the merged data set to a shapefile
st_write(df_stream, "C:/Users/kevin.turnbow/GIS/stream5.shp")

#read in the data from awqms of sampling dates
awqm <- read_excel(awqm_path)

#remove duplicate dates
awqm <-awqm[!duplicated(awqm$Activity_Start_Date), ]

# read in the shapefile for stations
station <- st_read(stations_path)
ggplot(station) +
  geom_sf(color = "Blue")

a <- st_filter(station, watershed)
ggplot(a) +
  geom_sf(color = "Blue")


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

f<-rbind(random_rowsa,random_rowsj,random_rowsat)


#write.xlsx(f,"C:/Users/kevin.turnbow/Desktop/rand.xlsx")
st_write(f, "C:/Users/kevin.turnbow/GIS/rand6.shp", delete_layer = TRUE)
f <- st_read("C:/Users/kevin.turnbow/GIS/rand6.shp")
g <- st_read("C:/Users/kevin.turnbow/GIS/stations5.shp")
g<-st_transform(g, crs = st_crs(f))

#create 100m buffer on streams for selecting stations
buffered_sf2 <- st_buffer(f, dist = 100)
#select stations within 100 meter of a stream line vector
stations_on_streams<-intersection_result <- st_intersection(buffered_sf2, g)
st_as_sf(stations_on_streams) 
ggplot() +
  geom_sf(data = f, fill = "lightblue", color = "blue") +
  geom_sf(data = watershed, fill = "white", color = "black") +
  geom_sf(data = stations_on_streams, fill = "lightcoral", color = "black") +
  labs(title = "Two Polygons (ggplot2)") +
  theme_minimal()
