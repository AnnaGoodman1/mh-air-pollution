# load the raster, sp, and rgdal packages
rm(list = ls())
library(dplyr)
library(raster)
library(rmapshaper)
library(sp)
library(rgdal)

proj_27700 <- CRS("+init=epsg:27700")   # UK easting/northing projection - 'projected' (need if working in metres)

# Load AP data
apdata_bno2 <- read.csv("01_DataInput/baseline_AP/UKIAM_2016_no2/no2_background_conc_edit.csv", header=FALSE)
apdata_bpm25 <- read.csv("01_DataInput/baseline_AP/UKIAM_2016_pm25/pm25_background_conc_edit.csv", header=FALSE)

# AP data to a raster
apdata_bno2 <- data.matrix(apdata_bno2, rownames.force = NA)
apraster_bno2 <- raster(apdata_bno2, xmn=-50000, xmx=762000, ymn=-50000, ymx=1328000, crs="+init=epsg:27700")
apdata_bpm25 <- data.matrix(apdata_bpm25, rownames.force = NA)
apraster_bpm25 <- raster(apdata_bpm25, xmn=-50000, xmx=762000, ymn=-50000, ymx=1328000, crs="+init=epsg:27700")

## SELECT 1 ##
# Load boundaries, option 1: LA
area_shape <- readOGR(file.path("../mh-route-commutes/01_DataInput/lad14_boundaries/Local_Authority_Districts_December_2015_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain.shp")) # [NB LAD15CD always same as LAD14CD]
area_shape <- spTransform(area_shape, proj_27700)
area_list <- read.csv("01_DataInput/lad14cd_apgroups.csv")
#area_list <- read.csv("01_DataInput/lad14cd_separate.csv")
area_shape@data <- left_join(area_shape@data, area_list, by = "lad15cd")

# # Load boundaries, option 2: LSOA
# area_shape <- readOGR(file.path("01_DataInput/lsoa_boundaries/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales.shp"))
# area_shape <- spTransform(area_shape, proj_27700)
# area_list <- read.csv("01_DataInput/lsoa_separate.csv")
# area_shape@data <- left_join(area_shape@data, area_list, by = "lsoa11cd")
###############

# Merge area groupings
area_shape <- area_shape[!is.na(area_shape@data$apgroup_num),]      # restrict to England
area_shape <- ms_dissolve(area_shape, field = "apgroup_num") 

# Calculate average value by area
apgroup_mean <- unique(area_list[,names(area_list) %in% c("apgroup_num", "apgroup_name")])
numlist <- unique(apgroup_mean$apgroup_num)

apgroup_mean$apmean_bno2 <- NA
apgroup_mean$apmean_bpm25 <- NA
for(j in numlist){
  no2list <- extract(apraster_bno2, area_shape[area_shape@data$apgroup_num==j,])
  apgroup_mean$apmean_bno2[apgroup_mean$apgroup_num==j] <- round(as.numeric(lapply(no2list, FUN=mean)), 3)
  pm25list <- extract(apraster_bpm25, area_shape[area_shape@data$apgroup_num==j,])
  apgroup_mean$apmean_bpm25[apgroup_mean$apgroup_num==j] <- round(as.numeric(lapply(pm25list, FUN=mean)), 3)
}

## SELECT 2 ##
# Save file, option 1: LA
write.csv(apgroup_mean, file = file.path("../mh-execute/inputs/background-air-pollution/1_apmeans.csv"), row.names=FALSE)
#write.csv(apgroup_mean, file = file.path("02_DataCreated/2_ladmeans.csv"), row.names=FALSE)

# Save file, option 2: LSOA
#write.csv(apgroup_mean, file = file.path("02_DataCreated/1_lsoameans.csv"), row.names=FALSE)