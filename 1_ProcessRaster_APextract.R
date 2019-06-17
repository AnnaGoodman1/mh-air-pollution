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

# Load LAD boundaries
lad14shape <- readOGR(file.path("../mh-route-commutes/01_DataInput/lad14_boundaries/Local_Authority_Districts_December_2015_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain.shp")) # [NB LAD15CD always same as LAD14CD]
lad14shape <- spTransform(lad14shape, proj_27700)
# plot(apraster_bpm25)
# plot(lad14shape, bg="transparent", add=TRUE)

# Define LA groupings
lad14 <- read.csv("01_DataInput/lad14cd_apgroups.csv")
lad14shape@data <- left_join(lad14shape@data, lad14, by = "lad15cd")
lad14shape <- lad14shape[!is.na(lad14shape@data$apgroup_num),]      # restrict to England
lad14shape <- ms_dissolve(lad14shape, field = "apgroup_num") 

# Calculate average value by LA grouping
apgroup_mean <- unique(lad14[,names(lad14) %in% c("apgroup_num", "apgroup_name")])
numlist <- unique(apgroup_mean$apgroup_num)

apgroup_mean$apmean_bno2 <- NA
apgroup_mean$apmean_bpm25 <- NA
for(j in numlist){
  no2list <- extract(apraster_bno2, lad14shape[lad14shape@data$apgroup_num==j,])
  apgroup_mean$apmean_bno2[apgroup_mean$apgroup_num==j] <- round(as.numeric(lapply(no2list, FUN=mean)), 3)
  pm25list <- extract(apraster_bpm25, lad14shape[lad14shape@data$apgroup_num==j,])
  apgroup_mean$apmean_bpm25[apgroup_mean$apgroup_num==j] <- round(as.numeric(lapply(pm25list, FUN=mean)), 3)
}

# Save file
write.csv(apgroup_mean, file = file.path("02_DataCreated/1_apmeans.csv"), row.names=FALSE)