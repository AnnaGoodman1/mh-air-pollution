# link the air pollution to postcode

library(tidyverse)
library(sf)
library(raster)


#Import all post codes
GBpostcodecen <- st_read('C:/Users/S M Labib/Desktop/METAHIT/ONS_Postcode_Directory_Centroids/ONS_Postcode_Directory_Centroids.shp')

#import LA boundary
LAbounary <- st_read('C:/Users/S M Labib/Desktop/METAHIT/Boundaries/METAHIT_LAs.shp')

#select the post codes for the LAs in METAHIT
LA_postcodes <- st_intersection(LAbounary, GBpostcodecen) %>%
  select(pcd, lat, long)


#### bring the change concentration raster ######

LAcngconNOxS2 <- raster(paste0(Globalpath, 'scenconNOx_all_LAsS2.tif'))
crs(LAcngconNOxS2) <- PRJc #project to BNG

LAcngconPM25S2 <- raster(paste0(Globalpath, 'scenconPM25_all_LAsS2.tif'))

rasStac <- stack(LAcngconNOxS2, LAcngconPM25S2)

####### Loop for joining to postcodes centroid ######

for (m in 1:length(LAbounary$lad15nm)) {
  
  print(m)
  
  lahome <- as.character(LAbounary$lad15nm[m])
  
  print(lahome)
  
  lahomecode <- as.character(LAbounary$lad15cd[m])
  
  print(lahomecode)
  
  
  region_shp <- LAbounary [m,]
  region_shp$region <- lahome
  region_shp <- region_shp [,"lad15nm"]
  
  #plot(region_shp)
  
  postcodesinreg <- st_intersection (region_shp, LA_postcodes)
  
  raspostcode <- raster::extract (rasStac, postcodesinreg, df=TRUE, sp = TRUE)
  
  raspostcodesf <- st_as_sf(raspostcode)
  
  #st_write(raspostcodesf, paste0(Globalpath, lahome, '/', "postcodesinreg.shp") , driver="ESRI Shapefile", overwrite = TRUE)
  
  raspostcodesfjoin <- raspostcodesf %>%
    select(lad15nm, 'pcd', 'scenconNOx_all_LAsS2', 'scenconPM25_all_LAsS2') %>%
    mutate(postcoder = str_replace_all(pcd, " ", "")) %>%
    rename(NOxConchangeS2 = "scenconNOx_all_LAsS2") %>%
    rename(PM25ConchangeS2 = "scenconPM25_all_LAsS2") %>%
    st_drop_geometry()
  
  #read synthetic pop data for each scenario
  Synpop <- readRDS(paste0('../mh-execute/inputs/scenarios/s2/', 'SPind_', lahomecode, '.Rds'))
  
  synpopchangecon <- inner_join(Synpop, raspostcodesfjoin, by = c("home_postcode" = "postcoder"))
  
  saveRDS(synpopchangecon, paste0('../mh-execute/inputs/scenarios/s2/', 'SPind_', lahomecode, 'S2c', '.Rds'))
  
}



#Delete later

synpopchangeconcheck <- synpopchangecon %>%
  select(census_id, home_postcode, pcd, NOxConchangeS2, PM25ConchangeS2)


do.call(file.remove, list(list.files(path = paste0(Globalpath),pattern = "postcodesinreg.shx$",full.names = TRUE, recursive = TRUE)))


ManchesterSyn <- readRDS('../mh-execute/inputs/scenarios/SPind_E08000003.Rds')

View(ManchesterSyn)

ManPostcodecent <- st_read('C:/Users/S M Labib/Desktop/METAHIT/mh-air-pollution/new_air_impact/ChangeConShape/ManchesterNoxChangeScen.shp')

plot(ManPostcodecent)

Manpostcode <- ManPostcodecent %>%
  select(objectid, 'pcd', 'oslaua', 'NOxch_1') %>%
  mutate(postcoder= str_replace_all(pcd, " ", "")) %>%
  rename(NOxConchange = "NOxch_1") %>%
  st_drop_geometry()


Manconjoin <- left_join(ManchesterSyn, Manpostcode, by = c("home_postcode" = "postcoder"))


Manconjoinshort <- Manconjoin %>%
  select("census_id", "home_lsoa", "home_postcode", "agecat_det", "pcd", "NOxch_1") %>%
  rename(NOxConchange = "NOxch_1")

cellnull<- sum(is.na(Manconjoinshort$NOxConchange)) #46985

TotalUIds <- count (Manconjoin) #405709

missingcon <- (cellnull/TotalUIds) #0.1158096


ManconjoinshortNeg <- Manconjoinshort %>%
  filter(NOxConchange < 0)
