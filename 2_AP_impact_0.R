
########### Import libraries, give gobal variables##########

rm(list=ls())

library(plyr)
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(sp)

#Global path to the folders data stored
Globalpath<- "C:/Users/S M Labib/Desktop/METAHIT/mh-air-pollution/new_air_impact/_LASpecific/"

#setwd(Globalpath)

#Names of the LA folder
LAlist <- read.csv(paste0(Globalpath, "lafolders.csv")) 

LAlistMan <- list ("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")

LAlistF <- read.csv("../mh-execute/inputs/mh_regions_lad_lookup.csv")

LAwithCR <- left_join(LAlist, LAlistF, by = c("LANames" = "lad11nm"))

#test change in concetration by LAs
LAwithCR_cocen <- LAwithCR %>%
  mutate(conchange = runif(84, 0.05, 0.5))

PRJc <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

############ if zipped files and folders ######
#list all the files in a directory
zipF <- list.files(path = Globalpath, pattern = "*.zip", full.names = TRUE)
outDir<- Globalpath
# unzip the zipped files
ldply(.data = zipF, .fun = unzip, exdir = outDir)

##############Converting to tif from asc with given projection #######

asctotifprj <- function(LAList, Globalpath, Projection) {
  
  for (j in 1:length(LAList$LANames)) {
    
    lahome <- as.character(LAList$LANames[j])
    
    print(lahome)
    
    ascfilela <- list.files(path = paste0(Globalpath, lahome),pattern = ".asc$",full.names = TRUE, recursive = TRUE)
    
    print(ascfilela)
    
    tifla <- gsub("\\.asc$", ".tif", ascfilela)
    #convert to tif
    
    for (k in 1:length(ascfilela)) {
      rtif <- raster(ascfilela[k])
      crs(rtif) <- Projection
      rtif <- writeRaster(rtif, tifla [k], overwrite=TRUE )}
  }
  
}



#call the function
asctotifprj(LAlist, Globalpath, PRJc)

############# local impact ###############


for (m in 1:length(LAlist$LANames)) {
  
  lahome <- as.character(LAlist$LANames[m])
  
  print(lahome)
  
  tifilela <- list.files(path = paste0(Globalpath, lahome),pattern = ".asc$",full.names = TRUE, recursive = TRUE)
  
  print(tifilela)
  
  LAtifStack <- stack(tifilela)
  
  fun0 <- function(x) {x[2] + x[4]} #sum of NOx across agglomeration in each cell
  AggNOxR0 <- calc(LAtifStack, fun0)
  crs(AggNOxR0) <- PRJc
  
  #local impact factor for in-square VKM and emission of NOx
  fun1 <- function(x) {(x[1] + x[3])/((x[13] - x[14]) + x[15])} #these numbers are layer index in the stack, here x[1] is NOX_DieselCars_InSqConc.asc
  LifNx <- calc(LAtifStack, fun1)
  crs(LifNx) <- PRJc
  print(LifNx)
  
  #non local LA impact factor
  #for total vkm without motor cycle
  fun2 <- function(x) {sum((x[13] - x[14]) + x[15])}
  
  VKMtotal <- calc (LAtifStack, fun2) #calculate both Petrol and Disel VKM minus motor cycle vkm
  
  vkmvect <- sum(as.vector(VKMtotal)) #get the LA spacfic total vkm
  
  fun3 <- function(x) {((x[2] + x[4]) - (x[1] + x[3]))/((vkmvect - (x[13] - x[14]) + x[15]))} #function to estimate non local IF for each LA
  
  #non local impact factor for NOx for each la
  NonLifNx <- calc (LAtifStack, fun3)
  crs(NonLifNx) <- PRJc
  
  
  #save the sum of R0 layers
  writeRaster(AggNOxR0, filename= file.path(Globalpath, lahome, "AggNOxR0.tif"), format="GTiff", overwrite=TRUE)
  
  #save the impact factors
  #save the local impact factor as tif file
  writeRaster(LifNx, filename= file.path(Globalpath, lahome, "LIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  #save the non local impact factor as tif file
  writeRaster(NonLifNx, filename= file.path(Globalpath, lahome, "NonLIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  tifilela <- NA

}

#################testing for Manchester ##############



for (m in 1:length(LAlistMan)) {
  
  lahome <- as.character(LAlistMan[m])
  
  print(lahome)
  
  tifilela <- list.files(path = paste0(Globalpath, lahome),pattern = ".asc$",full.names = TRUE, recursive = TRUE)
  
  print(tifilela)
  
  LAtifStack <- stack(tifilela)
  
  fun0 <- function(x) {x[2] + x[4]} #sum of NOx across agglomeration in each cell
  AggNOxR0 <- calc(LAtifStack, fun0)
  crs(AggNOxR0) <- PRJc
  
  #local impact factor for in-square VKM and emission of NOx
  fun1 <- function(x) {(x[1] + x[3])/((x[13] - x[14]) + x[15])} #these numbers are layer index in the stack, here x[1] is NOX_DieselCars_InSqConc.asc
  LifNx <- calc(LAtifStack, fun1)
  crs(LifNx) <- PRJc
  print(LifNx)
  
  #non local LA impact factor
  #for total vkm without motor cycle
  fun2 <- function(x) {sum((x[13] - x[14]) + x[15])}
  
  VKMtotal <- calc (LAtifStack, fun2) #calculate both Petrol and Disel VKM minus motor cycle vkm
  
  vkmvect <- sum(as.vector(VKMtotal)) #get the LA spacfic total vkm
  
  fun3 <- function(x) {((x[2] + x[4]) - (x[1] + x[3]))/((vkmvect - (x[13] - x[14]) + x[15]))} #function to estimate non local IF for each LA
  
  #non local impact factor for NOx for each la
  NonLifNx <- calc (LAtifStack, fun3)
  crs(NonLifNx) <- PRJc
  
  
  #save the sum of R0 layers
  writeRaster(AggNOxR0, filename= file.path(Globalpath, lahome, "AggNOxR0.tif"), format="GTiff", overwrite=TRUE)
  
  #save the impact factors
  #save the local impact factor as tif file
  writeRaster(LifNx, filename= file.path(Globalpath, lahome, "LIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  #save the non local impact factor as tif file
  writeRaster(NonLifNx, filename= file.path(Globalpath, lahome, "NonLIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  tifilela <- NA
  
}

##################### changed concentration for all LAs ################



for (cla in 1:length(LAwithCR_cocen$LANames)) {
  
  lahomeC <- as.character(LAwithCR_cocen$LANames[cla])
  
  print(lahomeC)
  
  changecon <- LAwithCR_cocen$conchange[cla]
  
  print(changecon)
  
  lanonlocalraster <- raster(paste0(Globalpath, lahomeC, '/','AggNOxR0.tif'))
  
  changeconraster <- lanonlocalraster * changecon
  
  print(lanonlocalraster)
  
  writeRaster(changeconraster, filename= file.path(Globalpath, lahomeC,  "changeconrasterR0.tif"), format="GTiff", overwrite=TRUE)

}


#lanonlocalraster <- raster(paste0(Globalpath, "Bolton", "/", 'AggNOxR0.tif'))

######################## Blame Matrix #########################

CityRegions <- list ('greatermanchester', 'nottingham', 'bristol')



for (crh in 1:length(CityRegions)) {
  
  crigon <- as.character(CityRegions[crh])
  
  print(crigon)
  
  citylahome <- subset (LAwithCR_cocen, cityregion == crigon)
  
  CRfilela <- list()
  
  for (clarg in 1:length(citylahome$LANames)) {
    
    crlahome <- as.character(citylahome$LAName[clarg])
    
    CRfilela [clarg] <- list.files(path = paste0(Globalpath, crlahome),pattern = "changeconrasterR0.tif$",full.names = TRUE, recursive = TRUE)
    
    print(crlahome)
  }
  
  CRLAfilesS <- unlist(CRfilela, recursive = TRUE)
  
  print(CRLAfilesS)
  
  CRLAfilesStack <- stack(CRLAfilesS)
  
  
  for (clarg in 1:length(citylahome$LANames)) {
    
    print(clarg)

    crlahomeX <- as.character(citylahome$LAName[clarg])
    print(crlahomeX)
  
    
    funX <- function(x) {sum((x [-clarg]))}
    
    ConOthers <- calc (CRLAfilesStack, funX)
    
    writeRaster(ConOthers, filename= file.path(Globalpath, crlahomeX,  "ConOthers.tif"), format="GTiff", overwrite=TRUE)
  }
  
  if (CityRegions == 'greatermanchester'){
    
    break
  }

  
}


#CRLAfilesS <- unlist(CRfilela, recursive = TRUE)

#CRLAfilesStack <- stack(tifilela)

### Check this for dynamic function to calculate multiple attribute in the function 
### https://stat.ethz.ch/pipermail/r-sig-geo/2015-July/023186.html



##################### blame matrix non-local if ############# TEST


CityRegions <- list ('greatermanchester', 'nottingham', 'bristol')



for (crh in 1:length(CityRegions)) {
  
  crigon <- as.character(CityRegions[crh])
  
  print(crigon)
  
  citylahome <- subset (LAwithCR_cocen, cityregion == crigon)
  
  CRfilela <- list()
  
  for (clarg in 1:length(citylahome$LANames)) {
    
    crlahome <- as.character(citylahome$LAName[clarg])
    
    CRfilela [clarg] <- list.files(path = paste0(Globalpath, crlahome),pattern = "AggNOxR0.tif$",full.names = TRUE, recursive = TRUE)
    
    print(crlahome)
  }
  
  CRLAfilesS <- unlist(CRfilela, recursive = TRUE)
  
  print(CRLAfilesS)
  
  CRLAfilesStack <- stack(CRLAfilesS)
  
  print(CRLAfilesStack)
  
  conchange = list()
  
  for (clarg in 1:length(citylahome$LANames)) {
    
    print(clarg)
    
    crlahomeX <- as.character(citylahome$LAName[clarg])
    print(crlahomeX)
    
    conchange [clarg] <- citylahome$conchange 
    
    print(conchange)
    
    funX <- function(x) {x * conchange}
    
    ConOthers <- calc (CRLAfilesStack, funX)
    
    Conothers <- stackApply(CRLAfilesStack, indices= 1, funX)
    
    writeRaster(ConOthers, filename= file.path(Globalpath, crlahomeX,  "NonLocalOthers2.tif"), format="GTiff", overwrite=TRUE)
  }
  
  if (CityRegions == 'greatermanchester'){
    
    break
  }
  
  
}


#########

#loop in the stack
for (i in 1:nlayers (CRLAfilesStack)) {
  
  #print(CRLAfilesStack)
  
  rn <- (CRLAfilesStack[i])
  
  print(rn)
}





###test for impact factors ######

tifilela <- list.files(path = paste0(Globalpath, "Oldham"),pattern = ".asc$",full.names = TRUE, recursive = TRUE)

print(tifilela)

setwd(paste0(Globalpath, "Oldham", "/"))

LAtifStack <- stack(tifilela)

#print(LAtifStack)

#local impact factor
fun2 <- function(x) {(x[1] + x[3])/((x[13] - x[14]) + x[15])}

#for total vkm without motor cycle
fun3 <- function(x) {sum((x[13] - x[14]) + x[15])}



Lif <- calc(LAtifStack, fun2)

VKMtotal <- calc (LAtifStack, fun3)

vkmvect <- sum(as.vector(VKMtotal))


fun4 <- function(x) {((x[2] + x[4]) - (x[1] + x[3]))/((vkmvect - (x[13] - x[14]) + x[15]))}

NonLif <- calc (LAtifStack, fun4)

print(Lif)
rLif <- writeRaster(Lif, filename= file.path(Globalpath, 'Oldham', "LIF4.tif"), overwrite=TRUE )
NonrLif <- writeRaster(NonLif , 'NonLifasc.tif', overwrite=TRUE )

