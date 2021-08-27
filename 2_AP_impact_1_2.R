
########### Import libraries, give gobal variables##########

rm(list=ls())

{
  #Load libraris and set directories
  library(plyr)
  library(tidyverse)
  library(rgdal)
  library(raster)
  library(sf)
  library(sp)

  #Global path to the folders data stored, change it based on the folder location
  Globalpath<- "C:/Users/S M Labib/Desktop/METAHIT/mh-air-pollution/new_air_impact/_LASpecific/"

  #setwd(Globalpath)

  #Names of the LA folder
  LAlist <- read.csv(paste0(Globalpath, "lafolders.csv")) 

  LAlistF <- read.csv("../mh-execute/inputs/mh_regions_lad_lookup.csv")

  LAwithCR <- left_join(LAlist, LAlistF, by = c("LANames" = "lad11nm"))

  PRJc <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
  
  CityRegions <- list ('bristol', 'greatermanchester', 'leeds', 'liverpool', 'london', 'northeast','nottingham', 'sheffield', 'westmidlands')
}

############ if zipped files and folders #################################
#list all the files in a directory
#zipF <- list.files(path = Globalpath, pattern = "*.zip", full.names = TRUE)
#outDir<- Globalpath
# unzip the zipped files
#ldply(.data = zipF, .fun = unzip, exdir = outDir)

############# local and non-local impact factor for each LA ###############

#this section of code need to do once. If all these impact factor cacluated for the base files, we do not need to run this bit again

for (m in 1:length(LAlist$LANames)) {
  
  lahome <- as.character(LAlist$LANames[m])
  
  print(lahome)
  
  ascfilela <- list.files(path = paste0(Globalpath, lahome),pattern = ".asc$",full.names = TRUE, recursive = TRUE)
  
  #check the asc files listed has the same index number used in the following function
  print(ascfilela)
  
  LAtifStack <- stack(ascfilela)
  
  #The index numbers are layer index in the stack, here: 
  #x[1] is NOX_DieselCars_InSqConc.asc
  #x[3] is NOX_PetrolCars_InSqConc.asc
  #x[2] is NOX_DieselCars_R0.asc
  #x[4] is NOX_PetrolCars_R0.asc
  #x[5] is PM25_brake_DieselCars_R0.asc
  #x[6] is PM25_brake_PetrolCars_R0.asc
  #x[7] is PM25_DieselCars_InSqConc.asc
  #x[8] is PM25_exh_DieselCars_R0.asc
  #x[9] is PM25_exh_PetrolCars_R0.asc
  #x[10] is PM25_PetrolCars_InSqConc.asc
  #x[11] is PM25_tyre_DieselCars_R0.asc
  #x[12] is PM25_tyre_PetrolCars_R0.asc
  #x[13] is s46_vkm_2020.asc, petrol car vkm
  #x[14] is s46mc_vkm_2020.asc, motor cycle
  #x[15] is s47_vkm_2020.asc, diesel car vkm
  
  #for total vkm without motor cycle
  fun0 <- function(x) {sum((x[13] - x[14]) + x[15])}
  VKMtotal <- calc (LAtifStack, fun0) #calculate both Petrol and Diesel VKM minus motor cycle vkm
  crs(VKMtotal) <- PRJc
  vkmvect <- sum(as.vector(VKMtotal)) #get the LA specific total vkm
  
  #sum of NOx across agglomeration in each cell, the R0 files for petrol and diesel car
  #fun1 <- function(x) {x[2] + x[4]} 
  #AggNOxR0 <- calc(LAtifStack, fun1)
  #crs(AggNOxR0) <- PRJc #project to BNG
  
  #local impact factor for in-square VKM and emission of NOx
  #fun2 <- function(x) {(x[1] + x[3])/((x[13] - x[14]) + x[15])} 
  #LifNx <- calc(LAtifStack, fun2)
  #crs(LifNx) <- PRJc
  #print(LifNx)
  
  #non local authority LA impact factor
  #function to estimate non local IF for each LA
  #fun3 <- function(x) {((x[2] + x[4]) - (x[1] + x[3]))/((vkmvect - (x[13] - x[14]) + x[15]))} 
  #non local impact factor for NOx for each la
  #NonLifNx <- calc (LAtifStack, fun3)
  #crs(NonLifNx) <- PRJc
  
  #sum of PM2.5 across agglomeration in each cell, the R0 files for petrol and diesel car
  fun4 <- function(x) {x[5] + x[8] +x[11] + x[6] + x[9] + x[12]} 
  AggPM25R0 <- calc(LAtifStack, fun4)
  crs(AggPM25R0) <- PRJc
  print(AggPM25R0)
  
  
  #local impact factor for in-square VKM and emission of PM2.5
  fun5 <- function(x) {(x[7] + x[10])/((x[13] - x[14]) + x[15])} 
  LifPM <- calc(LAtifStack, fun5)
  crs(LifPM ) <- PRJc
  print(LifPM)
  
  #non local authority LA impact factor for PM2.5
  #function to estimate non local IF for each LA
  #fun6 <- function(x) {((x[5] + x[8] +x[11] + x[6] + x[9] + x[12]) - (x[7] + x[10]))/((vkmvect - (x[13] - x[14]) + x[15]))} 
  #non local impact factor for NOx for each la
  #NonLifPM <- calc (LAtifStack, fun6)
  #crs(NonLifPM) <- PRJc
  
  #save the combined of R0 layer for each LA
  #writeRaster(AggNOxR0, filename= file.path(Globalpath, lahome, "AggNOxR0.tif"), format="GTiff", overwrite=TRUE)
  
  #save the combined of VKM layer for each LA
  #writeRaster(VKMtotal, filename= file.path(Globalpath, lahome, "VKMtotal_2020.tif"), format="GTiff", overwrite=TRUE)
  
  #save the impact factors
  #save the local impact factor as tif file
  #writeRaster(LifNx, filename= file.path(Globalpath, lahome, "LIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  #save the non local impact factor as tif file
  #writeRaster(NonLifNx, filename= file.path(Globalpath, lahome, "NonLIFNx.tif"), format="GTiff", overwrite=TRUE)
  
  
  #save the combined of R0 layer for each LA for PM2.5
  writeRaster(AggPM25R0, filename= file.path(Globalpath, lahome, "AggPM25R0.tif"), format="GTiff", overwrite=TRUE)
  
  #save the local impact factor for PM2.5 as tif file
  writeRaster(LifPM, filename= file.path(Globalpath, lahome, "LifPM.tif"), format="GTiff", overwrite=TRUE)
  
  #save the non local impact factor as tif file
  writeRaster(NonLifPM, filename= file.path(Globalpath, lahome, "NonLifPM.tif"), format="GTiff", overwrite=TRUE)
  
  #empty the list for the next LA
  ascfilela <- NA

}


##################### changed concentration for all LAs #####################################
#Hypothetical change in concentration by LAs
#This can be used if we want certain fixed changes for all LAs
#LAwithCR_cocen <- LAwithCR %>%
  #mutate(conchange = 0.9) #10% reduction in concentration
  #mutate(conchange = runif(84, 0.05, 0.5)) #create random concentration change for each LA. 


scenchangedist <- read.csv("../mh-air-pollution/01_DataInput/APdistance_changeS2.csv")

#In the main calculation this would come from scenarios

scenchangedistadd <- scenchangedist %>%
  dplyr::select('la', 'changallratio')

LAwithCR_cocen <- left_join(LAwithCR, scenchangedistadd, by = c("lad11cd" = "la")) %>%
  dplyr::rename(conchange = changallratio) %>%
  dplyr::mutate(conchange = replace_na(conchange, 0.1)) #replace the NA values of the LA concentration with 0.1

#LAwithCR_cocen2 <- LAwithCR %>%
  #mutate(conchange = runif(84, 0.05, 0.5)) #create random concentration change for each LA5

#loop in each LA to estimate the changed concentration for combined R0 layer. 
#this estimation process is conducted to add the change concentration of surrounding to a LA, surrounded by other LAs in city region. 

for (cla in 1:length(LAwithCR_cocen$LANames)) {
  
  lahomeC <- as.character(LAwithCR_cocen$LANames[cla])
  
  print(lahomeC)
  
  changecon <- LAwithCR_cocen$conchange[cla] #Scenario based change concentration of each LA
  
  print(changecon)
  
  #read the R0 concentration layer for each LA, that has concentration cell values beyond the LA boundary
  lanonlocalraster <- raster(paste0(Globalpath, lahomeC, '/','AggNOxR0.tif'))
  
  #multiply the R0 concentration layer with potential change values (e.g., 0.05, 0.1, 0.5)
  #Now this value is randomly generated but in main calculation this would be based on scenario
  changeconraster <- lanonlocalraster * changecon
  
  #Save the changed combined R0 concentration layer for each LA
  writeRaster(changeconraster, filename= file.path(Globalpath, lahomeC,  "changeconrasterR0S2.tif"), format="GTiff", overwrite=TRUE)

}

##############Adding Changed concentration from other LAs within a city region ####################

#CityRegions <- list ('greatermanchester')

for (crh in 1:length(CityRegions)) {
  
  crigon <- as.character(CityRegions[crh])
  
  print(crigon)
  
  #subset the LAs within a city region
  citylahome <- subset (LAwithCR_cocen, cityregion == crigon)
  
  CRfilela <- list()
  
  for (clarg in 1:length(citylahome$LANames)) {
    
    crlahome <- as.character(citylahome$LAName[clarg])
    
    CRfilela [clarg] <- list.files(path = paste0(Globalpath, crlahome),pattern = "changeconrasterR0S2.tif$",full.names = TRUE, recursive = TRUE)
    
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
    
    writeRaster(ConOthers, filename= file.path(Globalpath, crlahomeX,  "ChangeConOthersS2.tif"), format="GTiff", overwrite=TRUE)
  }
  
}

######### Estimating changed concentration for each LA based on impact factors and surrounding LAs in the city region #########


for (laname in 1:length(LAwithCR_cocen$LANames)) {
  
  lahomeName <- as.character(LAwithCR_cocen$LANames[laname])
  print(lahomeName)
  
  lahomeC <- as.character(LAwithCR_cocen$LANames[cla])
  
  changecon <- 1- LAwithCR_cocen$conchange[laname] #Scenario based change concentration of each LA
  #changecon <- 0.9 #if we want fixed change
  print(changecon)
  
  #read the total VKM for each LA and extract the sum of all cells
  CellVKM <- raster(paste0(Globalpath, lahomeName, '/','VKMtotal_2020.tif'))
  vkmsum <- sum(as.vector(CellVKM)) 
  
  print(vkmsum)
  
  #import local in-square impact factor
  LocalIF <- raster(paste0(Globalpath, lahomeName, '/','LIFNx.tif'))
  
  #import non-local impact factor
  NonLocalIF <- raster(paste0(Globalpath, lahomeName, '/','NonLIFNx.tif'))
  
  SorroundLAChangecon <- raster(paste0(Globalpath, lahomeName, '/','ChangeConOthersS2.tif'))
  
  LAchangedcon <- (((changecon * LocalIF * CellVKM) + (changecon * NonLocalIF * (vkmsum - CellVKM))) + SorroundLAChangecon)

  
  diffcon <- basecon - LAchangedcon
  
  #Save the changed combined R0 concentration layer for each LA
  writeRaster(LAchangedcon, filename= file.path(Globalpath, lahomeName,  "LAchangedconNOxS2.tif"), format="GTiff", overwrite=TRUE)
  
  writeRaster(diffcon, filename= file.path(Globalpath, lahomeName,  "diffconNOxS2.tif"), format="GTiff", overwrite=TRUE)
  
}



#clean unnecessary files or other files that do not overwrite.

do.call(file.remove, list(list.files(path = paste0(Globalpath),pattern = "LifPM.tif$",full.names = TRUE, recursive = TRUE)))


#change concentration after running all the LAs
chcon <- list.files(path =Globalpath,pattern = "diffconNOxS2.tif$",full.names = TRUE, recursive = TRUE )
chcon_stack <- stack(chcon)
chcon_all_LAs <- calc(chcon_stack, fun = sum, na.rm =T)
writeRaster(chcon_all_LAs,filename=file.path(Globalpath, "chcon_all_LAsS2.tif"),options=c('TFW=YES'), overwrite=TRUE)


#scen concentration after running all the LAs
scencon <- list.files(path =Globalpath,pattern = "LAchangedconNOxS2.tif$",full.names = TRUE, recursive = TRUE )
scencon_stack <- stack(scencon)
scencon_all_LAs <- calc(scencon_stack, fun = sum, na.rm =T)
writeRaster(scencon_all_LAs,filename=file.path(Globalpath, 'scencon_all_LAsS2.tif'),options=c('TFW=YES'), overwrite=TRUE)


#VKM all the LAs
VKMLAs <- list.files(path =Globalpath,pattern = "VKMtotal_2020.tif$",full.names = TRUE, recursive = TRUE )
VKMLAs_stack <- stack(VKMLAs)
VKMLAs_stack_all_LAs <- calc(VKMLAs_stack, fun = sum, na.rm =T)
writeRaster(VKMLAs_stack_all_LAs,filename=file.path(Globalpath, 'VKMLAs_stack_all_LAs.tif'),options=c('TFW=YES'), overwrite=TRUE)


#Non local impact running all the LAs
NonLImpLAs <- list.files(path =Globalpath,pattern = "NonLIFNx.tif$",full.names = TRUE, recursive = TRUE )
NonLImpLAs_stack <- stack(NonLImpLAs)
NonLImpLAs_all_LAs <- calc(NonLImpLAs_stack, fun = sum, na.rm =T)
writeRaster(NonLImpLAs_all_LAs,'NonLImpLAs_all_LAs.tif',options=c('TFW=YES'))



