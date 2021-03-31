#Change distance travel based on scenarios

library(tidyverse)
library(reshape)

basedistance <- readRDS("../mh-execute/inputs/distances/base_emissions_distances.Rds")

scendistance <- readRDS("../mh-execute/inputs/distances/scen_emissions_distances.Rds")


#base_emission_dist <- basedistance[["distance_for_emission"]]
#scen_emission_dist <- scendistance [["distance_for_emission"]]

#Instead of indexing, use the Purr package's pluck function to extract data frame from a list of data frames
base_emission_dist <- pluck(basedistance,1) %>%
  filter(mode_name == "cardrive") %>%
  dplyr::rename(b_motorways = motorway, b_others = other) %>%
  mutate(basedrivedist = b_motorways + b_others) %>%
  melt(id = c("la", "mode_name")) %>%
  cast(la~variable, sum)


scen_emission_dist <- pluck(scendistance,1) %>%
  filter(mode_name == "cardrive") %>%
  dplyr::rename(s_motorways = motorway, s_others = other) %>%
  mutate(scebdrivedist = s_motorways + s_others) %>%
  melt(id = c("la", "mode_name")) %>%
  cast(la~variable, sum) 



changedist <- left_join(base_emission_dist, scen_emission_dist, by = 'la') %>%
  mutate(changeother = b_others- s_others, changemotor = b_motorways- s_motorways) %>%
  mutate(changothersratio = (b_others- s_others)/b_others,
         changemotorratio = (b_motorways- s_motorways)/ b_motorways,
         changallratio = (basedrivedist - scebdrivedist)/basedrivedist)





