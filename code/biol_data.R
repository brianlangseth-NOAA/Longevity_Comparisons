##
#This is Nityamms pull request code addition to biol_data.R
#https://github.com/DFO-NOAA-Pacific/surveyjoin-db/pull/35
#
#With additions by Brian Langseth
##

library(dplyr)
library(readr)
library(tidyr)
library(here)

# pull these data from Nityamm's repo (see the pull request for the link)
species <- readRDS("data/species_all.rds")
haul <- readRDS("data/haul_data.rds")
species$common_name <- gsub('"', "", species$common_name) #remove the double quotes in common names

# biological data from shared google drive
afsc <- readRDS("data/afsc-specimen.rds")
nwfsc_combo <- readRDS("data/nwfsc_nwfsccombo_age_and_length.rds")
nwfsc_slope <- readRDS("data/nwfsc_nwfscslope_age_and_length.rds")

afsc <- afsc %>%
  semi_join(haul, by = "event_id") %>%
  semi_join(species, by = c("worms" = "species_id")) %>%
  left_join(haul %>% select(survey_id, event_id, lat_end, lon_end), by = "event_id") %>%
  transmute(
    event_id,
    survey_id,
    species_id = worms,
    length_cm = length_mm / 10,
    sex,
    age,
    weight_kg = weight_g / 1000,
    sex = case_when(
      sex == c(1) ~ "male",
      sex == c(2) ~ "female",
      sex == c(3) ~ "unknown"
    ),
    common_name = tolower(common_name),
    lat = lat_end,
    lon = lon_end
  ) %>%
  filter(
    !if_all(c(length_cm, weight_kg, sex, age), is.na)
  )


nwfsc_combo <- nwfsc_combo %>%
  mutate(Trawl_id = as.double(Trawl_id)) %>%
  semi_join(haul, by = c("Trawl_id" = "event_id")) %>%
  mutate(scientific_name = tolower(Scientific_name)) %>%
  semi_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  left_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  )  %>%
  left_join(haul %>% select(survey_id, event_id), by = c("Trawl_id" = "event_id")) %>%
  transmute(
    event_id = Trawl_id,
    survey_id,
    weight_kg = Weight_kg,
    age = Age_years,
    length_cm = Length_cm,
    species_id,
    sex = Sex,
    sex = case_when(
      sex == c('M') ~ "male",
      sex == c('F') ~ "female",
      sex == c('U') ~ "unknown"
    ),
    common_name = tolower(Common_name),
    lat = Latitude_dd,
    lon = Longitude_dd
  ) %>%
  filter(
    !if_all(c(length_cm, weight_kg, sex, age), is.na)
  )

nwfsc_slope <- nwfsc_slope %>%
  mutate(Trawl_id = as.double(Trawl_id)) %>%
  semi_join(haul, by = c("Trawl_id" = "event_id")) %>%
  mutate(scientific_name = tolower(Scientific_name)) %>%
  semi_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  left_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  )  %>%
  left_join(haul %>% select(survey_id, event_id), c("Trawl_id" = "event_id")) %>%
  transmute(
    event_id = Trawl_id,
    survey_id,
    weight_kg = Weight_kg,
    age = Age_years,
    length_cm = Length_cm,
    species_id,
    sex = Sex,
    sex = case_when(
      sex == c('M') ~ "male",
      sex == c('F') ~ "female",
      sex == c('U') ~ "unknown"
    ),
    common_name = tolower(Common_name),
    lat = Latitude_dd,
    lon = Longitude_dd
  ) %>%
  filter(
    !if_all(c(length_cm, weight_kg, sex, age), is.na)
  )

biol_data <- bind_rows(afsc, nwfsc_combo, nwfsc_slope)
#saveRDS(biol_data, "data/biol_data.rds")


##---------------------------------------------------------------##
#Start of my additions
##---------------------------------------------------------------##

##
#Add the triennial data
##

#biological data from shared google drive
nwfsc_tri <- readRDS("data/nwfsc_triennial_age_and_length.rds")
nwfsc_triCA <- readRDS("data/nwfsc_triennialcanada_age_and_length.rds")

#Tri data from Canada are not in the haul database. 
#For bio data this isn't a problem so remove the linkage with haul
nwfsc_triCA <- nwfsc_triCA %>%
  mutate(Trawl_id = as.double(Trawl_id)) %>%
  #semi_join(haul, by = c("Trawl_id" = "event_id")) %>%
  mutate(scientific_name = tolower(Scientific_name)) %>%
  semi_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  left_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  transmute(
    event_id = Trawl_id,
    survey_id = "Triennial Canada",
    weight_kg = Weight_kg,
    age = Age,
    length_cm = Length_cm,
    species_id,
    sex = Sex,
    sex = case_when(
      sex == c('M') ~ "male",
      sex == c('F') ~ "female",
      sex == c('U') ~ "unknown"
    ),
    common_name = tolower(Common_name),
    lat = Latitude_dd,
    lon = Longitude_dd
  ) %>%
  filter(
    !if_all(c(length_cm, weight_kg, sex, age), is.na)
  )

#Triennial data is split and at times has no unique identifier between
#the two datasets (lengths and ages). 
#For records with no clear identifier (same Trawl_id, species, sex, length) just
#assign an age to one of the lengths. Since length data are same, doesn't affect
#our purpose. Do so by adding a counter to any unidentifiable lengths and ages 
nwfsc_tri_length <- nwfsc_tri$length_data %>%
  mutate(Trawl_id = as.double(Trawl_id)) %>%
  semi_join(haul, by = c("Trawl_id" = "event_id")) %>%
  mutate(scientific_name = tolower(Scientific_name)) %>%
  semi_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  left_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  #Add a counter for duplicates
  group_by(Trawl_id, Common_name, Sex, Length_cm) %>%
  mutate(Counter = row_number()) %>%
  ungroup() 

nwfsc_tri_age <- nwfsc_tri$age_data %>%
  mutate(Trawl_id = as.double(Trawl_id)) %>%
  semi_join(haul, by = c("Trawl_id" = "event_id")) %>%
  mutate(scientific_name = tolower(Scientific_name)) %>%
  semi_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  left_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  #Add a counter for duplicates
  group_by(Trawl_id, Common_name, Sex, Length_cm) %>%
  mutate(Counter = row_number()) %>%
  ungroup()

#Now add ages to the length data. Not using the 'by' parameters automatically
#joines on common variables
nwfsc_tri_all <- nwfsc_tri_length %>%
  left_join(
    nwfsc_tri_age
  ) %>%
  transmute(
    event_id = Trawl_id,
    survey_id = "Triennial",
    weight_kg = Weight_kg,
    age = Age,
    length_cm = Length_cm,
    species_id,
    sex = Sex,
    sex = case_when(
      sex == c('M') ~ "male",
      sex == c('F') ~ "female",
      sex == c('U') ~ "unknown"
    ),
    common_name = tolower(Common_name),
    lat = Latitude_dd,
    lon = Longitude_dd
  ) %>%
  filter(
    !if_all(c(length_cm, weight_kg, sex, age), is.na)
  ) %>%
  as.data.frame()

#Combine original biological data with that from the triennial (Canada and US)
biol_data_all_US <- bind_rows(biol_data, nwfsc_triCA, nwfsc_tri_all) 


##
#Add the PBS trawl data
##

#Synaptic trawl biodata. Downloaded from:
#https://github.com/DFO-NOAA-Pacific/surveyjoin-db/issues/27
pbs_trawl <- readRDS("data/pbs-all-samples-2025-07-25.rds")

pbs_trawl <- pbs_trawl %>%
  mutate(scientific_name = tolower(species_science_name)) %>%
  semi_join(haul, by = c("fishing_event_id" = "event_id")) %>%
  left_join(haul %>% select("event_id", lat_end, lon_end), 
            by = c("fishing_event_id" = "event_id")) %>%
  semi_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  left_join(
    species %>% mutate(scientific_name = tolower(scientific_name)),
    by = "scientific_name"
  ) %>%
  transmute(
    event_id = fishing_event_id,
    survey_id = survey_abbrev,
    species_id,
    length_cm = total_length,
    sex,
    age,
    weight_kg = weight / 1000,
    sex = case_when(
      sex == "male" ~ "male",
      sex == "female" ~ "female",
      is.na(sex) ~ "unknown"
    ),
    common_name = tolower(sub(".*\\[(.*?)\\,.*", "\\1", common_name)),
    lat = lat_end,
    lon = lon_end
  ) %>%
  filter(
    !if_all(c(length_cm, weight_kg, sex, age), is.na)
  )

##
#Add the non-trawl PBS data
#First need to download DwC-A files for inside and outside surveys
##

library(finch)
source(here("code", "get_dwca_bio.R"))

pbs_out <- dwca_read(here("data", "dwca-hbll-out-n-v3.0.zip"), read = TRUE) %>%
  get_dwca_bio()

pbs_in <- dwca_read(here("data", "dwca-hbll-in-n-v3.0.zip"), read = TRUE) %>%
  get_dwca_bio()

biol_data_all <- bind_rows(biol_data_all_US, pbs_trawl, pbs_out, pbs_in) 
#saveRDS(biol_data_all, "data/biol_data_ALL.rds")


