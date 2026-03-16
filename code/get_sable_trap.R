#Function to reformat data from sablefish trap survey located at:
# - https://open.canada.ca/data/en/dataset/813ff561-b38d-4241-b370-0a17c60976af
# - https://open.canada.ca/data/dataset/016035c7-dbcd-4559-bf54-d2658d00f4c2 
#into a single flat file with necessary biological data
#
#Input is the folder location were the data reside
#Output is reformatted biology file with lat/long from effort file

get_sable_trap <- function(folder){
  
  if(!dir.exists(here("data", folder))) {
    warning("Folder does not exist. Recheck name")
  }
  
  area_name = grep("Sablefish|Survey|EN", unlist(strsplit(folder, "_")), 
                   value = TRUE, invert = TRUE)
  
  bio_file <- grep("biology", value = TRUE, list.files(path = here("data", folder)))
  #catch_file <- grep("catch", value = TRUE, list.files(path = here("data", folder)))
  effort_file <- grep("effort", value = TRUE, list.files(path = here("data", folder)))
  
  bio <- read.csv(here("data", folder, bio_file), header = TRUE)
  #catch <- read.csv(here("data", folder, catch_file), header = TRUE)
  effort <- read.csv(here("data", folder, effort_file), header = TRUE)
  
  temp_data <- bio %>%
    left_join(effort %>% 
                select(Trip.identifier, Start.longitude, End.longitude, 
                       Start.latitude, End.latitude, Set.number), 
              by = c("Trip.identifier", "Set.number"))
  
  data <- temp_data |>
    transmute(
      event_id = Trip.identifier,
      year = Survey.Year,
      survey_id = paste("Trap", area_name),
      species_id = as.numeric(stringr::str_extract(LSID, "\\d+")),
      length_cm = Fork.length..mm. / 10, #Measurements with total length have no ages
      age = Age,
      weight_kg = Weight..g. / 1000,
      sex = case_when(
        Sex == 1 ~ "male",
        Sex == 2 ~ "female",
        Sex %in% c(0,3) ~ "unknown",
        is.na(Sex) ~ "unknown"
      ),
      common_name = tolower(English.common.name),
      lat = (Start.latitude + End.latitude)/2,
      lon = (Start.longitude + End.longitude)/2
    )
  
  return(data)
  
}