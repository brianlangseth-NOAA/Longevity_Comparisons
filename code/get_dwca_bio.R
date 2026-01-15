#Function to take a data file read in by finch and outputs 
#a single flat file with necessary biological data

get_dwca_bio <- function(dwca){
  
  #Load datasets within the dwca structure. 
  #Event: gives location
  #ExtendMeasurement: gives measurements of samples and fishing
  #Occurence: gives species present
  
  temp_event <- dwca$data$event.txt %>%
    select(id, eventID, datasetName, decimalLatitude, decimalLongitude)
  
  if(nrow(temp_event) != sum(temp_event$id == temp_event$eventID)){
    warning("Event id does not equal id. Check event data")
  }
  
  temp_measure <- dwca$data$extendedmeasurementorfact.txt %>%
    dplyr::select(id, occurrenceID, measurementType, measurementValue, measurementUnit) %>%
    dplyr::filter(grepl("fork length|sex|age|weight", measurementType))
  
  temp_occur <- dwca$data$occurrence.txt %>% 
    mutate(species_id = as.numeric(stringr::str_extract(scientificNameID, "\\d+"))) %>%
    dplyr::filter(!is.na(occurrenceID)) %>%
    dplyr::select(id, occurrenceID, eventID, occurrenceStatus, species_id, vernacularName)
  
  if(nrow(temp_occur) != sum(temp_occur$id == temp_occur$eventID)){
    warning("Occurrance event id does not equal id. Check occurance data")
  }
  
  #Combine all data sets together and pivot wider to get multiple measurements
  #(that were separate rows) together in different columns for the same record (to do
  #this have to ensure no other column has unique values - so remove measurementUnit)
  temp_data <- temp_measure %>% select(!measurementUnit) %>%
    left_join(temp_occur, by = c("occurrenceID", "id")) %>%
    left_join(temp_event, by = c("id", "eventID")) %>%
    pivot_wider(names_from = measurementType, values_from = measurementValue) %>%
    mutate(length = as.numeric(`Length (fork length) of biological entity specified elsewhere`),
           weight = as.numeric(`Specimen weight of biological entitiy specified elsewhere`),
           age = as.numeric(`Specimen age of biological entity specified elsewhere`))
  
  data <- temp_data %>% 
    transmute(
      event_id = eventID,
      survey_id = case_when(
        grepl("Outside", datasetName) ~ "HBLL OUT",
        grepl("Inside", datasetName) ~ "HBLL INS",
        is.na(datasetName) ~ "unknown"
      ),
      species_id,
      length_cm = length / 10,
      age,
      weight_kg = weight / 1000,
      sex = case_when(
        sex == "Male" ~ "male",
        sex == "Female" ~ "female",
        is.na(sex) ~ "unknown"
      ),
      common_name = tolower(vernacularName),
      lat = decimalLatitude,
      lon = decimalLongitude
    )
  
  return(data)
  
}