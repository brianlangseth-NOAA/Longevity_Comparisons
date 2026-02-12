#Function to modify scientific names for complexes in the NWFSC combo survey data
#Input is the nwfsc_combo data set
#Output is a modified data set so scientific names match what is in the 
#species data set so that these data aren't removed when use semi_join

modify_combo_complex_names <- function(data){
  
  #Based on the combo survey data set
  
  # [1] "rougheye and blackspotted rockfish"
  # [2] "rock sole unident."                
  # [3] "sandpaper skate"                   
  # [4] "calico rockfish"                   
  # [5] "rockfish unident."                 
  # [6] "Sunset rockfish"                   
  # [7] "vermilion and sunset rockfish"     
  # [8] "hybrid tanner crab"                
  # [9] "vermilion and canary rockfish"     
  # [10] "blue / deacon rockfish"            
  # [11] "fish unident." 
  
  #Update "rougheye and blackspotted rockfish"
  rows <- grep("rougheye", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "Sebastes aleutianus" #was Sebastes sp. (aleutianus / melanostictus)
  
  "rock sole unident."
  rows <- grep("rock sole", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "Lepidopsetta bilineata" #was Lepidopsetta
  
  # "sandpaper skate"
  # rows <- grep("sandpaper", data$Common_name)
  # data[rows, "Scientific_name"] <- "bathyraja interrupta" #was Bathyraja kincaidii
  # #The species dataset lists these together though the combo data does not have 
  # #berring skate, only sandpaper skate. Because of that, keep sandpaper as kincaidii. 
  
  # "calico rockfish"
  # rows <- grep("calico", data$Common_name)
  # data[rows, "Scientific_name"]
  # grep("calico", species$common_name)
  # #There is no calico common name in the species dataset, keep as is. 
  
  # "rockfish unident."
  # "fish unident."
  # rows <- grep("unident", data$Common_name)
  # table(data[rows, "Scientific_name"])
  # #No way to identify these so keep as is. 
  
  "sunset rockfish"
  "vermilion and sunset rockfish"
  rows <- grep("sunset", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "sebastes miniatus" #was Sebastes sp. (miniatus / crocotulus)
  
  # "hybrid tanner crab"
  # #Dont care about this one
  
  "vermilion and canary rockfish"
  rows <- grep("vermilion", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "sebastes miniatus" #Sebastes sp. (miniatus / pinniger)
  
  "blue / deacon rockfish"
  rows <- grep("deacon", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "sebastes diaconus" #Sebastes Sebastes sp. (mystinus / diaconus)
  rows <- grep("blue", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "sebastes diaconus" #Sebastes Sebastes sp. (mystinus / diaconus)
  
  return(data)
  
}


modify_tri_complex_names <- function(data){
  
  #Based on the tri canada survey data set
  
  # [1] "rock sole unident."                
  # [2] "rougheye and blackspotted rockfish"
  
  #and based on the tri length data set
  # [1] "sandpaper skate"                   
  # [2] "rougheye and blackspotted rockfish"
  # [3] "salmon unident."                   
  # [4] "calico rockfish"                   
  # [5] "rockfish unident."                 
  # [6] "rock sole unident." 
  
  #and based on the tri age data set
  # [1] "rougheye and blackspotted rockfish"
  
  #Update "rougheye and blackspotted rockfish"
  rows <- grep("rougheye", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "Sebastes aleutianus" #was Sebastes sp. (aleutianus / melanostictus)
  
  "rock sole unident."
  rows <- grep("rock sole", data$Common_name, ignore.case = TRUE)
  data[rows, "Scientific_name"] <- "Lepidopsetta bilineata" #was Lepidopsetta
  
  # For non-Canada triennial data, the remaining species are ignored for the same
  # reasons as are provided for the combo survey data in modify_combo_complex_names
  
  return(data)
  
}


modify_pbs_complex_names <- function(data){
  
  #Based on the pbs trawl survey data set
  
  # [1] "sebastes aleutianus/melanostictus complex"               
  
  #Update "sebastes aleutianus/melanostictus complex"
  rows <- grep("aleutianus", data$species_science_name, ignore.case = TRUE)
  data[rows, "species_science_name"] <- "SEBASTES ALEUTIANUS" #was SEBASTES ALEUTIANUS/MELANOSTICTUS COMPLEX
  
  return(data)
  
}
