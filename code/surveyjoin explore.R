##
#Explore the surveyjoin package, including install and determining what 
#is available. Intent is to build on for presentation at NSAW
#
#By: Brian Langseth
#
#surveyjoin is available at: https://dfo-noaa-pacific.github.io/surveyjoin/
##

# install.packages("pak")
#pak::pkg_install("DFO-NOAA-Pacific/surveyjoin")
library(surveyjoin)
library(here)

#Download and store locally
surveyjoin::cache_data()
surveyjoin::load_sql_data()

#Find available species
species <- data.frame(get_species())

#Looks like only have 55 species


##
#Look at surveyjoin-db packages which is the extension of surveyjoin
#available at: https://github.com/DFO-NOAA-Pacific/surveyjoin-db
#
#A pull request by Nityamm seems to incorporate biological data
#Use the link to google drive from Eric via email to download the necessary data files
#
#This only includes AK and NWFSC data so looks like I will need PBS data (link in email from Eric)
##

#Need to include triennial and triennial canada data:
#UPDATE: DONE in biol_data.R

#Need PBS non-trawl data: 
# https://obis.org/dataset/563f7733-16c7-4069-903a-392665f69593
# https://portal.obis.org/dataset/d6dc7351-aae6-4ed1-8f75-5b81e356408a

#Need PBS trawl data:
# https://github.com/DFO-NOAA-Pacific/surveyjoin-db/issues/27


#Rest of code is moved to separate R script: biol_data