### This script will read in the 3 tables: study, pulse, record
### Each table will be cleaned for errors and downstream matching
### Only select variables will be kept in record table
### Initial figures that summarize the types of data

library(tidyverse)
library(lubridate)


# Import data
dataIN_study = read.csv("./data_raw/Combined_study-source_info.csv", 
                        header = TRUE, skip = 1,
                        na.strings = "")
dataIN_pulse = read.csv("./data_raw/Combined_pulse-plot_info.csv", 
                        header = TRUE, skip = 1,
                        na.strings = "")
dataIN_record = read.csv("./data_raw/Combined_record_info.csv", 
                         header = TRUE, skip = 1,
                         na.strings = "")[,1:17] # Remove extraneous empty columns

##### Clean study table #####
d_study <- dataIN_study %>%
  rename(Elevation.m = Elevation..m.,
         MAT.C = MAT..oC.,
         MAP.mm = MAP..mm.,
         percent.C4 = X.C4) %>%
  mutate(Paper.ID = substr(Study.ID, 1L, 3L), # extract first character
         Soil.texture = ifelse(Soil.texture == "sandy loams", 
                               "sandy loam", # Combine identical variables
                               Soil.texture),
         Vegetation.type = ifelse(Vegetation.type == "annual grassland", 
                               "grassland", # Combine identical variables
                               Vegetation.type),
         Vegetation.type = ifelse(Vegetation.type == "riperian", 
                                  "riparian", # Fix typo
                                  Vegetation.type)) %>%
  relocate(Paper.ID)

d_study <- d_study[rowSums(is.na(d_study)) != ncol(d_study), ] # remove extra rows

##### Clean pulse table, join with study table #####
d_pulse <- dataIN_pulse %>%
  rename(percent.C4 = X.C4,
         Plant.biomass.cover.LAI = Plant.biomass..cover.LAI)

d_pulse <- d_pulse[rowSums(is.na(d_pulse)) != ncol(d_pulse), ] # remove extra rows

# Split into two tables w/o overlapping study IDs
for(i in 1:nrow(d_pulse)){
  d_pulse$n.Study.vars[i] <- sum(!is.na(d_pulse[i,13:27]))
}

# d_pulse1 has no pulse-level covariates (study level)
d_pulse1 <- d_pulse %>%
  filter(n.Study.vars == 0) %>%
  select(-c(13:27)) %>% # remove site variable columns
  left_join(d_study, by= c("Study.ID")) %>% # Combine pulse1 and study table
  rename(Notes.pulse = Notes.x, 
         Notes.site = Notes.y) %>%
  mutate(Rooting.depth = NA, 
         Rooting.depth.units = NA, 
         Planting.date = NA, 
         Established.or.planted = NA,
         Soil.pH = as.numeric(Soil.pH)) # Add columns to match d_pulse2

# d_pulse2 has pulse-level covariates
temp_d_study <- subset(d_study, select = -c(Soil.texture, Soil.C.N, Soil.C.content, 
                                            Soil.organic.matter.content, Soil.pH,
                                            Plant.biomass.cover.LAI, Plant.bio.cov.LAI.units,
                                            Dominant.species, Species.diversity,
                                            Species.diversity.type, percent.C4)) # remove duplicates for combining

d_pulse2 <- d_pulse %>%
  filter(n.Study.vars != 0) %>%
  left_join(temp_d_study, by= c("Study.ID")) %>% # Combine pulse2 and study table
  rename(Notes.pulse = Notes.x, 
         Notes.site = Notes.y) %>%
  mutate(Soil.C.content = as.character(Soil.C.content),
         Soil.organic.matter.content = as.character(Soil.organic.matter.content),
         Plant.biomass.cover.LAI = as.character(Plant.biomass.cover.LAI))

##### Combine d_pulse1 (site level) and d_pulse2 (plot level) #####

# Check all column names identical between two tables
ind <- c()
for(i in 1:length(colnames(d_pulse1))) {
  ind[i] <- which(colnames(d_pulse2) == colnames(d_pulse1)[i])
}

# Join tables
d_pulse <- full_join(d_pulse1, d_pulse2) 


##### Clean record table, join with pulse/study table #####
# drop variable names we don't need yet for the prelim analysis 
# (see 11/3 /21 meeting notes)
# If the newVariable column has NAs, fill in with Variable column
d_record <- dataIN_record %>%
  #filter(!Variable %in% c("RH", "VPD", "Air temp", "observed leaf delta18O", "
   #                       observed evaporation delta18O", "observed ET delta18O",
   #                       "modelled T delta18O", "modelled evaporation delta18O", 
   #                       "observed leaf delta18O", "Euclidean distance from orgin",
    #                      "Euclidean distance", "Cumulative euclidean distance", 
    #                      "ci", "modelled evaporation")) %>%
  mutate(varType = case_when(Variable %in% c("rsoil", "rhetero", "rauto", "LN(rsoil)") ~ "belowgroundR",
                             Variable %in% c("rd") ~ "abovegroundR",
                             Variable %in% c("reco") ~ "ecosystemR",
                             Variable %in% c("npp", "nee") ~ "NPP",
                             Variable %in% c("gpp", "gee") ~ "GPP",
                             Variable %in% c("anet (photosynthesis)") ~ "Anet",
                             Variable %in% c("et") ~ "ET",
                             Variable %in% c("wue") ~ "WUE",
                             Variable %in% c("gs", "stomatal conductance") ~ "Gs",
                             Variable %in% c("soil water potential") ~ "SWP",
                             Variable %in% c("plant water potential") ~ "PWP",
                             Variable %in% c("VWC", "soil water content") ~ "SWC",
                             Variable %in% c("stem sapflow velocity", "sap velocity", 
                                             "root sap velocity", "lateral root sapflow velocity", 
                                             "primary sinker root sapflow velocity", "t (transpiration)") ~ "T"),
         varType = ifelse(is.na(varType), Variable, varType)) %>%
  drop_na(Study.ID)# drop rows with Study.ID NAs (gets rid of extra rows) 

# Join d_record with combined pulse table
d_all <- d_record %>%
  left_join(d_pulse, by = c("Study.ID", "Pulse.ID"))


# Create folder for cleaned data if it does not already exist
if(!file.exists("data_clean")) { dir.create("data_clean")} 

write.csv(d_all, file = "data_clean/Clean_record_info.csv",
          row.names = FALSE)


