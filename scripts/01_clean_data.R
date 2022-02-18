### This script will read in the 3 tables: study, pulse, record
### Each table will be cleaned for errors and downstream matching
### Only select variables will be kept in record table
### Initial figures that summarize the types of data

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
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

#
#temp <- dataIN_study %>%
#  group_by(Study.ID) %>%
#  summarize( n = n())


# Clean study table
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


# Notes:
# for the overlapping papers, people input data slightly differently
# so we probably don't want to just go with the first name
# but for now, this could work for testing the code
#d_study <- d_study[!duplicated(d_study$Study.ID), ] # remove extra records (will have to do this in records and pulse table)
                                                    # but the pulse table might be ok if we use distinct()

# Clean pulse table
d_pulse <- dataIN_pulse %>%
  rename(percent.C4 = X.C4)

d_pulse <- d_pulse[rowSums(is.na(d_pulse)) != ncol(d_pulse), ] # remove extra rows

# Split into two tables w/o overlapping study IDs
for(i in 1:nrow(d_pulse)){
  d_pulse$n.Study.vars[i] <- sum(!is.na(d_pulse[i,13:27]))
}


temp <- d_pulse %>%
  group_by(Study.ID)%>%
  summarize(un = unique(n.Study.vars))

# duplicate d_pulse

# d_pulse1 has no pulse-level covariates (study level)
d_pulse1 <- d_pulse[d_pulse$n.Study.vars == 0, ]
d_pulse1 <- d_pulse1[,-c(13:27) ] # remove site variable columns
d_pulse1 <- left_join(d_pulse1, d_study, by= c("Study.ID")) # Combine pulse1 and study table

# d_pulse2 has pulse-level covariates
d_pulse2 <- d_pulse[d_pulse$n.Study.vars != 0, ] %>%
  rename(Plant.biomass.cover.LAI = Plant.biomass..cover.LAI)

temp_d_study <- subset(d_study, select = -c(Soil.texture, Soil.C.N, Soil.C.content, Soil.organic.matter.content, Soil.pH,
                             Plant.biomass.cover.LAI, Plant.bio.cov.LAI.units, Dominant.species, Species.diversity,
                             Species.diversity.type, percent.C4)) # remove duplicates for combining

d_pulse2 <- left_join(d_pulse2, temp_d_study, by= c("Study.ID")) %>% # Combine pulse2 and study table
  rename(Notes.pulse = Notes.x, Notes.site = Notes.y)

d_pulse2$Soil.C.content <- as.numeric(d_pulse2$Soil.C.content) # this was messing up combining the tables
d_pulse2$Soil.organic.matter.content<- as.character(d_pulse2$Soil.organic.matter.content)
d_pulse2$Plant.biomass.cover.LAI<- as.character(d_pulse2$Plant.biomass.cover.LAI)

# Clean up d_pulse1 so column order and name are identical  to d_pulse2
d_pulse1 <- d_pulse1 %>%
  rename(Notes.pulse = Notes.x, Notes.site = Notes.y)%>%
  mutate(Rooting.depth = NA, Rooting.depth.units = NA, Planting.date = NA, Establish.or.planted = NA)
d_pulse1 <- d_pulse1[, c(1,2,3,4,5,6,7,8,9,10,11,12, 42,43,44,45, 30, 55,56, 36,37,38,39,40,41, 57, 58, 
                         13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29, 31,32,33,34,35, 46,47,48,49,50,51,52,53,54)] # Reorder columns

# this was messing up combining the tables
d_pulse1$Soil.C.content <- gsub("%", "", d_pulse1$Soil.C.content)
d_pulse1$Soil.organic.matter.content <- gsub("%", "", d_pulse1$Soil.organic.matter.content)
d_pulse1$Soil.C.content <- as.numeric(d_pulse1$Soil.C.content)
d_pulse1$Soil.pH <- as.numeric(d_pulse1$Soil.pH )

# Rbind

test <- full_join(d_pulse1, d_pulse2) # works?

d_pulse_study <- rbind(d_pulse1, d_pulse2) # does not work


# Currently, still duplicate pulse numbers for study 150 (all done by one person), but some entries are more
# complete than others, so this is likely just an duplicate entry mistake. We can probably use a
# slightly edited version of distinct() to fix this, but should check the record data for duplicates first

# Clean record table

# Remove duplicate entries from people overlapping on papers
# distinct Study.ID, Pulse.ID, Time.relative.to.pulse


# Combine variable names that are the same, 
# drop variable names we don't need yet for the prelim analysis 
# (see 11/3 /21 meeting notes)
# If the newVariable column has NAs, fill in with Variable column
d_record <- dataIN_record %>%
  filter(!Variable %in% c("RH", "VPD", "Air temp", "observed leaf delta18O", "observed evaporation delta18O", "observed ET delta18O",
                          "modelled T delta18O", "modelled evaporation delta18O", "observed leaf delta18O", "Euclidean distance from orgin",
                          "Euclidean distance", "Cumulative euclidean distance", "biomass increment", "ci", "modelled evaporation")) %>%
  mutate(newVariable = case_when(Variable %in% c("rsoil", "rhetero", "rauto", "LN(rsoil)") ~ "belowgroundR",
                                 Variable %in% c("rd") ~ "abovegroundgroundR",
                                 Variable %in% c("reco") ~ "ecosystemR",
                                 Variable %in% c("npp", "nee") ~ "NPP",
                                 Variable %in% c("gpp", "gee") ~ "GPP",
                                 Variable %in% c("gs", "stomatal conductance") ~ "stomatal conductance",
                                 Variable %in% c("VWC", "soil water content") ~ "soil water content",
                                 Variable %in% c("stem sapflow velocity", "sap velocity", "root sap velocity" , 
                                                 "lateral root sapflow velocity", "primary sinker root sapflow velocity", "t (transpiration)") ~ "T"),
         newVariable = ifelse(is.na(newVariable), Variable, newVariable)) %>%
  drop_na(Study.ID)# drop rows with Study.ID NAs (gets rid of extra rows) 
        
# should we also combine anet and PAR?





# Save the cleaned data as .Rdata 
# Create folder for cleaned data if it does not already exist
if(!file.exists("data_clean")) { dir.create("data_clean")} 

write.csv(d_record, file = "data_clean/Clean_record_info.csv",
          row.names = FALSE)


#### Initial graphs
if(!file.exists("plots")) { dir.create("plots")} # create plots folder if it doesn't exist
path_out = "./plots/" # set save path

### Make vegetation count graph
d_veg_count <- d_study %>%
  count(Vegetation.type)

# The parentheses here just automatically call the plot
# similar to just typing p_veg after
(p_veg <- ggplot(d_veg_count) +
    geom_bar(aes(x=n, y=Vegetation.type), position="dodge", stat = "identity") +
    labs(title = NULL, y = NULL, x = "count") +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text=element_text(size=9),
          text = element_text(size=12),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black", angle = 90),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_veg.png", plot = p_veg, path = path_out) # save veg type count plot

### Make variable count graph
d_var_count <- dataIN_record %>%
  count(Variable)

(p_var <- ggplot(d_var_count) +
    geom_bar(aes(x=n, y=Variable), position="dodge", stat = "identity") +
    labs(title = NULL, y = "variables", x = "measurements") +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text=element_text(size=9),
          text = element_text(size=12),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black", angle = 90),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_var.png", plot = p_var, path = path_out)

### Make a new variable count plot
d_var_count <- d_record %>%
  count(newVariable)


(p_var_filtered <- ggplot(d_var_count) +
    geom_bar(aes(x=n, y=newVariable), position="dodge", stat = "identity") +
    labs(title = NULL, y = "variables", x = "measurements") +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text=element_text(size=9),
          text = element_text(size=12),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black", angle = 90),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_var_filtered.png", plot = p_var_filtered, path = path_out)