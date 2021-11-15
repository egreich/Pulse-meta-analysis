### This script will input the combined pulse meta-analysis extraction csv files
### create figures that summarize the collection
### and select variables for prelim analysis


library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(lubridate)


# Import data
dataIN_study = read.csv("./data_raw/Combined_study-source_info.csv", header = TRUE, skip = 1)
dataIN_pulse = read.csv("./data_raw/Combined_pulse-plot_info.csv", header = TRUE, skip = 1)
dataIN_record = read.csv("./data_raw/Combined_record_info.csv", header = TRUE, skip = 1)


# Make paper column for counting
d_study <- dataIN_study %>% 
  mutate(paper = substr(Study.ID, 1L, 3L), # extract first character
         Vegetation.type = ifelse(Vegetation.type == "", 
                                  NA, 
                                  Vegetation.type)) # Categorize blanks as NA

total_papers <- unique(d_study$paper)
length(total_papers)

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

if(!file.exists("plots")) { dir.create("plots")} # create plots folder if it doesn't exist
path_out = "./plots/" # set save path

ggsave2("p_veg.png", plot = p_veg, path = path_out) # save veg type count plot

### Count natural and experimental pulses
length(which(is.na(dataIN_pulse$Pulse.ID))) # 5 NAs
length(which(!is.na(dataIN_pulse$Pulse.ID))) # 682 pulse IDs

d_pulse_nat <- dataIN_pulse$Pulse.type[which(dataIN_pulse$Pulse.type == "Natural")] #168
d_pulse_exp <- dataIN_pulse$Pulse.type[which(dataIN_pulse$Pulse.type == "Experimental/applied")] #504
# 672 total, some were not filled out


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


### Here I made a map of the lat and long points, but some of the points don't seem real/ need to be checked

#library(sf)

#locations <- d_study %>%
#  select(lat = Latitude, long = Longitude)

# remove 30, 39, 40 because they don't seem real
#locations <- locations[-c(30,39,40),]
#locations <-  locations[complete.cases(locations [,1:2]),]

#my_sf <- st_as_sf(locations, coords = c('long', 'lat'))

#my_sf <- st_set_crs(my_sf, crs = 4326)
                  
#Plot it:
                  
#ggplot(my_sf) + 
#  geom_sf()

#plot(locations$long, locations$lat)


### Combine variable names that are the same, 
### drop variable names we don't need yet for the prelim analysis 
### (see 11/3 /21 meeting notes)

# Drop rows with variables we don't care about right now
# Combine variable names in a new row
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
         newVariable = ifelse(is.na(newVariable), Variable, newVariable))
        
  # should we also combine anet and PAR?

(var_names <- unique(d_record$newVariable)) # check the new variable names

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

# Save the filtered variables
# Create folder for cleaned data if it does not already exist
if(!file.exists("data_clean")) { dir.create("data_clean")} 

write.csv(d_record, file = "data_clean/Clean_record_info.csv",
          row.names = FALSE)
