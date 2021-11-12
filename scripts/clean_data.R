### This script will input the combined pulse meta-analysis extraction csv files
### and create figures that summarize the collection

setwd("~/Documents/Emma/NAU/Pulse_Meta") # set to your working directory


library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(lubridate)


# Import data
dataIN_study = read.csv("Combined_study-source_info.csv", header = TRUE, skip = 1)
dataIN_pulse = read.csv("Combined_pulse-plot_info.csv", header = TRUE, skip = 1)
dataIN_record = read.csv("Combined_record_info.csv", header = TRUE, skip = 1)


# Make paper column for counting
d_study <- dataIN_study %>% mutate(paper = substr(Study.ID,1L,3L)) # extract first character

total_papers <- unique(d_study$paper)
length(total_papers)

# Make vegetation count graph

d_veg_count <- d_study %>%
  count(Vegetation.type)
d_veg_count <- d_veg_count[-1,]
d_veg_count <- d_veg_count %>%
  mutate()

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

path_out = "~/Documents/Emma/NAU/Pulse_Meta/graphs/" # set save path

ggsave2("p_veg.png", plot = p_veg, path = path_out)

### Count natural and experimental pulses

length(dataIN_pulse$Pulse.ID) #682 (687 - blanks)

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

path_out = "~/Documents/Emma/NAU/Pulse_Meta/graphs/" # set save path

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
d_record <-subset(dataIN_record, 
                  Variable!="RH" & Variable!="VPD" & Variable!="Air temp" & Variable!="observed leaf delta18O"
                  & Variable!="observed evaporation delta18O" & Variable!="observed ET delta18O" & Variable!="modelled T delta18O"
                  & Variable!="modelled evaporation delta18O" & Variable!="observed leaf delta18O" & Variable!="Euclidean distance from orgin"
                  & Variable!="Euclidean distance" & Variable!="Cumulative euclidean distance" & Variable!="biomass increment"
                  & Variable!="ci" & Variable!="modelled evaporation")

# Combine variable names in a new row
d_record <- d_record %>%
  mutate(newVariable = case_when(Variable %in% c("rsoil", "rhetero", "rauto", "LN(rsoil)") ~ "belowgroundR",
                                 Variable %in% c("rd") ~ "abovegroundgroundR",
                                 Variable %in% c("reco") ~ "ecosystemR",
                                 Variable %in% c("npp", "nee") ~ "NPP",
                                 Variable %in% c("gpp", "gee") ~ "GPP",
                                 Variable %in% c("gs", "stomatal conductance") ~ "stomatal conductance",
                                 Variable %in% c("VWC", "soil water content") ~ "soil water content",
                                 Variable %in% c("stem sapflow velocity", "sap velocity", "root sap velocity" , 
                                                 "lateral root sapflow velocity", "primary sinker root sapflow velocity", "t (transpiration)") ~ "T"))
        # should we also combine anet and PAR?

# If the newVariable column has NAs, fill in with Variable column
d_record$newVariable <- ifelse(is.na(d_record$newVariable), d_record$Variable, d_record$newVariable )


(var_names <- unique(d_record$newVariable)) # check the new variable names

# Make a new variable count plot
d_var_count <- d_record %>%
  count(newVariable)

(p_var <- ggplot(d_var_count) +
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



