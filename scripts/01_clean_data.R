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

# Clean pulse table
# d_pulse1 has no pulse-level covariates (study level)
# d_pulse2 has pulse-level covariates
d_pulse2 <- dataIN_pulse %>%
  filter()

### Combine variable names that are the same, 
### drop variable names we don't need yet for the prelim analysis 
### (see 11/3 /21 meeting notes)
### If the newVariable column has NAs, fill in with Variable column
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