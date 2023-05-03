### This script will read in 2 versions of the 3 tables: study, pulse, record
### Tables will be compared to test for differences in person-to-person collection


library(tidyverse)
library(gridExtra)


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

# Import duplicate data
dataIN_study_dup = read.csv("./data_raw/Combined_study-source_duplicate.csv", 
                        header = TRUE, skip = 1,
                        na.strings = "")
dataIN_pulse_dup = read.csv("./data_raw/Combined_pulse-plot_duplicate.csv", 
                        header = TRUE, skip = 1,
                        na.strings = "")
dataIN_record_dup = read.csv("./data_raw/Combined_record_duplicate.csv", 
                         header = TRUE, skip = 1,
                         na.strings = "")[,1:17] # Remove extraneous empty columns


# Pick out studies
studoi <- dataIN_study_dup$Study.ID # studies of interest
studoi <- studoi[1:4]

d_study_dup <- dataIN_study_dup %>% # Need to fix shifted columns manually
  filter(Study.ID %in% studoi)

d_pulse_dup <- dataIN_pulse_dup %>%
  filter(Study.ID %in% studoi)

d_record_dup <- dataIN_record_dup %>%
  filter(Study.ID %in% studoi)

d_study <- dataIN_study %>% # Need to fix shifted columns manually
  filter(Study.ID %in% studoi)

d_pulse <- dataIN_pulse %>%
  filter(Study.ID %in% studoi)

d_record <- dataIN_record %>%
  filter(Study.ID %in% studoi)
d_record$Mean <- as.numeric(d_record$Mean)


# Combine data frames in a way to make it convenient to graph and compare
  # x columns = original data and y columns = duplicate data
d_study2 <- full_join(d_study, d_study_dup, by = "Study.ID")
d_pulse2 <- full_join(d_pulse, d_pulse_dup, by = "Study.ID", "Pulse.ID")
d_record2 <- full_join(d_record, d_record_dup, by = c("Study.ID", "Pulse.ID"))


# Explore graphs

(p1 <- d_record %>%
  filter(Study.ID == studoi[4]) %>%
  ggplot() +
  geom_point(aes(x = Time.relative.to.pulse, y = Mean, color = "org")))
  
(p2 <- d_record_dup %>%
  filter(Study.ID == studoi[4]) %>%
  ggplot() +
  geom_point(aes(x = Time.relative.to.pulse, y = Mean, color = "dup")))

grid.arrange(p1,p2, nrow = 2)


