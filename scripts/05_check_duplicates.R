### This script will read in 2 versions of the 3 tables: study, pulse, record
### Tables will be compared to test for differences in person-to-person collection


library(tidyverse)


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