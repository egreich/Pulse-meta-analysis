# Populate matrix of overlapping parameters

library(readr)
library(dplyr)
library(ggplot2)

# Read in record-level data
d_record <- read_csv("data_clean/Clean_record_info.csv")
str(d_record)
unique(d_record$Variable)

single <- count(d_record, Study.ID, Pulse.ID, Variable) %>%
  filter(n == 1)
