# Populate matrix of overlapping parameters

library(readr)
library(dplyr)
library(ggplot2)

# Read in record-level data
d_clean <- read_csv("data_clean/Clean_record_info.csv")
str(d_clean)
unique(d_clean$Variable)

single <- count(d_clean, Study.ID, Pulse.ID, Variable) %>%
  filter(n == 1)
