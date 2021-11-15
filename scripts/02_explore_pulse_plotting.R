# Explore the pulse responses

library(readr)
library(dplyr)
library(ggplot2)

# Read in record-level data
d_record <- read_csv("data_clean/Clean_record_info.csv")
str(d_record)


# Convert pulse timing units to days
count(d_record, Time.relative.to.pulse.unit)

d_record <- d_record %>%
  mutate(time.days = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                               Time.relative.to.pulse.unit == "hr" ~ Time.relative.to.pulse/24,
                               Time.relative.to.pulse.unit == "min" ~ Time.relative.to.pulse/(24*60)))

hist(d_record$time.days)

# Confirm that each study and pulse and variable combo has multiple measurements
foo <- count(d_record, Study.ID, Pulse.ID, Variable)
goo <- count(d_record, Study.ID, Pulse.ID)
d_pulse_max <- d_record %>%
  group_by()
