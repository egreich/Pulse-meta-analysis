# Export clean subsets of data to models folders as .Rdata for ingestion

library(readr)
library(dplyr)
library(udunits2)
library(ggplot2)

# Read in data
d <- read_csv("./data_clean/Clean_record_info.csv")

#### Select ET (evapotranspiration) ####
et <- d %>%
  filter(varType == "ET") %>%
  # Fix an Excel typo in Units, label unitDuration, convert to Days since pulse
  mutate(Units = case_when(grepl("mm day-", Units) ~ "mm day-1",
                         grepl("mmol m-2 sec-1", Units) ~ "mmol m-2 sec-1",
                         grepl("mmol m-2 d-1", Units) ~ "mmol m-2 d-1"),
         unitDuration = case_when(grepl("mm day-1", Units) ~ "integrated",
                                  grepl("mmol m-2 sec-1", Units) ~ "instantaneous",
                                  grepl("mmol m-2 d-1", Units) ~ "integrated"),
         Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                            Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day"))) %>%
  # Remove Study.ID 907, response not measured until 30 days after pulse, no before measurements
  # Remove Study.ID 910, Fig. 5 parameters averaged, not by pulse
  filter(Study.ID != 907,
         Study.ID != 910)

# Check units of response and time
unique(et$Units)
count(et, Units)

unique(et$Time.relative.to.pulse.unit)
unique(et$SD.type)

# Summarize by pulse
# Use -1 or 0 measurement as control
et_pulse <- et %>%
  group_by(Study.ID, Source.file.name, Pulse.ID) %>%
  summarize(initialDay = min(Days.relative.to.pulse),
            controlMean = Mean[which.min(Days.relative.to.pulse)],
            controlSD = SD[which.min(Days.relative.to.pulse)])

# Study 855_3 pulse 1 has missing ET value for -1 days since pulse
# Emma to re-extract and add

# Merge et_pulse back to ET
# Calculate LRR for each day
et2 <- et %>%
  left_join(et_pulse, by = c("Study.ID", "Pulse.ID", "Source.file.name")) %>%
  mutate(LRR = log(Mean/controlMean),
         poolVar = case_when(SD.type == "SD" ~ ((SD^2 + controlSD^2)/(2*N)*(1/Mean^2 + 1/controlMean^2))))

# Sanity check
et2 %>%
  ggplot(aes(x = Days.relative.to.pulse,
             y = LRR, 
             color = Study.ID)) +
  geom_point() +
  scale_x_continuous(limits = c(-1, 5))

# Save to models/01-test-Ricker
save(et2, file = "models/01-test-Ricker/input.Rdata")
