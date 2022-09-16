# Export clean subsets of data to models folders as .Rdata for ingestion

library(readr)
library(dplyr)
library(udunits2)
library(ggplot2)

# Read in data
d <- read_csv("./data_clean/Clean_record_info.csv")

##### Select ET (evapotranspiration) #####
et <- d %>%
  filter(varType == "ET") %>%
  # Fix an Excel typo in Units, label unitDuration, convert to Days since pulse
  mutate(Units = case_when(grepl("^mm$", Units, ) ~ "mm",# to avoid some units turning to NA when they shouldn't
                           grepl("mm day-", Units) ~ "mm day-1",
                           grepl("mmol m-2 sec-1", Units) ~ "mmol m-2 sec-1",
                           grepl("mmol m-2 d-1", Units) ~ "mmol m-2 d-1"),
         unitDuration = case_when(grepl("mm day-1", Units) ~ "integrated",
                                  grepl("mmol m-2 sec-1", Units) ~ "instantaneous",
                                  grepl("mmol m-2 d-1", Units) ~ "integrated"),
         Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                            Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))


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

# Merge et_pulse back to ET
# Calculate LRR for each day
et2 <- et %>%
  left_join(et_pulse, by = c("Study.ID", "Pulse.ID", "Source.file.name")) %>%
  mutate(LRR = log(Mean/controlMean),
         poolVar = case_when(SD.type == "SD" ~ ((SD^2 + controlSD^2)/(2*N)*(1/Mean^2 + 1/controlMean^2)),
                             SD.type == "SE" ~ ((SD^2 + controlSD^2)/2*(1/Mean^2 + 1/controlMean^2))))

# Sanity check
et2 %>%
  ggplot(aes(x = Days.relative.to.pulse,
             y = LRR, 
             color = Study.ID)) +
  geom_point()


##### Check how many studies have ET and SWC #####
pulse <- d %>%
  filter(varType %in% c("soil water content volumetric", "soil water content gravimetric", "soil water content unknown"),
         Study.ID %in% et2$Study.ID) 

length(unique(pulse$Study.ID))

pulse %>%
  group_by(Study.ID, Pulse.ID) %>%
  summarize(n = n())
# only 4 sites, 19 pulses with SWC data -> 3 sites after more data cleaning


# Merge et_pulse back to ET
# Calculate LRR for each day
et2 <- et %>%
  left_join(et_swc, by = c("Study.ID", "Pulse.ID", "Source.file.name")) %>%
  mutate(LRR = log(Mean/controlMean),
         poolVar = case_when(SD.type == "SD" ~ ((SD^2 + controlSD^2)/(2*N)*(1/Mean^2 + 1/controlMean^2)),
                             SD.type == "SE" ~ ((SD^2 + controlSD^2)/2*(1/Mean^2 + 1/controlMean^2))))

#### Create a pre-SWC as a new column

# Check unique units and depths by SWC type
d %>%
  filter(varType %in% c("soil water content volumetric", "soil water content gravimetric", "soil water content unknown"),
         Study.ID %in% et2$Study.ID) %>%
  group_by(Study.ID, varType, Units, soilm.depth, soilm.depth.units) %>%
  summarise(n = n(), mean.swc = mean(Mean))

et_swc <- d %>%
  filter(varType %in% c("soil water content volumetric", "soil water content gravimetric", "soil water content unknown"),
         Study.ID %in% et2$Study.ID) %>%
  mutate(Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                            Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day"))) %>%
  group_by(Study.ID, Source.file.name, Pulse.ID, varType, Units, soilm.depth, soilm.depth.units) %>%
  summarize(initialDay.swc = min(Days.relative.to.pulse),
            preSWC = Mean[which.min(Days.relative.to.pulse)],
            preSWC.SD = SD[which.min(Days.relative.to.pulse)]) %>%
  rename(SWCunit = Units, SWCtype = varType)

et3 <- et2 %>%
  left_join(et_swc, by = c("Study.ID", "Pulse.ID", "Source.file.name"))


# Save to models/01-test-Ricker
save(et3, file = "models/01-test-Ricker/inputET.Rdata")
