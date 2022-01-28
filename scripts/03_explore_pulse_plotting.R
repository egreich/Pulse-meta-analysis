# Explore the pulse responses

library(readr)
library(dplyr)
library(ggplot2)
library(ggdist)

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
single <- count(d_record, Study.ID, Pulse.ID, Variable) %>%
  filter(n == 1) # only 2 studies for which there were only a single measurement of a response var
# soil water content and rsoil

# exclude those records for which only a single measurement was available, also NA values
d_record2 <- d_record %>%
  anti_join(single) %>%
  filter(!is.na(Mean))

# Calculate the maximum size of the response 
# excepting plant water potential and gpp which are negative
d_pulse_max <- d_record %>%
  group_by(Study.ID, Pulse.ID, Variable, Soil.texture, Vegetation.type, MAP) %>%
  summarize(max_val = max(Mean)) %>%
  filter(max_val > 0)

# Join d_record2 and d_pulse_max, excluding variables with negative max values
d_record3 <- d_record2 %>%
  right_join(d_pulse_max) %>%
  mutate(rescale_Mean = Mean/max_val)

# Plot
d_record3 %>%
  filter(!is.na(newVariable)) %>%
  ggplot(aes(x = time.days, y = rescale_Mean)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~newVariable, scales = "free") +
  theme_bw()

# Try standardizing rather than scaling by the max
d_record4 <- d_record2 %>%
  group_by(Study.ID, Pulse.ID, Variable, Soil.texture, Vegetation.type, MAP) %>%
  summarize(rescale_Mean = scale(Mean),
            newVariable = unique(newVariable),
            time.days = time.days)

d_record4 %>%
  filter(!is.na(newVariable)) %>%
  ggplot(aes(x = time.days, y = rescale_Mean)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~newVariable, scales = "free") +
  theme_bw()

# Color by soil type
imp_var <- c("stomatal conductance", "anet (photosynthesis)","T", "ecosystem R", "plant water potential", "wue", "soil water potential") # narrow down variables

d_record4 %>%
  filter(newVariable %in% imp_var) %>%
  ggplot(aes(x = time.days, y = rescale_Mean, color = Soil.texture)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~newVariable, scales = "free")

# Color by veg type
d_record4 %>%
  filter(newVariable %in% imp_var) %>%
  ggplot(aes(x = time.days, y = rescale_Mean, color = Vegetation.type)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~newVariable, scales = "free")

# Color by avg precip
d_record4 %>%
  filter(newVariable %in% imp_var) %>%
  ggplot(aes(x = time.days, y = rescale_Mean, color = MAP)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~newVariable, scales = "free")

# Raincloud plot of # study IDs per variable per time
d_record4 %>%
  filter(!is.na(newVariable)) %>%
  ggplot(aes(x = newVariable, y = time.days)) +
  ggdist::stat_halfeye(
                       alpha = 0.35,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 1.9, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(width = .12) +
  theme(panel.background = element_rect(fill="white"),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(size = 12, colour="black", 
                                 angle = 30, vjust = 1, hjust = 1))

