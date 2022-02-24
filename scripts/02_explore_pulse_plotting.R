# Explore the pulse responses

library(readr)
library(dplyr)
library(ggplot2)
library(ggdist)

# Import data
d_clean = read.csv("./data_clean/Clean_record_info.csv", 
                        header = TRUE,
                        na.strings = "")
# Make select columns numeric
d_clean = d_clean %>%
  mutate(Pulse.ID = as.numeric(Pulse.ID),
         Time.relative.to.pulse = as.numeric(Time.relative.to.pulse),
         Mean = as.numeric(Mean),
         SD = as.numeric(SD),
         N = as.numeric(N),
         Duration.of.pulse = as.numeric(Duration.of.pulse),
         Pulse.amount = as.numeric(Pulse.amount),
         Elevation.m = as.numeric(Elevation.m),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         MAT.C = as.numeric(MAT.C),
         MAP.mm = as.numeric(MAP.mm))

## Create a folder for plots
if(!file.exists("plots")) { dir.create("plots")} # create plots folder if it doesn't exist
path_out = "./plots/" # set save path

### Make vegetation count graph
d_veg_count <- d_clean %>%
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
d_var_count <- d_clean %>%
  count(varType)

(p_var <- ggplot(d_var_count) +
    geom_bar(aes(x=n, y=varType), position="dodge", stat = "identity") +
    labs(title = NULL, y = "Variable Types", x = "measurements") +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text=element_text(size=9),
          text = element_text(size=12),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black", angle = 90),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_var.png", plot = p_var, path = path_out)

#####

# Convert pulse timing units to days
count(d_clean, Time.relative.to.pulse.unit)

d_clean <- d_clean %>%
  mutate(time.days = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                               Time.relative.to.pulse.unit == "hr" ~ Time.relative.to.pulse/24,
                               Time.relative.to.pulse.unit == "min" ~ Time.relative.to.pulse/(24*60)))

hist(d_clean$time.days)

# Confirm that each study and pulse and variable combo has multiple measurements
single <- count(d_clean, Study.ID, Pulse.ID, Variable) %>%
  filter(n == 1) # only 2 studies for which there were only a single measurement of a response var
# soil water content and rsoil

# exclude those records for which only a single measurement was available, also NA values
d_clean2 <- d_clean %>%
  anti_join(single) %>%
  filter(!is.na(Mean))

# Calculate the maximum size of the response 
# excepting plant water potential and gpp which are negative
d_pulse_max <- d_clean %>%
  group_by(Study.ID, Pulse.ID, Variable, Soil.texture, Vegetation.type, MAP.mm) %>%
  summarize(max_val = max(Mean)) %>%
  filter(max_val > 0)

# Join d_record2 and d_pulse_max, excluding variables with negative max values
d_clean3 <- d_clean2 %>%
  right_join(d_pulse_max) %>%
  mutate(rescale_Mean = Mean/max_val)

# Plot
d_clean3 %>%
  filter(!is.na(varType)) %>%
  ggplot(aes(x = time.days, y = rescale_Mean)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~varType, scales = "free") +
  theme_bw()

# Try standardizing rather than scaling by the max
d_clean4 <- d_clean2 %>%
  group_by(Study.ID, Pulse.ID, Variable, Soil.texture, Vegetation.type, MAP.mm) %>%
  summarize(rescale_Mean = scale(Mean),
            varType = unique(varType),
            time.days = time.days)

d_clean4 %>%
  filter(!is.na(varType)) %>%
  ggplot(aes(x = time.days, y = rescale_Mean)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~varType, scales = "free") +
  theme_bw()

# Color by soil type
imp_var <- c("stomatal conductance", "anet (photosynthesis)","T", "ecosystem R", "plant water potential", "wue", "soil water potential") # narrow down variables

d_clean4 %>%
  filter(varType %in% imp_var) %>%
  ggplot(aes(x = time.days, y = rescale_Mean, color = Soil.texture)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~varType, scales = "free")

# Color by veg type
d_clean4 %>%
  filter(varType %in% imp_var) %>%
  ggplot(aes(x = time.days, y = rescale_Mean, color = Vegetation.type)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~varType, scales = "free")

# Color by avg precip
d_clean4 %>%
  filter(varType %in% imp_var) %>%
  ggplot(aes(x = time.days, y = rescale_Mean, color = MAP.mm)) +
  geom_vline(xintercept = 0, color = "red") +
  geom_point(alpha = 0.25) +
  facet_wrap(~varType, scales = "free")

# Raincloud plot of # study IDs per variable per time
d_clean4 %>%
  filter(!is.na(varType)) %>%
  ggplot(aes(x = varType, y = time.days)) +
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

