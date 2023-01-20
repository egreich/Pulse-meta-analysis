# Export clean subsets of data to models folders as .Rdata for ingestion

library(readr)
library(dplyr)
library(udunits2)
library(ggplot2)
library(purrr)
# Create a "not in" function
`%nin%` <- negate(`%in%`)

# Read in data
d <- read_csv("./data_clean/Clean_record_info.csv")


######## Create a function to clean variables the same way ########

# NOTE: will need to add to/check Units in first code chunk for all variables

clean_vars <- function(dfin, varname){
  
  # to test uncomment
  #c("ET", "WUE", "T", "Gs", "PWP","ecosystemR", "abovegroundR", "belowgroundR","NPP", "GPP", "Anet")
  dfin = d
  varname = "belowgroundR"
  df1 <- dfin %>%
    filter(varType == varname)
  unique(df1$Units)
  # WUE has all same units
  # T has any different units, sometimes proportion "mmol m-2 sec-1"    "cm h-1"    "mm day-1"     "mmol m-2 d-1"      "umol g-1 s-1"      "proportion of max"  "l day-1"  
  # Gs "mmol m-2 sec-1" "mol m-2 sec-1"  "mmol g-1 s-1" 
  # GPP "g m-2 day-1"    "umol m-2 sec-1"
  # PWP "MPa"
  # ecosystemR "umol m-2 sec-1" "mg m-2 h-1"     "g m-2 day-1"  
  # abovegroundR "umol m-2 sec-1"
  # belowgroundR "umol m-2 sec-1" "mg m-2 h-1"     "g m-2 hr-1"     NA
  
  df1 %>% group_by(Units) %>% summarise(min = min(Mean, na.rm = TRUE), max = max(Mean,  na.rm = TRUE),
                                        n = length(unique(Study.ID)))
  foo <- filter(df1, Units == "g m-2 h-1") 
  hist(foo$Mean)
  
  ##### Select var of interest #####
  df1 <- dfin %>%
    filter(varType == varname) %>%
    # label unitDuration, convert to Days since pulse
    mutate(unitDuration = case_when(grepl("^mm day-1$", Units) ~ "integrated",
                                    grepl("mmol m-2 sec-1", Units) ~ "instantaneous",
                                    grepl("mmol m-2 d-1", Units) ~ "integrated",
                                    grepl("^g m-2 day-1$", Units) ~ "integrated",
                                    grepl("^umol m-2 sec-1$", Units) ~ "instantaneous"),
           Mean = case_when(grepl("^mol m-2 sec-1$", Units) ~ Mean,
                             grepl("^mmol m-2 sec-1$", Units) ~ Mean,
                             grepl("^mmol g-2 s-1$", Units) ~ Mean),
           Units = "mmol m-2 sec-1",
           Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                              Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  
  if(varname == "ET"){
    df1 <- dfin %>%
      filter(varType == varname) %>%
      # label unitDuration, convert to Days since pulse
      mutate(unitDuration = case_when(grepl("^mm day-1$", Units) ~ "integrated",
                                      grepl("mmol m-2 sec-1", Units) ~ "instantaneous",
                                      grepl("mmol m-2 d-1", Units) ~ "integrated"),
             Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                                Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  }
  if(varname == "GPP"){
    df1 <- dfin %>%
      filter(varType == varname) %>%
      # label unitDuration, convert to Days since pulse
      mutate(unitDuration = case_when(grepl("^g m-2 day-1$", Units) ~ "integrated",
                                      grepl("^umol m-2 sec-1$", Units) ~ "instantaneous"),
             Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                                Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  }
  if(varname == "Gs"){
    # Removing paper 964_1 and 964_2 that reported mass-based stomatal conductance
    df1 <- dfin %>%
      filter(varType == varname, Study.ID %nin% c("964_1", "964_2")) %>%
      # label unitDuration, convert to Days since pulse
      mutate(unitDuration = "instantaneous",
             Mean = case_when(grepl("^mol m-2 sec-1$", Units) ~ ud.convert(Mean, "mol", "mmol"),
                              grepl("^mmol m-2 sec-1$", Units) ~ Mean),
             Units = "mmol m-2 sec-1",
             Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                                Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  }
  if(varname == "PWP"){
    df1 <- dfin %>%
      filter(varType == varname) %>%
      # label unitDuration, convert to Days since pulse
      mutate(unitDuration = "instantaneous",
             Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                                Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  }
  if(varname == "ecosystemR"){
    df1 <- dfin %>%
      filter(varType == varname) %>%
      # label unitDuration, convert to Days since pulse
      # Assume hourly rate holds constant, convert to daily value
      mutate(unitDuration = case_when(grepl("^mg m-2 h-1$", Units) ~ "integrated",
                                      grepl("^g m-2 day-1$", Units) ~ "integrated",
                                      grepl("^umol m-2 sec-1$", Units) ~ "instantaneous"),
             Mean = case_when(grepl("^mg m-2 h-1$", Units) ~ ud.convert(Mean, "mg h^-1", "g d^-1"),
                              grepl("^g m-2 day-1$", Units) ~ Mean,
                              grepl("^mmol m-2 sec-1$", Units) ~ Mean),
             Units = case_when(grepl("^mg m-2 h-1$", Units) ~ "g m-2 day-1",
                               grepl("^g m-2 day-1$", Units) ~ Units,
                               grepl("^umol m-2 sec-1$", Units) ~ Units),
             Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                                Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  }
  if(varname == "abovegroundR"){
    df1 <- dfin %>%
      filter(varType == varname) %>%
      # label unitDuration, convert to Days since pulse
      mutate(unitDuration = "instantaneous",
             Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                                Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  }
  if(varname == "belowgroundR"){
    df1 <- dfin %>%      
      filter(varType == varname) %>%
      # label unitDuration, convert to Days since pulse
      # Assume hourly rate holds constant, convert to daily value
      mutate(unitDuration = case_when(grepl("^mg m-2 h-1$", Units) ~ "integrated",
                                      grepl("^g m-2 hr-1$", Units) ~ "integrated",
                                      grepl("^umol m-2 sec-1$", Units) ~ "instantaneous"),
             Mean = case_when(grepl("^mg m-2 h-1$", Units) ~ ud.convert(Mean, "mg", "g"),
                              grepl("^g m-2 hr-1$", Units) ~ Mean,
                              grepl("^umol m-2 sec-1$", Units) ~ Mean),
             Units = case_when(grepl("^mg m-2 h-1$", Units) ~ "g m-2 h-1",
                               grepl("^g m-2 hr-1$", Units) ~ "g m-2 h-1",
                               grepl("^umol m-2 sec-1", Units) ~ Units),
             Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                                Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day")))
  }
  
  # Check units of response and time
  #unique(df1$Units)
  #count(df, Units)
  
  #unique(df1$Time.relative.to.pulse.unit)
  #unique(df1$SD.type)
  
  # Summarize by pulse
  # Use -1 or 0 measurement as control
  df1_pulse <- df1 %>%
    group_by(Study.ID, Source.file.name, Pulse.ID) %>%
    summarize(initialDay = min(Days.relative.to.pulse),
              controlMean = Mean[which.min(Days.relative.to.pulse)],
              controlSD = SD[which.min(Days.relative.to.pulse)])
  
  #### Merge df1_pulse back to df1 ####
  # Calculate LRR for each day
  df2 <- df1 %>%
    left_join(df1_pulse, by = c("Study.ID", "Pulse.ID", "Source.file.name")) %>%
    mutate(LRR = log(Mean/controlMean),
           poolVar = case_when(SD.type == "SD" ~ ((SD^2 + controlSD^2)/(2*N)*(1/Mean^2 + 1/controlMean^2)),
                               SD.type == "SE" ~ ((SD^2 + controlSD^2)/2*(1/Mean^2 + 1/controlMean^2))))
  
  # Sanity check
  #df2 %>%
  #  ggplot(aes(x = Days.relative.to.pulse,
  #             y = LRR, 
  #             color = Study.ID)) +
  #  geom_point()
  
  
  ##### Check how many studies have the variable of interest and SWC #####
  pulse <- dfin %>%
    filter(varType %in% c("soil water content volumetric", "soil water content gravimetric", "soil water content unknown"),
           Study.ID %in% df2$Study.ID) 
  
  #length(unique(pulse$Study.ID))
  
  #pulse %>%
  #  group_by(Study.ID)%>%
  #  summarize(n = n())
  #pulse %>%
  #  group_by(Study.ID, Pulse.ID)%>%
  #  summarize(n = n())
  # 3 sites, 19 pulses with both ET and SWC 
  
  
  #### Create a pre-SWC as a new column ####
  
  # Check unique units and depths by SWC type
  #dfin %>%
  #  filter(varType %in% c("soil water content volumetric", "soil water content gravimetric", "soil water content unknown"),
  #         Study.ID %in% df2$Study.ID) %>%
  #  group_by(Study.ID, varType, Units, soilm.depth, soilm.depth.units) %>%
  #  summarise(n = n(), mean.swc = mean(Mean))
  
  df_swc <- dfin %>%
    filter(varType %in% c("soil water content volumetric", "soil water content gravimetric", "soil water content unknown"),
           Study.ID %in% df2$Study.ID) %>%
    mutate(Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                              Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day"))) %>%
    group_by(Study.ID, Source.file.name, Pulse.ID, varType, Units, soilm.depth, soilm.depth.units) %>%
    summarize(initialDay.swc = min(Days.relative.to.pulse),
              preSWC = Mean[which.min(Days.relative.to.pulse)],
              preSWC.SD = SD[which.min(Days.relative.to.pulse)]) %>%
    rename(SWCunit = Units, SWCtype = varType) %>%
    ungroup()
  
  df3 <- df2 %>%
    left_join(df_swc, by = c("Study.ID", "Pulse.ID", "Source.file.name"))
  
  #### Create a preVar as a new column ####
  df_y <- d %>%
    filter(varType == varname,
           Study.ID %in% df2$Study.ID) %>%
    mutate(Days.relative.to.pulse = case_when(Time.relative.to.pulse.unit == "day" ~ Time.relative.to.pulse,
                                              Time.relative.to.pulse.unit == "hr" ~ ud.convert(Time.relative.to.pulse, "hr", "day"))) %>%
    group_by(Study.ID, Source.file.name, Pulse.ID) %>%
    summarize(initialDay.var = min(Days.relative.to.pulse),
              preVar = Mean[which.min(Days.relative.to.pulse)],
              preVar.SD = SD[which.min(Days.relative.to.pulse)]) %>%
    ungroup()
  
  df3 <- df3 %>%
    left_join(df_y, by = c("Study.ID", "Pulse.ID", "Source.file.name"))
  
  return(df3)
}

### Use the function to clean each variable of interest ###

# Important variables
variables <- c("ET", "WUE", "T", "Gs", "PWP",
               "ecosystemR", "abovegroundR", "belowgroundR",
               "NPP", "GPP", "Anet")

out_list <- list()
for(v in variables){
  out_list[[v]] <- clean_vars(d, v)
}

#test <- as.data.frame(out_list[2])

# Save to models/01-test-Ricker
save(out_list, file = "models/01-test-Ricker/model_input.Rdata")


