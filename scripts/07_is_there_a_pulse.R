### Save coda output in convenient format for clustering analysis

library(dplyr)
library(tidyr)
library(jagsUI)
library(ggforce)
library(ggh4x) # for facet_nested
library(cowplot)
# Load self-made functions
source("./scripts/functions.R")


### Format output and combine table with study and pulse-level data we care about

# Load cleaned data
load("models/model_input.Rdata") # out_list

variables <- c("ET", "T", "Gs", "PWP",
               "ecosystemR","belowgroundR",
               "NPP", "GPP", "Anet")

# Format study and pulse-level data we want in the context of the model output
pulse_table <- list()
for( i in 1:length(variables)){
  
  dfin <- out_list[[i]]
  
  # remove pulses outside of range, restrict to 14 days after pulse, remove pre-pulse days
  dfin <- dfin %>%
    filter(Days.relative.to.pulse > -1) %>%
    filter(Days.relative.to.pulse <= 14)
  
  
  # Create study_pulse combination, create integer sID, pull study and pulse-level variables we care about
  pulse_table[[i]] <- dfin %>%
    expand(nesting(Study.ID, Pulse.ID, varType, MAT.C.wc, MAP.mm.wc, elev.m.wc, unitDuration, Vegetation.type, Sample.unit, Pulse.type, Pulse.amount.mm, preVar)) %>%
    mutate(sID = as.numeric(factor(Study.ID))) %>%
    arrange(Study.ID) %>%
    tibble::rownames_to_column() %>%
    rename(pID = rowname) %>%
    mutate(pID = as.numeric(pID)) %>%
    relocate(pID, .after = sID) %>%
    relocate(sID, pID)
  
}


modelfolders <- c("01-Ricker-simple", "02-Linear-simple", "03-Selection-simple", "04-Mixture-simple", "04-Mixture-simple")
modeltypes <- c("ricker", "linear", "selection", "mixture", "mixture_fixed_w")
model_table <- list()
model_table_longer <- list()
df_model_longer <- list()
df_all <- list()

for(i in 1:length(variables)){ # variable loop
  print(paste("i:",i,sep=""))
  var <- variables[i]
  
  for(j in 1:5){ # model loop
    print(paste("j:",j,sep=""))
    modeltype <- modeltypes[j]
    
    # Load jagsUI object
    if(j!=5){
      load(paste("models/", modelfolders[j], "/coda/jm_coda_", var, ".RData", sep="")) # jagsui
    }else if(j==5){
      load(paste("models/", modelfolders[j], "/coda/jm_coda_fixed_w_", var, ".RData", sep="")) # jagsui
    }

    
    model_table_longer[[j]] <- data.frame(ID = get_index(jagsui), varType = var, modeltype = modeltype, # identifiers
                                          #param = names(do.call(c, jagsui$mean)), # parameter names
                                          param = gsub('[[:digit:]]+', '', names(do.call(c, jagsui$mean))), # parameter names
                                          mean = do.call(c, jagsui$mean), 
                                          pc2.5 = do.call(c, jagsui$q2.5), pc97.5 = do.call(c, jagsui$q97.5),
                                          overlap0 = do.call(c, jagsui$overlap0),
                                          gel = do.call(c, jagsui$Rhat))
  }
  
  df_model_longer[[i]] <- bind_rows(model_table_longer) # # combine list into df of all models for one variable

}



### dataframe of all models and variables connected to site- and pulse-level data, 
### dataframe of all models and variables in more of a coda-style format, more convenient for graphing
df_output_longer <- bind_rows(df_model_longer) # convert list into a dataframe

### df_output_longer pulse-level parameters connected to pulse-level data
pulse_table2 <- bind_rows(pulse_table)
df_output_longer2 <- df_output_longer %>%
  filter(param %in% c("t.peak","y.peak","bb", "mm","w","S","Dsum","Dsump")) %>%
  rename(pID = ID) %>%
  mutate(converged = ifelse(gel<1.4, "yes", "no"))
  
# # remove w's for selection model
# row_index <- c()
# for(i in c(1:nrow(df_output_longer2))){
#   if(df_output_longer2$param[i]=="w"){
#     if(df_output_longer2$modeltype[i]=="selection"){
#       row_index[i] <- i
#     }else{
#       row_index[i] <- NA
#     }
#   }else{
#     row_index[i] <- NA
#   }
# }
# row_index <- na.omit(row_index)
# df_output_longer2 <- df_output_longer2 %>% slice(-c(row_index))

pulse_table2 <- right_join(pulse_table2, df_output_longer2, by=c("pID","varType"))

# R2 explicit column
# tempR <- df_output_longer %>% 
#   filter(param=="R") %>%
#   rename(R_mean = mean, R_mean = mean, R_pc2.5 = pc2.5, R_pc97.5 = pc97.5) %>%
#   select(varType, modeltype, R_mean, R_pc2.5, R_pc97.5)
# 
# pulse_table2 <- left_join(pulse_table2, tempR, by=c("varType","modeltype"))


### plot

pulse_table2 %>%
  ggplot() +
  geom_point(aes(x=pID, y=param, color= converged), alpha=0.5, size=1) +
  facet_grid(modeltype~varType, scales="free") +
  #scale_color_continuous(type = "viridis") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text=element_text(size=12),
        text = element_text(size=12),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6, hjust = 1),
        #legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

pulse_table2 %>%
filter(param %in% c("w", "S")) %>%
ggplot() +
  geom_point(aes(x=pID, y=param, color= converged), alpha=0.5, size=1) +
  facet_grid(modeltype~varType, scales="free") +
  #scale_color_continuous(type = "viridis") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text=element_text(size=12),
        text = element_text(size=12),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6, hjust = 1),
        #legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

pulse_table2 %>%
  filter(modeltype %in% c("mixture")) %>%
  filter(param %in% c("w")) %>%
  ggplot() +
  geom_point(aes(x=pID, y=mean, color= converged), alpha=0.5, size=1) +
  facet_grid(param~varType, scales="free_x") +
  #scale_color_continuous(type = "viridis") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text=element_text(size=12),
        text = element_text(size=12),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6, hjust = 1),
        #legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

p <- pulse_table2 %>%
  filter(modeltype == "mixture_fixed_w") %>%
  filter(param %nin% c("Dsum", "Dsump")) %>%
  ggplot(aes(x=pID, y=mean)) +
  geom_pointrange(aes(ymin=pc2.5, ymax=pc97.5), position = position_dodge(width = 1), fatten = .5, alpha=.5) +
  facet_grid(param ~ varType, scales="free") +
  #scale_color_continuous(type = "viridis") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text=element_text(size=12),
        text = element_text(size=12),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6, hjust = 1),
        plot.title = element_text(hjust = 0.5))
p
ggsave2("output_means.png", plot = p, path = "./plots/", width = 8, height = 6)

p <- pulse_table2 %>%
  filter(modeltype == "mixture_fixed_w") %>%
  filter(param %in% c("bb", "mm", "y.peak")) %>%
  ggplot(aes(x=pID, y=mean)) +
  geom_pointrange(aes(ymin=pc2.5, ymax=pc97.5), position = position_dodge(width = 1), fatten = .5, alpha=.5) +
  facet_grid(param ~ varType, scales="free") +
  #scale_color_continuous(type = "viridis") +
  ylim(-15,15) +
  theme_bw() +
  theme(legend.position = "right",
        legend.text=element_text(size=12),
        text = element_text(size=12),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave2("output_means_cropped.png", plot = p, path = "./plots/", width = 8, height = 6)

################## Look at semi-deterministic mixture model
################## Here we will also group to look at study/pulse-specific variables
################## reorganizing for "post-analysis"

pulse_table_final <- pulse_table2 %>%
  filter(modeltype %in% c("mixture_fixed_w")) %>%
  mutate(varGroup = ifelse(varType %in% c("ET", "T", "Gs", "PWP"), "water", "carbon")) %>%
  mutate(Sample.unit = ifelse(Study.ID==962, "individual", Sample.unit)) # Fill gaps: 962 is individual for Sample.unit

# Create table for - "Is there a pulse?"
df_all <- pulse_table_final %>%
  filter(param %in% c("w", "y.peak", "t.peak", "mm", "bb")) %>%
  dplyr::select(c(sID, pID, Study.ID, Pulse.ID, varType, varGroup, MAT.C.wc, MAP.mm.wc, unitDuration, Sample.unit, Pulse.type, Pulse.amount.mm, preVar, param, mean, pc2.5, pc97.5, overlap0)) %>%
  pivot_wider(names_from = param, values_from = c(mean, pc2.5, pc97.5, overlap0))

# Make NAs if the parameters are just pulling from the prior
df_all$mean_y.peak <- ifelse(df_all$mean_w==0, NA, df_all$mean_y.peak)
df_all$mean_t.peak <- ifelse(df_all$mean_w==0, NA, df_all$mean_t.peak)
df_all$mean_mm <- ifelse(df_all$mean_w==1, NA, df_all$mean_mm)
df_all$mean_bb <- ifelse(df_all$mean_w==1, NA, df_all$mean_bb)
df_all$pc2.5_y.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc2.5_y.peak)
df_all$pc2.5_t.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc2.5_t.peak)
df_all$pc2.5_mm <- ifelse(df_all$mean_w==1, NA, df_all$pc2.5_mm)
df_all$pc2.5_bb <- ifelse(df_all$mean_w==1, NA, df_all$pc2.5_bb)
df_all$pc97.5_y.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc97.5_y.peak)
df_all$pc97.5_t.peak <- ifelse(df_all$mean_w==0, NA, df_all$pc97.5_t.peak)
df_all$pc97.5_mm <- ifelse(df_all$mean_w==1, NA, df_all$pc97.5_mm)
df_all$pc97.5_bb <- ifelse(df_all$mean_w==1, NA, df_all$pc97.5_bb)

# Pulse response category rulese
# a) w CI above 0.5 - ricker - classic pulse response (code 1)
# b) w CI overlapping 0.5 - intermediate pulse response (code 2)
# c) w CI below 0.5 - linear with sig slope - linear pulse response (code 3)
# d) w CI below 0.5 - linear with non sig slope - no pulse response (code 4)
catlist <- c()
for(i in 1:nrow(df_all)){
  if(df_all$pc2.5_w[i] > 0.5 & df_all$pc97.5_w[i] > 0.5){ # a
    catlist[i] <- 1
  } else if(df_all$pc2.5_w[i] < 0.5 & df_all$pc97.5_w[i] > 0.5){ # b
    catlist[i] <- 2
  } else if(df_all$pc2.5_w[i] < 0.5 & df_all$pc97.5_w[i] < 0.5){ # c and d
    if(df_all$overlap0_mm[i] == F){ # sig slope
      catlist[i] <- 3
    } else if(df_all$overlap0_mm[i] == T){ # non sig slope
      catlist[i] <- 4
    }
    
  }
}
# create column for response category
df_all$response_cat <- catlist

# Save data to output folder
# To do: Turn 1s and 0 params into NAs for save file
# Include CIs in df_all
save(pulse_table_final, file = "data_output/pulse_table_final.Rdata")
save(df_all, file = "data_output/df_all.Rdata")


