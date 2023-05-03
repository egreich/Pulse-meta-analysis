### Determine which model fits which pulse the best

library(dplyr)
library(tidyr)
library(jagsUI)
library(ggforce)
library(ggh4x) # for facet_nested
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
    expand(nesting(Study.ID, Pulse.ID, varType, MAT.C.wc, MAP.mm.wc, elev.m.wc, unitDuration, Vegetation.type, Sample.unit, Pulse.type)) %>%
    mutate(sID = as.numeric(factor(Study.ID))) %>%
    arrange(Study.ID) %>%
    tibble::rownames_to_column() %>%
    rename(pID = rowname) %>%
    mutate(pID = as.numeric(pID)) %>%
    relocate(pID, .after = sID) %>%
    relocate(sID, pID)
  
}


modelfolders <- c("01-Ricker-simple", "02-Linear-simple", "03-Selection-simple", "04-Mixture-simple")
modeltypes <- c("ricker", "linear", "selection", "mixture")
model_table <- list()
model_table_longer <- list()
df_model_longer <- list()
df_all <- list()

for(i in 1:length(variables)){ # variable loop
  print(paste("i:",i,sep=""))
  var <- variables[i]
  
  for(j in 1:4){ # model loop
    print(paste("j:",j,sep=""))
    modeltype <- modeltypes[j]
    
    # Load jagsUI object
    load(paste("models/", modelfolders[j], "/coda/jm_coda_", var, ".RData", sep="")) # jagsui

      if(modeltype=="ricker"){
        model_table1 <- data.frame(pID = c(1:length(jagsui$mean$t.peak)), # to combine with pulse table
                                       t.peak_mean = jagsui$mean$t.peak,
                                       t.peak_pc2.5 = jagsui$q2.5$t.peak,
                                       t.peak_pc97.5 = jagsui$q97.5$t.peak,
                                       t.peak_gel = jagsui$Rhat$t.peak,
                                       y.peak_mean = jagsui$mean$y.peak,
                                       y.peak_pc2.5 = jagsui$q2.5$y.peak,
                                       y.peak_pc97.5 = jagsui$q97.5$y.peak,
                                       y.peak_gel = jagsui$Rhat$y.peak
                                       )
      }else if(modeltype=="selection"){
        model_table1 <- data.frame(pID = c(1:length(jagsui$mean$t.peak)), # to combine with pulse table
                                       t.peak_mean = jagsui$mean$t.peak,
                                       t.peak_pc2.5 = jagsui$q2.5$t.peak,
                                       t.peak_pc97.5 = jagsui$q97.5$t.peak,
                                       t.peak_gel = jagsui$Rhat$t.peak,
                                       y.peak_mean = jagsui$mean$y.peak,
                                       y.peak_pc2.5 = jagsui$q2.5$y.peak,
                                       y.peak_pc97.5 = jagsui$q97.5$y.peak,
                                       y.peak_gel = jagsui$Rhat$y.peak,
                                       mm_mean = jagsui$mean$mm,
                                       mm_pc2.5 = jagsui$q2.5$mm,
                                       mm_pc97.5 = jagsui$q97.5$mm,
                                       mm_gel = jagsui$Rhat$mm,
                                       bb_mean = jagsui$mean$bb,
                                       bb_pc2.5 = jagsui$q2.5$bb,
                                       bb_pc97.5 = jagsui$q97.5$bb,
                                       bb_gel = jagsui$Rhat$bb,
                                       S_mean = jagsui$mean$S,
                                       S_pc2.5 = jagsui$q2.5$S,
                                       S_pc97.5 = jagsui$q97.5$S,
                                       S_gel = jagsui$Rhat$S
                                       )
      }else if(modeltype=="mixture"){
        model_table1 <- data.frame(pID = c(1:length(jagsui$mean$t.peak)), # to combine with pulse table
                                   t.peak_mean = jagsui$mean$t.peak,
                                   t.peak_pc2.5 = jagsui$q2.5$t.peak,
                                   t.peak_pc97.5 = jagsui$q97.5$t.peak,
                                   t.peak_gel = jagsui$Rhat$t.peak,
                                   y.peak_mean = jagsui$mean$y.peak,
                                   y.peak_pc2.5 = jagsui$q2.5$y.peak,
                                   y.peak_pc97.5 = jagsui$q97.5$y.peak,
                                   y.peak_gel = jagsui$Rhat$y.peak,
                                   mm_mean = jagsui$mean$mm,
                                   mm_pc2.5 = jagsui$q2.5$mm,
                                   mm_pc97.5 = jagsui$q97.5$mm,
                                   mm_gel = jagsui$Rhat$mm,
                                   bb_mean = jagsui$mean$bb,
                                   bb_pc2.5 = jagsui$q2.5$bb,
                                   bb_pc97.5 = jagsui$q97.5$bb,
                                   bb_gel = jagsui$Rhat$bb,
                                   w_mean = jagsui$mean$w,
                                   w_pc2.5 = jagsui$q2.5$w,
                                   w_pc97.5 = jagsui$q97.5$w,
                                   w_gel = jagsui$Rhat$w
        )
      } else if(modeltype=="linear"){
        model_table1 <- data.frame(pID = c(1:length(jagsui$mean$mm)), # to combine with pulse table
                                   mm_mean = jagsui$mean$mm,
                                   mm_pc2.5 = jagsui$q2.5$mm,
                                   mm_pc97.5 = jagsui$q97.5$mm,
                                   mm_gel = jagsui$Rhat$mm,
                                   bb_mean = jagsui$mean$bb,
                                   bb_pc2.5 = jagsui$q2.5$bb,
                                   bb_pc97.5 = jagsui$q97.5$bb,
                                   bb_gel = jagsui$Rhat$bb
        )
      }

    
    colnames(model_table1) <- paste(colnames(model_table1),modeltypes[j],sep="_") # rename column names to be model-specific
    
    model_table[[j]] <- model_table1 # save out in list
    
    model_table_longer[[j]] <- data.frame(ID = get_index(jagsui), varType = var, modeltype = modeltype, # identifiers
                                          #param = names(do.call(c, jagsui$mean)), # parameter names
                                          param = gsub('[[:digit:]]+', '', names(do.call(c, jagsui$mean))), # parameter names
                                          mean = do.call(c, jagsui$mean), 
                                          pc2.5 = do.call(c, jagsui$q2.5), pc97.5 = do.call(c, jagsui$q97.5),
                                          overlap0 = do.call(c, jagsui$overlap0),
                                          gel = do.call(c, jagsui$Rhat))
  }
  
  df_model_longer[[i]] <- bind_rows(model_table_longer) # # combine list into df of all models for one variable
  
  df_model <- bind_cols(model_table) %>% # combine list into df of all models for one variable
    rename(pID = pID_ricker) %>%
    select(-c(pID_linear,pID_selection,pID_mixture))
  
  pulse_temp <- as.data.frame(pulse_table[[i]])
  
  df_all[[i]] <- left_join(pulse_temp, df_model, by = c("pID"))

}



### dataframe of all models and variables connected to site- and pulse-level data, 
# for clustering analysis and visualization
df_output <- bind_rows(df_all) # convert list into a dataframe

### dataframe of all models and variables in more of a coda-style format, more convenient for graphing
df_output_longer <- bind_rows(df_model_longer) # convert list into a dataframe

### df_output_longer pulse-level parameters connected to pulse-level data
pulse_table2 <- bind_rows(pulse_table)
df_output_longer2 <- df_output_longer %>%
  filter(param %in% c("t.peak","y.peak","bb", "mm","w","S")) %>%
  rename(pID = ID) %>%
  mutate(converged = ifelse(gel<1.1, "yes", "no"))
  
# remove w's for selection model
row_index <- c()
for(i in c(1:nrow(df_output_longer2))){
  if(df_output_longer2$param[i]=="w"){
    if(df_output_longer2$modeltype[i]=="selection"){
      row_index[i] <- i
    }else{
      row_index[i] <- NA
    }
  }else{
    row_index[i] <- NA
  }
}
row_index <- na.omit(row_index)
df_output_longer2 <- df_output_longer2 %>% slice(-c(row_index))

pulse_table2 <- right_join(pulse_table2, df_output_longer2, by=c("pID","varType"))

tempR <- df_output_longer %>% 
  filter(param=="R") %>%
  rename(R_mean = mean, R_mean = mean, R_pc2.5 = pc2.5, R_pc97.5 = pc97.5) %>%
  select(varType, modeltype, R_mean, R_pc2.5, R_pc97.5)

pulse_table2 <- left_join(pulse_table2, tempR, by=c("varType","modeltype"))

### Determine priors for mixture weights
list_combined <- c()
list_out <- list()
for(i in 1:length(variables)){ # variable loop
  print(paste("i:",i,sep=""))
  var <- variables[i]
  
  temp_linear <- pulse_table2 %>%
    filter(varType == var) %>%
    filter(modeltype == "linear") %>%
    select(R_mean)
  
  list_linear <- temp_linear$R_mean
  
  temp_ricker <- pulse_table2 %>%
    filter(varType == var) %>%
    filter(modeltype == "ricker")%>%
    select(R_mean)
  
  list_ricker <- temp_linear$R_mean
  
  for(j in 1:length(list_linear)){
    list_combined[j] <- ifelse(list_linear[j]>list_ricker[j], list_linear[j], list_ricker[j])
  }
  
  list_out[[i]] <- list_combined
 
  }

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
  filter(modeltype != "selection") %>%
  ggplot(aes(x=pID, y=mean, color=modeltype)) +
  geom_pointrange(aes(ymin=pc2.5, ymax=pc97.5), position = position_dodge(width = 1), fatten = .5, alpha=.5) +
  facet_grid(param ~ varType, scales="free") +
  #scale_color_continuous(type = "viridis") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text=element_text(size=12),
        text = element_text(size=12),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6, hjust = 1),
        plot.title = element_text(hjust = 0.5))

pulse_table2 %>%
  filter(modeltype %in% c("mixture","selection")) %>%
  filter(param %in% c("w", "S")) %>%
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












