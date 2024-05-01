### Determine what w's should be stochastic and what should not be

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

### dataframe of all models and variables in more of a coda-style format
df_output_longer <- bind_rows(df_model_longer) # convert list into a dataframe

### df_output_longer pulse-level parameters connected to pulse-level data
df_output_longer2 <- df_output_longer %>%
  filter(param %in% c("t.peak","y.peak","bb", "mm","w","S","Dsum","Dsump")) %>%
  rename(pID = ID) %>%
  mutate(converged = ifelse(gel<1.2, "yes", "no"))

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

# Mixture model "rules":
#1) All 5 parameters converged -> weights stochastic (for that pulse).
#2) Not all parameters converge: (2a) if ricker parameter converge (ypeak, tpeak), but not linear (mm or bb), then set w = 1 (ricker only).
#(2b) if linear parameters (mm, bb) converge, set w = 0 (only use linear).
#3) if 4 or 5 parameters don't converge, but w has clear pattern (always near 1 or near 0), set w to it's preferred values (0 or 1).
#4) set w according to best model selected Dsum and/or -2loglikelihood (based on evaluating linear-only and ricker-only models).

w_list <- list(list(),list(),list(),list(),list(),list(),list(),list(),list())
names(w_list) <- variables
for(i in 1:length(variables)){ # variable loop
  print(paste("i:",i,sep=""))
  var <- variables[i]
  
  df_output_longer_var <- df_output_longer2 %>%
    filter(varType==var)
  
  for(j in 1:length(unique(df_output_longer_var$pID))){ # pulse loop
    
    df_mix<- df_output_longer_var %>%
      filter(modeltype=="mixture") %>%
      filter(pID == j)
    
    df_lin<- df_output_longer_var %>%
      filter(modeltype=="linear") %>%
      filter(pID == j)
    
    df_ric<- df_output_longer_var %>%
      filter(modeltype=="ricker") %>%
      filter(pID == j)
    
    #1) All 5 parameters converged -> weights stochastic (for that pulse).
    if(df_mix$converged[which(df_mix$param=="t.peak")]=="yes" &
       df_mix$converged[which(df_mix$param=="y.peak")]== "yes" &
       df_mix$converged[which(df_mix$param=="mm")]== "yes" &
       df_mix$converged[which(df_mix$param=="bb")]== "yes"){
      w_list[[i]][[j]] <- NA #df_mix$mean[which(df_mix$param=="w")] #NA
      #2) Not all parameters converge:
      #(2a) if ricker parameter converge (ypeak, tpeak), but not linear (mm or bb), then set w = 1 (ricker only). 
    }else if(df_mix$converged[which(df_mix$param=="t.peak")]=="yes" &
             df_mix$converged[which(df_mix$param=="y.peak")]== "yes" &
             (df_mix$converged[which(df_mix$param=="mm")]== "no" ||
              df_mix$converged[which(df_mix$param=="bb")]== "no")){
      w_list[[i]][[j]] <- 1
      #(2b) if linear parameters (mm, bb) converge, set w = 0 (only use linear).  
    }else if((df_mix$converged[which(df_mix$param=="t.peak")]=="no" ||
              df_mix$converged[which(df_mix$param=="y.peak")]== "no") &
             df_mix$converged[which(df_mix$param=="mm")]== "yes" &
             df_mix$converged[which(df_mix$param=="bb")]== "yes"){
      w_list[[i]][[j]] <- 0
      #3) if 4 or 5 parameters don't converge, but w has clear pattern (always near 1 or near 0)
      # this is typically t.peak and bb both not converging
    }else if(count_f(df_mix$converged[which(df_mix$param=="t.peak")]=="yes",
                     df_mix$converged[which(df_mix$param=="y.peak")]== "yes",
                     df_mix$converged[which(df_mix$param=="mm")]== "yes",
                     df_mix$converged[which(df_mix$param=="bb")]== "yes")>=2){
      #4) set w according to best model selected Dsum and/or -2loglikelihood (based on evaluating linear-only and ricker-only models).
      if(df_ric$mean[which(df_ric$param=="Dsump")]<df_lin$mean[which(df_lin$param=="Dsump")]){
        w_list[[i]][[j]] <- 1
      }else if(df_lin$mean[which(df_lin$param=="Dsump")]<df_ric$mean[which(df_ric$param=="Dsump")]){
        w_list[[i]][[j]] <- 0
        # or set w to it's preferred values (0 or 1).
      }else if(df_mix$mean[which(df_mix$param=="w")]>0.5){
        w_list[[i]][[j]] <- 1
      }else if(df_mix$mean[which(df_mix$param=="w")]<0.5){
        w_list[[i]][[j]] <- 0
      }
    }else{ # if it gets this far, we did not properly consider all scenarios
      w_list[[i]][[j]] <- "you thought"
      print(j)
      print(df_mix$mean[which(df_mix$param=="w")])
      print(paste("t.peak", df_mix$converged[which(df_mix$param=="t.peak")]))
      print(paste("y.peak", df_mix$converged[which(df_mix$param=="y.peak")]))
      print(paste("mm", df_mix$converged[which(df_mix$param=="mm")]))
      print(paste("bb", df_mix$converged[which(df_mix$param=="bb")]))
    }
    
  }
  
}

# Save to w_list data to models folder
save(w_list, file = "models/w_input.Rdata")

