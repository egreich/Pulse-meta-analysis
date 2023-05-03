### Initial Ricker model for ET subset of response variables

if(!"postjags" %in% installed.packages()) {
  devtools::install_github("fellmk/PostJAGS/postjags")
}
library(dplyr)
library(tidyr)
library(rjags)
load.module('dic')
library(ggplot2)
library(mcmcplots)
library(postjags)
library(udunits2)
library(jagsUI)
# Load self-made functions
source("./scripts/functions.R")

# Load data
load("models/model_input.Rdata") # out_list

# A function to run the model on all
# set overwrite=T to save a new version of the coda object and initials
# set lowdev=T to save the chain with the lowest deviance and have the
# other two chains vary slightly around it
run_mod <- function(dfin, varname, overwrite = F, lowdev = F){
  
  # Uncomment the next two lines to test the function line-by-line
  # Index Key: 1:"ET", 2:"WUE", 3:"T", 4:"Gs", 5:"PWP", 6:"ecosystemR", 7:"abovegroundR", 8:"belowgroundR", 9:"NPP", 10:"GPP", 11:"Anet"
  #varname <- "ET" #ET, GPP, Gs
  dfin <- out_list[[varname]]
  
  initfilename <- paste("./models/04-Mixture-simple/inits/inits_", varname,".RData", sep = "")
  rickerinitfilename <- paste("./models/01-Ricker-simple/inits/inits_", varname,".RData", sep = "")
  linearinitfilename <- paste("./models/02-Linear-simple/inits/inits_", varname,".RData", sep = "")
  jm_codafilename <- paste("./models/04-Mixture-simple/coda/jm_coda_", varname,".RData", sep = "")
  jm_repfilename <- paste("./models/04-Mixture-simple/coda/jm_rep_", varname,".RData", sep = "")
  mcmcfoldername <- paste("./models/04-Mixture-simple/convergence/", varname, sep = "")
  mcmcfilename <- paste("MCMC_", varname, sep = "")
  
  
  # remove pulses outside of range, restrict to 14 days after pulse, remove pre-pulse days
  dfin <- dfin %>%
    filter(Days.relative.to.pulse > -1) %>%
    filter(Days.relative.to.pulse <= 14)
  
  
# Create study_pulse combination, create integer sID

pulse_table <- dfin %>%
  expand(nesting(Study.ID, Pulse.ID)) %>%
  mutate(sID = as.numeric(factor(Study.ID))) %>%
  arrange(Study.ID) %>%
  tibble::rownames_to_column() %>%
  rename(pID = rowname) %>%
  mutate(pID = as.numeric(pID)) %>%
  relocate(pID, .after = sID)

# Join with full table, restrict to 14 days after pulse, remove pre-pulse days
df <- dfin %>%
  left_join(pulse_table) %>%
  relocate(sID, pID)


# Look at the linear R2 fit, if the fit is really good, set initial w=0
fit_linear <- c()
pulses <- unique(df$pID)
for(i in c(1:length(pulses))){
  #print(i)
  df_onepulse <- df %>%
    filter(pID == pulses[i])
  
  lm_onepulse <- lm(LRR ~ Days.relative.to.pulse, data = df_onepulse)
  
  fit_linear[i] <- summary(lm_onepulse)$r.squared
  
}


# Prepare data list
datlist <- list(Y = df$LRR,
                t = df$Days.relative.to.pulse + 1,
                pID = df$pID,
                sID = pulse_table$sID,
                Nobs = nrow(df),
                Npulse = nrow(pulse_table),
                Nstudy = length(unique(pulse_table$sID)),
                log.maxT = log(max(df$Days.relative.to.pulse + 1))
                )

# Initial values: manual specification to get model started (option 1)

inits <- function(){
  list(mu.Lt.peak = rep(log(mean(df$Days.relative.to.pulse)), datlist$Nstudy),
       mu.y.peak = rnorm(datlist$Nstudy,log(mean(df$Mean)),2),
       mu.bb = rnorm(datlist$Nstudy,0,10),
       mu.mm = rnorm(datlist$Nstudy,0,10),
       #w = runif(datlist$Npulse,0,1),
       sig.Lt.peak = .5,
       sig.y.peak = 3,
       sig.bb = 3,
       sig.mm = 3,
       tau = 2)
}

initslist <- list(inits(), inits(), inits())


# Initial values: load ricker and linear inits to get model started (option 2)
if(file.exists(rickerinitfilename)){
  
  # ricker
  load(rickerinitfilename)
  initslist <- saved_state[[2]]
  
  # linear
  load(linearinitfilename)
  linearlist <- saved_state[[2]]
  for(i in c(1:3)){
    initslist[[i]][["mu.bb"]] <- linearlist[[i]][["mu.bb"]]
    initslist[[i]][["mu.mm"]] <- linearlist[[i]][["mu.mm"]] 
    initslist[[i]][["sig.bb"]] <- linearlist[[i]][["sig.bb"]]
    initslist[[i]][["sig.mm"]] <- linearlist[[i]][["sig.mm"]]
    
    # add w initial
    initslist[[i]][["w"]] <- runif(datlist$Npulse,0,1)
  }

}


# Initial values: from saved state (option 3) # temp
# if(file.exists(initfilename)){
#   load(initfilename)
#   initslist <- saved_state[[2]]
# }else if(!file.exists(initfilename)){
#   initslist <- initslist
# }


# Run and monitor parameters

params <- c("w",
            "bb", "mm", # intercept and slope for linear model
            "t.peak","y.peak", "Lt.peak", # pulse-level parameters
            "Sigs", "sig.Lt.peak", "sig.y.peak", "tau",
            "M.Lt.peak","M.y.peak",
            "mu.Lt.peak", "mu.y.peak", # population-level parameters
            "M.bb", "M.mm",
            "mu.bb", "mu.mm",
            "M.w", # overall-level w
            "mu.w", # study-level w
            "deviance", "Dsum", # model performance metrics
            "R2") # Model fit


# Run model with jagsui package
jagsui <- jags(data = datlist,
               inits = initslist,
               model.file = "models/04-Mixture-simple/Mixture_model.R",
               parameters.to.save = params,
               n.chains = 3,
               n.adapt = 1000,
               n.thin = 10,
               n.iter = 100000,
               parallel = TRUE)

jm_coda <- jagsui$samples

# If converged, save out
if(!dir.exists("models/04-Mixture-simple/coda")) {
  dir.create("models/04-Mixture-simple/coda")
}

if(overwrite==T){
  save(jagsui, file = jm_codafilename) #for local
}


# Plot output
if(!dir.exists("models/04-Mixture-simple/convergence")) {
  dir.create("models/04-Mixture-simple/convergence")
}
if(!dir.exists(mcmcfoldername)) {
  dir.create(mcmcfoldername)
}
mcmcplot(jm_coda, parms = params, 
         dir = mcmcfoldername,
         filename = mcmcfilename)

# Save state

if(lowdev == T){
# Save inits based on chains with lowest deviance
dev_col <- which(colnames(jm_coda[[1]]) == "deviance")
dev1<- mean(jm_coda[[1]][,dev_col])
dev2<- mean(jm_coda[[2]][,dev_col])
dev3<- mean(jm_coda[[3]][,dev_col])
dev_min <- min(dev1, dev2, dev3)
if(dev1 == dev_min){
  devin = 1
} else if(dev2 == dev_min){
  devin = 2
} else if(dev3 == dev_min){
  devin = 3
}
}

# inits to save
init_names = names(initslist[[1]])

# use get_remove_index function to find which variables to remove
remove_vars = get_remove_index(init_names, params, type="jagsUI")

newinits <- initfind(jm_coda, OpenBUGS = FALSE)
newinits[[1]]
saved_state <- removevars(initsin = newinits, 
                          variables = remove_vars)

if(lowdev == T){
# saved chain with lowest deviance, and make remaining chains vary around it
saved_state[[2]][[1]] = saved_state[[2]][[devin]] # Best (low dev) initials for chain 1
saved_state[[2]][[2]] = lapply(saved_state[[2]][[devin]],"*",2)
saved_state[[2]][[3]] = lapply(saved_state[[2]][[devin]],"/",2)
}

saved_state[[1]]
if(!dir.exists("models/04-Mixture-simple/inits")) {
  dir.create("models/04-Mixture-simple/inits")
}

if(overwrite==T){
  save(saved_state, file = initfilename) #for local
}

# If converged, run and save replicated data
jm_rep <- update(jagsui, parameters.to.save = "Y.rep",
                 n.iter = 15000, n.thin = 5)

if(overwrite==T){
  save(jm_rep, file = jm_repfilename) #for local
}


}

######## Use function to run model #########

# To run all at once
variables <- c("ET", "T", "Gs", "PWP",
               "ecosystemR","belowgroundR",
               "NPP", "GPP", "Anet")

for(i in 1:length(variables)){
  df_var <- as.data.frame(out_list[i])
  run_mod(df_var, variables[i], overwrite = T)
}



