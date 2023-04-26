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
library(jagsUI)

# Load data
load("models/model_input.Rdata") # out_list

# A function to run the model on all
# set overwrite=T to save a new version of the coda object and initials
# set lowdev=T to save the chain with the lowest deviance and have the
# other two chains vary slightly around it
run_mod <- function(dfin, varname, overwrite = F, lowdev = F){
  
  # Uncomment the next line to test the function line-by-line
  #varname <- "belowgroundR" #ET, GPP, Gs
  
  dfin <- out_list[[varname]]
  
  initfilename <- paste("./models/01-Ricker-simple/inits/inits_", varname,".RData", sep = "")
  jm_codafilename <- paste("./models/01-Ricker-simple/coda/jm_coda_", varname,".RData", sep = "")
  jm_repfilename <- paste("./models/01-Ricker-simple/coda/jm_rep_", varname,".RData", sep = "")
  mcmcfoldername <- paste("./models/01-Ricker-simple/convergence/", varname, sep = "")
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
  
  # Join with full table
  df <- dfin %>%
    left_join(pulse_table) %>%
    relocate(sID, pID)
  
  # Plot
  # df %>%
  #   ggplot(aes(x = Days.relative.to.pulse + 1,
  #              y = LRR)) +
  #   geom_point(aes(color = as.factor(sID))) +
  #   geom_hline(yintercept = 0) +
  #   theme_bw()
  
  # ggplot(df, aes(x = Days.relative.to.pulse + 1,
  #                y = LRR)) +
  #   # geom_errorbar(aes(ymin = LRR - sqrt(poolVar),
  #   #  ymax = LRR + sqrt(poolVar),
  #   #  color = as.factor(sID)),
  #   # width = 0) +
  #   geom_point(aes(color = as.factor(sID))) +
  #   geom_hline(yintercept = 0, lty = 2) +
  #   facet_wrap(~pID, scales = "free_y") +
  #   theme_bw() +
  #   guides(color = "none")
  
  
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
  
  # Initial values: manual specification to get model started
  
  # inits to use when removing "overall" hierarchical level for functions
  inits <- function(){
    list(mu.Lt.peak = rep(log(mean(df$Days.relative.to.pulse)), datlist$Nstudy),
         mu.y.peak = rnorm(datlist$Nstudy,log(mean(df$Mean)),2),
         sig.Lt.peak = .5,
         sig.y.peak = 3,
         tau = 2)
  }
  
  initslist <- list(inits(), inits(), inits())
  
  # Initial values: from saved state
  if(file.exists(initfilename)){
    load(initfilename)
    initslist <- saved_state[[2]]
  }else if(!file.exists(initfilename)){
    initslist <- initslist
  }
  
  # Run and monitor parameters
  
  params <- c("Sigs", "sig.Lt.peak", "sig.y.peak", "tau",
              "M.Lt.peak","M.y.peak",
              "deviance", "Dsum", # model performance metrics
              "t.peak","y.peak", "Lt.peak", # pulse-level parameters
              "mu.Lt.peak", "mu.y.peak", # population-level parameters
              "R2") # Model fit
  
  # Run model with jagsui package
  jagsui <- jags(data = datlist,
                 inits = initslist,
                 model.file = "models/01-Ricker-simple/Ricker_model.R",
                 parameters.to.save = params,
                 n.chains = 3,
                 n.adapt = 1000,
                 n.thin = 5,
                 n.iter = 40000,
                 parallel = TRUE)
  
  jm_coda <- jagsui$samples
  
  # If converged, save out
  if(!dir.exists("models/01-Ricker-simple/coda")) {
    dir.create("models/01-Ricker-simple/coda")
  }
  
  if(overwrite==T){
    save(jagsui, file = jm_codafilename) #for local
  }
  
  
  # Plot output
  if(!dir.exists("models/01-Ricker-simple/convergence")) {
    dir.create("models/01-Ricker-simple/convergence")
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
  init_names = c("mu.Lt.peak","mu.y.peak",
                 "sig.Lt.peak","sig.y.peak",
                 "tau")
  
  # function that finds the index of variables to remove
  get_remove_index <- function(to_keep, list){
    list <- list[list != "deviance"] # remove deviance
    list <- sort(list, method = "radix")
    out_list <- c()
    for(j in c(1:length(list))){
      if(list[j] %in% to_keep){
        out_list[j] = NA
      } else{
        out_list[j] = j
      }
    }
    out_list <- out_list[!is.na(out_list)]
    out_list
  }
  
  # use get_remove_index function to find which variables to remove
  remove_vars = get_remove_index(init_names, params)
  
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
  if(!dir.exists("models/01-Ricker-simple/inits")) {
    dir.create("models/01-Ricker-simple/inits")
  }
  
  if(overwrite==T){
    save(saved_state, file = initfilename) #for local
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




