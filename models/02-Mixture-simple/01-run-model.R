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

# Load data
load("models/01-test-Ricker/model_input.Rdata") # out_list

# A function to run the model on all
# set overwrite=T to save a new version of the coda object and initials
# set lowdev=T to save the chain with the lowest deviance and have the
# other two chains vary slightly around it
run_mod <- function(dfin, varname, overwrite = F, lowdev = F){
  
  # Uncomment the next two lines to test the function line-by-line
  # Index Key: 1:"ET", 2:"WUE", 3:"T", 4:"Gs", 5:"PWP", 6:"ecosystemR", 7:"abovegroundR", 8:"belowgroundR", 9:"NPP", 10:"GPP", 11:"Anet"
  varname <- "Gs" #ET, GPP, Gs
  dfin <- out_list[[varname]]
  #dfin <- et3
  
  initfilename <- paste("./models/02-Mixture-simple/inits/inits_", varname,".RData", sep = "")
  jm_codafilename <- paste("./models/02-Mixture-simple/coda/jm_coda_", varname,".RData", sep = "")
  jm_repfilename <- paste("./models/02-Mixture-simple/coda/jm_rep_", varname,".RData", sep = "")
  
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
  relocate(sID, pID) %>%
  filter(Days.relative.to.pulse > -1) %>%
  filter(Days.relative.to.pulse <= 14)

# Plot
df %>%
  ggplot(aes(x = Days.relative.to.pulse + 1,
             y = LRR)) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(df, aes(x = Days.relative.to.pulse + 1,
           y = LRR)) +
  # geom_errorbar(aes(ymin = LRR - sqrt(poolVar),
                #  ymax = LRR + sqrt(poolVar),
                #  color = as.factor(sID)),
                # width = 0) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~pID, scales = "free_y") +
  theme_bw() +
  guides(color = "none")

# Prepare pulse vars (nrow = nrow(pulse_table))
pulse_vars <- df %>%
  group_by(sID, pID) %>%
  summarize(MAP = unique(MAP.mm.wc),
            pulse_amount = unique(Pulse.amount),
            preSWC = unique(preSWC),
            preVar = unique(preVar),
            SWCunit = unique(SWCunit), # cm and mm
            SWCtype = factor(unique(SWCtype), levels = c("soil water content volumetric",
                                                         "soil water content unknown")),
            SWCtype = as.numeric(SWCtype) - 1) %>% # needs to be in two steps
  mutate(preSWC = case_when(SWCunit == "mm" ~ ud.convert(preSWC, "mm", "cm"),
                            SWCunit == "cm" ~ preSWC),
         SWCunit = ifelse(!is.na(SWCunit), "cm", NA))

#sum(!is.na(pulse_vars$MAP))
#sum(!is.na(pulse_vars$pulse_amount))



# Prepare data list
datlist <- list(Y = df$LRR,
                t = df$Days.relative.to.pulse + 1,
                pID = df$pID,
                sID = pulse_table$sID,
                Nobs = nrow(df),
                Npulse = nrow(pulse_table),
                #pulse_amount = as.vector(scale(pulse_vars$pulse_amount)),
                #MAP = as.vector(scale(pulse_vars$MAP)),
                #Yinit = pulse_vars$preVar,
                Nstudy = max(pulse_table$sID)
                #S.Lt = 2,
                #S.y = 2
                )

# Initial values: manual specification to get model started

inits <- function(){
  list(M.Lt.peak = 0.5,
       M.y.peak = 2,
       M.bb = 2,
       M.mm = 2,
       M.w = .5,
       sig.Lt.peak = .5,
       sig.y.peak = 3,
       sig.bb = 3,
       sig.mm = 3,
       tau = 2)
}
# inits to use when removing "overall" hierarchical level for functions
inits <- function(){
  list(M.Lt.peak = 0.5,
       M.y.peak = 2,
       mu.bb = rnorm(datlist$Nstudy,0,10),
       mu.mm = rnorm(datlist$Nstudy,0,10),
       mu.w = rep(0.5,datlist$Nstudy),
       sig.Lt.peak = .5,
       sig.y.peak = 3,
       sig.bb = 3,
       sig.mm = 3,
       tau = 2)
}

initslist <- list(inits(), inits(), inits())

inits_list <- c("bb","mm","sig","sig.Lt.peak","sig.y.peak","sig.bb","sig.mm","sig.w","S.Lt","S.y","S.bb","S.mm","S.w")
sigs_list <- c("sig","sig.Lt.peak","sig.y.peak","sig.bb","sig.mm","sig.w","S.Lt","S.y","S.bb","S.mm","S.w")
# Initial values: from saved state
if(file.exists(initfilename)){
  load(initfilename)
  # Make sure initials are for stochastic sds
  for(i in 1:3){
    for(j in 1:length(sigs_list)){
      sig_temp <- saved_state[["initials"]][[i]][["Sigs"]][[j]]
      saved_state[["initials"]][[i]][[sigs_list[j]]] <- sig_temp
    }
    saved_state[["initials"]][[i]][["sig"]] <- NULL
    saved_state[["initials"]][[i]][["Sigs"]] <- NULL
    saved_state[["initials"]][[i]][["S.Lt"]] <- NULL
    saved_state[["initials"]][[i]][["S.y"]] <- NULL
    saved_state[["initials"]][[i]][["bb"]] <- NULL
    saved_state[["initials"]][[i]][["mm"]] <- NULL
  }
  initslist <- saved_state[[2]]
}else if(!file.exists(initfilename)){

}

# Initialize JAGS model
jm <- jags.model("models/02-Mixture-simple/Emma_test.R", # "models/02-Mixture-simple/Mixture_model_simpler.R" or "models/02-Mixture-simple/Emma_test.R"
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, 100000)

# Run and monitor parameters

params <- c("w",
            "bb", "mm", # intercept and slope for linear model
            "Sigs", "sig.Lt.peak", "sig.y.peak", "tau",
            "M.Lt.peak","M.y.peak",
            "mu.Lt.peak", "mu.y.peak",
            #"M.bb", "M.mm",
            "mu.bb", "mu.mm",
            #"M.w", # overall-level w
            "mu.w", # study-level w
            "deviance", "Dsum", # model performance metrics
            "t.peak","y.peak", "Lt.peak", # pulse-level parameters
            "mu.Lt.peak", "mu.y.peak", # population-level parameters
            "R2") # Model fit

jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 40000, thin = 5)

# If converged, save out
if(!dir.exists("models/02-Mixture-simple/coda")) {
  dir.create("models/02-Mixture-simple/coda")
}

if(overwrite==T){
  save(jm_coda, file = jm_codafilename) #for local
}


# Plot output
mcmcplot(jm_coda, parms = c("w",
                            "deviance", "Dsum",
                            "t.peak","y.peak", 
                            "bb", "mm",
                            "M.Lt.peak","M.y.peak",
                            "mu.bb", "mu.mm",
                            "mu.w",
                            "Sigs",
                            "sig.Lt.peak", "sig.y.peak", "tau",
                            "R2"))

caterplot(jm_coda, parms = "w", reorder = F)

library(broom.mixed)
foo <- broom.mixed::tidyMCMC(jm_coda, conf.int = T, conf.method = "HPDinterval")

# Check convergence
gel <- data.frame(gelman.diag(jm_coda, multivariate = FALSE)$psrf) %>%
  tibble::rownames_to_column(var = "term")
  
filter(gel, grepl("Dsum", term))
filter(gel, grepl("^y.peak", term))
filter(gel, grepl("^t.peak", term))
filter(gel, grepl("mu\\.", term))
filter(gel, grepl("sig", term))

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
init_names = c("M.Lt.peak","M.y.peak",
               "M.bb", "M.mm",
               "M.w",
               "Sigs", "tau")

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
if(!dir.exists("models/02-Mixture-simple/inits")) {
  dir.create("models/02-Mixture-simple/inits")
}

if(overwrite==T){
  save(saved_state, file = initfilename) #for local
}

# If converged, run and save replicated data
jm_rep <- coda.samples(jm, variable.names = "Y.rep",
                       n.iter = 15000, thin = 5)

if(overwrite==T){
  save(jm_rep, file = jm_repfilename) #for local
}

}

######## Use function to run model #########

#df_et <- as.data.frame(outlist[1])
#run_mod(df_et, "ET")

# To run all at once
variables <- c("ET", "WUE", "T", "Gs", "PWP",
               "ecosystemR", "abovegroundR", "belowgroundR",
               "NPP", "GPP", "Anet")

for(i in 1:length(variables)){
  df_var <- as.data.frame(out_list[i])
  run_mod(df_var, variables[i])
}



