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

run_mod <- function(dfin, varname){
  
  # Uncomment the next two lines to test the function line-by-line
  # Index Key: 1:"ET", 2:"WUE", 3:"T", 4:"Gs", 5:"PWP", 6:"ecosystemR", 7:"abovegroundR", 8:"belowgroundR", 9:"NPP", 10:"GPP", 11:"Anet"
  varname <- "Gs"
  dfin <- out_list[[varname]]
  
  
  initfilename <- paste("./models/01-test-Ricker/inits/inits_", varname,".RData", sep = "")
  jm_codafilename <- paste("./models/01-test-Ricker/coda/jm_coda_", varname,".RData", sep = "")
  jm_repfilename <- paste("./models/01-test-Ricker/coda/jm_rep_", varname,".RData", sep = "")
  
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
  filter(Days.relative.to.pulse <= 14,
         Days.relative.to.pulse > -1)

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
                Nparam = 5,
                preSWC = pulse_vars$preSWC,
                mean.SWC = mean(pulse_vars$preSWC, na.rm = TRUE),
                sd.SWC = sd(pulse_vars$preSWC, na.rm = TRUE),
                pulse_amount = as.vector(scale(pulse_vars$pulse_amount)),
                MAP = as.vector(scale(pulse_vars$MAP)),
                Yinit = pulse_vars$preVar,
                Nstudy = max(pulse_table$sID),
                S.Lt = 2,
                S.y = 2)

# Initial values: manual specification to get model started
inits <- function(){
  list(A = rnorm(datlist$Nparam, 0, 10),
       B = rnorm(datlist$Nparam, 0, 10),
       lmu.swc = runif(1, 0.1, 5),
       ltau.swc = runif(1, 0.5, 5),
       tau.Eps.Lt = runif(1, 0, 10),
       tau.Eps.y = runif(1, 0, 10),
       sig.Lt.peak = runif(1, 0, 10),
       sig.y.peak = runif(1, 0, 10),
       tau = runif(1, 0, 3))
}
initslist <- list(inits(), inits(), inits())

# Initial values: from saved state
#load("models/01-test-Ricker/inits/inits.Rdata") #temp
if(file.exists(initfilename)){
  load(initfilename)
}else if(!file.exists(initfilename)){
  saved.state <- list()
  saved.state[[2]] <- initslist
}

# Restart from chains with lowest deviance
# dev_col <- which(colnames(jm_coda[[1]]) == "deviance")
# dev1<- mean(jm_coda[[1]][,dev_col])
# dev2<- mean(jm_coda[[2]][,dev_col])
# dev3<- mean(jm_coda[[3]][,dev_col])
# dev_min <- min(dev1, dev2, dev3)
# if(dev1 == dev_min){
#   devin = 1
# } else if(dev2 == dev_min){
#   devin = 2
# } else if(dev3 == dev_min){
#   devin = 3
# }
# 
# ss <- list(saved_state[[2]][[devin]],
#            saved_state[[2]][[devin]],
#            saved_state[[2]][[devin]])
# 
# names(initslist[[1]]) %in% names(ss[[1]])

# Initialize JAGS model
jm <- jags.model("models/01-test-Ricker/Ricker_model2.R",
                 data = datlist,
                 inits = saved.state[[2]],
                 n.chains = 3)

update(jm, 100000)

# Run and monitor parameters
# params <- c("A", "B", # coefficients for linear model
#             "lmu.swc", "ltau.swc", # parameters for missing SWC
#             "tau.Eps.lpeakt", "tau.Eps.lmaxy", # precision for random effects
#             "Eps.lpeakt", "Eps.lmaxy", # pulse-level random effects
#             "deviance", "Dsum", # model performance metrics
#             "mu.lpeakt","mu.lmaxy", # population-level parameters on log scale
#             "sig","tau", # sample sd and precision
#             "sig.lpeakt", "sig.lmaxy") # sd among pulse-level log parameters
params <- c("A", "B", # coefficients for linear model
            "lmu.swc", "ltau.swc", # parameters for missing SWC
            "tau.Eps.Lt", "tau.Eps.y", # precision for random effects
            "Sigs",
            "Estar.Lt.peak", "Estar.y.peak", # pulse-level random effects
            "deviance", "Dsum", # model performance metrics
            "t.peak","y.peak", "Lt.peak", # pulse-level parameters
            "mu.Lt.peak", "mu.y.peak", # population-level parameters
            "sig.Lt.peak", "sig.y.peak","tau", # sample sd and precision, sd among pulse-level log parameters
            "R2") # Model fit

jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 40000, thin = 5)

# If converged, save out
if(!dir.exists("models/01-test-Ricker/coda")) {
  dir.create("models/01-test-Ricker/coda")
}

save(jm_coda, file = jm_codafilename) #for local

# Plot output
mcmcplot(jm_coda, parms = c("deviance", "Dsum",
                            "t.peak","y.peak", 
                            "A", "B",
                            "lmu.swc", "ltau.swc",
                            "Sigs",
                            "sig.Lt.peak", "sig.y.peak", "tau",
                            "R2"))

caterplot(jm_coda, parms = "Estar.Lt.peak", reorder = F)
caterplot(jm_coda, parms = "Estar.y.peak", reorder = F)
caterplot(jm_coda, parms = "A", reorder = F)
caterplot(jm_coda, parms = "B", reorder = F)

# Check convergence
gel <- data.frame(gelman.diag(jm_coda, multivariate = FALSE)$psrf) %>%
  tibble::rownames_to_column(var = "term")
  
filter(gel, grepl("Dsum", term))
filter(gel, grepl("^y.peak", term))
filter(gel, grepl("^t.peak", term))
filter(gel, grepl("mu\\.", term))
filter(gel, grepl("sig", term))

# Save state

# inits to save
init_names = c("A","B","lmu.swc","ltau.swc" ,"tau.Eps.Lt","tau.Eps.y", 
               "sig.Lt.peak", "sig.y.peak", "tau")

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
saved_state[[1]]
if(!dir.exists("models/01-test-Ricker/inits")) {
  dir.create("models/01-test-Ricker/inits")
}
save(saved_state, file = initfilename) #for local

# If converged, run and save replicated data
jm_rep <- coda.samples(jm, variable.names = "Y.rep",
                       n.iter = 15000, thin = 5)

save(jm_rep, file = jm_repfilename) #for local
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



