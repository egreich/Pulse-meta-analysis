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
load("models/01-test-Ricker/inputET.Rdata") # et3


# Create study_pulse combination, create integer sID

pulse_table <- et3 %>%
  expand(nesting(Study.ID, Pulse.ID)) %>%
  mutate(sID = as.numeric(factor(Study.ID))) %>%
  arrange(Study.ID) %>%
  tibble::rownames_to_column() %>%
  rename(pID = rowname) %>%
  mutate(pID = as.numeric(pID)) %>%
  relocate(pID, .after = sID)

# Join with et2
et3 <- et3 %>%
  left_join(pulse_table) %>%
  relocate(sID, pID)

# Plot
et3 %>%
  ggplot(aes(x = Days.relative.to.pulse + 1,
             y = LRR)) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(et3, aes(x = Days.relative.to.pulse + 1,
           y = LRR)) +
  geom_errorbar(aes(ymin = LRR - sqrt(poolVar),
                 ymax = LRR + sqrt(poolVar),
                 color = as.factor(sID)),
                width = 0) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0) +
  facet_wrap(~sID, scales = "free_y") +
  theme_bw() +
  guides(color = "none")

# Prepare pulse vars (nrow = nrow(pulse_table))
pulse_vars <- et3 %>%
  group_by(sID, pID) %>%
  summarize(MAP = unique(MAP.mm.wc),
            pulse_amount = unique(Pulse.amount),
            preSWC = unique(preSWC),
            SWCunit = unique(SWCunit), # cm and mm
            SWCtype = factor(unique(SWCtype), levels = c("soil water content volumetric",
                                                         "soil water content unknown")),
            SWCtype = as.numeric(SWCtype) - 1) %>% # needs to be in two steps
  mutate(preSWC = case_when(SWCunit == "mm" ~ ud.convert(preSWC, "mm", "cm"),
                            SWCunit == "cm" ~ preSWC),
         SWCunit = ifelse(!is.na(SWCunit), "cm", NA))

#sum(!is.na(pulse_vars$MAP))
#sum(!is.na(pulse_vars$pulse_amount))

# Get rid of pre-pulse data in et3 table to anchor at 0
et3 <- et3[et3$Days.relative.to.pulse != -1,] 

# Prepare data list
datlist <- list(et = et3$LRR,
                t = et3$Days.relative.to.pulse + 1,
                pID = et3$pID,
                sID = pulse_table$sID,
                Nobs = nrow(et3),
                Npulse = nrow(pulse_table),
                Nparam = 4,
                #NSWCtype = max(pulse_vars$SWCtype, na.rm=T) + 1,
                preSWC = pulse_vars$preSWC,
                #SWCtype = pulse_vars$SWCtype,
                pulse_amount = as.vector(scale(pulse_vars$pulse_amount)),
                MAP = as.vector(scale(pulse_vars$MAP)),
                Nstudy = max(pulse_table$sID),
                Slpeakt = 2,
                Slmaxy = 2)

# Initial values: manual specification to get model started
inits <- function(){
  list(A = rnorm(datlist$Nparam, 0, 10),
       B = rnorm(datlist$Nparam, 0, 10),
       #pp = runif(1, 0, 1),
       lmu.swc = runif(1, .5, 5),
       ltau.swc = runif(1, .5, 5),
       #a.swc = runif(datlist$NSWCtype, 0, 100),
       #b.swc = runif(datlist$NSWCtype, 0, 100),
       #U = c(1, runif(1, 0, 100)),
       tau.Eps.lpeakt = runif(1, 0, 10),
       tau.Eps.lmaxy = runif(1, 0, 10),
       sig.lpeakt = runif(1, 0, 10),
       sig.lmaxy = runif(1, 0, 10),
       tau = runif(1, 0, 3))
}
initslist <- list(inits(), inits(), inits())

# Initial values: from saved state
load("models/01-test-Ricker/inits/inits.Rdata")

# Restart from chains with lowest deviance
# which(colnames(jm_coda[[1]]) == "deviance")
# mean(jm_coda[[1]][,10])
# mean(jm_coda[[2]][,10])
# mean(jm_coda[[3]][,10])
# 
# ss <- list(saved_state[[2]][[2]], 
#            saved_state[[2]][[2]],
#            saved_state[[2]][[2]])

# names(initslist[[1]]) %in% names(ss[[1]])

# Initialize JAGS model
jm <- jags.model("models/01-test-Ricker/model2.R",
                 data = datlist,
                 inits = saved_state[[2]],
                 n.chains = 3)

update(jm, 10000)

# Run and monitor parameters
params <- c("A", "B", # coefficients for linear model
            "lmu.swc", "ltau.swc", # parameters for missing SWC
            "tau.Eps.lpeakt", "tau.Eps.lmaxy", # precision for random effects
            "deviance", "Dsum", # model performance metrics
            "mu.lpeakt","mu.lmaxy", # population-level parameters on log scale
            "sig","tau", # sample sd and precision
            "sig.lpeakt", "sig.lmaxy") # sd among study-level log parameters

jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 9000, thin = 3)

# If converged, save out
if(!dir.exists("models/01-test-Ricker/coda")) {
  dir.create("models/01-test-Ricker/coda")
}
save(jm_coda, file = "models/01-test-Ricker/coda/jm_coda.Rdata") #for local


# Plot output
mcmcplot(jm_coda, parms = c("deviance", "Dsum",
                            "mu.lpeakt","mu.lmaxy",
                            "A", "B",
                            "lmu.swc", "ltau.swc",
                            "sig", "sig.lpeakt", "sig.lmaxy"))

# Check convergence
gel <- data.frame(gelman.diag(jm_coda, multivariate = FALSE)$psrf) %>%
  tibble::rownames_to_column(var = "term")
  
filter(gel, grepl("Dsum", term))
filter(gel, grepl("^maxy", term))
filter(gel, grepl("^peakt", term))
filter(gel, grepl("mu\\.", term))
filter(gel, grepl("sig", term))

# Save state

# inits to save
init_names = c("A","B","lmu.swc","ltau.swc" ,"tau.Eps.lpeakt","tau.Eps.lmaxy", "sig.lpeakt", "sig.lmaxy", "tau")

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
                          variables = remove_vars) #c(3,6:8)
saved_state[[1]]
if(!dir.exists("models/01-test-Ricker/inits")) {
  dir.create("models/01-test-Ricker/inits")
}
save(saved_state, file = "models/01-test-Ricker/inits/inits.Rdata") #for local

# If converged, run and save replicated data
jm_rep <- coda.samples(jm, variable.names = "et.rep",
                       n.iter = 9000, thin = 3)

save(jm_rep, file = "models/01-test-Ricker/coda/jm_rep.Rdata") #for local
