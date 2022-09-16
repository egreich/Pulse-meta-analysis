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

# Load data
load("models/01-test-Ricker/inputET.Rdata")


# Create study_pulse combination, create integer sID

pulse_table <- et2 %>%
  expand(nesting(Study.ID, Pulse.ID)) %>%
  mutate(sID = as.numeric(factor(Study.ID))) %>%
  arrange(Study.ID) %>%
  tibble::rownames_to_column() %>%
  rename(pID = rowname) %>%
  mutate(pID = as.numeric(pID))

# Join with et2
et2 <- et2 %>%
  left_join(pulse_table) %>%
  relocate(sID, pID)

# Plot
et2 %>%
  filter(sID %in% c(2, 4:5, 8:11, 15)) %>%
  ggplot(aes(x = Days.relative.to.pulse + 1,
             y = LRR)) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(et2, aes(x = Days.relative.to.pulse + 1,
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
pulse_vars <- et2 %>%
  group_by(pID) %>%
  summarize(MAP = unique(MAP.mm.wc),
            pulse_amount = unique(Pulse.amount))

#sum(!is.na(pulse_vars$MAP))
#sum(!is.na(pulse_vars$pulse_amount))

# Prepare data list
datlist <- list(et = et2$LRR,
                t = et2$Days.relative.to.pulse + 1,
                pID = et2$pID,
                sID = pulse_table$sID,
                Nobs = nrow(et2),
                Npulse = nrow(pulse_table),
                Nparam = 4,
                preSWC = ,
                pulse_amount =et2$Pulse.amount,
                MAP = et2$MAP.mm.wc,
                Nstudy = max(pulse_table$sID),
                Slpeakt = 2,
                Slmaxy = 2)

# Initial values: manual specification to get model started
inits <- function(){
  list(A = rnorm(datlist$Nparam, 0, 10),
       B = rnorm(datlist$Nparam, 0, 10),
       tau.Eps.lpeakt = runif(1, 0, 10),
       tau.Eps.lmaxy = runif(1, 0, 10),
       sig.lpeakt = runif(1, 0, 10),
       sig.lmaxy = runif(1, 0, 10),
       tau = runif(1, 0, 3))
}
initslist <- list(inits(), inits(), inits())

# Initial values: from saved state
load("models/01-test-Ricker/inits/inits.Rdata")

# Initialize JAGS model
jm <- jags.model("models/01-test-Ricker/model1.jags",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, 10000)

# Run and monitor parameters
params <- c("deviance", "Dsum", # model performance metrics
            "peakt", "maxy", # study-level peakt and maxy
            "mu.peakt","mu.maxy", # population-level peakt and maxy
            "mu.Lpeakt","mu.Lmaxy", # population-level parameters on log scale, needed to reinitialize
            "sig","tau", # sample sd and precision, needed to reinitialize
            "sig.Lpeakt", "sig.Lmaxy") # sd among study-level log parameters, needed to reinitialize)

jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 9000, thin = 3)

# If converged, save out
if(!dir.exists("models/01-test-Ricker/coda")) {
  dir.create("models/01-test-Ricker/coda")
}
save(jm_coda, file = "models/01-test-Ricker/coda/jm_coda.Rdata") #for local


# Plot output
mcmcplot(jm_coda, parms = c("deviance", "Dsum", 
                            "peakt", "maxy", 
                            "mu.peakt","mu.maxy", 
                            "sig", "sig.Lpeakt", "sig.Lmaxy"))
                            # "mu.Lpeakt","mu.Lmaxy", "tau"))

# Check convergence
gel <- data.frame(gelman.diag(jm_coda, multivariate = FALSE)$psrf) %>%
  tibble::rownames_to_column(var = "term")
  
filter(gel, grepl("Dsum", term))
filter(gel, grepl("^maxy", term))
filter(gel, grepl("^peakt", term))
filter(gel, grepl("mu\\.", term))
filter(gel, grepl("sig", term))

# Save state
newinits <- initfind(jm_coda, OpenBUGS = FALSE)
newinits[[1]]
saved_state <- removevars(initsin = newinits, 
                          variables = c(1:2, 5:8))
saved_state[[1]]
if(!dir.exists("models/01-test-Ricker/inits")) {
  dir.create("models/01-test-Ricker/inits")
}
save(saved_state, file = "models/01-test-Ricker/inits/inits.Rdata") #for local

# If converged, run and save replicated data
jm_rep <- coda.samples(jm, variable.names = "et.rep",
                       n.iter = 9000, thin = 3)

save(jm_rep, file = "models/01-test-Ricker/coda/jm_rep.Rdata") #for local
