### Initial Ricker model for ET subset of response variables

library(rjags)
load.module('dic')
library(dplyr)
library(ggplot2)
library(mcmcplots)

# Load data
load("models/01-test-Ricker/inputET.Rdata")

# Convert study to factor/numeric
et2 <- et2 %>%
  mutate(sID = as.numeric(factor(Study.ID)))

et2 %>%
  filter(sID %in% c(2, 4:5, 8:11, 15)) %>%
  ggplot(aes(x = Days.relative.to.pulse + 1,
             y = LRR)) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0) +
  theme_bw()

# Prepare data list
datlist <- list(et = et2$LRR,
                t = et2$Days.relative.to.pulse + 1,
                sID = et2$sID,
                N = nrow(et2),
                Nstudy = max(et2$sID))

# Initial values
inits <- function(){
  list(#a = runif(datlist$Nstudy, 0, 10),
       #b = runif(datlist$Nstudy, 0, 10),
       tau = runif(1, 0, 1))
}
initslist <- list(inits(), inits(), inits())

# Initialize JAGS model
jm <- jags.model("models/01-test-Ricker/model1.jags",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, 10000)

# Run and monitor parameters
# took out "a" and "b"
params <- c("deviance", "Dsum",
            "peakt", "maxy", "sig", 
            "tau")

jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 9000, thin = 3)

# Plot output
mcmcplot(jm_coda, parms = c("deviance", "Dsum", "peakt", "maxy", "sig", "tau"))

ricker = function(x, a = 1, b = 1) {
  a * x * exp(-b * x)
}
plot(seq(-1, 10, 0.1), ricker(seq(-1, 10, 0.1), 1.2, 0.8))
