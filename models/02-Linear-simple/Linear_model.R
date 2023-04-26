# This is the linear-only model that:
#   2) Treats study-level is root node
#   3) Compute overall effects (across all studies)


model{
  for(i in 1:Nobs){ # number of observations
    # Likelihood for the log-response ratio (e.g., LRR for ET)
    Y[i] ~ dnorm(mu[i], tau)
    # Replicated data
    Y.rep[i] ~ dnorm(mu[i], tau)
    
    # Mean model: Linear model
    mu[i] <- bb[pID[i]] + mm[pID[i]]*t[i]
    
    # Squared differences
    Sqdiff[i] <- pow(Y[i] - Y.rep[i], 2) 
  }
  # Compute Bayesian R2 value
  var.pred <- pow(sd(mu[]),2)
  var.resid <- 1/tau
  R2 <- var.pred/(var.pred + var.resid)
  
  # Hierarchical priors for pulse-level parameters, centered on study-level 
  # parameters.  
  for(p in 1:Npulse) {
    # Hierarchical prior for intercept parameter:
    bb[p] ~ dnorm(mu.bb[sID[p]], tau.bb)
    # slope parameter:
    mm[p] ~ dnorm(mu.mm[sID[p]], tau.mm)
  }
  
  # Independent priors for study-level parameters (root nodes). Again, 
  # truncate the study-level t.peak
  for(s in 1:Nstudy){ # number of studies
    mu.bb[s] ~ dnorm(0, 0.0001)
    mu.mm[s] ~ dnorm(0, 0.0001)
  }
  
  # Compute overall effects or parameters
  M.bb <- mean(mu.bb[])
  M.mm <- mean(mu.mm[])

  # Overall estimates to monitor:
  Parms[1] <- M.bb
  Parms[2] <- M.mm

  # Priors for scalar parameters (precisions, standard deviations):
  sig.bb ~ dunif(0,100)
  tau.bb <- pow(sig.bb,-2)
  sig.mm ~ dunif(0,100)
  tau.mm <- pow(sig.mm,-2)

  # Prior for observation precision
  tau ~ dgamma(0.01, 0.01)
  sig <- pow(tau, -0.5)
  
  # Standard deviations and other quantities to monitor
  Sigs[1] <- sig # SD among observations
  Sigs[2] <- sig.bb # standard deviation of intercept parameter in linear model among pulses
  Sigs[3] <- sig.mm # standard deviation of slope parameter in linear model among pulses

  # Dsum
  Dsum <- sum(Sqdiff[])
}