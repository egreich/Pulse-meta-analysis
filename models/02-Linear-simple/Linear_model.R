# This is the linear-only model that:
#   2) Treats study-level is root node
#   3) Compute overall effects (across all studies)


model{
  for(i in 1:Nobs){ # number of observations
    # Likelihood for the log-response ratio (e.g., LRR for ET)
    Y[i] ~ dnorm(mu[i], tau[i])
    # Replicated data
    Y.rep[i] ~ dnorm(mu[i], tau[i])
    
    # Estimate precision when values are missing
    tau[i] ~ dgamma(a.t, b.t)
    sig[i] <- pow(tau[i], -0.5)
    
    # Mean model: Linear model
    mu[i] <- bb[pID[i]] + mm[pID[i]]*t[i]
    
    # Squared differences
    Sqdiff[i] <- pow(Y[i] - Y.rep[i], 2) 
  }
  
  # Compute Bayesian R2 value
  # var.pred <- pow(sd(mu[]),2)
  # var.resid <- 1/tau[]
  # R2 <- var.pred/(var.pred + var.resid)
  
  # Hierarchical priors for pulse-level parameters, centered on study-level 
  # parameters.  
  for(p in 1:Npulse) {
    # Hierarchical prior for intercept parameter:
    bb[p] ~ dnorm(mu.bb[sID[p]], tau.bb)
    # slope parameter:
    mm[p] ~ dnorm(mu.mm[sID[p]], tau.mm)
    # pulse-level Dsum
    Dsump[p] <- sum(Sqdiff[startID[p]:stopID[p]])
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

  # Priors for observation precision
  a.t ~ dunif(0,100)
  b.t ~ dunif(0,100)
  
  # Dsum (overall)
  Dsum <- sum(Sqdiff[])
}