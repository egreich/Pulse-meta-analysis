# This is the "simple" Ricker-only model that does not include pulse-level
# covariates, and with the following revisions:
#   1) Truncate t.peak
#   2) Treat study-level is root node
#   3) Compute overall effects (across all studies)


model{
  
  for(i in 1:Nobs){ # number of observations
    # Likelihood for the log-response ratio (e.g., LRR for ET)
    Y[i] ~ dnorm(mu[i], tau)
    # Replicated data
    Y.rep[i] ~ dnorm(mu[i], tau)
    
    # Mean model: Ricker model
    mu[i] <- y.peak[pID[i]]*TimePart[i]
    TimePart[i] <- exp(LogPart[i])
    # For multiplicative models, we can often avoid numerical overflow errors by 
    # modeling appropriate parts on the additive log scale, then exponentiating
    # to get the predicted (e.g, mean) value
    LogPart[i] <- 1-(t[i]/t.peak[pID[i]]) + log(t[i]) - Lt.peak[pID[i]]

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
    # Since the time at which the peak occurs should be after the pulse, t.peak > 0, 
    # so give hierarchical prior on log scale, centered on study-level parameters:
    # Truncate t.peak at upper value of maxT, which means we truncate Lt.peak at
    # log(maxT) = log.maxT.
    Lt.peak[p] ~ dnorm(mu.Lt.peak[sID[p]], tau.Lt.peak)T(0,log.maxT)
    t.peak[p] <- exp(Lt.peak[p])
    # y.peak can be positive (increase in response after pulse) or negative (e.g.,
    # decrease in response after pulse), so model on original scale:
    y.peak[p] ~ dnorm(mu.y.peak[sID[p]], tau.y.peak)
  }
  
  # Independent priors for study-level parameters (root nodes). Again, 
  # truncate the study-level t.peak
  for(s in 1:Nstudy){ # number of studies
    # Can potentially incorporate MAP and MAT here to get at 
    # biome type/ treat studies differently
    mu.Lt.peak[s] ~ dnorm(0, 0.0001)T(0,log.maxT) 
    # back-transform to get study-level t.peak:
    mu.t.peak[s] <- exp(mu.Lt.peak[s])
    mu.y.peak[s] ~ dnorm(0, 0.0001)
  }
  
  # Compute overall effects or parameters (averaged across all studies)
  M.Lt.peak <- mean(mu.Lt.peak[])
  M.t.peak <- mean(mu.t.peak[])
  M.y.peak <- mean(mu.y.peak[])

  # Overall estimates to monitor:
  Parms[1] <- M.Lt.peak
  Parms[2] <- M.t.peak
  Parms[3] <- M.y.peak

  # Priors for scalar parameters (precisions, standard deviations):
  sig.Lt.peak ~ dunif(0,100)
  sig.y.peak ~ dunif(0,100)
  tau.Lt.peak <- pow(sig.Lt.peak,-2)
  tau.y.peak <- pow(sig.y.peak,-2)

  # Prior for observation precision
  tau ~ dgamma(0.01, 0.01)
  sig <- pow(tau, -0.5)
  
  # Standard deviations and other quantities to monitor
  Sigs[1] <- sig # SD among observations
  Sigs[2] <- sig.Lt.peak # SD of peak t parameter (log scale) among pulses
  Sigs[3] <- sig.y.peak # SD of peak y parameter among pulses

  # Dsum
  Dsum <- sum(Sqdiff[])
}