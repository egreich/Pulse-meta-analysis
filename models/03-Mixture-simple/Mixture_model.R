# This model does not include pulse-level covariates, and just models pulse-level 
# parameters hierarchically.
# 3/30/2023: This is the mixture model, with the following revisions:
#   1) Truncate t.peak
#   2) Treat study-level is root node
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
    
    # Mean model involves a mixture of potential functions to describe the 
    # pulse response. E.g., Function1 is based on a reparameterization of the 
    # Ricker function, expressed on the "original" LRR scale (i.e., not transformed).
    # Function2 is a simple linear function of time since pulse.
    # The mixture weight, w, is allowed to vary by pulse ID.
    mu[i] <- w[pID[i]]*Function1[i] + (1-w[pID[i]])*Function2[i]
    
    # Function1: Ricker model
    Function1[i] <- y.peak[pID[i]]*TimePart[i]
    TimePart[i] <- exp(LogPart[i])
    # For multiplicative models, we can often avoid numerical overflow errors by 
    # modeling appropriate parts on the additive log scale, then exponentiating
    # to get the predicted (e.g, mean) value
    LogPart[i] <- 1-(t[i]/t.peak[pID[i]]) + log(t[i]) - Lt.peak[pID[i]]
    # Function2: Linear model
    Function2[i] <- bb[pID[i]] + mm[pID[i]]*t[i]
    
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
    ## For Function1 (Ricker Model):
    # Since the time at which the peak occurs should be after the pulse, t.peak > 0, 
    # so give hierarchical prior on log scale, centered on study-level parameters:
    # Truncate t.peak at upper value of maxT, which means we truncate Lt.peak at
    # log(maxT) = log.maxT.
    Lt.peak[p] ~ dnorm(mu.Lt.peak[sID[p]], tau.Lt.peak)T(,log.maxT)
    t.peak[p] <- exp(Lt.peak[p])
    # y.peak can be positive (increase in response after pulse) or negative (e.g.,
    # decrease in response after pulse), so model on original scale:
    y.peak[p] ~ dnorm(mu.y.peak[sID[p]], tau.y.peak)
    
    ## For Function2 (Linear model)
    # Hierarchical prior for intercept parameter:
    bb[p] ~ dnorm(mu.bb[sID[p]], tau.bb)
    # slope parameter:
    mm[p] ~ dnorm(mu.mm[sID[p]], tau.mm)
    
    ## Hierarchical model for mixture weight, based on a beta distribution:
    w[p] ~ dbeta(a.w[sID[p]], b.w[sID[p]]) # both were a.w
    
    # pulse-level Dsum
    Dsump[p] <- sum(Sqdiff[startID[p]:stopID[p]])
  }
  
  # Independent priors for study-level parameters (root nodes). Again, 
  # truncate the study-level t.peak
  for(s in 1:Nstudy){ # number of studies
    # Can potentially incorporate MAP and MAT here to get at 
    # biome type/ treat studies differently
    mu.Lt.peak[s] ~ dnorm(0, 0.0001)T(,log.maxT) 
    # back-transform to get study-level t.peak:
    mu.t.peak[s] <- exp(mu.Lt.peak[s])
    mu.y.peak[s] ~ dnorm(0, 0.0001)
    mu.bb[s] ~ dnorm(0, 0.0001)
    mu.mm[s] ~ dnorm(0, 0.0001)
    # For mixture weight hyperparameters, truncate at 1 to avoid errors
    # with the pulse-level beta prior.
    a.w[s] ~ dunif(1,100)
    b.w[s] ~ dunif(1,100)
    # Expected study-level mixture weight:
    mu.w[s] <- a.w[s]/(a.w[s]+b.w[s])
  }
  
  # Compute overall effects or parameters (averaged across all studies)
  M.Lt.peak <- mean(mu.Lt.peak[])
  M.t.peak <- mean(mu.t.peak[])
  M.y.peak <- mean(mu.y.peak[])
  M.bb <- mean(mu.bb[])
  M.mm <- mean(mu.mm[])
  M.w <- mean(mu.w[])
  
  # Overall estimates to monitor:
  Parms[1] <- M.Lt.peak
  Parms[2] <- M.t.peak
  Parms[3] <- M.y.peak
  Parms[4] <- M.bb
  Parms[5] <- M.mm
  Parms[6] <- M.w
  
  # Priors for scalar parameters (precisions, standard deviations):
  sig.Lt.peak ~ dunif(0,100)
  sig.y.peak ~ dunif(0,100)
  tau.Lt.peak <- pow(sig.Lt.peak,-2)
  tau.y.peak <- pow(sig.y.peak,-2)
  sig.bb ~ dunif(0,100)
  tau.bb <- pow(sig.bb,-2)
  sig.mm ~ dunif(0,100)
  tau.mm <- pow(sig.mm,-2)
  
  # Prior for observation precision
  a.t ~ dunif(0,100)
  b.t ~ dunif(0,100)
  
  # Dsum
  Dsum <- sum(Sqdiff[])
}