model{
  
  for(i in 1:Nobs){ # number of observations
    # Likelihood
    et[i] ~ dnorm(mu[i], tau)
    # Replicated data
    et.rep[i] ~ dnorm(mu[i], tau)
    
    # Mean model
    # KO: Here's an alternative that is reparameterized in terms of "meaningful" parameters and first
    # evaluated on the Log-scale:
    Lmu[i] <- 1 - t[i]/exp(Lpeakt[pID[i]]) + Lmaxy[pID[i]] + Lpeakt[pID[i]] + log(t[i])
    
    # KO: exponential to "regular" scale:
    mu[i] <- exp(Lmu[i])
    
    # Square differences
    Sqdiff[i] <- pow(et[i] - et.rep[i], 2) 
  }
  
  # Hierarchical linear model for Lpeakt and Lmaxy
  # Varies with pulse-level env variables
  # With study random effect
  
  for(p in 1:Npulse) {
    Lpeakt[p] ~ dnorm(mu.lpeakt[p], tau.lpeakt)
    Lmaxy[p] ~ dnorm(mu.lmaxy[p], tau.lmaxy)
    
    # Linear regression
    mu.lpeakt[p] <- A[1] + A[2]*U[SWCtype[p]]*preSWC[p] + A[3]*pulse_amount[p] + A[4]*MAP[p] + Eps.lpeakt[sID[p]]
    # A[2] -> effect when volumetric
    # U[k] -> multiplicative effect when unknown
    # SWCtype[p] = 1 if unknown; = 2 if v/v; = 3 if NA, no gravimetric in this dataset
    
    # missing SWCtype (when preSWC = NA, set SWCtype = 1)
    SWCtype[p] ~ dbern(pp)
    
    mu.lmaxy[p] <- B[1] + B[2]*preSWC[p] + B[3]*pulse_amount[p] + B[4]*MAP[p] + Eps.lmaxy[sID[p]]
    
    # pulse[p] ~ dlnorm(mu.pulse, tau.pulse) No missing pulse amounts
    preSWC[p] ~ dbeta(a.swc[SWCtype[p]], b.swc[SWCtype[p]])
    # if plot of preSWC vs MAP show "strong" relationship: (No, plus units problem comparing across SWC types)
    #preSWC[p] ~ dlnorm(mu.swc[p], tau.swc)
    #mu.swc[p] <- a.swc + b.swc*MAP[p]
  }
  
  # Prior for missing data parameters
  pp ~ dunif(0,1)
  for(t in 1:3){ # I think t is for "type"
    a.swc[t] ~ dunif(1,100)
    b.swc[t] ~ dunif(1,100)}
  
  U[1] ~ dunif(0,100)
  U[2] ~ dunif(0,100)
  U[3] <- 1
  
  # Priors for linear model 
  for(j in 1:Nparam){ # number of linear regression parameters
    # normal priors for root nodes
    A[j] ~ dnorm(0, 0.0001)
    B[j] ~ dnorm(0, 0.0001)
    
    # Identifiable regression parameters
    Astar[j] <- A[j] + equals(j, 1) * mean.eps.lpeakt
    Bstar[j] <- B[j] + equals(j, 1) * mean.eps.lmaxy
  }
  
  # Priors for RE
  for(s in 1:Nstudy){ # number of studies
    # Non-identifiable RE
    Eps.lpeakt[s] ~ dnorm(0, tau.eps.lpeakt)
    Eps.lmaxy[s] ~ dnorm(0, tau.eps.lmaxy)
    
    # Identifiable RE (centered)
    Estar.lpeakt[s] <-  Eps.lpeakt[s] - mean.eps.lpeakt
    Estar.lmaxy[s] <-  Eps.lmaxy[s] - mean.eps.lmaxy
  }
  
  # Define mean of RE
  mean.eps.lpeakt <- mean(Eps.lpeakt[])
  mean.eps.lmaxy <- mean(Eps.lmaxy[])
  
  # Priors for RE precision
  # Folded t distribution with 2 degrees of freedom for standard deviation
  tau.Eps.lpeakt ~ dt(0, Tlpeakt, 2)
  sig.eps.lpeakt <- abs(tau.Eps.lpeakt)
  tau.eps.lpeakt<- pow(sig.eps.lpeakt, -2)
  
  tau.Eps.lmaxy ~ dt(0, Tlmaxy, 2)
  sig.eps.lmaxy <- abs(tau.Eps.lmaxy)
  tau.eps.lmaxy<- pow(sig.eps.lmaxy, -2)
  
  # Parms for folded t; set as data Salpha
  Tlpeakt <- pow(Slpeakt, -2)
  Tlmaxy <- pow(Slmaxy, -2)
  
  
  # Priors for pulse level parameters:
  sig.lpeakt ~ dunif(0,100)
  sig.lmaxy ~ dunif(0,100)
  tau.lpeakt <- pow(sig.lpeakt,-2)
  tau.lmaxy <- pow(sig.lmaxy,-2)
  
  # Prior for observation precision
  tau ~ dgamma(0.01, 0.01)
  sig <- pow(tau, -0.5)
  
  # Standard deviations to monitor
  Sigs[1] <- sig # SD among observations
  Sigs[2] <- sig.lpeakt # SD of peak t parameter (log scale) among pulses
  Sigs[3] <- sig.lmaxy # SD of max y parameter (log scale) among pulses
  Sigs[4] <- sig.eps.lpeakt # SD of study RE for linear model of peak t (log scale)
  Sigs[5] <- sig.eps.lmaxy # SD of study RE for linear model of max y (log scale)
  
  # Dsum
  Dsum <- sum(Sqdiff[])
}