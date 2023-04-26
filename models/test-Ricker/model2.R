
# data{
#   for(p in 1:Npulse){
#     SWCtype[p] <- SWCtype[p] - 1
#   }
# }


model{
  
  for(i in 1:Nobs){ # number of observations
    # Likelihood
    Y[i] ~ dnorm(mu[i], tau)
    # Replicated data
    Y.rep[i] ~ dnorm(mu[i], tau)
    
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
    Lpeakt[p] ~ dnorm(mu.lpeakt[p], tau.lpeakt)#T(-10,10)
    #Lpeakt[p] ~ dunif(0,2) # just a test because Lpeakt was providing invalid parent values
    Lmaxy[p] ~ dnorm(mu.lmaxy[p], tau.lmaxy)
    
    # Linear regression
    #mu.lpeakt[p] <- A[1] + A[2]*U[SWCtype[p] + 1]*preSWC.scaled[p]+ A[3]*pulse_amount[p] + A[4]*MAP[p] + Eps.lpeakt[sID[p]]
    #mu.lmaxy[p] <- B[1] + B[2]*U[SWCtype[p] + 1]*preSWC.scaled[p] + B[3]*pulse_amount[p] + B[4]*MAP[p] + Eps.lmaxy[sID[p]]
    mu.lpeakt[p] <- A[1] + A[2]*preSWC.scaled[p]+ A[3]*pulse_amount[p] + A[4]*MAP[p] + Eps.lpeakt[p]
    mu.lmaxy[p] <- B[1] + B[2]*preSWC.scaled[p] + B[3]*pulse_amount[p] + B[4]*MAP[p] + Eps.lmaxy[p]
    # A[2], B[2] -> effect when volumetric
    # U[k] -> multiplicative effect when unknown
    # SWCtype[p] = 1 if volumetric; = 2 if unknown; = 3 if gravimetric (no gravimetric in this dataset)
    
  
    
    # Model for missing SWCtype
    # pp is prob SWC.type = 2
    # Assigns 1 or 2 to NAs
    #SWCtype[p] ~ dbern(pp) # use SWCtype.temp[p] ~dbern(pp) if 2, dcat if 3
    #SWCtype[p] <- SWCtype.temp[p] +1 # for dbern
    
    # Model for missing preSWC
    #preSWC[p] ~ dbeta(a.swc[SWCtype[p] + 1], b.swc[SWCtype[p] + 1])
    preSWC[p] ~ dlnorm(lmu.swc, ltau.swc)
    
    # Scale SWC
    #preSWC.scaled[p] <- (preSWC[p] - mean.SWC[SWCtype[p] + 1])/sd.SWC[SWCtype[p] + 1]
    preSWC.scaled[p] <- (preSWC[p] - mean.SWC)/sd.SWC
  }
  
  
  # Prior for missing data parameters
  # pp is prob of SWCtype = 2
  #pp ~ dunif(0,1)
  # for each soil type:
  # for(tt in 1:NSWCtype){
  #   a.swc[tt] ~ dunif(1,100)
  #   b.swc[tt] ~ dunif(1,100)
  #   
  #   # Mean and SD for scaled preSWC
  #   mean.SWC[tt] <- mean(preSWC[])
  #   sd.SWC[tt]<- sd(preSWC[])
  #   }
  lmu.swc ~ dunif(.5,5)
  ltau.swc ~ dunif(.5,5)
  mean.SWC <- mean(preSWC[])
  sd.SWC<- sd(preSWC[])
  
  # Priors for multiplicative effect of SWC type
  #U[1] ~ dunif(.99,1.01) # v/v is coded as "1"
  #U[2] ~ dunif(0,100) # unknown is coded as "2"
  
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
  for(p in 1:Npulse){ # number of pulses for pulse-level RE
    # Non-identifiable RE
    Eps.lpeakt[p] ~ dnorm(0, tau.eps.lpeakt)
    Eps.lmaxy[p] ~ dnorm(0, tau.eps.lmaxy)
    
    # Identifiable RE (centered)
    Estar.lpeakt[p] <-  Eps.lpeakt[p] - mean.eps.lpeakt
    Estar.lmaxy[p] <-  Eps.lmaxy[p] - mean.eps.lmaxy
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