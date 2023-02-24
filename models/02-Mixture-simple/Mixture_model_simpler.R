# This model does not include pulse-level covariates, and just models pulse-level 
# parameters heirarchically.

model{
  
  for(i in 1:Nobs){ # number of observations
    # Likelihood for the log-response ratio (e.g., LRR for ET)
    Y[i] ~ dnorm(mu[i], tau)
    # Replicated data
    Y.rep[i] ~ dnorm(mu[i], tau)
    
    # Mean model the involves a mixture of potential functions to describe the 
    # pulse response. E.g., Function1 is based on a reparameterization of the 
    # Ricker function, expressed on the "original" LRR scale (i.e., not transformed).
    # Function2 is a simple linear function of time since pulse.
    # The mixture weight, w, is allowed to vary by pulse ID.
    mu[i] <- w[pID[i]]*Function1[i] + (1-w[pID[i]])*Function2[i]
    # Alternative: Have an indicator that picks off (selects) one of the functions, 
    # rather than using a "weighted average" of the functions. If S = 0, then pick
    # Function2; if S = 1, then pick Function1:
    # mu[i] <- S[pID[i]]*Function1[i] + (1-S[pID[i]])*Function2[i]
    
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
  
  # Hierarchical linear model for Lt.peak and y.peak
  # t.peak = exp(Lt.peak) = time at which response reaches it "peak" (min or max)
  # y.peak = value of response at its peak
  # Varies with pulse-level env variables and the "initial" (pre-pulse) "state" 
  # of the system (e.g., response value, ET, etc. pre-pulse); if the pre-pulse
  # state (e.g., actual ET, not the LRR for ET) is included, the UNITS would need
  # to be the SAME for all pulses.
  # With study random effect
  
  for(p in 1:Npulse) {
    
    ## For Function1 (Ricker Model):
    # Since the time at which the peak occurs should be after the pulse, t.peak > 0, 
    # so give hierarchical prior on log scale, centered on study-level parameters:
    Lt.peak[p] ~ dnorm(mu.Lt.peak[sID[p]], tau.Lt.peak)#T(-10,10) # maybe try modeling tau.Lt.peak indexed by study, hierarchically
    t.peak[p] <- exp(Lt.peak[p])
    # y.peak can be positive (increase in response after pulse) or negative (e.g.,
    # decrease in response after pulse), so model on original scale:
    y.peak[p] ~ dnorm(mu.y.peak[sID[p]], tau.y.peak)
    
    
    ## For Function2 (Linear model)
    # Hierarchical prior for intercept parameter:
    bb[p] ~ dnorm(mu.bb[sID[p]], tau.bb)
    # slope parameter:
    mm[p] ~ dnorm(mu.mm[sID[p]], tau.mm)

    ## Hierarchical model for mixture weight:
    w[p] <-  ilogit(w1[p])
    w1[p] ~ dnorm(mu.w[sID[p]], tau.w)

    # If using the "alternative" that Selects one of the functions (not a mixture); 
    # Hierarchical bernoulli prior for S:
    # S[p] ~ dbern(w[sID[p]])
  }
  


  # Hierarchical priors for study-level parameters:
  for(s in 1:Nstudy){ # number of studies
    mu.Lt.peak[s] ~ dnorm(M.Lt.peak, T.Lt) # Can potentially incorporate MAP and MAT here to get at biome type/ treat studies differently
    mu.y.peak[s] ~ dnorm(M.y.peak, T.y)
    mu.bb[s] ~ dnorm(M.bb, T.bb)
    mu.mm[s] ~ dnorm(M.mm, T.mm)
    # For mixture model option:
    mu.w[s] ~ dnorm(M.w, T.w)
    # For selection model option:
    # w[s] ~ dbeta(a.w, b.w)
  }

  # Priors for overall ("population") level parameters:
  M.Lt.peak ~ dnorm(0,0.0001)
  M.y.peak ~ dnorm(0,0.0001)
  M.bb ~ dnorm(0,0.0001)
  M.mm ~ dnorm(0,0.0001)
  # For mixture model option:
  M.w ~ dnorm(0,0.0001)
  # For selection model option:
  # a.w ~ dunif(1,100)
  # b.w ~ dunif(1,100)
  # Expected probability of S = 1 (for selection model)
  # Ew <- a.w/(a.w + b.w)

  # Priors for precisions associated with study-level precisions:
  S.Lt ~ dunif(0,100)
  T.Lt <- pow(S.Lt,-2)
  S.y ~ dunif(0,100)
  T.y <- pow(S.y,-2)
  S.bb ~ dunif(0,100)
  T.bb <- pow(S.bb,-2)
  S.mm ~ dunif(0,100)
  T.mm <- pow(S.mm,-2)
  # For mixture model option:
  S.w ~ dunif(0,100)
  T.w <- pow(S.w,-2)
  
  # Priors for pulse level parameters:
  sig.Lt.peak ~ dunif(0,100)
  sig.y.peak ~ dunif(0,100)
  tau.Lt.peak <- pow(sig.Lt.peak,-2)
  tau.y.peak <- pow(sig.y.peak,-2)
  sig.bb ~ dunif(0,100)
  tau.bb <- pow(sig.bb,-2)
  sig.mm ~ dunif(0,100)
  tau.mm <- pow(sig.mm,-2)
  # For mixture model option:
  sig.w ~ dunif(0,100)
  tau.w <- pow(sig.w,-2)
  
  # Prior for observation precision
  tau ~ dgamma(0.01, 0.01)
  sig <- pow(tau, -0.5)
  
  # Standard deviations and other quantities to monitor
  Sigs[1] <- sig # SD among observations
  Sigs[2] <- sig.Lt.peak # SD of peak t parameter (log scale) among pulses
  Sigs[3] <- sig.y.peak # SD of peak y parameter among pulses
  Sigs[4] <- sig.bb # standard deviation of intercept parameter in linear model among pulses
  Sigs[5] <- sig.mm # standard deviation of slope parameter in linear model among pulses
  Sigs[6] <- sig.w # standard deviation for weight for mixture weight model among pulses
  Sigs[7] <- S.Lt # SD of peak t parameter (log scale) among studies (Ricker)
  Sigs[8] <- S.y # SD of peak y parameter among studeis (Ricker)
  Sigs[9] <- S.bb # standard deviation of intercept parameter in linear model among studies
  Sigs[10] <- S.mm # standard deviation of slope parameter in linear model among studies
  Sigs[11] <- S.w # standard deviation of weight for mixture model among studies
 # Sigs[12] <- Ew # For selection model
  
  # Dsum
  Dsum <- sum(Sqdiff[])
}