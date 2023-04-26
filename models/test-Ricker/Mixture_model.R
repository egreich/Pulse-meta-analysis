
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
    mu[i] <- S[pID[i]]*Function1[i] + (1-S[pID[i]])*Function2[i]
    
    # Function1: Ricker model
    Function1[i] <- y.peak[pID[i]]*TimePart[i]
    TimePart[i] <- exp(LogPart[i])
    # For multiplicative models, we can often avoid numerical overflow errors by 
    # modeling appropriate parts on the additive log scale, then exponentiating
    # to get the predicted (e.g, mean) value
    LogPart[i] <- 1-(t[i]/t.peak[pID[i]]) + log(t[i]) - Lt.peak[pID[i]]
    # Function1: Linear model
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
    # so give prior on log scale:
    Lt.peak[p] ~ dnorm(mu.Lt.peak[p], tau.Lt.peak)#T(-10,10)
    t.peak[p] <- exp(Lt.peak[p])
    # y.peak can be positive (increase in response after pulse) or negative (e.g.,
    # decrease in response after pulse), so model on original scale:
    y.peak[p] ~ dnorm(mu.y.peak[p], tau.y.peak)
    
    # Linear regression for Lt.peak and y.peak means:
    # In both models, Yinit is the initial (pre-pulse) value of the actual response 
    # variable (e.g., ET), not of the LRR value (which is zero for "pre-pulse")
    mu.Lt.peak[p] <- A[1] + A[2]*preSWC.scaled[p] + A[3]*pulse_amount[p] + A[4]*MAP[p] + A[5]*Yinit[p] + Eps.Lt.peak[sID[p]]
    mu.y.peak[p] <- B[1] + B[2]*preSWC.scaled[p] + B[3]*pulse_amount[p] + B[4]*MAP[p] + B[5]*Yinit[p] + Eps.y.peak[sID[p]]
    
    # @Emma: I thought you said we don't have much data for the "preSWC.scaled" variable, and so
    # you left it out of the model, but it shows up in the lines above (I kept the active
    # code from the model version I downloaded)
    
    # Model for missing SWCtype
    # pp is prob SWC.type = 2
    # Assigns 1 or 2 to NAs
    #SWCtype[p] ~ dbern(pp) # use SWCtype.temp[p] ~dbern(pp) if 2, dcat if 3
    #SWCtype[p] <- SWCtype.temp[p] +1 # for dbern
    
    # Model for missing preSWC
    # preSWC[p] ~ dbeta(a.swc[SWCtype[p] + 1], b.swc[SWCtype[p] + 1])
    # @Emma: same comment as above. Is there "sufficient data" to include this?
    preSWC[p] ~ dlnorm(lmu.swc, ltau.swc)
    
    # Scale SWC
    # preSWC.scaled[p] <- (preSWC[p] - mean.SWC[SWCtype[p] + 1])/sd.SWC[SWCtype[p] + 1]
    preSWC.scaled[p] <- (preSWC[p] - mean.SWC)/sd.SWC
    
    ## For Function2 (Linear model)
    # Intercept parameter:
    bb[p] ~ dnorm(mu.bb[p], tau.bb)
    # slope parameter:
    mm[p] ~ dnorm(mu.mm[p], tau.mm)
    # Linear models for means in above hierarchical priors:
    mu.bb[p] <- A.bb[1] + A.bb[2]*preSWC.scaled[p] + A.bb[3]*pulse_amount[p] + A.bb[4]*MAP[p] + A.bb[5]*Yinit[p] + Eps.bb[sID[p]]
    mu.mm[p] <- A.mm[1] + A.mm[2]*preSWC.scaled[p] + A.mm[3]*pulse_amount[p] + A.mm[4]*MAP[p] + A.mm[5]*Yinit[p] + Eps.mm[sID[p]]
    
    ## Hierarchical model for mixture weight:
    logit(w[p]) ~ dnorm(mu.w[p],tau.w)
    # Linear model for logit-scale mixture weight mean:
    mu.w[p] <- A.w[1] + A.w[2]*preSWC.scaled[p] + A.w[3]*pulse_amount[p] + A.w[4]*MAP[p] + A.w[5]*Yinit[p] + Eps.w[sID[p]]
    
    # If using the "alternative" that Selects one of the functions (not a mixture):
    S[p] ~ dbern(w[p])
    # Use above linear model for "logit-scale" weight (here, probability of S = 1), 
    # then apply inverse-logit to compute w:
    w[p] <- ilogit(mu.w[p])
  }
  
  
  # Prior for missing data parameters
  # pp is prob of SWCtype = 2
  # pp ~ dunif(0,1)
  # for each soil type:
  # for(tt in 1:NSWCtype){
  #   a.swc[tt] ~ dunif(1,100)
  #   b.swc[tt] ~ dunif(1,100)
  #   
  #   # Mean and SD for scaled preSWC
  #   mean.SWC[tt] <- mean(preSWC[])
  #   sd.SWC[tt]<- sd(preSWC[])
  #   }
  
  # @Emma: Are these priors "okay"? E.g., lmu.swc is on the log(swc) scale, 
  # so need to make sure that the U(0.5, 5) is appropriate on this scale (e.g.,
  # not cutting off potentially reasonable values).
  lmu.swc ~ dunif(0.1,5)
  ltau.swc ~ dunif(0.5,5)
  
  # @Emma: Since you have some missing data, the mean and sd below will change
  # with every MCMC iteration, which I don't think we want. I would compute
  # these in the Rscript based on the observed values, then put these in the 
  # data list for jags.model.
  # mean.SWC <- mean(preSWC[])
  # sd.SWC<- sd(preSWC[])
  ## KO: Agree with Jessica's suggestion about reading in data for mean.SWC and sd.SWC.
  
  # Priors for multiplicative effect of SWC type
  #U[1] ~ dunif(.99,1.01) # v/v is coded as "1"
  #U[2] ~ dunif(0,100) # unknown is coded as "2"
  
  # Priors for 2nd-level linear models
  for(j in 1:Nparam){ # number of linear regression parameters
    # normal priors for root nodes
    A[j] ~ dnorm(0, 0.0001)
    B[j] ~ dnorm(0, 0.0001)
    
    A.bb[j] ~ dnorm(0, 0.0001)
    A.mm[j] ~ dnorm(0, 0.0001)
    A.w[j] ~ dnorm(0, 0.0001)
    
    # Identifiable regression parameters
    Astar[j] <- A[j] + equals(j, 1) * mean.eps.Lt
    Bstar[j] <- B[j] + equals(j, 1) * mean.eps.y
    Astar.bb[j] <- A.bb[j] + equals(j, 1) * mean.eps.bb
    Astar.mm[j] <- A.mm[j] + equals(j, 1) * mean.eps.mm
    Astar.w[j] <- A.w[j] + equals(j, 1) * mean.eps.w
  }
  
  # Priors for random effects
  for(s in 1:Nstudy){ # number of studies
    # Non-identifiable random effects
    Eps.Lt.peak[s] ~ dnorm(0, tau.eps.Lt)
    Eps.y.peak[s] ~ dnorm(0, tau.eps.y)
    Eps.mm[s] ~ dnorm(0, tau.eps.mm)
    Eps.bb[s] ~ dnorm(0, tau.eps.bb)
    Eps.w[s] ~ dnorm(0, tau.eps.w)
    
    # Identifiable RE (centered)
    Estar.Lt.peak[s] <-  Eps.Lt.peak[s] - mean.eps.Lt
    Estar.y.peak[s] <-  Eps.y.peak[s] - mean.eps.y
    Estar.mm[s] <-  Eps.mm[s] - mean.eps.mm
    Estar.bb[s] <-  Eps.bb[s] - mean.eps.bb
    Estar.w[s] <-  Eps.w[s] - mean.eps.w
  }
  
  # Define mean of RE
  mean.eps.Lt <- mean(Eps.Lt.peak[])
  mean.eps.y <- mean(Eps.y.peak[])
  mean.eps.mm <- mean(Eps.mm[])
  mean.eps.bb <- mean(Eps.bb[])
  mean.eps.w <- mean(Eps.w[])
  
  # Priors for RE precision
  # Folded t distribution with 2 degrees of freedom for standard deviation
  tau.Eps.Lt ~ dt(0, T.Lt, 2)
  sig.eps.Lt <- abs(tau.Eps.Lt)
  tau.eps.Lt <- pow(sig.eps.Lt, -2)
  
  tau.Eps.y ~ dt(0, T.y, 2)
  sig.eps.y <- abs(tau.Eps.y)
  tau.eps.y <- pow(sig.eps.y, -2)
  
  tau.Eps.bb ~ dt(0, T.bb, 2)
  sig.eps.bb <- abs(tau.Eps.bb)
  tau.eps.bb <- pow(sig.eps.bb, -2)

  tau.Eps.mm ~ dt(0, T.mm, 2)
  sig.eps.mm <- abs(tau.Eps.mm)
  tau.eps.mm <- pow(sig.eps.mm, -2)
  
  tau.Eps.w ~ dt(0, T.w, 2)
  sig.eps.w <- abs(tau.Eps.w)
  tau.eps.w <- pow(sig.eps.w, -2)
  
  # Parms for folded t; set as data Salpha
  T.Lt <- pow(S.Lt, -2)
  T.y <- pow(S.y, -2)
  T.bb <- pow(S.bb, -2)
  T.mm <- pow(S.mm, -2)
  T.w <- pow(S.w, -2)
  
  
  # Priors for pulse level parameters:
  sig.Lt.peak ~ dunif(0,100)
  sig.y.peak ~ dunif(0,100)
  tau.Lt.peak <- pow(sig.Lt.peak,-2)
  tau.y.peak <- pow(sig.y.peak,-2)
  
  sig.bb ~ dunif(0,100)
  tau.bb <- pow(sig.bb,-2)

  sig.mm ~ dunif(0,100)
  tau.mm <- pow(sig.mm,-2)

  sig.w ~ dunif(0,100)
  tau.w <- pow(sig.w,-2)
  
  # Prior for observation precision
  tau ~ dgamma(0.01, 0.01)
  sig <- pow(tau, -0.5)
  
  # Standard deviations to monitor
  Sigs[1] <- sig # SD among observations
  Sigs[2] <- sig.Lt.peak # SD of peak t parameter (log scale) among pulses
  Sigs[3] <- sig.y.peak # SD of peak y parameter among pulses
  Sigs[4] <- sig.eps.Lt # SD of study RE for linear model of peak t (log scale)
  Sigs[5] <- sig.eps.y # SD of study RE for linear model of peak y
  Sigs[6] <- sig.bb
  Sigs[7] <- sig.mm
  Sigs[8] <- sig.w
  Sigs[9] <- sig.eps.bb
  Sigs[10] <- sig.eps.mm
  Sigs[11] <- sig.eps.w
  
  # Dsum
  Dsum <- sum(Sqdiff[])
}