##' Creates a logistic phenology forecast model based on PhenoCam and MODIS data
##'
##' @param data The data in the form of a list with data$p, data$mn, data$me, data$n, data$x_ic, and data$tau_ic
##' @param nchain The desired number of chains in the MCMC
##' @export
##' @import rjags
##' @import coda
logisticPhenoModel <- function(data,nchain){
  ##Set priors
  data$s1 <- 0.5
  data$s2 <- 0.2

  ##JAGS code
  LogisticModel = "
  model{
  #### Data Model: PhenoCam
  for(i in 1:n){
  p[i] ~ dnorm(x[i],p.PC)
  }

  #### Data Model: MODIS NDVI
  for(i in 1:n){
  mn[i] ~ dnorm(x[i],p.MN)
  }

  #### Data Model: MODIS EVI
  for(i in 1:n){
  me[i] ~ dnorm(x[i],p.ME)
  }

  #### Process Model
  #### Color is the expected new phenology stage given the previous stage and logistic
  #### subtraction instead of addition in the discrete logistic eqn makes r negative (so logistic goes down).
  for(i in 2:n){
  color[i] <- x[i-1] - r * x[i-1] * (1-x[i-1])  ## latent process
  xl[i] ~ dnorm(color[i],p.proc)  ## process error
  x[i] <- max(0, min(1,xl[i]) ) ## trunate normal process error
  }

  #### Priors
  color[1] ~ dnorm(x_ic,tau_ic)
  x[1] ~ dnorm(x_ic,tau_ic)
  p.PC ~ dgamma(s1,s2)
  p.ME ~ dgamma(s1,s2)
  p.MN ~ dgamma(s2,s2)
  p.proc ~ dgamma(s1,s2)
  r ~ dexp(0.148) # Exp is the maximum entropy distribution for constraints of positive with given mean
  # 0.148 is from Richardson et al. 2006.
  }"

  ###Create the JAGS model using the basic RandomWalk Model

  j.model   <- jags.model (file = textConnection(LogisticModel),
                           data = data,
                           n.chains = nchain)
  return(j.model)

}
