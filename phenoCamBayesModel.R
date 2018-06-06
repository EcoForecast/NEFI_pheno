source("graphMultiplePheno.R")

bayesianPhenoCam <- function(URL,season){
  if(season=="spring"){
    a.mu.pr <- 30
    a.sd.pr <- 3
    b.mu.pr <- -0.11
    b.mu.pr <- 0.01
  }
  if(season=="fall"){
    a.mu.pr <- -30
    a.sd.pr <- 3
    b.mu.pr <- 0.11
    b.mu.pr <- 0.01
  }
  c.a.pr <- 2.5
  c.b.pr <- 7.5
  d.a.pr <- 3
  d.b.pr <- 7
  model <-  "
  model{
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  a ~ dnorm(a.mu.pr,a.sd.pr)
  b ~ dnorm(b.mu.pr,b.sd.pr)
  c ~ dbeta(c.a.pr,c.b.pr)
  d ~ dbeta(d.a.pr,d.b.pr)

  }

  for(i in 1:n){
  b <-  a/x[i] - a
  a <-  10 - b
  y[i] ~ dbeta(a,b) ## data model
  x[i] ~ c/(1+exp(a+b*i))+d ## process model
  }
  }
  "
  phenoData <- donwload.phenocam(URL)
  y <- phenoData$gcc_mean
  data <- list(y=y,n=length(y),x_ic=y[1],tau_i=1/(phenoData$g_std[1]**2))
  time <-  as.Date(phenoData$date)
  nchain <-  3
  init <- list()
  for(i in 1:nchain){
    pheno.samp = sample(y,length(y),replace=TRUE)
    init[[i]] <- list(tau_add=1/var(diff(pheno.samp)),tau_obs=5/var(pheno.samp))
  }
  j.model   <- jags.model (file = textConnection(RandomWalk),
                           data = data,
                           inits = init,
                           n.chains = 3)
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("tau_add","tau_obs"),
                              n.iter = 1000)

}