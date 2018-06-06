library("rjags")
library("runjags")

runMCMC_Model <- function(j.model,variableNames){
  var.out   <- coda.samples (model = j.model,
                             variable.names = variableNames,
                             n.iter = 10000)
  numb <- 1
  continue <- TRUE
  while(continue){
    print(numb*10000)
    new.out   <- coda.samples (model = j.model,
                               variable.names = variableNames,
                               n.iter = 10000)
    var.out <- combine.mcmc(mcmc.objects=list(var.out,new.out),collapse.chains = FALSE)
    GBR.vals <- gelman.diag(var.out)
    continue <- FALSE
    for(i in 1:nrow(GBR.vals$psrf)){
      for(j in 1:ncol(GBR.vals$psrf)){
        if(GBR.vals$psrf[i,j]>1.04){
          continue = TRUE
        }
      }
    }
    if(!continue){
      GBR <- gelman.plot(var.out)
      burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
      if(length(burnin) == 0) burnin = 1
      var.burn <- window(var.out,start=burnin)
      var.burn <- var.out
      effsize <- effectiveSize(var.burn)
      for(i in 1:length(effsize)){
        if(effsize[i]<5000){
          continue = TRUE
        }
      }
    }
    numb <- numb+1
  }
  return(var.out)
}
