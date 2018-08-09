library("rjags")
library("runjags")

runMCMC_Model <- function(j.model,variableNames,maxIter=1000000000,baseNum=80000,iterSize =40000){
  var.out   <- coda.samples (model = j.model,
                             variable.names = variableNames,
                             n.iter = baseNum)
  numb <- baseNum
  continue <- TRUE
  GBR.bad <- TRUE
  burnin <- 0
  while(continue & numb<maxIter){
    print(numb)
    new.out   <- coda.samples (model = j.model,
                               variable.names = variableNames,
                               n.iter = iterSize)
    var.out <- combine.mcmc(mcmc.objects=list(var.out,new.out),collapse.chains = FALSE)
    continue <- FALSE
    if(GBR.bad){
      GBR.vals <- gelman.diag(var.out)
      GBR.bad <- FALSE
      for(i in 1:nrow(GBR.vals$psrf)){
        for(j in 1:ncol(GBR.vals$psrf)){
          if(GBR.vals$psrf[i,j]>10){
            print(GBR.vals)
            print("GBR values too high")
            return(FALSE)
          }
          if(GBR.vals$psrf[i,j]>1.04){
            continue <-  TRUE
            GBR.bad <- TRUE
          }
        }
      }
      print(GBR.vals)
    }

    if(!continue){
      if(burnin==0){
        GBR <- gelman.plot(var.out)
        burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
        if(length(burnin) == 0) burnin = 1
      }
      var.burn <- window(var.out,start=burnin)
      var.burn <- var.out
      effsize <- effectiveSize(var.burn)
      for(i in 1:length(effsize)){
        if(effsize[i]<5000){
          continue = TRUE
        }
      }
      print(effsize)
    }
    numb <- numb+iterSize
  }
  if(continue==TRUE){
    print("Model Did not Converge")
    var.burn <- FALSE
  }
  return(var.burn)
}
