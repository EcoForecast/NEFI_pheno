#' Runs the MCMC until chains converge and effective sample size > 5000
#'
#' @param j.model The jags model
#' @param variableNames Vector of variable names. Default is c("a","c","k","prec").
#' @param maxIter The maximum number of iterations to run to wait for both convergence and enough effective samples.
#' @param baseNum The base number of iterations to run before checking the Gelman-Brooks-Rubin value
#' @param iterSize The number of iterations to run before checking the Gelman-Brooks-Rubin value again
#' @param maxGBR The maximum allowable Gelman-Brooks-Rubin value after the base number of iterations have been run. This can be set low (i.e. 3) if you only want to fit days that converge quickly.
#' @param ID Identification information to print out if the model does not converge
#' @export
#' @import rjags
#' @import runjags
runMCMC_Model <- function(j.model,variableNames=c("a","c","k","prec"),maxIter=1000000000,baseNum=80000,iterSize =40000,maxGBR=10,ID=""){
  var.out   <- coda.samples (model = j.model,
                             variable.names = variableNames,
                             n.iter = baseNum)
  numb <- baseNum
  continue <- TRUE
  GBR.bad <- TRUE
  burnin <- 0
  while(continue & numb<maxIter){
    print(numb)
    new.out   <- coda.samples(model = j.model,
                               variable.names = variableNames,
                               n.iter = iterSize)
    var.out <- combine.mcmc(mcmc.objects=list(var.out,new.out),collapse.chains = FALSE)
    continue <- FALSE
    if(GBR.bad){
      GBR.vals <- rjags::gelman.diag(var.out)
      GBR.bad <- FALSE
      for(i in 1:nrow(GBR.vals$psrf)){
        for(j in 1:ncol(GBR.vals$psrf)){
          if(GBR.vals$psrf[i,j]>maxGBR){
            print(GBR.vals)
            print(c("GBR values too high:",ID))
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
        GBR <- rjags::gelman.plot(var.out)
        burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1]
        if(length(burnin) == 0) burnin = 1
      }
      var.burn <- window(var.out,start=burnin)
      #var.burn <- var.out
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
