# LQAS code to determine optimal n, d for given alpha, beta, p_l, and p_u #
###########################################################################
# Function to find the appropriate d, n for a given alpha, beta, p_upper and p_lower 
#####################################################################################
getDecisionRule <- function(p.u,p.l,alpha=0.10,beta=0.10,ns=seq(1,100,1)){

  for(i in 1:length(ns)) {
    # for each n, cycle through possible values for d and get temp alpha and betas
    # determine if any of these meet the criteria. If they do, keep the values for n
    ds <- seq(0,ns[i],1)
    tmp.alpha <- pbinom(ds-1,size=ns[i],prob=p.u) # probability of being less than d
    tmp.beta <- 1-pbinom(ds-1,size=ns[i],prob=p.l) #probability of being greater than or equal to d

    test <- intersect(which(tmp.alpha<=alpha),which(tmp.beta<=beta))

    if(length(test)>0)
      break
  }
  d <- ds[test]
  n <- ns[i]
  return(list(d=d,n=n,alpha=tmp.alpha[test],beta=tmp.beta[test]))
}

# Create and plot the operating characteristic curve #
######################################################
getOCC <- function(n,d,alpha=0.10,beta=0.10){
  ps <- seq(0,1,0.01)
  prob.high <- 1-pbinom(d-1,n,prob=ps) #binomal probability of being >= d
  par(mfrow=c(1,1))
  plot(ps,prob.high,type='l',main="Operating Characteristic Curve",ylab="OC",xlab="population prevalence")
}

# get Risk curve #
##################
getRiskCurve <- function(n,d,p.target){ # p.target is the value that we want to achieve
  ps <- seq(0,1,0.01)
  risks1 <- 1-pbinom(d-1,n,prob=ps[ps<p.target]) #probability of being greater than or = to d if true p is less than target p
  risks2 <- pbinom(d-1,n,prob=ps[ps>=p.target]) #probability of being less than d if true p is greater than target p
  risks <- c(risks1,risks2)
  plot(ps[ps<p.target],risks1,xlim=c(0,1),type='l',xlab="Prevalence",ylab="Probability of Missclassification")
  lines(ps[ps>=p.target],risks2)
} 
