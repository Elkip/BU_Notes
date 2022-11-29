##############################
### LQAS in class examples ###
##############################
source('/home/elkip/Workspace/BU_Notes/BS728_HthSrv/LQASprograms.R')

#######################################
## Example 1:                        ##
## Decision rule for ITN in children ##
#######################################
result1 <- getDecisionRule(p.u=0.6,p.l=0.3,alpha=0.05,beta=0.05)
getOCC(n=result1$n,d=result1$d)
getRiskCurve(n=result1$n,d=result1$d,p.target=0.6)

#######################################
## Example 2:                        ##
## pl=0.4, pu=0.6                    ##
#######################################
# What is the decision rule?
# Plot the OCC and risk curves

result2 <- getDecisionRule(p.u=0.6,p.l=0.4,alpha=0.1,beta=0.1)
getOCC(n=result2$n,d=result2$d,alpha=0.1,beta=0.1)
getRiskCurve(n=result2$n,d=result2$d,p.target=0.6)

######################################
## Class exercises                  ##
######################################
# Exercise 1: change alpha and beta to 5% #
###########################################
exercise1 <- getDecisionRule(p.u=0.6,p.l=0.3,alpha=0.05,beta=0.05)
getOCC(n=exercise1$n,d=exercise1$d,alpha=0.05,beta=0.05)
getRiskCurve(n=exercise1$n,d=exercise1$d,p.target=0.6)

# Exercise 2: change lower limit to 50%   #
###########################################
exercise2 <- getDecisionRule(p.u=0.6,p.l=0.5,ns=seq(1,200,1))
getOCC(n=exercise2$n,d=exercise2$d,alpha=0.05,beta=0.05)
getRiskCurve(n=exercise2$n,d=exercise2$d,p.target=0.6)
