source('/home/elkip/Workspace/BU_Notes/BS728_HthSrv/LQASprograms.R')

# 1. pu=60%, pl=30%, alpha=10%, and beta=10%
res1 <- getDecisionRule(p.u=0.6, p.l=0.3, alpha=0.1, beta=0.1)
getOCC(n=res1$n, d=res1$d)
getRiskCurve(n=res1$n, d=res1$d, p.target=0.7)

# 2. pu=60%, pl=40%, alpha=10%, and beta=10%
res2 <- getDecisionRule(p.u=0.6, p.l=0.4, alpha=0.1, beta=0.1)
getOCC(n=res2$n, d=res2$d)
getRiskCurve(n=res2$n, d=res2$d, p.target=0.7)

# 3. pu=80%, pl=60%, alpha=10%, and beta=10%
res3 <- getDecisionRule(p.u=0.8, p.l=0.6, alpha=0.1, beta=0.1)
getOCC(n=res3$n, d=res3$d)
getRiskCurve(n=res3$n, d=res3$d, p.target=0.7)

# 4.pu=80%, pl=60%, alpha=10%, and beta=5%
res4 <- getDecisionRule(p.u=0.8, p.l=0.6, alpha=0.1, beta=0.05)
getOCC(n=res4$n, d=res4$d)
getRiskCurve(n=res4$n, d=res4$d, p.target=0.6)