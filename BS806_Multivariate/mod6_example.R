teen <- read.csv("/home/elkip/Datasets/teengamb.csv", header = T)

# Backwards Elimination
g <- lm(gamble ~ sex + status + income + verbal, data = teen)
summary(g)

g2 <- lm(gamble ~ sex + income + verbal, data = teen)
summary(g2)

g3 <- lm(gamble ~ sex + income, data = teen)
summary(g3)

# AIC
forward <- ~ sex + status + income + verbal
moo <- lm(gamble ~ 1, data = teen)
g.forward <- step(moo, scope = forward, direction = "forward", k=2)

aov(lm(gamble ~ sex + status + income + verbal, data=teen))
# AIC = n*log(RSS/n) + 2p
n <- nrow(teen)
n*log(21623.767/n)+2*4 # 296.1758

extractAIC(m.forward, k=2)		# by default k=2, AIC
extractAIC(m.forward,k=log(50))		# k=log(n), BIC

## final model using AIC
summary(m.forward)$coefficients