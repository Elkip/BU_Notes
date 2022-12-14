# 1.
# a. Read in the data
# 7 ROWS, 4 COLUMNS
sim1 <- as.data.frame(cbind(c(1,1,1,1,0,0,0),
                            c(1,1,0,0,1,1,0),
                            c(1,0,1,0,1,0,1),
                            c(0,8,3,13,1,3,8)))
names(sim1) <- c("source1","source2","source3", "count")
sim1
# b. Fit the linear model and estimate total population
fit1 <- glm(count ~ ., data=sim1, family = poisson)
summary(fit1)
# Estimate of uncaptured
exp(fit1$coefficients[1])
# Estimate of total population
exp(fit1$coefficients[1])+sum(sim1$count)

# c. Fit a model that assumes dependence between sources 1 and 2, and 2 and 3
fit2 <- glm(count ~ source1 + source2 + source3 + source1*source2 + source2*source3, data=sim1, family = poisson)
# Estimate total population
exp(fit2$coefficients[1])+sum(sim1$count)