# 1. Conduct multiple linear regression
# using savings rate as the outcome variable
load(file="~/Datasets/savings.rda")
savings
pairs(savings)
sr_reg <- lm(sr ~ ., data=savings)

