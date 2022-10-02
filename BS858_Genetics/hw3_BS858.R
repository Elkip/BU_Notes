famdata <- read.csv("/home/elkip/Datasets/HW3_ldl2022.csv")
summary(famdata)
famdata[1:20,]

#3
#a
n_fam <- length(table(famdata$famid))

#b
famdata$parentavg1 <- (famdata$momldl + famdata$dadldl) / 2
lm(kid1ldl ~ momldl, data=famdata)
lm(kid1ldl ~ dadldl, data=famdata)
lm(kid1ldl ~ parentavg1, data=famdata)
lm(kid2ldl ~ parentavg1, data=famdata)

# c method 1
kidldl <- c(famdata$kid1ldl, famdata$kid2ldl)
meankidldl <- mean(kidldl, na.rm = T)
sdkidldl <- sd(kidldl, na.rm = T)
N = nrow(famdata)
kid1adj <- famdata$kid1ldl - meankidldl
kid2adj <- famdata$kid2ldl - meankidldl
icc = sum(kid1adj*kid2adj)/sdkidldl^2 / (N-1)
h = 2 * icc

# c method 2
kid1 <- famdata[,c("famid", "kid1id", "kid1ldl")]
kid2 <- famdata[,c("famid", "kid2id", "kid2ldl")]
colnames(kid1) <- c("famid", "kidid", "ldl")
colnames(kid2) <- c("famid", "kidid", "ldl")
kids <- rbind(kid1, kid2)
summary(aov(ldl ~ as.factor(famid), data=kids))
2*((1115.2 - 653.2) / (1115.2 + (2 -1)*653.2))
