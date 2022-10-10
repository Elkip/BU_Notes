pulse <- read.table("/home/elkip/Datasets/pulsedata.csv", header=T, sep = ",")
attach(pulse)
# 1
reg <- lm(Pulse1 ~ Height + Weight + Sex + Smokes + Alcohol + Exercise, data = pulse)
summary(reg)

# 2 
par(mfrow=c(2,2))
plot(fitted(reg), residuals(reg), xlab="fitted", ylab="Residuals")

qqnorm(residuals(reg), ylab="Residuals")
qqline(residuals(reg))
hist(reg$residuals)

shapiro.test(reg$residuals)

# 3
n <- nrow(pulse)
pprime <- 7
stud <- rstudent(reg)
lim = abs(qt(.05/(n*2), df = n - pprime - 1, lower.tail = T))
stud[which(abs(stud) > lim)]

# 4
cook <- cooks.distance(reg)
cook[cook > 4/n]
cook[cook>.05]
cook[(pf(cook, pprime, n-pprime) > .05)]

# 5

library(alr4)
influenceIndexPlot(reg)

reg2 <-  lm(Pulse1 ~ Height + Weight + Sex + Smokes + Alcohol + Exercise
            , data = pulse, subset = (cook < max(cook)))
summary(reg2)

summary(reg)$coefficients
summary(reg2)$coefficients

shapiro.test(reg2$residuals)

detach(pulse)
