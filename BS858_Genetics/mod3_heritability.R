famdata <- read.csv("hdl.csv")

print(summary(famdata))

print(famdata[1:20,])

famdata$pavehdl = (famdata$momhdl + famdata$dadhdl)/2

#Heritability based on parent offspring

print(lm(kid1hdl ~ momhdl,data=famdata))
print(lm(kid1hdl ~ dadhdl,data=famdata))
print(lm(kid1hdl ~ pavehdl,data=famdata))

print(summary(lm(kid2hdl ~ momhdl,data=famdata)))
print(summary(lm(kid2hdl ~ dadhdl,data=famdata)))
print(summary(lm(kid2hdl ~ pavehdl,data=famdata)))

#Heritability based on sibling pair

kidhdl = c(famdata$kid1hdl,famdata$kid2hdl)
meankidhdl = mean(kidhdl,na.rm=T)
sdkidhdl = sd(kidhdl,na.rm=T)
N = nrow(famdata)

adjkid1 = famdata$kid1hdl - meankidhdl
adjkid2 = famdata$kid2hdl - meankidhdl
icc = sum(adjkid1*adjkid2)/sdkidhdl^2/(N-1)

h2 = 2*icc

print(h2)

#Heritability based on siblings (ANOVA)
kid1 = famdata[,c("famid","kid1id","kid1hdl")]
kid2 = famdata[,c("famid","kid2id","kid2hdl")]
names(kid1) <- c("famid","kidid","hdl")
names(kid2) <- c("famid","kidid","hdl")
kids <- rbind(kid1,kid2)

print(summary(aov(hdl ~ as.factor(famid),data=kids)))
