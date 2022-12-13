# 2
score <- read.table("/home/elkip/Datasets/hwscore.profile", header = T, as.is = T)
pheno <- read.table("/home/elkip/Datasets/bs858.fall2022.qtrait.txt", header = T, as.is = T)
mean(score$SCORE)
sd(score$SCORE)

both <- merge(score, pheno, by="IID")
summary(lm(QT~SCORE, data= both))

# 3
hdl <- na.omit(read.csv("/home/elkip/Datasets/HDL_CHD_results.csv"))

alpha <- sum(hdl$BETA_HDL*hdl$CAD_BETA*(hdl$CAD_SE**-2)) /
  sum((hdl$BETA_HDL**2)*(hdl$CAD_SE**-2))
exp(alpha)      

se_alpha = sqrt(1/sum((hdl$BETA_HDL**2)*(hdl$CAD_SE**-2)))
chi <- (alpha/se_alpha)**2

pchisq(chi, 1)
