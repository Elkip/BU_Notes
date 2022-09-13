

#install.packages("epitools")
library(epitools)


cancer           <- matrix(c(183,441,189,298),ncol=2,byrow=TRUE)
rownames(cancer) <- c('HighVitD','LowVitD')
colnames(cancer) <- c('Cancer','NoCancer')

# View data
cancer

chisq.test(cancer)

oddsratio(cancer, method='wald')



























