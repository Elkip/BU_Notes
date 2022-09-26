library(epiR)
library(epitools)

# 1

smkrs <- c(41, 39, 192, 199, 145, 36, 205, 102)
dim(smkrs) <- c(2,2,2)

dimnames(smkrs)[[1]] <- c("MI", "No MI")
dimnames(smkrs)[[2]] <- c("5+ Coffee", "0-4 Coffees")
dimnames(smkrs)[[3]] <- c("Never/Former Smoker", "Current Smoker")
print(smkrs)
mi.BD <- epi.2by2(dat = smkrs, method = "case.control", conf.level = .95)

summary(mi.BD)$massoc.detail$OR.homog.woolf
summary(mi.BD)$massoc.detail$OR.homog.brday

mantelhaen.test(smkrs)
mantelhaen.test(smkrs)$estimate
oddsratio(smkrs[[1]])

# 2
heart <- read.csv("/home/elkip/Datasets/framdat2.txt", sep = "", na.strings = ".")
# Keep observations for women (SEX ==2), after exam 4 or no event (CHD = 0 or CHD > 4)
# And only existing data for GLI
heart2 <- heart[ which(!is.na(heart$GLI) & heart$SEX == 2 & (heart$CHD == 0 | heart$CHD > 4)),]
attach(heart2)

#heart2 <- heart[ which(heart$SEX == 2 & (heart$CHD == 0 | heart$CHD > 4)),]
#attach(heart2)

## Create Bins for Age
hist(AGE)
agegrp <- floor((AGE - 35)/10 + 1)

# Indicator Variable
chd_sw = CHD > 4
# Create labels
chd_sw <- factor(chd_sw, levels = c(TRUE, FALSE), labels = c("Event", "No event"))
glci <- factor(GLI, levels = c(1,0), labels = c("GI", "Not GI"))
fram.data <- table(glci, chd_sw, agegrp)
res <- epi.2by2(dat=fram.data, method = "cohort.count", conf.level = .95)

res
names(summary(res)$massoc.detail)
summary(res)$massoc.detail$OR.crude.wal$est
summary(res)$massoc.detail$OR.strata.wald$est
summary(res)$massoc.detail$OR.mh.wald$est
summary(res)$massoc.detail$chi2.mh

fram.data.ajd <- fram.data + .5
mi.BD.adj <- epi.2by2(dat = fram.data.ajd, method = "case.control", conf.level = .95)

summary(mi.BD.adj)$massoc.detail$OR.homog.woolf
summary(mi.BD.adj)$massoc.detail$OR.homog.brday

get.odds <- function(mtrxList) {
  t_size = length(mtrxList)
  a <- vector(length = t_size)
  b <- vector(length = t_size)
  c <- vector(length = t_size)
  d <- vector(length = t_size)
  n_total <- vector(length = t_size)
  n0 <-  vector(length = t_size)
  n1 <- vector(length = t_size)
  m0 <- vector(length = t_size)
  m1 <- vector(length = t_size)
  mor_num = 0
  mor_den = 0
  mh_num = 0
  mh_den = 0
  
  for(i in 1:t_size) {
    mat = mtrxList[[i]]
    # print(mat)
    a[i] = mat[1,1]
    b[i] = mat[1,2]
    c[i] = mat[2,1]
    d[i] = mat[2,2]
    
    n_total[i] <- a[i] + b[i] + c[i] + d[i]
    n1[i] <- a[i] + c[i]
    n0[i] <- b[i] + d[i]
    m1[i] = a[i] + b[i]
    m0[i] = c[i] + d[i]
    
    mor_num <- mor_num + (a[i]*d[i]/n_total[i])
    mor_den <- mor_den + (b[i]*c[i]/n_total[i])
    mh_num <- mh_num + ((a[i]*d[i] - b[i]*c[i]) / n_total[i])
    mh_den <- mh_den + ((n0[i]*n1[i]*m0[i]*m1[i])/(n_total[i]^2*(n_total[i] - 1)))
  
    print(paste("Odds Ratio:", (((a[i]/c[i])/(b[i]/d[i])))))
    } 
  
  mh <- mh_num^2 / mh_den 
  print(mh)
  print(pchisq(mh,t_size-1,lower.tail = F))
  
  mor <- mor_num / mor_den
  print(paste("mOR", mor))
  mor_ci_top <- mor^(1+1.96/sqrt(mh))
  mor_ci_btm <- mor*(1-1.96/sqrt(mh))
  print("Confidence interval:")
  print(paste(mor_ci_btm, mor_ci_top))
  
}


get.odds(list(smkrs[,,1], smkrs[,,2]))
get.odds(list(fram.data[,,1], fram.data[,,2], fram.data[,,3], fram.data[,,4]))
