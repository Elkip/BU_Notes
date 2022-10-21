dat <- read.csv("/home/elkip/Datasets/Heart.csv")
dat <- na.omit(dat)

set.seed(1)
train <- sample(1:nrow(dat), 2*nrow(dat)/3)
train = dat[train,]
test = dat[-train,]
