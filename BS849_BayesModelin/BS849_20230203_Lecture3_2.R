hdl.data <- read.table("hdl-data.txt",sep="\t",header=T)
hdl.data$BMI= hdl.data$BMI5
new.hdl.data <- na.omit(hdl.data)

## generate data in format for jags
data.fhs <- list(N = nrow(new.hdl.data),
                 hdl=as.numeric(new.hdl.data[,1]),
                 age=as.numeric(new.hdl.data$AGE5),
                 gender=as.numeric(new.hdl.data$SEX),bmi=new.hdl.data$BMI)