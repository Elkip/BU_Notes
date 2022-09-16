
data <- read.csv("/home/elkip/Datasets/gala.txt", sep = "\t")
y <- data$Species
x <- as.matrix(data.frame(x0=rep(1, nrow(data)), x1=data$Area, x2=data$Elevation, x3=data$Nearest, x4=data$Scruz, x5=data$Adjacent))

beta <- solve(t(x)%*%x)%*%(t(x)%*%y)

RSS <- t(y) %*% y - t(beta) %*% t(x) %*% y
s_sq <- RSS / (nrow(data) - 5 - 1)
