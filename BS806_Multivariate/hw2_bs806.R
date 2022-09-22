vapor <- read.csv("/home/elkip/Datasets/gas_vapor.csv",as.is=T,na=".")

y_vapor <- as.matrix(vapor$y)
x_vapor <- as.matrix(data.frame(x0=rep(1,nrow(vapor)), x1=vapor$x1, x2=vapor$x2
                                , x3=vapor$x3, x4=vapor$x4))
beta_vapr <- solve(t(x_vapor)%*%x_vapor)%*%(t(x_vapor)%*%y_vapor)

lin_reg <- lm(y~ x1 + x2 + x3 + x4, data=vapor)
cor(lin_reg$fitted.values, lin_reg$residuals, method="pearson")

rss_vapor <- t(y_vapor) %*% y_vapor - t(beta_vapr) %*% t(x_vapor) %*% y_vapor
syy_vapor <- (t(y_vapor) %*% y_vapor - length(y_vapor) * mean(y_vapor)^2)

summary(lin_reg)

rsq_vapor <- 1 - (rss_vapor / syy_vapor)
rsq_vapor_2 <- 1 - deviance(lin_reg)/ sum((y_vapor - mean(y_vapor))^2) 
rsqa_vapor <- (rss_vapor / (length(y_vapor) - 4 - 1)) / (syy_vapor / (length(y_vapor) - 1))
rsqa_vapor_2 <- ((length(y_vapor) - 1)*rsq_vapor - 4) / (length(y_vapor) - 4 - 1)

ssreg_vapor <- t(beta_vapr) %*% t(x_vapor) %*% y_vapor - length(y_vapor) * mean(y_vapor)^2
msreg_vapor <- ssreg_vapor / 4

sserr_vapor <- t(y_vapor) %*% y_vapor - t(beta_vapr) %*% t(x_vapor) %*% y_vapor
mserr_vapor <- sserr_vapor / (32 - 4 - 1)

summary(lin_reg)
