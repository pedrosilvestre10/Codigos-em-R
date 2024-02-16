concentracao<-c(49.06,52.99,56.91,60.84,64.76,68.69,72.61,76.54)
y1<-c(2,7,9,14,23,29,29,29)
y2<-c(4,6,9,14,29,24,32,31)
m1<-c(29,30,28,27,30,31,30,29)
m2<-c(30,30,34,29,33,28,32,31)

y<-c(y1,y2)
m<-c(m1,m2)

beetles <- data.frame(concentracao,y1,m1,y2,m2)
attach(beetles)
plot1<-rep(concentracao,2)
plot2<-c(y1,y2)
plot(plot1, plot2, xlab = "Dose", ylab = "Quantidade de besouros", col=c(rep("green",8), rep("red",8)))
resp<-cbind(y, m-y)

#modelo log-log
log_x <- log(plot1)
log_y <- log(plot2)
modelo <- lm(log_y ~ log_x)
summary(modelo)
#x=-19,5744+log_x*5,37

#modelo logistico
mod2 <- glm(resp~m, family=binomial)
summary(mod2)
#a eq obtida eh resp = -3.2611 + 0.1224 * m

library(MASS)
dose.p(mod2)
#a dose para matar 50% dos besouros eh 26.641287

dose.p(mod2)
dose.p(mod2,p=1:9/10)
#a dose para matar 90% dos besouros eh 44.591146
