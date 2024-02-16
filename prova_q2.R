dose<-c(0,1,10,50,100,200)
l<-c(13,5,5,3,4,18)
oc<-c(378,200,151,47,31,33)
total<-c(391,205,156,50,35,51)
data<- data.frame(dose,l,oc,total)

#modelo obtido:
mod3 <-glm(total ~ l/total, family = poisson)
summary(mod3)

#letra b
plot(dose,l/total, xlab = "Dose", ylab = "Porcentagem de morte", pch = "*")
x <- seq(0, 0.3, 0.06)
lp <- predict(mod3, data.frame(idade = x))
pe <- exp(lp)/(1+exp(lp))
lines(x, pe)

#letra c

dose.p(mod3,p=1:1/10)
#o valor esperado sera a dose de 106.2122