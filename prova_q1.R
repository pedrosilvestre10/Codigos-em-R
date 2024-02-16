#Codigo questão 1

dose <- c(1,2,4,8,16,32)
d <- as.factor(dose)
ldose<-log(dose)
m <- c(1,4,9,13,18,20)
f <- c(0,2,6,10,12,16)
total <- c(1,6,15,23,30,36)
data.dat <- data.frame(dose,m,f,total)
plot1<-c(1,2,4,8,16,32,1,2,4,8,16,32)
plot2<-c(1,4,9,13,18,20,0,2,6,10,12,16)
plot(plot1, plot2, xlab = "Dose", ylab = "Observed proportions", col=c(rep("green",6), rep("red",6)))
#Como podemos observar o grafico podemos dizer que as proporcoes seguem uma distribuição logaritimica

#letra b

mod1 <-glm(dose ~ m/total, family = poisson)
summary(mod1)
mod2 <-glm(dose ~ f/total, family = poisson)
summary(mod2)

#letra c
library(MASS)

dose.p(mod1)
dose.p(mod2)
#a dose suficiente para matar 50% dos macho eh 11,41
#a dose suficiente para matar 50% das femeas eh de 4,56

#letra d


#letra e
mod3 <-glm(plot1 ~ plot2, family = poisson)
summary(mod3)


###########################################################
#questão 2



