library(readxl)
df <- read_excel("Trab5.xlsx")

dose <- df$Dose
dose <- c(rep(dose,3))
d <- as.factor(dose)
ldose<-log(dose)
y <- df$Mortos
m <- df$Expostos
insecticid<-as.factor(c(rep("r",5),rep("D",6),rep("M",6)))
Tribolium.dat <- data.frame(dose, y, m, insecticid)
attach(Tribolium.dat)
resp<-cbind(y,m-y)
pe=y/m
plot(dose,pe, pch=c(rep("*",5),rep("+",6), rep("-",6)),
     col=c(rep("green",5), rep("red",6),rep("blue",6)),
     xlab="Dose", ylab="Proporcoes de insetos mortos")
plot(ldose,pe, pch=c(rep("*",5),rep("+",6), rep("-",6)),
     col=c(rep("green",5), rep("red",6),rep("blue",6)),
     xlab="Log(Dose)", ylab="Proporcoes de insetos mortos")

################################################################################
mod1<-glm(resp~1, family=binomial)
mod2<-glm(resp~d, family=binomial)
mod3<-glm(resp~insecticid, family=binomial)
mod4<-glm(resp~insecticid+ldose-1, family=binomial)
anova(mod1, mod2, mod3, mod4, test="Chisq")
summary(mod4, test="Chisq")


,0#################################################################################
plot(c(1.8,8), c(0,1), type="n", xlab="dose", ylab="proporcoes", log="x")
points(dose,pe, pch=c(rep("*",6),rep("+",6), rep("-",6)),
       col=c(rep("green",6), rep("red",6),rep("blue",6)))
ld<-seq(log(2),log(8),0.005)
lines(exp(ld), predict(mod4,data.frame(ldose=ld,
                                       insecticid=factor(rep("DDT",length(ld)),levels=levels(insecticid))),
                       type="response"), col="green")
lines(exp(ld), predict(mod4,data.frame(ldose=ld,
                                       insecticid=factor(rep("BHC",length(ld)),levels=levels(insecticid))),
                       type="response"), col="red")
lines(exp(ld), predict(mod4,data.frame(ldose=ld,
                                       insecticid=factor(rep("DDT+BHC",length(ld)),levels=levels(insecticid))),
                       type="response"), col="blue")
#################################################################################
