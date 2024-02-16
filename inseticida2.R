dose <- c(2.00,2.64,3.48,4.59,6.06,8.00)
dose <- c(rep(dose,3))
d <- as.factor(dose)
ldose<-log(dose)
y <- c(3,5,19,19,24,35,2,14,20,27,41,40,28,37,46,48,48,50)
m <- c(50,49,47,50,49,50,50,49,50,50,50,50,50,50,50,50,50,50)
insecticid<-as.factor(c(rep("DDT",6),rep("BHC",6),rep("DDT+BHC",6)))
Tribolium.dat <- data.frame(dose, y, m, insecticid)
attach(Tribolium.dat)
resp<-cbind(y,m-y)
pe=y/m
plot(dose,pe, pch=c(rep("*",6),rep("+",6), rep("-",6)),
col=c(rep("green",6), rep("red",6),rep("blue",6)),
xlab="Dose", ylab="Proporcoes de insetos mortos")
plot(ldose,pe, pch=c(rep("*",6),rep("+",6), rep("-",6)),
col=c(rep("green",6), rep("red",6),rep("blue",6)),
xlab="Log(Dose)", ylab="Proporcoes de insetos mortos")

################################################################################
mod1<-glm(resp~1, family=binomial)
mod2<-glm(resp~d, family=binomial)
mod3<-glm(resp~insecticid, family=binomial)
mod4<-glm(resp~insecticid+ldose-1, family=binomial)
anova(mod1, mod2, mod3, mod4, test="Chisq")
summary(mod4, test="Chisq")
#################################################################################
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


















