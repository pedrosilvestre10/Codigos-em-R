y <- c(6, 13, 18, 28, 52, 53, 61, 60)
m <- c(59, 60, 62, 56, 63, 59, 62, 60)
ldose <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
beetle.dat <- data.frame(ldose, m, y)
attach(beetle.dat)
resp<-cbind(y,m-y)
mod2<-glm(resp~ldose, family=binomial)
anova(mod2)
summary(mod2)
plot(c(1.69,1.89), c(0,1), type="n", xlab="dose", ylab="Proportions")
points(ldose,y/m,pch="*")
x<-seq(1.69,1.89,0.01)
lp<-predict(mod2,data.frame(ldose=x))
pe<-exp(lp)/(1+exp(lp))
lines(x,pe,lty=1)
##########################################################################
# Normal plot with simulated envelope
GLMBin<-glm(resp~ldose,family=binomial())
res<-sort(summary(GLMBin)$deviance.resid)
env<-quantile(res,c(.05,0.5,.95))
for (i in 1:999){
y.<-rbinom(rep(1,length(GLMBin$fit)),m, GLMBin$fit)
resp.<-cbind(y.,m-y.)
res<-cbind(res,sort(summary(glm(resp.~ldose,
family=binomial()))$deviance.resid))}
for (i in 1:nrow(res)){
env<-cbind(env,quantile(res[i,],c(.05,0.5,.95)))}
matplot(qnorm((1:nrow(res))/(nrow(res)+1)),cbind(t(env[,-1]),res[,1]),
col=c(1,1,1,1),pch=c(NULL,NULL,1),cex=.5,type=c("l","l","l","p"),lty=c(1,2,1,1),
xlab="quantile", ylab="deviance residuals")
##########################################################################
## Complementary log-log link function

mod3<-glm(resp ~ ldose, family=binomial(link="cloglog"))
anova(mod3)
summary(mod3<-glm(resp ~ ldose, family=binomial(link="cloglog")))
library(MASS)
# variance-covariance matrix of estimated parameters
vcov(mod3)
# Doses LD50 and LD90
dose.p(mod3); dose.p(mod3, p=0.9)
# Doses LD25, LD50, LD75
dose.p(mod3, p = 1:3/4)
# Plotting
plot(c(1.69,1.89), c(0,1), type="n", xlab="Log(dose)",
ylab="Proportion of beetles killed")
points(ldose, y/m, pch="*")
ld<-seq(1.69,1.89,0.01)
lines(ld, predict(mod2,data.frame(ldose=ld),type="response"), col="blue", lty=1)
lines(ld, predict(mod3,data.frame(ldose=ld),type="response"), col="red", lty=2)
legend(1.7,1.0,c("Observed","Logit", "Cloglog"), lty=c(-1,1,2),
pch=c("*"," "," "), col=c("black","blue", "red"))
#################################################################################
# Normal plot with simulated envelope
GLMBin<-glm(resp~ldose,family=binomial(link="cloglog"))
res<-sort(summary(GLMBin)$deviance.resid)
env<-quantile(res,c(.05,0.5,.95))
for (i in 1:999){
y.<-rbinom(rep(1,length(GLMBin$fit)),m, GLMBin$fit)
resp.<-cbind(y.,m-y.)
res<-cbind(res,sort(summary(glm(resp.~ldose,
family=binomial(link="cloglog")))$deviance.resid))}
for (i in 1:nrow(res)){
env<-cbind(env,quantile(res[i,],c(.05,0.5,.95)))}
matplot(qnorm((1:nrow(res))/(nrow(res)+1)),cbind(t(env[,-1]),res[,1]),
col=c(1,1,1,1),pch=c(NULL,NULL,1),cex=.5,type=c("l","l","l","p"),lty=c(1,2,1,1),
xlab="quantile", ylab="deviance residuals")


