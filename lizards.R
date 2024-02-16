# 2 by 2 table - lizard habitat data
y <- c(61, 41, 73, 70)
height <- factor(c(1, 1, 2, 2))
diam <- factor(c(1, 2, 1, 2))
lizard.dat <- data.frame(height, diam, y)
attach(lizard.dat)

# calculate observed odds ratio

61*70/(41*73)

# now set up log linear model

mod1<-glm(y ~ height*diam, family=poisson)
anova(mod1, test="Chisq")
summary(mod1)

# note that this model reproduces the data also the fitted log-odds ratio 
is 0.3553 giving a fitted odds-ratio of

exp(mod1$coef[4])

# the interaction term is not significant, so we cannot reject the hypothesis 
that the odds-ratio is 1, i.e. diameter and height are independent refit 
simplest adequate model 
the main effects, or independence, model

mod2<-glm(y ~ height + diam, family=poisson)
anova(mod2, test="Chisq")
summary(mod2)