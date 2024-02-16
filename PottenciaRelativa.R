
dose = c(2.00,2.64,3.48,4.59,6.06,8.00)
dose

dose = c(rep(dose,3))
dose 

d = as.factor(dose)
d

ldose=log(dose)
ldose

y = c(3,5,19,19,24,35,2,14,20,27,41,40,28,37,46,48,48,50)
m = c(50,49,47,50,49,50,50,49,50,50,50,50,50,50,50,50,50,50)

insecticid= as.factor(c(rep("DDT",6),rep("BHC",6),rep("DDT+BHC",6)))
insecticid

Tribolium.dat = data.frame(dose, y, m, insecticid)
Tribolium.dat

attach(Tribolium.dat)

resp=cbind(y,m-y)

pe=y/m
pe

plot(dose,pe, pch=c(rep("*",6),rep("+",6), rep("-",6)),
     col=c(rep("green",6), rep("red",6),rep("blue",6)),
     xlab="Dose", ylab="Proporcoes de insetos mortos")
plot(ldose,pe, pch=c(rep("*",6),rep("+",6), rep("-",6)),
     col=c(rep("green",6), rep("red",6),rep("blue",6)),
     xlab="Log(Dose)", ylab="Proporcoes de insetos mortos")

################################################################################
mod1=glm(resp~1, family=binomial)
mod1
### O modelo 1 (resp ~ 1) é o modelo mais simples possível, sem variáveis preditoras. Ele simplesmente calcula 
## a proporção de insetos mortos, independentemente da dose ou tipo de inseticida utilizados. 
## Este modelo é usado como referência para comparar os outros modelos mais complexos.

mod2=glm(resp~d, family=binomial)
mod2
### O modelo 2 (resp ~ d) inclui a variável dose como um fator categórico. Ele testa se há diferenças significativas na 
## proporção de insetos mortos em diferentes níveis de dose. Isso permite avaliar se a eficácia do inseticida varia com a dose, 
## o que pode ser importante para determinar a dose ideal a ser utilizada.

mod3=glm(resp~insecticid, family=binomial)
mod3
### O modelo 3 (resp ~ insecticid) inclui a variável insecticida como um fator categórico. Ele testa se há diferenças significativas na 
## proporção de insetos mortos entre os diferentes tipos de inseticida. Isso permite avaliar se há diferenças na eficácia entre diferentes
## tipos de inseticidas e se um tipo é significativamente mais eficaz do que outro.


mod4=glm(resp~insecticid+ldose-1, family=binomial)
mod4
### O modelo 4 (resp ~ insecticid + ldose - 1) inclui a variável insecticida e a variável ldose (logaritmo da dose) como fatores categóricos.
## Ele testa se há diferenças significativas na proporção de insetos mortos entre os diferentes tipos de inseticida em diferentes níveis de dose, 
## sem incluir uma constante na equação. Este modelo permite avaliar se a eficácia do inseticida varia entre os diferentes tipos de inseticida 
## em diferentes níveis de dose, e se há uma interação significativa entre o tipo de inseticida e a dose. Este modelo também permite comparar 
## as diferenças entre os tipos de inseticidas em diferentes níveis de dose, o que pode ser útil para selecionar o inseticida mais eficaz em 
## diferentes circunstâncias.


anova(mod1, mod2, mod3, mod4, test="Chisq")
summary(mod4, test="Chisq")


#################################################################################
plot(c(1.8,8), c(0,1), type="n", xlab="dose", ylab="proporcoes", log="x")
points(dose,pe, pch=c(rep("*",6),rep("+",6), rep("-",6)),
       col=c(rep("green",6), rep("red",6),rep("blue",6)))
ld=seq(log(2),log(8),0.005)
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


DL50per = -mod4$coefficients[2]/mod4$coefficients[4]
DL50per
### O modelo é capaz de prever a proporção de insetos mortos em relação à dose do inseticida.
## Ao dividir o coeficiente associado à variável "ldose" pelo coeficiente associado à variável "insecticidDDT" no modelo 4,
## é possível obter o valor de DL50per, que representa a dose letal de DDT necessária para matar 50% dos insetos.
## Portanto, o valor de DL50per de 1.689781 significa que, em média, a dose letal de DDT necessária para matar 50% dos insetos é
## de 1.689781 unidades na escala logarítmica de dose.



p1 = exp((mod4$coefficients[3]+-mod4$coefficients[2])/mod4$coefficients[4])
p1
## = (-1.424756 + 4.5555258) / 2.695768
## O valor de p1, que é igual a 3.193972, representa as potências relativas da mistura do inseticida DDT e BHC em relação somente ao inseticida DDT. 
## Isso significa que a mistura é 3,19 vezes mais potente em relação ao inseticida DDT sozinho.

p2 = exp((mod4$coefficients[3]+-mod4$coefficients[1])/mod4$coefficients[4])
p2
## = (-1.424756 + 3.842467) / 2.695768
## O valor de p2, que é igual a 2.451878, representa as potências relativas da mistura do inseticida DDT e BHC em relação somente ao inseticida BHC. 
## Isso significa que a mistura é 2,45 vezes mais potente em relação ao inseticida BHC sozinho.




____________________________________________________________________________________________________________________________________________________
### TRABALHO 

dose = c(0.41,0.58,0.71,0.89,1.01,0.71,1,1.31,1.48,1.61,1.7,0.4,0.71,1,1.18,1.31,1.4)
d = as.factor(dose)
ldose = log(dose)
ldose
y = c(6,16,24,42,44,16,18,34,47,47,48,7,22,27,38,43,48)
m = c(50,48,46,49,50,49,48,48,49,50,48,47,46,46,48,46,50)

insecticid = as.factor(c(rep("R",5),rep("D",6),rep("M",6)))

dados = data.frame(dose, y, m, insecticid)
dados

head(dados)

resp = cbind(y,m-y)
resp

pe = y/m
pe

plot(dose,pe, pch=c(rep("*",5),rep("+",6), rep("-",6)), col=c(rep("green",5), rep("red",6),rep("blue",6)),
     xlab="Dose", ylab="Proporcoes de insetos mortos")

############ Modelos:
mod1 = glm(resp~1, family=binomial)
mod2 = glm(resp~d, family=binomial)
mod3 = glm(resp~insecticid, family=binomial)
mod4 = glm(resp~insecticid+dose-1, family=binomial)

anova(mod1, mod2, mod3, mod4, test="Chisq")
summary(mod4, test="Chisq")

p1 = exp((mod4$coefficients[2]+-mod4$coefficients[1])/mod4$coefficients[4])
p1


p2 = exp((mod4$coefficients[2]+-mod4$coefficients[3])/mod4$coefficients[4])
p2
