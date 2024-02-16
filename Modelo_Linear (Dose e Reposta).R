### Nível de dose:
dose = c(0,2.6,3.8,5.1,7.7,10.2)

### Número de insetos mortos em cada nível de dose:
y = c(0,6,16,24,42,44)

### Número total de insetos em cada nível de dose. 
m = c(49,50,48,46,49,50)

Rotenone.dat = data.frame(dose,y,m)

### Possibilidade de acessar as colunas diretamente pelo nome.
attach(Rotenone.dat)

### Gráfico de dispersão, com a proporção de insetos mortos (y/m) no eixo y e a dose 
### no eixo x.
plot(dose,y/m,xlab = "Dose",ylab = "Observed proportions", pch="*")

### Matriz trazendo o número de insetos mortos e o número de insetos vivos ((m-y),isto é, total - mortos)
resp = cbind(y,m-y)
resp

### Modelo nulo: Considera que a proporção de insetos mortos é constante em todas
## as doses testadas e não há relação com a dose do inseticida.
Rotenon1 = glm(resp~1,family = "binomial")

### Modelo Regressão Linear: Inclui a variável dose como preditora da probabilidade de sucesso (Incetos mortos).
## A probabilidade de sucesso aumento ou diminui de forma constante com o aumento da dose de inseticida.
Rotenon2 = glm(resp~dose, family = "binomial")


### Note que o Modelo nulo, apresentou um AIC de 184.96 e uma deviance de 163,74.
summary(Rotenon1)

### O modelo de regressão linear apresentou um AIC de 33,479 e um residual deviance de 10.258.
summary(Rotenon2)

### A análise de variância abaixo, mostra as informações acima, porém traz, outra conclusão legal:
## "Pr(>Chi)", apresenta o valor p do teste de qui-quadrado para comparar os dois modelos. Esse valor p
## é uma medida da significância estatística da diferença na deviance entre os modelos. No caso, o valor p 
## é extremamente baixo (menor que 2.2e-16), indicando que a diferença na deviance é altamente significativa.
## Portanto, podemos concluir que o modelo de regressão linear é significativamente melhor do que o modelo nulo 
## em explicar as variações na proporção de insetos mortos, e que a inclusão da variável dose no modelo é importante para o ajuste adequado aos dados.
anova(Rotenon1,Rotenon2,test = "Chisq")


plot(c(0,10.2),c(0,1),type = "n", xlab = "dose", ylab = "Proportions")
points(dose,y/m,pch="*")
x = seq(0,10.2,0.2)

### Gerando valores preditos para a variável resposta, a proporção de insetos mortos, para diferentes valores da variável dose.
lp = predict(Rotenon2,data.frame(dose=x))
lp


### O resultado do código é um vetor com as probabilidades de sucesso para cada dose de pesticida avaliada. 
## Interpretando o resultado, podemos dizer que a probabilidade de um inseto morrer aumenta conforme a dose de pesticida aumenta. 
## Por exemplo, para uma dose de 1, a probabilidade de sucesso é de aproximadamente 4%, enquanto que para uma dose de 50, a probabilidade de sucesso é de aproximadamente 89%.
pe = exp(lp)/(1+exp(lp))
pe


###  A curva de probabilidade estimada de mortalidade em relação à dose de um determinado pesticida.
lines(x,pe,lty=1)
title(sub = "Observed values and fitted curve")


library(MASS)
#variance~covariance matrix of estimate parameters

## Matriz de convariância : O valor da covariância entre o intercepto e o coeficiente da dose é negativo 
## (-0.022661239), indicando que há uma relação inversa entre eles. Isso significa que, quando o valor da dose aumenta, o valor do intercepto tende a diminuir e vice-versa.
vcov(Rotenon2)


#doses ld50 and ld90
dose.p(Rotenon2)
dose.p(Rotenon2, p = 0.9)
#doses ld25 ld50 e ld75
dose.p(Rotenon2,p=1:3/4)

### Sim, de acordo com os resultados apresentados, a dose estimada para obter uma probabilidade de mortalidade de 25% é de 3,515057, com um erro padrão de 0,3062646.

___________________________________________________________________________________________________________________________________________________________________________________________ 

############# MENINAS MENSTRUADAS ##############
setwd("C:\\Users\\guilh\\OneDrive\\Documentos")
getwd()

### Importação dos dados:
meninas <- read.csv("meninas.dat.txt", sep="")
meninas

attach(meninas)

### Resumo Estatístico: 
summary(meninas)


### dose =  O que estamos querendo administrar, nesse caso estamos interessados em saber
### qual é a idade média em que as meninas da Varsóvia menstruaram.Portanto, o que procuramos é

### a idade é nossa dose.
dose = meninas$Idade 
### menstruadas =  A resposta que estamos buscando
yi = meninas$Mesntruadas
### Total por classe
mi = meninas$Entrevistadas


plot(dose,yi/mi,xlab = "dose",ylab = "Prop. Observada",pch = "*")
resp = cbind(yi,mi-yi)
resp
################################################################################
mod1 = glm(resp~1, family=binomial)
mod1

mod2 = glm(resp~dose, family=binomial)
mod2

anova(mod1,mod2,test="Chisq")
summary(mod2)

################################################################################
plot(c(8,18),c(0,1), type ="n" , xlab="dose" , ylab="Proporcao")
plot(c(9,18),c(0,1), type ="n" , xlab="dose" , ylab="Proporcao")
points(dose,yi/mi,pch="*")
x=seq(8,18,0.2)
lp=predict(mod2,data.frame(dose=x))
lp
pe=exp(lp) / (1+exp(lp))
pe
lines(x,pe,lty=1)
title(sub="Observed values and fitted curve")

################################################################################
library(MASS)
# variance-covariance matrix of estimated parameters
vcov(mod2)

### Doses LD50 : Indica que estamos buscando a dose(idade) em que a proporção esperada de meninas menstruadas
# é 50%. Em outras palavras, estamos procurando a idade em que a metade das meninas deveria ter iniciado a menstruação.
dose.p(mod2,p=0.5)

### O modelo previu uma idade mediana de início do período de menstruação é de de 13 anos,assossiado a um erro padrão baixo de 3%.



