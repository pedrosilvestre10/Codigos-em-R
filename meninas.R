---
title: ''
output: pdf_document
---
Os problemas de dose-resposta não se esgotam em Toxicologia. Milecer e Szczotka (1966) investigam a
idade do início da menstruação em 3918 garotas de Varsóvia. Para 25 médias de idade foram observadas a
ocorrência (Y = 1) ou não (Y = 0) do início de períodos de menstruação nas adolescentes. Realizar as mesmas
análises da aula anterior e encontrar a estimativa da idade mediana de início do período de menstruação.

## Import dos dados e Pacotes
```{r}
library(MASS)
meninas <- read.csv("", sep="")
head(meninas)
```



```{r}
di <- meninas$Idade
yi <- meninas$Mesntruadas
mi <- meninas$Entrevistadas
resp <- cbind(yi,mi-yi)
plot(di,yi/mi,xlab = "dose",ylab = "Prop. Observada",pch = "*")
```
Graficamente observarmos que o gráfico de dispersão das proporções (mestruadas/entrevistadas) tem um
aspecto sigmóide

```{r}
M1 <- glm(resp~1,family = binomial) #modelo nulo
M2 <- glm(resp~di,family = binomial) #modelo corrente(reg. linear)
anova(M1,M2,test = "Chisq")
```
Vemos que o modelo 2(corrente),apresentou diferença significante em relação ao modelo nulo
```{r}
summary(M2)
```

```{r}
{
plot(c(9,18),c(0,1),type = "n",xlab = "dose",ylab = "Proporcoes")
points(di,yi/mi,pch = "*")
x <- seq(9,18,0.2)
lp <- predict(M2,data.frame(di=x))
pe <- exp(lp)/(1+exp(lp))
lines(x,pe,lty = 1)
title(sub = "valores observados e curva ajustada")
}
```

```{r}
vcov(M2)
dose.p(M2,p = 0.5);
```
## Conclusão
Dado os dados de 3918 meninas da Varsovia e suas respostas para idade de inicio demestruação, encontramos
entao a estimativa da idade mediana(50%), para o inicio do perido de mestruação para meninas da Vársovia
é de: 13 anos


