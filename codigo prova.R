install.packages("readxl")

library(readxl)

dados <- read_excel("P1_Pratica_Q1.xlsx")
dados <- dados[,-1]

#questao a

xbarra <- colMeans(dados)
n <- nrow(dados)
p=3

di=NULL
for (i in 1:n) {
  xc <- as.numeric(dados[i,]-xbarra)
  d <- t(xc)%*%solve(S)%*%xc
  di<-c(di,d)
}

di

j <- seq(1:n)


q <- qchisq((j-0.5)/n,df=3,lower.tail = TRUE)

plot(sort(di), q, xlab = "distancias", ylab = "Percentil Qui-Quadrado")

#como podemos ver nosso grafico segue uma linha portanto podemos assumir que ele segue uma distribuição normal multivariada

#questão b

xbarra <- colMeans(dados)
S <- cov(dados)
R <- cor(dados)

#a variavel 6 (400m) é a com menor correlação com as demais, ja as outras estão mais equilibradas, tirando uma ou outra variavel que esta altamente correlacionada

#questao c

require(psych)
cortest.bartlett(cor(dados), n=54)

#como temos um p-valor muito baixo e um qui-quadrado alto temos uma variancia diferente entre as amostras

#questao d

acp1 <- princomp(dados, cor= T)
summary(acp1)
#como observado a primeira componente é a mais explicativa visto a alta proporção da variancia e com ela podemos explicar 83% dos dados
#podemos usar 3 componetes para explicar 94% dos dados

#questao e

acp <- princomp(dados, cor= F)
summary(acp)

#como observado a primeira componente é a mais explicativa visto a alta proporção da variancia
#podemos utilizar so 1 componente principal e assim explicar 98% dos dados

#questão f

#devemos utilizar a matriz de Covariância ja que ela explica melhor os dados com menos variaveis

#questao g

biplot(princomp(dados, cor=FALSE))

#podemos observar que a maioria das setas apontam pra mesma direção, portanto apresentam uma correlação alta.
#a unica variavel que difere um pouco das demais é a 400m, que não possui uma correlação tão alta assim
#observações como a 21 e a 34 possuem uma correlação negativa, ja 21x46 não apresentam correlação pois estão em 90 graus uma em relação a outra
#já a grande maioria possui uma alta correlação pois estão todas juntas em um conglomerado
