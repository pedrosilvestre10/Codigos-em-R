library(ggplot2)
library(stringr)
library(class)

dados <- read.csv("papagaio.csv")
str(dados)
summary(dados)
c <- dados$especie_papagaio

c <- str_replace(c,"^\\.F\\d\\d$|^\\.\\.F\\d$","arctica")#arctica
c <- str_replace(c,"^\\.\\.F\\d\\d$","cirrhata")#cirrhata
c <- str_replace(c,"^\\.F\\d\\d\\d$","corniculata")#corniculata

dados$nome_especie <- c

dados$peso <- str_remove(dados$peso, "kg")
dados

dados$peso <- as.numeric(dados$peso)

ggplot(data = dados,mapping = aes(x = peso, y = envergadura,color = nome_especie))+
  geom_jitter()

ggplot(data = dados,mapping = aes(x = peso, y = comprimento,color = nome_especie))+
  geom_jitter()

dados <- dados[,-5]
dados$nome_especie <- as.factor(dados$nome_especie)
str(dados)

n <- round(0.8*nrow(dados))
n 
dados <- dados[sample(nrow(dados)),]
treino.papagaio <- dados[1:n,]
teste.papagaio <- dados[-(1:n),]

papagaio.knn <- knn(train = scale(treino.papagaio[,2:4]), test = scale(teste.papagaio[,2:4]), k = 1, cl = treino.papagaio$nome_especie)
papagaio.knn 
mean(papagaio.knn == teste.papagaio$nome_especie)

indices <- seq(from = 0, to = 480, by = 16)
indices

medidas <- c()
for (j in 1:30){
  teste.cross_validation <- treino.papagaio[(indices[j]+1):(indices[j+1]),]
  treino.cv <- treino.papagaio[-((indices[j]+1):(indices[j+1])),]
  modelo.cv <- knn(train = scale(treino.cv[,2:4]), test = scale(teste.cross_validation[,2:4]), cl = treino.papagaio$nome_especie)
  medidas[j] <- mean(modelo.cv == teste.cross_validation$nome_especie)
}

# usando k=1 cheguei a acuracia de 0.9333333 mas não consegui identificar o melhor k pois não consegui plotar o grafico nem criar o vetor de medidas


