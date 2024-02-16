dados <- read.csv("Breast_cancer_data.csv", header = TRUE)

str(dados)
dados$diagnosis <- as.factor(dados$diagnosis)


head(dados)
table(dados$diagnosis)
colnames(dados)

dados <- dados[sample(nrow(dados)),]
head(dados)

n <- round(0.8*nrow(dados))
n

treino <- dados[1:n,]
teste <- dados[-(1:n),]


library(class)

#num de pastas
k <- n/5 

indices <- seq(0, n, by = k)

#calculando as medidas de cada k
medida_modelo <- c()
for (k in 1:15) {
  for (j in 1:5) {
    teste_cross <- treino[(indices[j]+1):(indices[j+1]),]
    treino_cross <- treino[-((indices[j]+1):(indices[j+1])),]
    modelo.cv <- knn(train = scale(treino_cross[,-6]), test = scale(teste_cross[,-6]), k, cl = treino_cross$diagnosis)
    medidas[j] <- mean(modelo.cv == teste_cross$diagnosis)
  }
  medida_modelo[k] <- mean(medidas)
}

#data frame com os valores de k e suas respectivas acurácias
dados <- data.frame(k = 1:15, medida_modelo)
dados

#grafico de comparação da acurácia dos k's 
plot(x = 1:15, y = dados$medida.final, type = "l")


library(dplyr)

#ordenando os k's em ordem decrescente de acurácia
dados_ordenados <- arrange(dados, desc(medida_modelo))

#modelo final com o melhor k
modelo_final <- knn(train = treino[,-6], test = teste[,-6], cl = treino$diagnosis, k = dados_ordenados$k[1])

#acurácia do modelo final
mean(modelo_final == teste$diagnosis)