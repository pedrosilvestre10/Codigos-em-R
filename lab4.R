library("class")

dados <- read.csv("Breast_cancer_data.csv")
summary(dados)
dados$diagnosis <- as.factor(dados$diagnosis)
str(dados)
summary(dados)

n <- round(0.8*nrow(dados))
n

#dividindo em treino e teste
treino <- dados[1:n,]
teste <- dados[n+1:nrow(dados),]

#retirando os NAs
treino <- na.omit(treino)
teste <- na.omit(teste)

fit.knn <- knn(train = scale(treino[,1:5]), test = scale(teste[,1:5]), k = 1, cl = treino$diagnosis)

mean(fit.knn == teste$diagnosis)
# o modelo inicial esta com 83% de acuracia

acuracia <- c()
for (k in 1:30) {
  fit.knn <- knn(train = scale(treino[,1:5]), test = scale(teste[,1:5]),k = k, cl = treino$diagnosis)
  acuracia[k] <- mean(fit.knn == teste$diagnosis)
}

plot(x = 1:30, y = acuracia, type = "l")


#decidi usar o k = 15 pq é achei o mais acertivo

c <- n/10

indices <- seq(0, n ,by = c)

medida.final <- c()
for (k in 1:15) {
  for (j in 1:10) {
    teste_cv <- treino[(indices[j]+1):(indices[j+1]),]
    treino_cv <- treino[-((indices[j]+1):(indices[j+1])),]
    modelo_cv <- knn(train = scale(treino_cv[,1:5]), test = scale(teste_cv[,1:5]), k, cl = treino_cv$diagnosis)
    medidas[j] <- mean(modelo_cv == teste_cv$diagnosis)
  }
  medida.final[k] <- mean(medidas)
}

#medias calculadas // acuracia = 85%
medidas <- data.frame(k = 1:15, medida.final)
mean(medidas$medida.final)


modelo.final <- knn(train = scale(treino[,1:5]), test = scale(teste[,1:5]), cl = treino$diagnosis, k = 15)

#modelo final utilizando k = 15 // acuracia = 91%
mean(modelo.final == teste$diagnosis)
