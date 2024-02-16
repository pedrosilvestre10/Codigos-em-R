dados <- read.csv("data.csv")
str(dados)

dados <- dados[,2:32]
head(dados)
str(dados)

dados$diagnosis <- as.factor(dados$diagnosis)

n <- 0.8*nrow(dados)

dados <- dados[sample(nrow(dados)),]
treino <- dados[1:n,]
teste <- dados[-(1:n),]

library(ggplot2)

ggplot(data = treino, aes(x= diagnosis))+
  geom_bar()

ggplot(data = treino, aes(x = radius_mean, y = diagnosis))+
  geom_boxplot()+
  theme_minimal()

#--------------------------------------knn-----------------------------------

library(class)

indices <- seq(0, n, by = n/10)

#calculando as medidas de cada k com 5 pastas
medida_mod <- c()
medidas <- c()

for (k in 1:30) {
  for (j in 1:10) {
    a <- (indices[j]+1)
    b <- (indices[j+1])
    teste_cross <- treino[(a:b),]
    treino_cross <- treino[-(a:b),]
    mod_cv <- knn(train = scale(treino_cross[,-1]), test = scale(teste_cross[,-1]), k, cl = treino_cross$diagnosis)
    medidas[j] <- mean(mod_cv == teste_cross$diagnosis)
  }
  medida_mod[k] <- mean(medidas)
}

#data frame com os valores de k e suas respectivas acurácias
acuracias <- data.frame(k = 1:30, medida_mod)
acuracias

#grafico de comparação da acurácia dos k's 
plot(x = 1:30, y = acuracias$medida, type = "l")


library(dplyr)

# escolhendo o melhor k
melhor_k <- which.max(acuracias$medida_mod)

#modelo final com o melhor k
modelo_final <- knn(train = treino[,-1], test = teste[,-1], cl = treino$diagnosis, k = melhor_k)

#acurácia do modelo final
mean(modelo_final == teste$diagnosis)

#matriz de confusão
matriz <- table(teste$diagnosis, modelo_final)
matriz


#-----------------arvore de decisão-----------------------------------------------

library(rpart)
library(rpart.plot)
library(tree)

#pacote rplot

modeloarvore <- rpart(formula = diagnosis~., data = treino, method = "class")
rpart.plot(modeloarvore, extra = 101)

mean(predict(modeloarvore, newdata = teste, type = "class")==teste$diagnosis)
matrizarvore <- table(teste$diagnosis, predict(modeloarvore, newdata = teste, type = "class"))
matrizarvore

#pacote tree

tree <- tree(data = treino, formula = diagnosis~., split = "gini")
plot(tree)
text(tree, pretty = 0)

previsao.cancer <- predict(tree, newdata = teste, type = "class")
mean(previsao.cancer==teste$diagnosis)

cv.cancer <- cv.tree(tree, FUN = prune.misclass)
cv.cancer

arvore.podada <- prune.misclass(tree, best = 10)
previsao.podagem <- predict(arvore.podada, newdata = teste, type = "class")
mean(previsao.podagem == teste$diagnosis)

#conseguimos um modelo com um dev menor usando a podagem e obtivemos uma acurácia igual 

#-----------------floresta aleatoria--------------------------------------

library(randomForest)
modelofloresta <- randomForest(diagnosis~., treino, importance = TRUE)
modelofloresta

importance(modelofloresta)
mean(predict(modelofloresta, newdata = teste, type = "class")==teste$diagnosis)
varImpPlot(modelofloresta)
