dados <- read.csv("winequality-white.csv", sep=";", header=TRUE)
head(dados)

#adicionando coluna de qualidade bom se maior que 5 e removendo a coluna de qualidade
dados$qualidade <- ifelse(dados$quality > 5, "bom", "ruim")
dados <- dados[,-12]

#removendo a coluna de densidade pois ela não tem correlação com a qualidade
dados <- dados[,-8]

str(dados)
dados$qualidade <- as.factor(dados$qualidade)

n <- 0.8*nrow(dados)

dados <- dados[sample(nrow(dados)),]
treino <- dados[1:n,]
teste <- dados[-(1:n),]

# Arvore de decisao
library(rpart)
library(rpart.plot)

modeloarvore <- rpart(formula = qualidade~ ., data = treino, method = "class")
rpart.plot(modeloarvore, extra = 101)

mean(predict(modeloarvore, newdata = teste, type = "class")==teste$qualidade)
table(predict(modeloarvore, newdata = teste, type = "class"), teste$qualidade)

#chegamos a acuracia de 0.7561224 com o modelo de arvore de decisao

#floresta aleatoria
library(randomForest)

modelofloresta <- randomForest(qualidade~., treino)
modelofloresta

mean(predict(modelofloresta, newdata = teste, type = "class")==teste$qualidade)

#chegamos a acuracia de 0.8479592 com o modelo de floresta aleatoria, assim podemos
#concluir que o modelo de floresta aleatoria é melhor que o de arvore de decisao
#já que além da acuracia ser maior tambem temos uma menor sensibilidade e especificidade
#de acordo com as tabelas de confusão


library(ggplot2)

ggplot(dados, aes(x = density, y = qualidade)) + geom_bar()
