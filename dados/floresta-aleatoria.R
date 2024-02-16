dados <- read.csv("dados.csv", header = TRUE, sep = ";")
str(dados)
library(ggplot2)
library(stringr)
library(class)
dados$TicketMédio <- str_replace(dados$TicketMédio,",","\\.")
dados$TicketMédio <- as.numeric(dados$TicketMédio)
dados1 <- dados[,-c(1:5,11,12,15)]
str(dados1)
dados1$churn <- as.factor(dados1$churn)
n <- 0.8*nrow(dados)

dados <- dados[sample(nrow(dados)),]
treino <- dados[1:n,]
teste <- dados[-(1:n),]


ggplot(dados1, aes(y = Frequencia.Meses.Aportados,x = churn)) +
  geom_boxplot()


library(rpart)
library(rpart.plot)

modeloarvore <- rpart(formula = churn~dias.primeiro.e.ultimo.cash.in, data = treino, method = "class")
rpart.plot(modeloarvore, extra = 101)

mean(predict(modeloarvore, newdata = teste, type = "class")==teste$churn)
matrizarvore <- table(teste$churn, predict(modeloarvore, newdata = teste, type = "class"))
matrizarvore

library(corrplot)
correlacao <- cor(dados1)
corrplot(correlacao, method = "circle")

library(randomForest)
modelofloresta <- randomForest(churn~dias.primeiro.e.ultimo.cash.in, treino)
modelofloresta

mean(predict(modelofloresta, newdata = teste, type = "class")==teste$churn)

names(dados)
