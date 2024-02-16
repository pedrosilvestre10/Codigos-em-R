library(ggplot2)
library(stringr)
library(rpart)
library(rpart.plot)

dados <- read.csv("weatherAUS.csv", header = TRUE)
str(dados)
summary(dados)

str_view(dados$Date[1:5], "\\d{4}-|-\\d{2}")
dados$Date <- str_replace_all(dados$Date, pattern = "\\d{4}-|-\\d{2}", "")
dados <- dados[,-c(6,7)]

n <- round(nrow(dados)*0.8)

dados <- dados[sample(nrow(dados)),]

treinamento <- dados[1:n,]
teste <- dados[-(1:n),]

arvore.chuva <- rpart(formula = RainTomorrow~., data = treinamento)
rpart.plot(arvore.chuva, extra = 101)

previsao <- predict(arvore.chuva, type = "class", newdata = teste)

sum(is.na(previsao))
mean(previsao==teste$RainTomorrow, na.rm = TRUE)
