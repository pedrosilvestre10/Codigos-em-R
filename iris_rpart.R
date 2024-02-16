library(ggplot2)
library(rpart)
library(rpart.plot)

ggplot(data = teste.iris,mapping = aes(x = Petal.Length, y = Petal.Width,color = Species))+
  geom_jitter()+
  labs(title = "Conjunto Iris",
       x = "Tamanho da petala",
       y = "Largura da petala")+
  scale_fill_discrete(name = "Especies", labels = c("Setosa", "Vercicolor", "Virginica"))

dados <- iris[sample(nrow(iris)),]
n <- round(0.8*nrow(dados))
treino.iris <- dados[1:n,]
teste.iris <- dados[(n+1):nrow(dados),]
classificacao <- c()

for (j in 1:nrow(teste.iris)) {
  if (teste.iris$Petal.Length[j] < 2.5){
    classificacao[j] <- "setosa"
  }else{
    if (teste.iris$Petal.Width[j] < 1.7) {
      classificacao[j] <- "versicolor"
    }else{
      classificacao[j] <- "virginica"
    }
  }
}

mean(teste.iris$Species == classificacao)
table(teste.iris$Species, classificacao)

arvore.iris <- rpart(formula = Species~., data = treino.iris)
rpart.plot(arvore.iris, extra = 101)
