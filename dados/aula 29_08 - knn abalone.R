#1-5 jovem
#6-13 adulto
#maior 13 velho


abalone <- read.csv("abalone.csv", header = TRUE)

str(abalone)
abalone$Sex <- as.factor(abalone$Sex)

summary(abalone)

Age <- c()

for (i in 1:nrow(abalone)) {
  if (abalone$Rings[i] <= 5){
    Age[i] <- "young"
  }
  else if (abalone$Rings[i] <= 13){
    Age[i] <- "adult"
  }else{
    Age[i] <- "old"
  }
}

abalone$Age <- as.factor(Age)
str(abalone)


#mudar nome das colunas
colnames(abalone)[1] <- "Sex"
head(abalone)
#retirando coluna rings
abalone <- abalone[,-9]
str(abalone)

library(ggplot2)


ggplot(data = abalone, aes(x = Age))+
  geom_bar()+
  facet_wrap(~Sex)

ggplot(data = abalone, aes(x = Diameter, fill = Sex))+
  geom_histogram(bins=20, alpha = 0.5) # position = "dodge" 


ggplot(data = abalone, aes(x = Viscera.weight, fill = Sex))+
  geom_histogram(bins=20, alpha = 0.5)


n <- round(0.8*nrow(abalone))
n
abalone

abalone <- abalone[sample(nrow(abalone)),]
abalone <- abalone[,-1]

treino_abalone <- abalone[1:n,]
teste_abalone <- abalone[-(1:n),]

str(teste_abalone)


boxplot(treino_abalone[,-8])

x <- scale(treino_abalone[,-8])
library(class)

abalone_knn <- knn(train = scale(treino_abalone[,-8]),
                   test =scale(teste_abalone[,-8]),
                   k = 1,
                   cl = treino_abalone$Age)

abalone_knn

mean(abalone_knn == teste_abalone$Age)


acuracia <- c()
for (k in 1:30) {
  fit.knn <- knn(train = treino_abalone[,-8], teste_abalone[,-8], cl = treino_abalone$Age, k)
  acuracia[k] <- mean(fit.knn == teste_abalone$Age)
}

plot(x = 1:30, y = acuracia, type = "l")


n <- 3340
treino_abalone <- abalone[1:n,]
teste_abalone <- abalone[-(1:n),]

k <- n/10

indices <- seq(0, 3340,by = 334)

medidas <- c()

for (k in 1:10) {
  teste.cv <- treino_abalone[(indices[k]+1):(indices[k+1]),]
  treino.cv <- treino_abalone[-((indices[k]+1):(indices[k+1])),]
  modelo.cv <- knn(train = scale(treino.cv[,-8]), test = scale(teste.cv[,-8]), k = 2, cl = treino.cv$Age)
  medidas[k] <- mean(modelo.cv == teste.cv$Age)
}
?knn
medida.final <- c()
for (k in 1:15) {
  for (j in 1:10) {
    teste.cv <- treino_abalone[(indices[j]+1):(indices[j+1]),]
    treino.cv <- treino_abalone[-((indices[j]+1):(indices[j+1])),]
    modelo.cv <- knn(train = scale(treino.cv[,-8]), test = scale(teste.cv[,-8]), k, cl = treino.cv$Age)
    medidas[j] <- mean(modelo.cv == teste.cv$Age)
  }
  medida.final[k] <- mean(medidas)
}

dados <- data.frame(k = 1:15, medida.final)
dados



modelo.final <- knn(train = treino_abalone[,-8], test = teste_abalone[,-8], cl = treino_abalone$Age, k = 11)

mean(modelo.final == teste_abalone$Age)

