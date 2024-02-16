library(readxl)
dados = read_excel("P2_2022_2-Pratica.xlsx")
dados

dados_eu <- dist(dados, method = "euclidean")
dados_eu


mod = hclust(dados_eu, method = "average")
plot(mod)

dados$cluster = cutree(mod, 5)
dados

media = aggregate(dados, list(dados$cluster), mean)
media

dados2 = read_excel("P2_2022_2-Pratica.xlsx", sheet = "Questão 2")
dados2$Grupo <- as.factor(dados2$Grupo)
dados2

modre <- lda(Grupo ~ X1 + X2, data = dados2)
modcv <- lda(Grupo ~ X1 + X2, data = dados2, CV = T)
modre
modcv

# Matriz de confusao (validacao cruzada)
tabela1 <- xtabs(~ modcv$class + dados2$Grupo, data = dados2)
tabela1

# Acerto (%) 
diag(tabela1) / colSums(tabela1) * 100

# Acerto global (%) 
sum(diag(tabela1)) / 24 * 100

# Matriz de confusão (ressubstituição)
modreclass <- predict(modre, dados2)$class
tabela2 <- xtabs(~ modreclass + dados2$Grupo, data = dados2)
tabela2


# Acerto (%) 
diag(tabela2) / colSums(tabela2) * 100

# Acerto global (%) 
sum(diag(tabela2)) / 24 * 100

# Escores das observações
FD <- as.matrix(dados2[, -c(1)]) %*% coef(modre)
dim(FD)

# Escores medios
(FDb <- modre$means %*% coef(modre))

stripchart(FD[, 1] ~ Grupo, pch = 20, xlab = "Função discriminante",
           ylab = "Grupo", method = "stack", data = dados2)
points(FDb[, 1], (1:length(modre$lev)) + 0.05, pch = 13, cex = 1.5)


# Histograma de FD1
ldahist(FD[, 1], dados2$Grupo)

