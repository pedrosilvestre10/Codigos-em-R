library(ggplot2)

dados <- read.table("protein.txt", header = TRUE, sep = "\t")
str(dados)

dados_padronizados <- scale(dados[,2:ncol(dados)])

clusterizacao1 <- kmeans(dados_padronizados, centers = 5,iter.max = 20, nstart = 50)
clusterizacao1

dados$cluster <- as.factor(clusterizacao1$cluster)

cluster1 <- dados[dados$cluster == 1,1]
cluster2 <- dados[dados$cluster == 2,1]
cluster3 <- dados[dados$cluster == 3,1]
cluster4 <- dados[dados$cluster == 4,1]
cluster5 <- dados[dados$cluster == 5,1]

cluster1
cluster2
cluster3
cluster4
cluster5

plot(dados$cluster, dados$Fish, col = clusterizacao1$cluster, pch = 20, main = "Clusterização de Proteínas", xlab = "Cluster", ylab = "Proteína")
ggplot(dados, aes(x = cluster, y = Fish, fill = cluster)) + geom_boxplot() + ggtitle("Clusterização de Proteínas") + xlab("Cluster") + ylab("Proteína")

