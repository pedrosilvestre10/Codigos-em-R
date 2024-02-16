dados <- read.table("protein.txt", header = TRUE, sep = "\t")

row.names(dados) <- dados$Country

dados <- dados[, -1]

dados_padronizados <- scale(dados)

matriz_de_distancias <- dist(dados_padronizados, method = "euclidean")
matriz_de_distancias

modelo_hierarquico <- hclust(matriz_de_distancias, method = "ward.D2")

modelo_hierarquico

plot(modelo_hierarquico, hang = -1, labels = dados$Country)

rect.hclust(modelo_hierarquico, k = 5, border = "red")

grupos <- cutree(modelo_hierarquico, k = 5)

aglomerado1 <- dados[grupos == 1, ]
aglomerado2 <- dados[grupos == 2, ]
aglomerado3 <- dados[grupos == 3, ]
aglomerado4 <- dados[grupos == 4, ]
aglomerado5 <- dados[grupos == 5, ]

boxplot(aglomerado1$Cereals,
        aglomerado2$Cereals,
        aglomerado3$Cereals,
        aglomerado4$Cereals,
        aglomerado5$Cereals)
